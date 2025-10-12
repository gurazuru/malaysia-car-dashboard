library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(bslib)

setwd("F:/Users/fis/Documents/JPJ_Car_Viz")

# ---- Load data ----
# car_data <- readr::read_csv("data/car_data.csv") 

# ---- UI ----
ui <- page_fluid(
  titlePanel("Malaysia Total Industry Volume (Vehicle Registrations)"),
  
  tags$h5("This report provides an overview of vehicle registration trends in Malaysia, 
           using JPJ data from data.gov.my.",
          style = "color: #555; margin-top: -2px;"),
  
  tabsetPanel(
    tabPanel("By Make",
             layout_columns(
               col_widths = c(12),
               card(
                 card_header("Monthly Vehicle Registrations by Make"),
                 DTOutput("summary_table")
               ),
               card(
                 card_header("Monthly Registration Trend for Top Brands"),
                 
                 # user input here
                 div(
                   class = "mb-3", # margin bottom for spacing
                   selectInput(
                     inputId = "top_n",
                     label = "Show Top Brands:",
                     choices = c(5, 10, 15),
                     selected = 10
                   )
                 ),
                 
                 plotlyOutput("trend_plot_make")
               )
             )),
    tabPanel("By Model",
             layout_columns(
               col_widths = c(12),
               card(
                 card_header("Monthly Vehicle Registrations by Model"),
                 DTOutput("summary_table_model")
               ),
               card(
                 card_header("Monthly Registration Trend for Top Models"),
                 
                 # user input here
                 div(
                   class = "mb-3", # margin bottom for spacing
                   selectInput(
                     inputId = "top_n_model",
                     label = "Show Top Models:",
                     choices = c(5, 10, 15),
                     selected = 10
                   )
                 ),
                 
                 plotlyOutput("trend_plot_model")
               )
             )),
    tabPanel("By Fuel Type",
             layout_columns(
               col_widths = c(12),
               card(
                 card_header("Monthly Vehicle Registrations by Fuel Type"),
                 DTOutput("summary_table_fuel")
               ),
               card(
                 card_header("Monthly Registration Trend by Fuel Type"),
                 
                 # user input here
                 div(
                   class = "mb-3", # margin bottom for spacing
                   # selectInput(
                   #   inputId = "top_n_fuel",
                   #   label = "Show Top Brands:",
                   #   choices = c(5, 10, 15),
                   #   selected = 5
                   # )
                 ),
                 
                 plotlyOutput("trend_plot_fuel")
               )
             ))
  )
)



# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- Key Date Variables ----
  latest_date <- max(car_data$date_reg)
  
  year_current <- year(latest_date)
  ytd_current_start <- ymd(paste0(year_current, "-01-01"))
  ytd_current_end <- latest_date
  
  ytd_previous_start <- ytd_current_start %m-% years(1)
  ytd_previous_end <- ytd_current_end %m-% years(1)
  
  month_current <- latest_date
  month_previous <- month_current %m-% months(1)
  month_previous_year <- month_current %m-% months(12)
  
  # Define all the relevant dates.
  month_current_name <- format(month_current, "%b-%Y")
  month_previous_name <- format(month_previous, "%b-%Y")
  month_previous_year_name <- format(month_previous_year, "%b-%Y")
  year_current_name <- paste0("YTD ", format(ytd_current_end, "%Y"))
  year_previous_name <- paste0("YTD ", format(ytd_previous_end, "%Y"))
  
  # ---- Summary Function ----
  make_summary <- function(data, group_col, group_col_name) {
    group_col <- sym(group_col)  # convert string to symbol for tidy evaluation
    
    if (group_col == "model"){
      month_current_count <- car_data |>
        filter(date_reg == month_current) |>
        count(maker, !!group_col, fuel_grouped, name = "count_current")
      
      month_previous_count <- car_data |>
        filter(date_reg == month_previous) |>
        count(maker, !!group_col, fuel_grouped, name = "count_previous")
      
      month_previous_year_count <- car_data |>
        filter(date_reg == month_previous_year) |>
        count(maker, !!group_col, fuel_grouped, name = "count_previous_year")
      
      ytd_current_count <- car_data |>
        filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
        count(maker, !!group_col, fuel_grouped, name = "count_ytd_current")
      
      ytd_previous_count <- car_data |>
        filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
        count(maker, !!group_col, fuel_grouped, name = "count_ytd_previous")
      
      month_current_count |>
        full_join(month_previous_count, by = c("maker", rlang::as_string(group_col), "fuel_grouped")) |>
        full_join(month_previous_year_count, by = c("maker", rlang::as_string(group_col), "fuel_grouped")) |>
        full_join(ytd_current_count, by = c("maker", rlang::as_string(group_col), "fuel_grouped")) |>
        full_join(ytd_previous_count, by = c("maker", rlang::as_string(group_col), "fuel_grouped")) |>
        mutate(
          across(starts_with("count"), \(x) replace_na(x, 0)),
          growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
          growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
          growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_)
        ) |>
        arrange(desc(count_current)) |>
        mutate(rank = row_number()) |>
        select(rank, maker, !!group_col, fuel_grouped, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD) |>
        rename(
          `Rank` := rank,
          `Make` := maker,
          !!group_col_name := !!group_col,
          `Fuel Type` := fuel_grouped,
          !!month_current_name := count_current,
          !!month_previous_name := count_previous,
          `Growth (MoM)` := growth_MoM,
          !!month_previous_year_name := count_previous_year,
          `Growth (YoY)` := growth_YoY,
          !!year_current_name := count_ytd_current,
          !!year_previous_name := count_ytd_previous,
          `Growth (YTD)` := growth_YTD
        ) 
    } else {
      month_current_count <- car_data |>
        filter(date_reg == month_current) |>
        count(!!group_col, name = "count_current")
      
      month_previous_count <- car_data |>
        filter(date_reg == month_previous) |>
        count(!!group_col, name = "count_previous")
      
      month_previous_year_count <- car_data |>
        filter(date_reg == month_previous_year) |>
        count(!!group_col, name = "count_previous_year")
      
      ytd_current_count <- car_data |>
        filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
        count(!!group_col, name = "count_ytd_current")
      
      ytd_previous_count <- car_data |>
        filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
        count(!!group_col, name = "count_ytd_previous")
      
      month_current_count |>
        full_join(month_previous_count, by = rlang::as_string(group_col)) |>
        full_join(month_previous_year_count, by = rlang::as_string(group_col)) |>
        full_join(ytd_current_count, by = rlang::as_string(group_col)) |>
        full_join(ytd_previous_count, by = rlang::as_string(group_col)) |>
        mutate(
          across(starts_with("count"), \(x) replace_na(x, 0)),
          growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
          growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
          growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_)
        ) |>
        arrange(desc(count_current)) |>
        mutate(rank = row_number()) |>
        select(rank, !!group_col, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD) |>
        rename(
          `Rank` := rank,
          !!group_col_name := !!group_col,
          !!month_current_name := count_current,
          !!month_previous_name := count_previous,
          `Growth (MoM)` := growth_MoM,
          !!month_previous_year_name := count_previous_year,
          `Growth (YoY)` := growth_YoY,
          !!year_current_name := count_ytd_current,
          !!year_previous_name := count_ytd_previous,
          `Growth (YTD)` := growth_YTD
        ) 
      
    }
   
  }
  
  # ---- Output Data Table Function ----
  data_table <- function(df) {
    df <- df() 
    
    datatable(df,
              rownames = FALSE,
              filter = 'top',
              # caption = 'Monthly Vehicles Registrations by',
              options = list(pageLength = 10),
              class = 'cell-border stripe') |>
      formatPercentage("Growth (MoM)", 1) |>
      formatPercentage("Growth (YoY)", 1) |>
      formatPercentage("Growth (YTD)", 1) |>
      formatStyle("Growth (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YTD)", color = styleInterval(c(0), c('red', 'green')))
  }
  
  # ---- Output Plotly Function ----
  plot_chart <- function(df, group_col, group_col_name, top_n_value) {
    df <- df()
    if ({{ group_col_name }} == "Fuel Type") {
      top_items <- df |>
        slice_head(n = 5) |>
        pull({{ group_col_name }})
    } else {
      top_items <- df |>
        slice_head(n = as.numeric(top_n_value)) |> 
        pull({{ group_col_name }})
    }
    
    monthly_trends_data <- car_data |>
      filter({{ group_col }} %in% top_items) |>
      mutate(month = date_reg) |>
      count(month, {{ group_col }}, name = "registration")
    
    trend_plot <- ggplot(monthly_trends_data, aes(x = month, y = registration, color = {{ group_col }})) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 1.5) +
      theme_minimal(base_size = 14) +
      labs(x = "Month", y = "Registration", color = NULL) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(trend_plot)
  }
  
  # ---- Reactive summary ----
  summary_data <- reactive(make_summary(car_data, "maker", "Make"))
  summary_data_model <- reactive(make_summary(car_data, "model", "Model"))
  summary_data_fuel <- reactive(make_summary(car_data, "fuel_grouped", "Fuel Type"))
  
  # ---- Output 1: DataTable ----
  output$summary_table <- renderDT(data_table(summary_data))
  output$summary_table_model <- renderDT(data_table(summary_data_model))
  output$summary_table_fuel <- renderDT(data_table(summary_data_fuel))
  
  # ---- Output 2: Plotly Trend ----
  output$trend_plot_make <- renderPlotly(plot_chart(summary_data, maker, "Make", input$top_n))
  output$trend_plot_model <- renderPlotly(plot_chart(summary_data_model, model, "Model", input$top_n_model))
  output$trend_plot_fuel <- renderPlotly(plot_chart(summary_data_fuel, fuel_grouped, "Fuel Type", input$top_n_fuel))
}

shinyApp(ui, server)