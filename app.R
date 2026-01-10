
# ---- Load packages ----
library(shiny)
library(tidyverse)
library(zoo)
library(DT)
library(plotly)
library(forecast)
library(bslib)

# ---- Load and combine data ----
car_data <- readr::read_csv("data/car_data_sum.csv")

# --- get unique make models ---
model_list <- car_data |>
  group_by(maker, model, fuel_grouped) |>
  summarise(total_count = sum(count), .groups = "drop")

maker_list <- car_data |>
  group_by(maker) |>
  summarise(total_count = sum(count), .groups = "drop")

fuel_grouped_list <- car_data |>
  group_by(fuel_grouped) |>
  summarise(total_count = sum(count), .groups = "drop")

# ---- Key Date Variables ----
latest_date <- max(car_data$date_reg)
min_date <- latest_date %m-% years(5)  

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

# ---- Forecasting ----
# aggregate data to monthly data to get monthly TIV
tiv_monthly <- car_data |>
  group_by(date_reg = floor_date(date_reg, "month")) |>
  summarise(TIV = sum(count, na.rm = TRUE)) |>
  arrange(date_reg)

# convert to time series object
tiv_ts <- ts(tiv_monthly$TIV, start = c(2010, 1), frequency = 12)

# auto.arima to select the best seasonal ARIMA
fit <- auto.arima(tiv_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(fit)

# forecast the upcoming 12 months and bind back to the original monthly tiv data
forecast_12 <- forecast(fit, h = 12, level = c(70, 95))

# convert to dataframe
forecast_df <- tibble(
  date_reg = seq(max(tiv_monthly$date_reg) %m+% months(1),
                 by = "1 month", length.out = 12),
  TIV = as.numeric(forecast_12$mean),
  lower_70 = as.numeric(forecast_12$lower[, 1]),
  upper_70 = as.numeric(forecast_12$upper[, 1]),
  lower_95 = as.numeric(forecast_12$lower[, 2]),
  upper_95 = as.numeric(forecast_12$upper[, 2]),
  type = "Forecast"
  ) 

tiv_plot_df <- tiv_monthly |>
  mutate(type = "Actual") |>
  bind_rows(forecast_df)

# ---- UI ----
ui <- page_fluid(
  titlePanel("Malaysia Total Industry Volume (Vehicle Registrations)"),
  
  tags$h5("This report provides an overview of vehicle registration trends in Malaysia, 
           using JPJ data from data.gov.my.",
          style = "color: #555; margin-top: -2px;"),
  card(
    height = c(160),
    card_header("data as of 31st October 2025"),
    DTOutput("summary_table_total")
  ),
  tabsetPanel(
    id = "toptabs",
    tabPanel("TIV",
             layout_columns( col_widths = c(12),
                             card(
                               height = c(300),
                               card_header(
                                 div(
                                   style = "display: flex; align-items: center; justify-content: space-between;",
                                   "View as: ",
                                   selectInput(
                                     inputId = "agg_level_ttl",
                                     label = NULL,
                                     choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                                     selected = "Monthly"
                                   )
                                 ),
                               ),
                               plotlyOutput("trend_plot_total"),
                             )
              )
    ),
    tabPanel("Forecast",
             layout_columns( col_widths = c(12),
                             card(
                               height = c(300),
                               card_header(
                                 div(
                                 ),
                               ),
                               plotlyOutput("forecast_plot_total"),
                             )
             )
    )
  ),
  
  
  tabsetPanel(
    id = "tabs",
    tabPanel("By Make",
             layout_columns(
               col_widths = c(12),
               
               card(
                 card_header("Monthly Vehicle Registrations by Make"),
                 DTOutput("summary_table")
               ),
               card(
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "Monthly Registration Trend for Selected Makes",
                     selectInput(
                       inputId = "top_n",
                       label = NULL,
                       choices = c(5, 10, 15),
                       selected = 5,
                       width = "130px" # optional, control width
                     )
                   )
                 ),
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "View as: ",
                     selectInput(
                       inputId = "agg_level",
                       label = NULL,
                       choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                       selected = "Monthly"
                     )
                   ) 
                 ),
                 plotlyOutput("trend_plot_make")
               )
              )
             ),
    tabPanel("By Model",
             layout_columns(
               col_widths = c(12),
                card(
                 card_header("Monthly Vehicle Registrations by Model"),
                 DTOutput("summary_table_model")
                ),     
               card(
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "Monthly Registration Trend for Selected Models",
                     selectInput(
                       inputId = "top_n_model",
                       label = NULL,
                       choices = c(5, 10, 15),
                       selected = 5,
                       width = "130px" # optional, control width
                     )
                   )
                 ),
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "View as: ",
                     selectInput(
                       inputId = "agg_level_model",
                       label = NULL,
                       choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                       selected = "Monthly"
                     )
                   ) 
                 ),
                 plotlyOutput("trend_plot_model")
                )
              )
            ),
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
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "View as: ",
                     selectInput(
                       inputId = "agg_level_fuel",
                       label = NULL,
                       choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                       selected = "Monthly"
                     )
                   ) 
                 ),
                 plotlyOutput("trend_plot_fuel")
               ),
              )
             )
  ),
  actionButton("reset_selection", "Reset Selection"),
  
  tags$footer(
    style = "bottom:0; right:0; width:100%; padding:5px; font-size:10px; text-align:right;",
    HTML("Created by Nur Nafis Naim | <i>nafisnaim@gmail.com</i>")
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  # ---- Summary Function ----
  make_summary <- function(data, group_col, group_col_name) {
    group_col <- sym(group_col)  # convert string to symbol for tidy evaluation
    
    if (group_col == "model"){
      month_current_count <- car_data |>
        filter(date_reg == month_current) |>
        group_by(maker, !!group_col, fuel_grouped) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      month_previous_count <- car_data |>
        filter(date_reg == month_previous) |>
        group_by(maker, !!group_col, fuel_grouped) |>
        summarise(count_previous = sum(count), .groups = "drop")
      
      month_previous_year_count <- car_data |>
        filter(date_reg == month_previous_year) |>
        group_by(maker, !!group_col, fuel_grouped) |>
        summarise(count_previous_year = sum(count), .groups = "drop")
      
      ytd_current_count <- car_data |>
        filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
        group_by(maker, !!group_col, fuel_grouped) |>
        summarise(count_ytd_current = sum(count), .groups = "drop")
      
      ytd_previous_count <- car_data |>
        filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
        group_by(maker, !!group_col, fuel_grouped) |>
        summarise(count_ytd_previous = sum(count), .groups = "drop")
      
      model_list |>
        left_join(month_current_count, by = c("maker", "model", "fuel_grouped")) |>
        mutate(count_current = replace_na(count_current, 0)) |>
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
        select(rank, maker, !!group_col, fuel_grouped, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD, total_count) |>
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
          `Growth (YTD)` := growth_YTD,
          `Total Since 2010` := total_count
        ) 
    } else {
      month_current_count <- car_data |>
        filter(date_reg == month_current) |>
        group_by(!!group_col) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      month_previous_count <- car_data |>
        filter(date_reg == month_previous) |>
        group_by(!!group_col) |>
        summarise(count_previous = sum(count), .groups = "drop")
      
      month_previous_year_count <- car_data |>
        filter(date_reg == month_previous_year) |>
        group_by(!!group_col) |>
        summarise(count_previous_year = sum(count), .groups = "drop")
      
      ytd_current_count <- car_data |>
        filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
        group_by(!!group_col) |>
        summarise(count_ytd_current = sum(count), .groups = "drop")
      
      ytd_previous_count <- car_data |>
        filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
        group_by(!!group_col) |>
        summarise(count_ytd_previous = sum(count), .groups = "drop")
      
      group_df <- get(paste0(group_col, "_list"))
      
      group_df |>
        left_join(month_current_count, by = c(rlang::as_string(group_col))) |>
        mutate(count_current = replace_na(count_current, 0)) |>
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
        select(rank, !!group_col, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD, total_count) |>
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
          `Growth (YTD)` := growth_YTD,
          `Total Since 2010` := total_count
        ) 
    }
  }
  
  # ---- Output Data Table Functions ----
  data_table_TIV <- function(df) {
    df <- df() 
    
    datatable(df,
      rownames = FALSE,
      options = list(dom = 't', ordering = FALSE),
      class = 'cell-border stripe') |>
      formatPercentage("Growth (MoM)", 1) |>
      formatPercentage("Growth (YoY)", 1) |>
      formatPercentage("Growth (YTD)", 1) |>
      formatStyle("Growth (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YTD)", color = styleInterval(c(0), c('red', 'green')))
  }
  
  data_table <- function(df) {
    df <- df() 
    
    datatable(df,
      rownames = FALSE,
      filter = 'top',
      selection = "multiple",
      
      extensions = 'Buttons',
      
      options = list(
        dom = '<"d-flex justify-content-between align-items-center mb-2"Bf>tip', # B = Buttons, f = filter, t = table, i/p = info/pagination
        
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        buttons = c('excel'),
        
        buttons = list(
          list(
            extend = "excel",
            text = "Export to Excel",
            title = NULL,
            filename = "data",
            exportOptions = list(
              modifier = list(page = "all")
            )
          )# export all pages
        ),
        
        pageLength = 10
      ),
      class = 'cell-border stripe') |>
      formatPercentage("Growth (MoM)", 1) |>
      formatPercentage("Growth (YoY)", 1) |>
      formatPercentage("Growth (YTD)", 1) |>
      formatStyle("Growth (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YTD)", color = styleInterval(c(0), c('red', 'green')))
  }
  
  # ---- Selections ----
  selected_make <- reactive({
    sel <- input$summary_table_rows_selected
    if (length(sel)) summary_data()$`Make`[sel] else NULL
  })
  selected_model <- reactive({
    sel <- input$summary_table_model_rows_selected
    if (length(sel)) summary_data_model()$`Model`[sel] else NULL
  })
  selected_fuel <- reactive({
    sel <- input$summary_table_fuel_rows_selected
    if (length(sel)) summary_data_fuel()$`Fuel Type`[sel] else NULL
  })
  
  # ---- Output Plotly Function ----
  plot_chart_total <- function(agg_choice) {
    monthly_trends_data_ttl <- car_data |>
      mutate(month = date_reg) |>
      group_by(month) |>
      summarise(registration = sum(count), .groups = "drop")
    
    if (!is.null(agg_choice)) {
      monthly_trends_data_ttl <- monthly_trends_data_ttl |>
        mutate(year = lubridate::year(month),
               quarter = lubridate::quarter(month),
               month_num = lubridate::month(month))
      
      if (agg_choice == "Quarterly") {
        monthly_trends_data_ttl <- monthly_trends_data_ttl |>
          group_by(year, quarter) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-", (quarter - 1)*3 +1, "-01")))
        
      } else if (agg_choice == "Annually") {
        monthly_trends_data_ttl <- monthly_trends_data_ttl |>
          group_by(year) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-01-01")))
        
      } else if (agg_choice == "5-Month Average") {
        monthly_trends_data_ttl <- monthly_trends_data_ttl |>
          arrange(month) |>
          mutate(registration = zoo::rollmean(registration, 5, fill = NA, align = "right")) |>
          ungroup()
      }
    }
    
    trend_plot <- ggplot(monthly_trends_data_ttl, aes(x = month, y = registration)) +
      geom_line(linewidth = 0.4) +
      geom_point(size = 0.8) +
      theme_minimal(base_size = 14) +
      labs(x = "Month", y = "Registration", color = NULL) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(trend_plot) |>
      layout(
        xaxis = list(
          rangeslider = list(visible = FALSE, thickness = 0.04)
        )
      ) 
  }
  
  plot_chart <- function(df, group_col, group_col_name, top_n_value, agg_choice) {
    df <- df() # call the reactive to get the data frame.

    group_col_name_sym <- rlang::as_name(rlang::ensym(group_col))
   
    selected_value <- switch(
      group_col_name_sym,
      "maker" = selected_make(),
      "model" = selected_model(),
      "fuel_grouped" = selected_fuel(),
      NULL
    )
    
    if (!is.null(selected_value)) {
      monthly_trends_data <- car_data |>
        filter({{ group_col }} %in% selected_value) |>
        mutate(month = date_reg) |>
        group_by(month, {{ group_col }}) |>
        summarise(registration = sum(count), .groups = "drop")
    } else {
      if(group_col_name == "Fuel Type"){
        monthly_trends_data <- car_data |>
          mutate(month = date_reg) |>
          group_by(month, {{ group_col }}) |>
          summarise(registration = sum(count), .groups = "drop")
      } else{
        top_items <- df |> 
          slice_head(n = as.numeric(top_n_value)) |> 
          pull({{ group_col_name }})
      
        monthly_trends_data <- car_data |>
          filter({{ group_col }} %in% top_items) |>
          mutate(month = date_reg) |>
          group_by(month, {{ group_col }}) |>
          summarise(registration = sum(count), .groups = "drop")
      }
    }
    
    if (!is.null(agg_choice)) {
      monthly_trends_data <- monthly_trends_data |>
        mutate(year = lubridate::year(month),
               quarter = lubridate::quarter(month),
               month_num = lubridate::month(month))
      
      if (agg_choice == "Quarterly") {
        monthly_trends_data <- monthly_trends_data |>
          group_by(year, quarter, {{ group_col }}) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-", (quarter - 1)*3 +1, "-01")))
        
      } else if (agg_choice == "Annually") {
        monthly_trends_data <- monthly_trends_data |>
          group_by(year, {{ group_col }}) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-01-01")))
        
      } else if (agg_choice == "5-Month Average") {
        monthly_trends_data <- monthly_trends_data |>
          arrange(month) |>
          group_by({{ group_col }}) |>
          mutate(registration = zoo::rollmean(registration, 5, fill = NA, align = "right")) |>
          ungroup()
      }
    }
    
    trend_plot <- ggplot(monthly_trends_data, aes(x = month, y = registration, color = {{ group_col }})) +
      geom_line(linewidth = 0.4) +
      geom_point(size = 0.8) +
      theme_minimal(base_size = 14) +
      labs(x = "Month", y = "Registration", color = NULL) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(trend_plot) |>
      layout(
        xaxis = list(
          rangeslider = list(visible = FALSE, thickness = 0.04)
        )
      ) 
  }
  
  plot_chart_fc_total <- function() {
    plot_ly() |>
      add_lines(
        data = tiv_monthly,
        x = ~date_reg, y = ~TIV,
        name = "Actual",
        line = list(color = "black"),
        hovertemplate = "Actual: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines(
        data = forecast_df,
        x = ~date_reg, y = ~TIV,
        name = "Forecast",
        line = list(color = "orange", dash = "dot"),
        hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
      ) |>
      add_ribbons( # This is the corrected section
        data = forecast_df,
        x = ~date_reg, ymin = ~lower_70, ymax = ~upper_70,
        name = "70% CI",
        fillcolor = 'rgba(255,165,0,0.2)', 
        line = list(color = 'transparent'),
        hovertemplate = "70% CI: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines( # This is the corrected section
        data = forecast_df,
        x = ~date_reg, y = ~lower_70,
        name = "70% CI",
        line = list(color = "rgba(255,165,0,0.2)", width = 0), 
        hovertemplate = "70% CI: %{y:.3~s}<extra></extra>"
      ) |>
      layout(
        title = "TIV with 12-Month Forecast",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Registration"),
        hovermode = "x"
      )
}
      
  # ---- Reactive summary total ----
  summary_total <- reactive({
    month_current_count_ttl <- car_data |>
      filter(date_reg == month_current) |>
      summarise(count_current = sum(count), .groups = "drop")
    
    month_previous_count_ttl <- car_data |>
      filter(date_reg == month_previous) |>
      summarise(count_previous = sum(count), .groups = "drop")
    
    month_previous_year_count_ttl <- car_data |>
      filter(date_reg == month_previous_year) |>
      summarise(count_previous_year = sum(count), .groups = "drop")
    
    ytd_current_count_ttl <- car_data |>
      filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
      summarise(count_ytd_current = sum(count), .groups = "drop")
    
    ytd_previous_count_ttl <- car_data |>
      filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
      summarise(count_ytd_previous = sum(count), .groups = "drop")
    
    # Add label column first
    label_col <- tibble(category = "TIV")
    
    bind_cols(
      label_col,
      month_current_count_ttl,
      month_previous_count_ttl,
      month_previous_year_count_ttl,
      ytd_current_count_ttl,
      ytd_previous_count_ttl
    ) |>
      mutate(
        across(starts_with("count"), \(x) replace_na(x, 0)),
        growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
        growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
        growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_)
      ) |>
      arrange(desc(count_current)) |>
      select(category, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD) |>
      rename(
        `Total` := category,
        !!month_current_name := count_current,
        !!month_previous_name := count_previous,
        `Growth (MoM)` := growth_MoM,
        !!month_previous_year_name := count_previous_year,
        `Growth (YoY)` := growth_YoY,
        !!year_current_name := count_ytd_current,
        !!year_previous_name := count_ytd_previous,
        `Growth (YTD)` := growth_YTD
      ) 
  })
  
  # ---- Reactive summary ----
  summary_data <- reactive(make_summary(car_data, "maker", "Make"))
  summary_data_model <- reactive(make_summary(car_data, "model", "Model"))
  summary_data_fuel <- reactive(make_summary(car_data, "fuel_grouped", "Fuel Type"))
  
  # ---- Output 1: DataTable ----
  output$summary_table_total <- renderDT(data_table_TIV(summary_total))
  output$summary_table <- renderDT(data_table(summary_data), server = FALSE)
  output$summary_table_model <- renderDT(data_table(summary_data_model), server = FALSE)
  output$summary_table_fuel <- renderDT(data_table(summary_data_fuel), server = FALSE)
  
  # ---- Output 2: Plotly Trend ----
  output$trend_plot_total <- renderPlotly(plot_chart_total(input$agg_level_ttl))
  output$trend_plot_make <- renderPlotly(plot_chart(summary_data, maker, "Make", input$top_n, input$agg_level))
  output$trend_plot_model <- renderPlotly(plot_chart(summary_data_model, model, "Model", input$top_n_model, input$agg_level_model))
  output$trend_plot_fuel <- renderPlotly(plot_chart(summary_data_fuel, fuel_grouped, "Fuel Type", input$top_n_fuel, input$agg_level_fuel))
  output$forecast_plot_total <- renderPlotly(plot_chart_fc_total())
  
  observeEvent(input$reset_selection, {
    if (input$tabs == "By Make") {
      proxy <- DT::dataTableProxy("summary_table")
      DT::selectRows(proxy, NULL)
      
    } else if (input$tabs == "By Model") {
      proxy <- DT::dataTableProxy("summary_table_model")
      DT::selectRows(proxy, NULL)
    } else if (input$tabs == "By Fuel Type") {
      proxy <- DT::dataTableProxy("summary_table_fuel")
      DT::selectRows(proxy, NULL)
    }
  })
}


shinyApp(ui, server)