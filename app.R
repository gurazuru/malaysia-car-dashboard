
# ---- Load packages ----
library(shiny)
library(tidyverse)
library(zoo)
library(DT)
library(plotly)
library(forecast)
library(bslib)
library(memoise)
library(scales)
library(readODS)

# setwd("C:/R Projects/JPJ_Car_Viz")
data_asof = "Data as of 31st October 2026. Next update: 10th December 2026." # Update latest description here
Next_FC_Text = "November 2026 Forecast"
latest_month = "10" # 1 = January, 2 = February, etc.

# ---- Load and combine data ----
car_data <- readr::read_csv("Data/car_data_sum.csv") |>
  mutate(date_reg = floor_date(date_reg, unit = "month"))

# ---- Load Forecast Data ----
# Next 12 months
forecast_next12 <- readRDS("Data/forecast_monthly.rds")
forecast_next12 <- forecast_next12 |>
  filter(month > max(month) %m-% months(12)) |>
  mutate(type = "Forecast") |>
  rename(date_reg := month, TIV := yhat)

forecast_nextmonth <- readRDS("Data/forecast_fd.rds")
forecast_nextmonth <- forecast_nextmonth |>
  rename(date_reg := Date, TIV := Forecast, TIV_Cum := Forecast_Cum)

forecast_segment <- function(segment_name, segment_column) {
  forecast_next12 <- readRDS(
    paste0("Data/forecast_monthly_segments.rds")
  ) |>
    filter(segment == segment_column) |>
    filter(month > max(month) %m-% months(12)) |>
    mutate(type = "Forecast") |>
    rename(date_reg := month, TIV := yhat)
  
  forecast_nextmonth <- readRDS(paste0("Data/forecast_fd_", segment_name, ".rds")) |>
    rename(date_reg := Date, TIV := Forecast, TIV_Cum := Forecast_Cum)
  
  list(
    next12 = forecast_next12,
    nextmonth = forecast_nextmonth
  )
}
hatchback <- forecast_segment("hatchback", "Hatchback")
mpv       <- forecast_segment("mpv", "MPV")
mini      <- forecast_segment("mini", "Mini")
pickup    <- forecast_segment("pickup", "Pick-Up")
sedana    <- forecast_segment("sedana", "Sedan-A")
sedanb    <- forecast_segment("sedanb", "Sedan-B")
sedancd   <- forecast_segment("sedancd", "Sedan-C/D")
suvb      <- forecast_segment("suvb", "SUV-B")
suvcd     <- forecast_segment("suvcd", "SUV-C/D")

# Create a list to call in reactive for segment forecasts
forecast_segments <- list(
  hatchback = hatchback,
  mpv = mpv,
  mini = mini,
  pickup = pickup,
  sedana = sedana,
  sedanb = sedanb,
  sedancd = sedancd,
  suvb = suvb,
  suvcd = suvcd
)

# ---- Load Validated Results (Actual v. Forecast) ----
validated_results <- readr::read_csv("Forecasts/actual_vs_forecast_summary.csv") |>
  select(segment, year, month, TIV_FC, TIV_ACTUAL, pct_error) |>
  mutate(
    TIV_FC = round(TIV_FC), 
    forecast_month = format(
      make_date(year, month, 1),
      "%b %Y"
    )
  ) |>
  select(segment, year, month, forecast_month, TIV_FC, TIV_ACTUAL, pct_error)

# ---- Load Validated Results Remarks ----
forecast_remarks <- read_ods("Forecasts/Actual v. Forecast Summary - working.ods", sheet=1)[, 1:4]

validated_results <- validated_results |>
  left_join(
    forecast_remarks,
    by = c("year", "month", "segment")
  )

# ---- Sample data ----
# car_data <- car_data |> filter(year(date_reg) >= 2025)
# car_data <- readr::read_csv("Data/car_data_sum_sample.csv") # either use this sample data, or filter the one above

# ---- Key Date Variables ----
latest_date <- max(car_data$date_reg)
latest_date_nextmonth <- ceiling_date(latest_date %m+% months(1), "month") %m-% days(1)
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
month_current_name <- format(month_current, "%b %Y")
month_previous_name <- format(month_previous, "%b %Y")
month_previous_year_name <- format(month_previous_year, "%b %Y")
year_current_name <- paste0("YTD ", format(ytd_current_end, "%Y"))
year_previous_name <- paste0("YTD ", format(ytd_previous_end, "%Y"))

# Get forecast month (m+1) and mtd (either sys.date yesterday or last day of the m+1)
latest_actual_month <- floor_date(max(car_data$date_reg), "month")

forecast_month <- latest_actual_month %m+% months(1)

mtd_date <- min(
  Sys.Date() - 1,
  ceiling_date(forecast_month, "month") - days(1)
)

# ---- Forecasting ----
# ---- FC - TIV ----
# aggregate data to monthly data to get monthly TIV
tiv_monthly <- car_data |>
  group_by(date_reg = floor_date(date_reg, "month")) |>
  summarise(TIV = sum(count, na.rm = TRUE)) |>
  arrange(date_reg)

# limit to previous 12 months to plot
tiv_monthly_l12m <- tiv_monthly |>
  filter(date_reg >= month_current %m-% months(11))

tiv_plot_df <- tiv_monthly |>
  filter(date_reg >= month_current %m-% months(11)) |>
  mutate(type = "Actual") |>
  bind_rows(forecast_next12)

# ---- UI ----
ui <- page_fluid(
  tags$head(
    tags$link(
      rel = "canonical", 
      href = "https://gurazuru.github.io/malaysia-car-dashboard/"
    ),
    tags$style(HTML("
      body {
      zoom: 95%;
      -moz-transform: scale(0.95);
      -moz-transform-origin: 0 0;
      }
      
      .forecast-note {
      margin-top: 5px;
      margin-bottom: 5px;
      font-size: 15px;
    }

    .forecast-note summary {
      cursor: pointer;
      color: #666;
      font-weight: 500;
    }

    .forecast-note p {
      margin: 8px 0 0 20px;
      color: #666;
      line-height: 1.5;
    }
      
      /* =========================
         DARK MODE
         ========================= */
      
      body.dark-mode {
        background-color: #1e1e1e !important;
        color: #eeeeee !important;
      }

      body.dark-mode .bslib-page {
        background-color: #1e1e1e !important;
      }

      body.dark-mode .card {
        background-color: #2b2b2b !important;
        color: #eeeeee !important;
        border-color: #444444 !important;
      }

      body.dark-mode .card-header {
        background-color: #333333 !important;
        color: #ffffff !important;
      }

      body.dark-mode h1,
      body.dark-mode h2,
      body.dark-mode h3,
      body.dark-mode h4,
      body.dark-mode h5,
      body.dark-mode h6 {
        color: #eeeeee !important;
      }

      body.dark-mode .form-control,
      body.dark-mode .selectize-input {
        background-color: #333333 !important;
        color: #eeeeee !important;
        border-color: #555555 !important;
      }

      body.dark-mode .selectize-dropdown {
        background-color: #333333 !important;
        color: #eeeeee !important;
      }

      body.dark-mode .selectize-dropdown .option:hover {
        background-color: #444444 !important;
      }

      body.dark-mode .dataTables_wrapper {
        color: #eeeeee !important;
      }

      body.dark-mode table.dataTable {
        color: #eeeeee !important;
      }

      body.dark-mode table.dataTable tbody tr {
        background-color: #2b2b2b !important;
      }

      body.dark-mode table.dataTable tbody tr:hover {
        background-color: #3a3a3a !important;
      }

      /* Night mode button */
      #dark_mode_toggle {
        position: fixed;
        top: 15px;
        right: 20px;
        z-index: 9999;
        border: none;
        border-radius: 20px;
        padding: 7px 12px;
        background-color: #eeeeee;
        color: #333333;
        cursor: pointer;
        font-size: 14px;
      }

      body.dark-mode #dark_mode_toggle {
        background-color: #444444;
        color: #ffffff;
      }   
    ")),
    
    tags$script(HTML("
      function toggleDarkMode() {
        document.body.classList.toggle('dark-mode');

        const isDark =
          document.body.classList.contains('dark-mode');

        localStorage.setItem('darkMode', isDark);
        
        document.getElementById('dark_mode_toggle').innerHTML =
          isDark ? '☀️' : '🌙';
      }

      document.addEventListener('DOMContentLoaded', function() {
        const darkMode =
          localStorage.getItem('darkMode') === 'true';

        if (darkMode) {
          document.body.classList.add('dark-mode');
          document.getElementById('dark_mode_toggle').innerHTML =
            '☀️';
        }
      });
    "))
  ),
  
  tags$button(
    id = "dark_mode_toggle",
    onclick = "toggleDarkMode()",
    "🌙"
  ),
  
  titlePanel("Malaysia JPJ Vehicle Registration Data Dashboard"),

  tags$h5("An interactive dashboard visualizing Malaysia's total industry volume. Sourced from data.gov.my.",
          style = "color: #555; margin-top: -2px;"),
  
  div(
    card(
      height = c(180),
      style = "overflow: visible;",
      
      card_header(
        style = "
        display: flex;
        align-items: center;
        justify-content: space-between;
        overflow: visible;
      ",
        
        div(
          data_asof
        ),
        
        div(
          style = "
          width: 180px;
          margin-left: 20px;
          margin-bottom: 0px;
        ",
          
          selectInput(
            inputId = "vehicle_segment_fc",
            label = NULL,
            choices = c(
              "All", "Hatchback", "Mini", "MPV", "Pick-Up",
              "Sedan-A", "Sedan-B", "Sedan-C/D",
              "SUV-B", "SUV-C/D"
            ),
            selected = "All",
            width = "100%"
          )
        )
      ),
      DTOutput("summary_table_total")
    )
  ),
  
  div(
    style = "padding-right: 0px;",
    
    tabsetPanel(
      id = "toptabs",
      
      tabPanel("TIV",
        layout_columns( width = 12,
          card(
            height = c(280),
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
      tabPanel("12-Month Forecast",
        layout_columns( width = 12,
          card(
            height = c(280),
            card_header(
              div(),
            ),
            plotlyOutput("forecast_plot_total_segment"),
          )
        )
      ),
      tabPanel(Next_FC_Text,
        fluidRow(
          column(
            width = 3,
            radioButtons(
              "chart_mode",
              "",
              choices = c("Cumulative", "Daily"),
              selected = "Cumulative",
              inline = TRUE
            )
          ),
          column(width = 3, uiOutput("kpi_full")),
          column(width = 3, uiOutput("kpi_mtd")),
          column(width = 3, uiOutput("kpi_progress")),
        ),
        layout_columns( width = 12,
          card(
            height = c(280),
            card_header(
              div(),
            ),
            plotlyOutput("forecast_plot_nextmonth_segment")
          )
        )
      ),
      tabPanel("Forecast Track Record",
        layout_columns( width = 12,
          card(
            height = c(540),
            
            tags$details(
              class = "forecast-note",
              
              tags$summary(
                HTML("&#9432; How is this calculated?")
              ),
              
              tags$p(
                "The M+1 forecast is generated using a Prophet-based time-series model incorporating ",
                "monthly and yearly seasonality and calendar effects ",
                "(e.g. public holidays, festive seasons, as well as COVID lockdown). ",
                "Historical forecasts are generated using only information available at the ",
                "respective forecast cutoff. The track record indicates",
                "the TIV's historical trend and does not guarantee future forecast accuracy."
              )
            ),
            
            DTOutput("validated_results"),
          )
        )
      )
    )
  ),
  
  # ---- UI - Breakdown ----
  fluidRow(
    column(width = 2,
           selectInput(
             inputId = "global_fuel_type",
             label = "Fuel Type:",
             choices = c("All", sort(unique(car_data$fuel_grouped))),
             selected = "All"
           )
    ),
    column(width = 2,
           selectInput(
             inputId = "global_segment",
             label = "Vehicle Segment:",
             choices = c("All", sort(unique(car_data$segment))),
             selected = "All"
           )
    ),
    column(width = 2,
           selectInput(
             inputId = "year_selected",
             label = "Year:",
             choices = c("2026", "2025", "2024", "2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016"),
             selected = "2026"
           )
    ),
    column(width = 1,
           selectInput(
             inputId = "month_selected",
             label = "Month:",
             choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
             selected = latest_month
           )
    ),
    column(width = 2,
           selectInput(
             inputId = "state_selected",
             label = "State*:",
             choices = c("All", "Rakan Niaga", sort(setdiff(unique(car_data$state), "Rakan Niaga")) ),
             selected = "All"
           )
    ),
    column(width = 3,
           selectInput(
             inputId = "make_selected",
             label = "Make:",
             choices = sort(unique(car_data$maker)),
             multiple = TRUE,
             selectize = TRUE
           )
    )
  ),
  
  # Note on "Rakan Niaga".
  tags$p(tags$em("*In recent years, more and more registrations have been made under 'Rakan Niaga'.")),
  
  # ---- Global controls row for Share ----
  div(
    style = "position: relative;",
    # toggle - only 200px wide, top-right corner
    div(
      style = "position: absolute; top: 0; right: 0; z-index: 5; width: 220px; background: white;",
      radioButtons("share_toggle", NULL, c("Volume","Share"), selected="Volume", inline=TRUE)
    ),
    # tabs - add right padding so the last tab isn't hidden under the toggle
    div(
      style = "padding-right: 0px;",
      
      tabsetPanel(
        id = "tabs",
        tabPanel("By Make",
          layout_columns(
            card(
              height = c(740),
              card_header(
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; gap: 20px;",
                  div("Vehicle Registration Summary by Make", style = "font-weight: 600;")
                )
              ),
              DTOutput("summary_table")
            ),
            card(
              height = c(740),
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
            ),
            col_widths = c(6, 6)
          )
        ),
        tabPanel("By Model",
          layout_columns(
            width = 12,
            card(
              height = c(740),
              card_header(
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; gap: 20px;",
                  div("Vehicle Registration Summary by Model", style = "font-weight: 600;"),
                )
              ),
              DTOutput("summary_table_model")
            ),     
            card(
              height = c(740),
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
            ),
            col_widths = c(6, 6)
          )
        ),
        tabPanel("By Fuel Type",
          layout_columns(
            width = 12,
            card(
              height = c(740),
              card_header(
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; gap: 20px;",
                  div("Vehicle Registration Summary by Fuel Type", style = "font-weight: 600;"),
                )
              ),
              DTOutput("summary_table_fuel")
            ),
            card(
              height = c(740),
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
            col_widths = c(6, 6)
          )
        ),
        tabPanel("By Make (Full Year)",
          layout_columns(
            col_widths =  c(12),
            card(
              height = c(740),
              card_header("Monthly Vehicle Registrations by Make"),
              DTOutput("annual_table")
            ),
            card(
              height = c(240),
              card_header("Monthly Vehicle Registrations Total"),
              DTOutput("annual_table_tiv")
            )
          )
        ),
        tabPanel("By Model (Full Year)",
          layout_columns(
            col_widths =  c(12),
            card(
              height = c(740),
              card_header("Monthly Vehicle Registrations by Model"),
              DTOutput("annual_table_model")
            ),
            card(
              height = c(240),
              card_header("Monthly Vehicle Registrations Total"),
              DTOutput("annual_table_model_tiv")
            )
          )
        )
      )
    )
  ),

  actionButton("reset_selection", "Reset Selection"),
  
  tags$footer(
    style = "bottom:0; right:0; width:100%; padding:5px; font-size:12px; text-align:right;",
    HTML("Created by Nur Nafis Naim | <i>nafisnaim@gmail.com</i>")
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- Get MTD Progress Values (KPI) ----
  kpi_values <- reactive({
    
    if (input$vehicle_segment_fc != "All"){
      segment_name <- tolower(gsub("[^A-Za-z0-9]", "", input$vehicle_segment_fc))
      
      df <- forecast_segments[[segment_name]]$nextmonth
    } else {
      df <- forecast_nextmonth
    } 
    
    today <- Sys.Date()
    
    # get MTD forecast (cumulative up to today)
    if (year(latest_date_nextmonth) < year(today) || 
       (year(latest_date_nextmonth) == year(today) && month(latest_date_nextmonth) == month(today)) ) {
        fc_mtd <- df |>
        filter(year(date_reg) == year(today) & month(date_reg) == month(today) & day(date_reg) == day(today)) |>
        summarise(value = sum(TIV_Cum)) |>
        pull(value)
    } else { 
      fc_mtd <- df |>
      summarise(value = sum(TIV)) |>
      pull(value)
    }
    
    # get full month
    fc_full <- sum(df$TIV)
    
    # % registration progression
    progress <- fc_mtd / fc_full
    
    list(
      fc_mtd = fc_mtd,
      fc_full = fc_full,
      progress = progress
    )
  })
  
  # ---- Filter by Fuel Type, Segment, & Latest Year & Latest Month ----
  filtered_data <- reactive({
    df <- car_data
    if (input$global_fuel_type != "All") {
      df <- df |> filter(fuel_grouped == input$global_fuel_type)
    }

    if (input$global_segment != "All") {
      df <- df |> filter(segment == input$global_segment)
    }

    if (input$state_selected != "All") {
      df <- df |> filter(state == input$state_selected)
    }

  
    if (!is.null(input$make_selected) && length(input$make_selected) > 0) {
      df <- df |> filter(maker %in% input$make_selected)
    }
    
    df <- df |> filter(year(date_reg) %in% c(as.numeric(input$year_selected), 
                                             as.numeric(input$year_selected) - 1,
                                             as.numeric(input$year_selected) - 2,
                                             as.numeric(input$year_selected) - 3,
                                             as.numeric(input$year_selected) - 4))
    
    latest_year <- max(year(df$date_reg))
    
    df <- df |> filter(
      year(date_reg) < latest_year | 
      (year(date_reg) == latest_year & month(date_reg) <= as.numeric(input$month_selected))
    )
  })
  
  # ---- Filter Forecast Data by Segment ----

  # prepare forecast_monthly for each segment here
  forecast_next12_segment <- reactive({
    if (input$vehicle_segment_fc != "All"){
      segment_name <- tolower(gsub("[^A-Za-z0-9]", "", input$vehicle_segment_fc))
      
      df <- forecast_segments[[segment_name]]$next12
    } else {
      df <- forecast_next12
    }
  })
  
  tiv_monthly_l12m_segment <- reactive({
    if (input$vehicle_segment_fc != "All"){
      df <- car_data |>
        filter(segment == input$vehicle_segment_fc) |>
        group_by(date_reg = floor_date(date_reg, "month")) |>
        summarise(TIV = sum(count, na.rm = TRUE)) |>
        arrange(date_reg)
      
      df <- df |> 
        filter(date_reg >= month_current %m-% months(11)) 
    } else {
      df <- tiv_monthly_l12m
    }
  })  
  
  forecast_nextmonth_segment <- reactive({
    if (input$vehicle_segment_fc != "All"){
      segment_name <- tolower(gsub("[^A-Za-z0-9]", "", input$vehicle_segment_fc))
      
      df <- forecast_segments[[segment_name]]$nextmonth
    } else {
      df <- forecast_nextmonth
    }
  })

  
  # ---- Get unique make models ----
  model_list <- reactive({
    df <- filtered_data() |>
      group_by(maker, model) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })
  model_list_annual <- reactive({     # this if for the "Full Year" tab
    df <- filtered_data() |>
      group_by(maker, model) |>
      filter(year(date_reg) == input$year_selected) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })
  
  maker_list <- reactive({
    df <- filtered_data() |>
      group_by(maker) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })  
  maker_list_annual <- reactive({     # this if for the "Full Year" tab
    df <- filtered_data() |>
      group_by(maker) |>
      filter(year(date_reg) == input$year_selected) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })
  
  fuel_grouped_list <- reactive({
    filtered_data() |>
      group_by(fuel_grouped) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
  })
  
  # ---- Summary Function ----
  make_summary <- function(data, group_col, group_col_name, data_type) {
    group_col <- sym(group_col)  # convert string to symbol for tidy evaluation
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
    } else {
      # ---- Key Date Variables ----
      month_current <- max(data$date_reg)
      month_previous <- month_current %m-% months(1)
      month_previous_year <- month_current %m-% months(12)
      
      year_current <- year(month_current)
      ytd_current_start <- ymd(paste0(year_current, "-01-01"))
      ytd_current_end <- month_current
      
      ytd_previous_start <- ytd_current_start %m-% years(1)
      ytd_previous_end <- ytd_current_end %m-% years(1)
      
      # Define all the relevant dates.
      month_current_name <- format(month_current, "%b %Y")
      month_previous_name <- format(month_previous, "%b %Y")
      month_previous_year_name <- format(month_previous_year, "%b %Y")
      year_current_name <- paste0("YTD ", format(ytd_current_end, "%Y"))
      year_previous_name <- paste0("YTD ", format(ytd_previous_end, "%Y"))
      
      if (group_col == "model"){
        month_current_count <- data |>
          filter(date_reg == month_current) |>
          group_by(maker, !!group_col) |>
          summarise(count_current = sum(count), .groups = "drop")
        
        month_previous_count <- data |>
          filter(date_reg == month_previous) |>
          group_by(maker, !!group_col) |>
          summarise(count_previous = sum(count), .groups = "drop")
        
        month_previous_year_count <- data |>
          filter(date_reg == month_previous_year) |>
          group_by(maker, !!group_col) |>
          summarise(count_previous_year = sum(count), .groups = "drop")
        
        ytd_current_count <- data |>
          filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
          group_by(maker, !!group_col) |>
          summarise(count_ytd_current = sum(count), .groups = "drop")
        
        ytd_previous_count <- data |>
          filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
          group_by(maker, !!group_col) |>
          summarise(count_ytd_previous = sum(count), .groups = "drop")
        
        base_df <- model_list() |>
          left_join(month_current_count, by = c("maker", "model")) |>
          mutate(count_current = replace_na(count_current, 0)) |>
          full_join(month_previous_count, by = c("maker", rlang::as_string(group_col))) |>
          full_join(month_previous_year_count, by = c("maker", rlang::as_string(group_col))) |>
          full_join(ytd_current_count, by = c("maker", rlang::as_string(group_col))) |>
          full_join(ytd_previous_count, by = c("maker", rlang::as_string(group_col))) |>
          mutate(
            across(starts_with("count"), \(x) replace_na(x, 0)),
            growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
            growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
            growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_),
            
          # Market share calculations
            share_current = count_current / sum(count_current),
            share_previous = count_previous / sum(count_previous),
            share_previous_year = count_previous_year / sum(count_previous_year),
            share_ytd_current = count_ytd_current / sum(count_ytd_current),
            share_ytd_previous = count_ytd_previous / sum(count_ytd_previous),
            change_MoM = share_current - share_previous,
            change_YoY = share_current - share_previous_year,
            change_YTD = share_ytd_current - share_ytd_previous
          ) 
        
          if (data_type == "Volume") {
          final_df <-  base_df |> 
              arrange(desc(count_current)) |>
              mutate(rank = row_number()) |>
              select(rank, maker, !!group_col, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD) |>
              rename(
                `Rank` := rank,
                `Make` := maker,
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
          } else {
            final_df <-  base_df |> 
              arrange(desc(count_current)) |>
              mutate(rank = row_number()) |>
              select(rank, maker, !!group_col, share_current, share_previous, change_MoM, share_previous_year, change_YoY, share_ytd_current, share_ytd_previous, change_YTD) |>
              rename(
                `Rank` := rank,
                `Make` := maker,
                !!group_col_name := !!group_col,
                !!month_current_name := share_current,
                !!month_previous_name := share_previous,
                `Change (MoM)` := change_MoM,
                !!month_previous_year_name := share_previous_year,
                `Change (YoY)` := change_YoY,
                !!year_current_name := share_ytd_current,
                !!year_previous_name := share_ytd_previous,
                `Change (YTD)` := change_YTD
              )
          }
      } else if (group_col == "maker") {
        month_current_count <- data |>
          filter(date_reg == month_current) |>
          group_by(!!group_col) |>
          summarise(count_current = sum(count), .groups = "drop")
        
        month_previous_count <- data |>
          filter(date_reg == month_previous) |>
          group_by(!!group_col) |>
          summarise(count_previous = sum(count), .groups = "drop")
        
        month_previous_year_count <- data |>
          filter(date_reg == month_previous_year) |>
          group_by(!!group_col) |>
          summarise(count_previous_year = sum(count), .groups = "drop")
        
        ytd_current_count <- data |>
          filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
          group_by(!!group_col) |>
          summarise(count_ytd_current = sum(count), .groups = "drop")
        
        ytd_previous_count <- data |>
          filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
          group_by(!!group_col) |>
          summarise(count_ytd_previous = sum(count), .groups = "drop")
        
        base_df <- maker_list() |>
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
            growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_),
            
            # Market share calculations
            share_current = count_current / sum(count_current),
            share_previous = count_previous / sum(count_previous),
            share_previous_year = count_previous_year / sum(count_previous_year),
            share_ytd_current = count_ytd_current / sum(count_ytd_current),
            share_ytd_previous = count_ytd_previous / sum(count_ytd_previous),
            change_MoM = share_current - share_previous,
            change_YoY = share_current - share_previous_year,
            change_YTD = share_ytd_current - share_ytd_previous
          )
          
          if (data_type == "Volume") {
            final_df <- base_df |> 
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
          } else{
            final_df <- base_df |> 
              arrange(desc(count_current)) |>
              mutate(rank = row_number()) |>
              select(rank, !!group_col, share_current, share_previous, change_MoM, share_previous_year, change_YoY, share_ytd_current, share_ytd_previous, change_YTD) |>
              rename(
                `Rank` := rank,
                !!group_col_name := !!group_col,
                !!month_current_name := share_current,
                !!month_previous_name := share_previous,
                `Change (MoM)` := change_MoM,
                !!month_previous_year_name := share_previous_year,
                `Change (YoY)` := change_YoY,
                !!year_current_name := share_ytd_current,
                !!year_previous_name := share_ytd_previous,
                `Change (YTD)` := change_YTD
              )
            
          }
      } else {
        month_current_count <- data |>
          filter(date_reg == month_current) |>
          group_by(!!group_col) |>
          summarise(count_current = sum(count), .groups = "drop")
        
        month_previous_count <- data |>
          filter(date_reg == month_previous) |>
          group_by(!!group_col) |>
          summarise(count_previous = sum(count), .groups = "drop")
        
        month_previous_year_count <- data |>
          filter(date_reg == month_previous_year) |>
          group_by(!!group_col) |>
          summarise(count_previous_year = sum(count), .groups = "drop")
        
        ytd_current_count <- data |>
          filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
          group_by(!!group_col) |>
          summarise(count_ytd_current = sum(count), .groups = "drop")
        
        ytd_previous_count <- data |>
          filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
          group_by(!!group_col) |>
          summarise(count_ytd_previous = sum(count), .groups = "drop")
        
        base_df <- fuel_grouped_list() |>
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
            growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_),
            
            # Market share calculations
            share_current = count_current / sum(count_current),
            share_previous = count_previous / sum(count_previous),
            share_previous_year = count_previous_year / sum(count_previous_year),
            share_ytd_current = count_ytd_current / sum(count_ytd_current),
            share_ytd_previous = count_ytd_previous / sum(count_ytd_previous),
            change_MoM = share_current - share_previous,
            change_YoY = share_current - share_previous_year,
            change_YTD = share_ytd_current - share_ytd_previous
            
          )
          
          if (data_type == "Volume") {
            final_df <- base_df |> 
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
          } else{
            final_df <- base_df |> 
              arrange(desc(count_current)) |>
              mutate(rank = row_number()) |>
              select(rank, !!group_col, share_current, share_previous, change_MoM, share_previous_year, change_YoY, share_ytd_current, share_ytd_previous, change_YTD) |>
              rename(
                `Rank` := rank,
                !!group_col_name := !!group_col,
                !!month_current_name := share_current,
                !!month_previous_name := share_previous,
                `Change (MoM)` := change_MoM,
                !!month_previous_year_name := share_previous_year,
                `Change (YoY)` := change_YoY,
                !!year_current_name := share_ytd_current,
                !!year_previous_name := share_ytd_previous,
                `Change (YTD)` := change_YTD
              )
            
          }
      }
    }
  }
  
  # ---- Annual YTD Function ----
  make_annual <- function(data, group_col, group_col_name, data_type) {
    group_col <- sym(group_col)  # convert string to symbol for tidy evaluation
    
    data <- filtered_data()
    
    if (nrow(data) == 0) return(tibble())
    
      if (group_col == "model"){
        
        # Filter the entire year's data ONCE
        year_data <- data |>
          filter(year(date_reg) == input$year_selected) |>
          mutate(
            month_lbl = format(date_reg, "%b %Y")
          )
        
        # Group and summarize by maker, group_col, and month
        monthly_summary <- year_data |>
          group_by(maker, !!group_col, month_lbl) |>
          summarise(count_current = sum(count, na.rm = TRUE), .groups = "drop")
        
        # Ensure all 12 months exist for every maker/group combination BEFORE pivoting
        all_months <- format(as.Date(paste(input$year_selected, 1:12, 1, sep="-")), "%b %Y")
        
        wide_data <- monthly_summary |>
          # Complete the grid so missing months get explicit 0s
          complete(
            nesting(maker, !!group_col), 
            month_lbl = all_months
          ) |>
          pivot_wider(
            names_from = month_lbl, 
            values_from = count_current
          )
        
        # Calculate Total, join with the base model list, and select
        year_colname <- paste0("Total ", input$year_selected)
        
        data <- model_list_annual() |>
          left_join(wide_data, by = c("maker", rlang::as_string(group_col))) |>
          mutate(across(all_of(all_months), ~ replace_na(.x, 0))) |>
          mutate(
            !!year_colname := rowSums(across(all_of(all_months)), na.rm = TRUE)
          ) |>
          arrange(desc(!!sym(year_colname))) |>
          mutate(Rank = row_number()) |>
          select(
            Rank,
            Make = maker,
            !!group_col_name := !!group_col,
            all_of(all_months),
            !!sym(year_colname)
          )
        
        # Share version (divide each column by column sum)
        share_data <- data |>
          mutate(
            across(all_of(all_months), ~ {
              tot <- sum(.x, na.rm = TRUE)
              if(tot > 0) .x / tot else 0
            }),
            !!year_colname := !!sym(year_colname) / sum(!!sym(year_colname), na.rm = TRUE)
          ) |>
          
          # Rank stays same as volume - don't re-rank on share
          arrange(desc(!!sym(year_colname))) |>
          mutate(Rank = row_number())
        
      } else if (group_col == "maker") {
        
        # Filter the entire year's data ONCE
        year_data <- data |>
          filter(year(date_reg) == input$year_selected) |>
          mutate(
            month_lbl = format(date_reg, "%b %Y")
          )
        
        # Group and summarize by group_col, and month
        monthly_summary <- year_data |>
          group_by(!!group_col, month_lbl) |>
          summarise(count_current = sum(count, na.rm = TRUE), .groups = "drop")
        
        # Ensure all 12 months exist for every maker/group combination BEFORE pivoting
        all_months <- format(as.Date(paste(input$year_selected, 1:12, 1, sep="-")), "%b %Y")
        
        wide_data <- monthly_summary |>
          # Complete the grid so missing months get explicit 0s
          complete(
            !!group_col, 
            month_lbl = all_months
          ) |>
          pivot_wider(
            names_from = month_lbl, 
            values_from = count_current
          )
        
        # Calculate Total, join with the base model list, and select
        year_colname <- paste0("Total ", input$year_selected)
        
        data <- model_list_annual() |>
          distinct(!!group_col) |>
          left_join(wide_data, by = c(rlang::as_string(group_col))) |>
          mutate(across(all_of(all_months), ~ replace_na(.x, 0))) |>
          mutate(
            !!year_colname := rowSums(across(all_of(all_months)), na.rm = TRUE)
          ) |>
          arrange(desc(!!sym(year_colname))) |>
          mutate(Rank = row_number()) |>
          select(
            Rank,
            !!group_col_name := !!group_col,
            all_of(all_months),
            !!sym(year_colname)
          )
        
        # Share version (divide each column by column sum)
        share_data <- data |>
          mutate(
            across(all_of(all_months), ~ {
              tot <- sum(.x, na.rm = TRUE)
              if(tot > 0) .x / tot else 0
            }),
            !!year_colname := !!sym(year_colname) / sum(!!sym(year_colname), na.rm = TRUE)
          ) |>
          
        # Rank stays same as volume - don't re-rank on share
        arrange(desc(!!sym(year_colname))) |>
        mutate(Rank = row_number())
      } else {
      }
    
    if (data_type  == "Volume") {
      return(data)
    } else {
      return(share_data)
    }  
  }
  
  # Get annual TIV for total all stats
  make_annual_tiv <- function(data, model) {
    data <- filtered_data()
    
    if (nrow(data) == 0) return(tibble())
    
      if (model == "yes") {
        monthly_data <- function(month_num, year_num){
          data <- data |>
            mutate(maker = "All Makes", model = "All Models") |>
            filter(month(date_reg) == month_num, year(date_reg) == year_num) |>
            group_by(maker, model) |>
            summarise(count_current = sum(count), .groups = "drop")
          
          colname <- format(as.Date(paste(year_num, month_num, 1, sep = "-")), "%b %Y")
          
          data <- data |> rename(!!colname := count_current)
          
          return(data)
        }
      
      Jan_Count <- monthly_data(1, input$year_selected)
      Feb_Count <- monthly_data(2, input$year_selected)
      Mar_Count <- monthly_data(3, input$year_selected)
      Apr_Count <- monthly_data(4, input$year_selected)
      May_Count <- monthly_data(5, input$year_selected)
      Jun_Count <- monthly_data(6, input$year_selected)
      Jul_Count <- monthly_data(7, input$year_selected)
      Aug_Count <- monthly_data(8, input$year_selected)
      Sep_Count <- monthly_data(9, input$year_selected)
      Oct_Count <- monthly_data(10, input$year_selected)
      Nov_Count <- monthly_data(11, input$year_selected)
      Dec_Count <- monthly_data(12, input$year_selected)
    
      Year_Total_Count <- data |>
        mutate(maker = "All Makes", model = "All Models") |>
        filter(year(date_reg) == input$year_selected) |>
        group_by(maker, model) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      year_colname <- paste0("Total ", format(as.Date(paste(input$year_selected, 1, 1, sep = "-")), "%Y"))
      
      Year_Total_Count <- Year_Total_Count |> rename(!!year_colname := count_current)
      
      maker_list_annual_tiv <- maker_list_annual() |>
        mutate(maker = "All Makes", model = "All Models")
      
      maker_list_annual_tiv <- head(maker_list_annual_tiv, 1)
      
      data <- maker_list_annual_tiv |>
        left_join(Year_Total_Count, by = c("maker", "model")) |>
        full_join(Jan_Count, by = c("maker", "model")) |>
        full_join(Feb_Count, by = c("maker", "model")) |>
        full_join(Mar_Count, by = c("maker", "model")) |>
        full_join(Apr_Count, by = c("maker", "model")) |>
        full_join(May_Count, by = c("maker", "model")) |>
        full_join(Jun_Count, by = c("maker", "model")) |>
        full_join(Jul_Count, by = c("maker", "model")) |>
        full_join(Aug_Count, by = c("maker", "model")) |>
        full_join(Sep_Count, by = c("maker", "model")) |>
        full_join(Oct_Count, by = c("maker", "model")) |>
        full_join(Nov_Count, by = c("maker", "model")) |>
        full_join(Dec_Count, by = c("maker", "model")) |>
        arrange(desc('Total')) |>
        mutate(rank = row_number()) |>
        select(rank, maker, model,
               all_of(paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total"), input$year_selected))
        ) |>
        rename(
          `Rank` := rank,
          `Make` := maker,
          `Model` := model,
        )  
      
      } else {
        monthly_data <- function(month_num, year_num){
          data <- data |>
            mutate(maker = "All Makes") |>
            filter(month(date_reg) == month_num, year(date_reg) == year_num) |>
            group_by(maker) |>
            summarise(count_current = sum(count), .groups = "drop")
          
          colname <- format(as.Date(paste(year_num, month_num, 1, sep = "-")), "%b %Y")
          
          data <- data |> rename(!!colname := count_current)
          
          return(data)
        }
        
        Jan_Count <- monthly_data(1, input$year_selected)
        Feb_Count <- monthly_data(2, input$year_selected)
        Mar_Count <- monthly_data(3, input$year_selected)
        Apr_Count <- monthly_data(4, input$year_selected)
        May_Count <- monthly_data(5, input$year_selected)
        Jun_Count <- monthly_data(6, input$year_selected)
        Jul_Count <- monthly_data(7, input$year_selected)
        Aug_Count <- monthly_data(8, input$year_selected)
        Sep_Count <- monthly_data(9, input$year_selected)
        Oct_Count <- monthly_data(10, input$year_selected)
        Nov_Count <- monthly_data(11, input$year_selected)
        Dec_Count <- monthly_data(12, input$year_selected)
        
        Year_Total_Count <- data |>
          mutate(maker = "All Makes") |>
          filter(year(date_reg) == input$year_selected) |>
          group_by(maker) |>
          summarise(count_current = sum(count), .groups = "drop")
        
        year_colname <- paste0("Total ", format(as.Date(paste(input$year_selected, 1, 1, sep = "-")), "%Y"))
        
        Year_Total_Count <- Year_Total_Count |> rename(!!year_colname := count_current)
        
        maker_list_annual_tiv <- maker_list_annual() |>
          mutate(maker = "All Makes")
        
        maker_list_annual_tiv <- head(maker_list_annual_tiv, 1)
        
        data <- maker_list_annual_tiv |>
          left_join(Year_Total_Count, by = c("maker")) |>
          full_join(Jan_Count, by = c("maker")) |>
          full_join(Feb_Count, by = c("maker")) |>
          full_join(Mar_Count, by = c("maker")) |>
          full_join(Apr_Count, by = c("maker")) |>
          full_join(May_Count, by = c("maker")) |>
          full_join(Jun_Count, by = c("maker")) |>
          full_join(Jul_Count, by = c("maker")) |>
          full_join(Aug_Count, by = c("maker")) |>
          full_join(Sep_Count, by = c("maker")) |>
          full_join(Oct_Count, by = c("maker")) |>
          full_join(Nov_Count, by = c("maker")) |>
          full_join(Dec_Count, by = c("maker")) |>
          arrange(desc('Total')) |>
          mutate(rank = row_number()) |>
          select(rank, maker,
                 all_of(paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total"), input$year_selected))
          ) |>
          rename(
            `Rank` := rank,
            `Make` := maker,
          )  
        
      }
    return(data)
  }
  
  # ---- Output Data Table Functions ----
  data_table_TIV <- function(df) {
    df <- df()  
    
    # which columns are numbers but NOT the growth % columns
    pct_cols <- c("Growth (MoM)", "Growth (YoY)", "Growth (YTD)")
    num_cols <- setdiff(names(df)[sapply(df, is.numeric)], pct_cols)
    
    datatable(df,
              rownames = FALSE,
              options = list(dom = 't', ordering = FALSE),
              class = 'cell-border stripe') |>
      formatRound(columns = num_cols, digits = 0, mark = ",") |>
      formatPercentage("Growth (MoM)", 1) |>
      formatPercentage("Growth (YoY)", 1) |>
      formatPercentage("Growth (YTD)", 1) |>
      formatStyle("Growth (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YTD)", color = styleInterval(c(0), c('red', 'green')))
  }
  
  validated_results_render <- function(df) {
    df <- df()
    
    # which columns are numbers but NOT the growth % columns
    pct_cols <- c("% Error")
    num_cols <- c("Forecast TIV", "Actual TIV")
    
    datatable(df,
              escape = FALSE,
              rownames = FALSE,
              selection = 'none',
              options = list(
                autoWidth = FALSE,
                columnDefs = list(
                  
                  # Width for all columns
                  list(
                    width = paste0(round(100 / ncol(df), 2), "%"),
                    targets = "_all"
                  ),
                  
                  # Renderer only for remarks
                  list(
                    targets = which(names(df) == "Remarks") - 1,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "  if (type === 'display' && data) {",
                      "    return '<span title=\"' + data.replace(/\"/g, '&quot;') + '\">ⓘ</span>';",
                      "  }",
                      "  return data;",
                      "}"
                    )
                  )
                ),
                dom = 't', ordering = FALSE
              ),
              class = 'cell-border stripe') |>
      formatRound(columns = num_cols, digits = 0, mark = ",") |>
      formatPercentage(columns = pct_cols, 1) |>
      formatStyle(
        columns = pct_cols,
        backgroundColor = styleInterval(
          c(-0.20, -0.10, -0.05, 0.05, 0.10, 0.20),
          c(
            "#f5b7b1",
            "#f8d7da",
            "#fdecea",
            "white",
            "#fdecea",
            "#f8d7da",
            "#f5b7b1"
          )
        )
      )
  }
  
  # input$share_toggle
  
  data_table <- function(df) {
    df <- df() 
    
    is_share <- input$share_toggle == "Share"
    
    if(is_share) {
      
      # which columns are numbers but NOT the growth % columns
      exclude_cols <- c("Rank", "Change (MoM)", "Change (YoY)", "Change (YTD)")
      num_cols <- setdiff(names(df)[sapply(df, is.numeric)], exclude_cols)
      
      datatable(df,
                rownames = FALSE,
                filter = 'top',
                selection = list(mode = "multiple"
                                 #, selected = 1:5
                ),
                
                extensions = 'Buttons',
                
                options = list(
                  dom = 'Btip', # B = Buttons, f = Global search box, t = table, i/p = info/pagination
                  
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  searching = TRUE,
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
        formatPercentage(columns = num_cols, digits = 1) |>
        formatPercentage("Change (MoM)", 1) |>
        formatPercentage("Change (YoY)", 1) |>
        formatPercentage("Change (YTD)", 1) |>
        formatStyle("Change (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
        formatStyle("Change (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
        formatStyle("Change (YTD)", color = styleInterval(c(0), c('red', 'green')))
    } else {
      pct_cols <- c("Growth (MoM)", "Growth (YoY)", "Growth (YTD)")
      num_cols <- setdiff(names(df)[sapply(df, is.numeric)], pct_cols)
      
      datatable(df,
                rownames = FALSE,
                filter = 'top',
                selection = list(mode = "multiple"
                                 #, selected = 1:5
                ),
                
                extensions = 'Buttons',
                
                options = list(
                  dom = 'Btip', # B = Buttons, f = Global search box, t = table, i/p = info/pagination
                  
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  searching = TRUE,
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
        formatRound(columns = num_cols, digits = 0, mark = ",") |>
        formatPercentage("Growth (MoM)", 1) |>
        formatPercentage("Growth (YoY)", 1) |>
        formatPercentage("Growth (YTD)", 1) |>
        formatStyle("Growth (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
        formatStyle("Growth (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
        formatStyle("Growth (YTD)", color = styleInterval(c(0), c('red', 'green')))
    }
  }
  
  # ---- Data table for annual (YTD) data. Diff is that there's no Growth data here ----
  data_table_annual <- function(df) {
    df <- df() 
    
    is_share <- input$share_toggle == "Share"
    
    # all columns are numbers, but ignore Rank pls
    exclude_cols <- c("Rank")
    num_cols <- setdiff(names(df)[sapply(df, is.numeric)], exclude_cols)
    
    if(is_share) {
      datatable(df,
                rownames = FALSE,
                filter = 'top',
                selection = list(mode = "multiple"
                                 #, selected = 1:5
                ),
                
                extensions = 'Buttons',
                
                options = list(
                  dom = 'Btip', # B = Buttons, f = Global search box, t = table, i/p = info/pagination
  
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  searching = TRUE,
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
                formatPercentage(columns = num_cols, 1)
    } else {
      datatable(df,
                rownames = FALSE,
                filter = 'top',
                selection = list(mode = "multiple"
                                 #, selected = 1:5
                ),
                
                extensions = 'Buttons',
                
                options = list(
                  dom = 'Btip', # B = Buttons, f = Global search box, t = table, i/p = info/pagination
                  
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  searching = TRUE,
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
                formatRound(columns = num_cols, digits = 0, mark = ",")
    }
  }
  
  data_table_annual_tiv <- function(df) {
    df <- df() 
    
    num_cols <- names(df)[sapply(df, is.numeric)]
    datatable(df,
              rownames = FALSE,
              
              options = list(
                dom = 'ft', # B = Buttons, f = filter, t = table, i/p = info/pagination
                
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                paging = FALSE,
                searching = FALSE
              ),
              class = 'cell-border stripe') |>
      formatRound(columns = num_cols, digits = 0, mark = ",")
  }
  
  # ---- Selections ----
  selected_make <- reactive({
    sel <- input$summary_table_rows_selected
    if (length(sel)) summary_data()$`Make`[sel] else summary_data()$`Make`[1:5]
  })
  selected_model <- reactive({
    sel <- input$summary_table_model_rows_selected
    if (length(sel)) summary_data_model()$`Model`[sel] else summary_data_model()$`Model`[1:5] 
  })
  selected_fuel <- reactive({
    sel <- input$summary_table_fuel_rows_selected
    if (length(sel)) summary_data_fuel()$`Fuel Type`[sel] else summary_data_fuel()$`Fuel Type`[1:5] 
  })
  
  # ---- Output Plotly Function ----
  monthly_trends_data_ttl_segment <- reactive({
    if (input$vehicle_segment_fc != "All") {
      df <- car_data |> filter(segment == input$vehicle_segment_fc)
    } else {
      df <- car_data
    }
  })
  
  plot_chart_total <- function(agg_choice) {
    
    if (nrow(car_data) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "lines") %>%
          layout(title = paste("No data available for the selected filters"))
      )
    }
    
    monthly_trends_data_ttl <- monthly_trends_data_ttl_segment() |>
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
  
  plot_chart <- function(df, group_col, group_col_name, agg_choice) {
    df <- filtered_data() # call the reactive to get the data frame.
    
    if (nrow(df) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "lines") %>%
          layout(title = paste("No data available for the selected filters"))
      )
    }
    
    group_col_name_sym <- rlang::as_name(rlang::ensym(group_col))
    
    selected_value <- switch(
      group_col_name_sym,
      "maker" = selected_make(),
      "model" = selected_model(),
      "fuel_grouped" = selected_fuel(),
      NULL
    )
    
    if (!is.null(selected_value)) {
      monthly_trends_data <- df |>
        filter({{ group_col }} %in% selected_value) |>
        mutate(month = date_reg) |>
        group_by(month, {{ group_col }}) |>
        summarise(registration = sum(count), .groups = "drop")
    } else {
      if(group_col_name == "Fuel Type"){
        monthly_trends_data <- df |>
          mutate(month = date_reg) |>
          group_by(month, {{ group_col }}) |>
          summarise(registration = sum(count), .groups = "drop")
      } else{
        
        monthly_trends_data <- df |>
          filter({{ group_col }} %in% selected_value) |>
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
        data = tiv_monthly_l12m, # use the last 12 months only for the plot
        x = ~date_reg, y = ~TIV,
        name = "Actual",
        line = list(color = "black"),
        hovertemplate = "Actual: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines(
        data = forecast_next12,
        x = ~date_reg, y = ~TIV,
        name = "Forecast",
        line = list(color = "orange", dash = "dot"),
        hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
      ) |>
      add_ribbons( 
        data = forecast_next12,
        x = ~date_reg, ymin = ~yhat_lower, ymax = ~yhat_upper,
        name = "90% CI",
        fillcolor = 'rgba(255,165,0,0.2)', 
        line = list(color = 'transparent'),
        hovertemplate = "90% CI: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines( 
        data = forecast_next12,
        x = ~date_reg, y = ~yhat_lower,
        name = "90% CI",
        line = list(color = "rgba(255,165,0,0.2)", width = 0), 
        hovertemplate = "90% CI: %{y:.3~s}<extra></extra>"
      ) |>
      layout(
        title = "TIV with 12-Month Forecast",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Registration"),
        hovermode = "x"
      )
  }
  plot_chart_fc_total_segment <- function() {
    plot_ly() |>
      add_lines(
        data = tiv_monthly_l12m_segment(), # use the last 12 months only for the plot
        x = ~date_reg, y = ~TIV,
        name = "Actual",
        line = list(color = "black"),
        hovertemplate = "Actual: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines(
        data = forecast_next12_segment(),
        x = ~date_reg, y = ~TIV,
        name = "Forecast",
        line = list(color = "orange", dash = "dot"),
        hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
      ) |>
      add_ribbons( 
        data = forecast_next12_segment(),
        x = ~date_reg, ymin = ~yhat_lower, ymax = ~yhat_upper,
        name = "90% CI",
        fillcolor = 'rgba(255,165,0,0.2)', 
        line = list(color = 'transparent'),
        hovertemplate = "90% CI: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines( 
        data = forecast_next12_segment(),
        x = ~date_reg, y = ~yhat_lower,
        name = "90% CI",
        line = list(color = "rgba(255,165,0,0.2)", width = 0), 
        hovertemplate = "90% CI: %{y:.3~s}<extra></extra>"
      ) |>
      layout(
        title = "TIV-Segment with 12-Month Forecast",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Registration"),
        hovermode = "x"
      )
  }
  
  plot_chart_fc_nextmonth <- function() {
    df <- forecast_nextmonth
    
    if (input$chart_mode == "Daily") {
      plot_ly() |>
        add_bars(
          data = df,
          x = ~date_reg, y = ~TIV,
          name = "Forecast",
          marker = list(color = "orange"),
          hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
        ) |>
        layout(
          title = "TIV with Daily Forecast",
          xaxis = list(title = "Day"),
          yaxis = list(title = "Registration"),
          hovermode = "x"
        )
    } else {
      plot_ly() |>
        add_lines(
          data = df,
          x = ~date_reg, y = ~TIV_Cum,
          name = "Forecast",
          line = list(color = "orange", dash = "dot"),
          hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
        ) |>
        layout(
          title = "TIV with Daily Forecast",
          xaxis = list(title = "Day"),
          yaxis = list(title = "Registration"),
          hovermode = "x"
        )
    }
  }
  plot_chart_fc_nextmonth_segment <- function() {
    df <- forecast_nextmonth_segment()
    
    if (input$chart_mode == "Daily") {
      plot_ly() |>
        add_bars(
          data = df,
          x = ~date_reg, y = ~TIV,
          name = "Forecast",
          marker = list(color = "orange"),
          hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
        ) |>
        layout(
          title = "TIV with Daily Forecast",
          xaxis = list(title = "Day"),
          yaxis = list(title = "Registration"),
          hovermode = "x"
        )
    } else {
      plot_ly() |>
        add_lines(
          data = df,
          x = ~date_reg, y = ~TIV_Cum,
          name = "Forecast",
          line = list(color = "orange", dash = "dot"),
          hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
        ) |>
        layout(
          title = "TIV with Daily Forecast",
          xaxis = list(title = "Day"),
          yaxis = list(title = "Registration"),
          hovermode = "x"
        )
    }
  }
  # ---- Reactive summary total ----
  summary_total <- reactive({
    if (input$vehicle_segment_fc != "All"){
      car_data <- car_data |>
        filter(segment == input$vehicle_segment_fc)
    } else {
      car_data <- car_data
    }
    
    month_current_count_ttl <- car_data |>
      filter(date_reg == latest_date) |>
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
      select(category, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, 
             !!month_current_name := count_current, count_ytd_previous, growth_YTD) |>
      rename(
        `Total` := category,
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
  summary_data          <- reactive(make_summary(car_data, "maker", "Make", input$share_toggle))
  summary_data_model    <- reactive(make_summary(car_data, "model", "Model", input$share_toggle))
  summary_data_fuel     <- reactive(make_summary(car_data, "fuel_grouped", "Fuel Type", input$share_toggle))
  
  annual_data           <- reactive(make_annual(car_data, "maker", "Make", input$share_toggle))
  annual_data_model     <- reactive(make_annual(car_data, "model", "Model", input$share_toggle))
  annual_data_tiv       <- reactive(make_annual_tiv(car_data, "no"))
  annual_data_tiv_model <- reactive(make_annual_tiv(car_data, "yes"))
  
  # ---- Reactive validated_results ----
  validated_results_reactive <- reactive({
      if (input$vehicle_segment_fc != "All"){
        df <- validated_results |>
          filter(segment == input$vehicle_segment_fc)
      } else {
        df <- validated_results
      }
    
    df <- df |>
      select(-segment, -year, -month) |>
      rename(
        `Forecast Month` := forecast_month,
        `Forecast TIV` := TIV_FC,
        `Actual TIV` := TIV_ACTUAL,
        `% Error` := pct_error,
        `Remarks` := remarks
      )
    
    return(df)
  })
  
  # ---- Output 1: DataTable ----
  output$summary_table_total    <- renderDT(data_table_TIV(summary_total))
  output$validated_results      <- renderDT(validated_results_render(validated_results_reactive))
  
  # Version that doesn't show error if there's no data when filtered (but creates a new error lol)
  safe_renderDT <- function(data_reactive, table_fun, msg = "No data available for the selected filters") {
    renderDT({
      df <- data_reactive()
      validate(need(nrow(df) > 0, msg))
      table_fun(data_reactive)   # pass the reactive if contains data
    }, server = FALSE)
  }
  
  output$summary_table            <- safe_renderDT(summary_data, data_table)
  output$summary_table_model      <- safe_renderDT(summary_data_model, data_table)
  output$summary_table_fuel       <- safe_renderDT(summary_data_fuel, data_table)
  
  output$annual_table             <- safe_renderDT(annual_data, data_table_annual)
  output$annual_table_model       <- safe_renderDT(annual_data_model, data_table_annual)
  output$annual_table_tiv         <- safe_renderDT(annual_data_tiv, data_table_annual_tiv)
  output$annual_table_model_tiv   <- safe_renderDT(annual_data_tiv_model, data_table_annual_tiv)
  
  # ---- Output 2: Plotly Trend ----
  output$trend_plot_total         <- renderPlotly(plot_chart_total(input$agg_level_ttl))
  output$trend_plot_make          <- renderPlotly(plot_chart(summary_data, maker, "Make", input$agg_level))
  output$trend_plot_model         <- renderPlotly(plot_chart(summary_data_model, model, "Model", input$agg_level_model))
  output$trend_plot_fuel          <- renderPlotly(plot_chart(summary_data_fuel, fuel_grouped, "Fuel Type", input$agg_level_fuel))
  output$forecast_plot_total      <- renderPlotly(plot_chart_fc_total())
  output$forecast_plot_nextmonth  <- renderPlotly(plot_chart_fc_nextmonth())
  output$forecast_plot_total_segment       <- renderPlotly(plot_chart_fc_total_segment())
  output$forecast_plot_nextmonth_segment   <- renderPlotly(plot_chart_fc_nextmonth_segment())
  
  # ---- Output 3: Cards ----
  output$kpi_full <- renderUI({
    vals <- kpi_values()
    div(
      style = "
      background:#f8f9fa;
      padding:12px;
      border-radius:6px;
      text-align:center;
      box-shadow:0 1px 3px rgba(0,0,0,0.1);
      ",
      div(style="font-size:12px; color:#666;", Next_FC_Text),
      div(style="font-size:22px; font-weight:bold;", scales::comma(vals$fc_full))
    )
  })
  output$kpi_mtd <- renderUI({
    vals <- kpi_values()
    div(
      style = "
      background:#f8f9fa;
      padding:12px;
      border-radius:6px;
      text-align:center;
      box-shadow:0 1px 3px rgba(0,0,0,0.1);
      ",
      div(
        style = "font-size:12px; color:#666;", 
        paste0(
          "MTD Forecast (as of ",
          format(mtd_date, "%d %b"),
          ")"
        )
      ),
      div(style="font-size:22px; font-weight:bold;", scales::comma(vals$fc_mtd))
    )
  })
  output$kpi_progress <- renderUI({
    vals <- kpi_values()
    div(
      style = "
      background:#f8f9fa;
      padding:12px;
      border-radius:6px;
      text-align:center;
      box-shadow:0 1px 3px rgba(0,0,0,0.1);
      ",
      div(style="font-size:12px; color:#666;", "Progress"),
      div(style="font-size:22px; font-weight:bold;", scales::percent(vals$progress, accuracy = 0.1))
    )
  })
  
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
} # END OF SERVER

shinyApp(ui, server)
