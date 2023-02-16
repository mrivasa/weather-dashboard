library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(fontawesome)
library(readr)
library(dplyr)
library(DT)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(gridExtra)
library(jtools)

# Loading the data
weather <- as.data.frame(read_csv("data/weather.csv",
  col_types = cols(
    date = col_date(format = "%m/%d/%Y"),
    `date and time` = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"),
    time = col_time(format = "%H:%M:%S")
  )
))

# Finding max and min dates from data (needed for the date picker)
max_calendar_date <- max(weather$date, na.rm = TRUE)
min_calendar_date <- min(weather$date, na.rm = TRUE)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Southern.edu Weather"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Date Range", tabName = "selected_dates", selected = TRUE),
      menuItem("Grow Weather", tabName = "grow_weather")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "weather.css"
      )
    ),
    tabItems(
      tabItem(
        tabName = "selected_dates",
        wellPanel(
          fluidRow(
            column(
              width = 6,
              dateRangeInput("date_range",
                label = span("Date Range:", id = "date_range_header"),
                start = max_calendar_date - 6,
                end = max_calendar_date,
                format = "mm/dd/yyyy",
                min = min_calendar_date,
                max = max_calendar_date
              ),
              checkboxInput("show_data", label = "Display data"),
              id = "input_controls"
            ),
            column(
              width = 6,
              br(),
              htmlOutput("display_selected_date"),
              htmlOutput("display_summary")
            )
          )
        ),
        conditionalPanel(
          condition = "output.total_rows > 0",
          fluidRow(
            infoBoxOutput("temp_info", width = 3),
            infoBoxOutput("humidity_info", width = 3),
            infoBoxOutput("rain_info", width = 3),
            infoBoxOutput("soil_info", width = 3)
          ),
          fluidRow(
            column(
              width = 6,
              panel(
                tags$p("Temperature", class = "panel-title"),
                plotlyOutput("line_temp")
              )
            ),
            column(
              width = 6,
              panel(
                tags$p("Soil Moisture", class = "panel-title"),
                plotlyOutput("line_soil")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              panel(
                tags$p("Average Soil Moisture Over Time", class = "panel-title"),
                plotlyOutput("week_soil")
              )
            )
          ),
          conditionalPanel(
            condition = "input.show_data",
            panel(
              tags$p("Data Filtered by Selected Date", class = "panel-title"),
              DTOutput("weather_data")
            )
          )
        ),
        conditionalPanel(
          condition = "output.total_rows <= 0",
          alert(
            status = "info",
            tags$b("Note:"), "There is no data available for the selected date range."
          )
        )
      ),
      tabItem(
        tabName = "grow_weather",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "year",
              label = "Select Year",
              choices = (unique(year(weather$date)))
            )
          ),
          column(
            width = 4,
            numericInput(
              "base",
              label = "Base Temperature",
              value = 50
            )
          ),
          column(
            width = 3,
            radioButtons(
              "scale",
              label = "Temperature Scale",
              choices = list("Farenheit" = 1, "Celsius" = 2),
              selected = 1,
              inline = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            panel(
              tags$p("Growing Degree Days", class = "panel-title"),
              plotlyOutput("growing_temp")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Filter dataframe based on selcted date range
  filtered_weather <- reactive({
    weather[weather$date >= input$date_range[1] & weather$date <= input$date_range[2], ]
  })

  # Most recent date will be today's date if data is available for that date
  most_recent_date <- reactive({
    temp <- Sys.Date()
    if (temp > max_calendar_date) {
      temp <- max_calendar_date
    }
    temp
  })

  # This reactive function is used to show/hide panels when data is available
  output$total_rows <- reactive({
    nrow(filtered_weather())
  })

  # Taken from: https://shiny.rstudio.com/articles/dynamic-ui.html
  outputOptions(output, "total_rows", suspendWhenHidden = FALSE)

  # Filter data by selected year
  filtered_year <- reactive({
    weather[year(weather$date) == input$year,]
  })

  # Group by date. Calculate GDD (daily max + daily min / 2 - base)
  grow_deg_day <- reactive({
    filtered_year() %>%
      group_by(date) %>%
      summarise(
        max_temp = max(temp_f),
        min_temp = min(temp_f),
        max_temp_c = max(temp_c),
        min_temp_c = min(temp_c),
        gdd_f = ((max_temp + min_temp)/2-input$base),
        gdd_c = ((max_temp_c + min_temp_c)/2-((input$base - 32) * 5/9)),
      ) %>%
      mutate(
        gdd_f = case_when(gdd_f <= 0 ~ 0, gdd_f > 0 ~ gdd_f),
        gdd_c = case_when(gdd_c <= 0 ~ 0, gdd_c > 0 ~ gdd_c)
      )
  })

  # Display selected date
  output$display_selected_date <- renderUI({
    start_date <- format(input$date_range[1], format = "%A, %B %d, %Y")
    end_date <- format(input$date_range[2], format = "%A, %B %d, %Y")
    HTML(sprintf("[%s] to [%s]", start_date, end_date))
  })

  output$display_summary <- renderUI({
    total_days <- difftime(input$date_range[2] + 1, input$date_range[1], units = "days")
    HTML(sprintf("%s days | %s observations", total_days, nrow(filtered_weather())))
  })

  # Display average temp for the selected date
  output$temp_info <- renderInfoBox({
    avg <- round(mean(filtered_weather()$temp_f, na.rm = TRUE), 0)
    min_t <- round(min(filtered_weather()$temp_f, na.rm = TRUE), 0)
    max_t <- round(max(filtered_weather()$temp_f, na.rm = TRUE), 0)
    box_color <- "light-blue"
    if (avg >= 80) {
      box_color <- "yellow"
    }
    infoBox(
      title = "Avg. Temperature",
      value = paste0(avg, " °F"),
      subtitle = sprintf("Max: %s | Min: %s", max_t, min_t),
      color = box_color,
      fill = TRUE,
      icon = icon("temperature-half")
    )
  })

  # Display average humidity
  output$humidity_info <- renderInfoBox({
    avg <- round(mean(filtered_weather()$relative_humidity, na.rm = TRUE), 0)
    min_h <- round(min(filtered_weather()$relative_humidity, na.rm = TRUE), 0)
    max_h <- round(max(filtered_weather()$relative_humidity, na.rm = TRUE), 0)
    infoBox(
      title = "Avg. Relative Humidity",
      value = paste0(avg, " %"),
      subtitle = sprintf("Max: %s | Min: %s", max_h, min_h),
      icon = icon("droplet"),
      color = "olive",
      fill = TRUE
    )
  })

  # Display total rain for the selected date
  output$rain_info <- renderInfoBox({
    total <- sum(filtered_weather()$rain_rate_in_per_hr)
    avg_r <- round(mean(filtered_weather()$rain_day_in), 2)
    infoBox(
      title = "Total Rain",
      value = paste0(total, " in"),
      subtitle = sprintf("Daily Avg. %s", avg_r),
      icon = icon("cloud-rain"),
      color = "aqua",
      fill = TRUE
    )
  })

  # Display average soil moisture for selected date
  output$soil_info <- renderInfoBox({
    avg <- round(mean(filtered_weather()$soil_moisture_1, na.rm = TRUE), 0)
    max_s <- round(max(filtered_weather()$soil_moisture_1, na.rm = TRUE), 0)
    min_s <- round(min(filtered_weather()$soil_moisture_1, na.rm = TRUE), 0)
    infoBox(
      title = "Avg. Soil Moisture",
      value = paste0(avg, " cb"),
      subtitle = sprintf("Max: %s | Min: %s", max_s, min_s),
      icon = icon("water"),
      color = "maroon",
      fill = TRUE
    )
  })

  # Display a line plot for temperature
  output$line_temp <- renderPlotly({
    ggplot(filtered_weather(), aes(x = `date and time`)) +
      geom_line(aes(y = temp_f, color = "Temperature")) +
      geom_line(aes(y = heat_index_f, color = "Heat Index")) +
      geom_line(aes(y = windchill_f, color = "Windchill")) +
      theme(legend.position = "top") +
      labs(x = NULL, y = "Temperature", color = "")
  })

  # Display a line plot for soil moisture
  output$line_soil <- renderPlotly({
    ggplot(filtered_weather(), aes(x = `date and time`)) +
      geom_line(aes(y = soil_moisture_1)) +
      labs(x = NULL, y = "Moisture")
  })

  # Display daily moisture average for all days of the week
  output$week_soil <- renderPlotly({
    df_temp <- data.frame(date = weather$date, moisture = weather$soil_moisture_1)

    by_day <- df_temp %>%
      group_by(date) %>%
      summarise(avg_moisture = mean(moisture))

    the_day <- df_temp[df_temp$date == most_recent_date(), ]

    day_avg <- round(mean(the_day$moisture), 0)

    all_time_avg <- round(mean(weather$soil_moisture_1), 0)

    ggplot() +
      geom_col(by_day, mapping = aes(x = date, y = avg_moisture)) +
      geom_hline(aes(yintercept = day_avg, color = "Day Average")) +
      geom_hline(aes(yintercept = all_time_avg, color = "All-time Average")) +
      labs(color = NULL, x = NULL, y = "Average Moisture")
  })

  # Display filtered data
  output$weather_data <- renderDT({
    datatable(filtered_weather(), options = list(
      scrollX = TRUE,
      searching = FALSE,
      pagelength = 50
    ))
  })

  # Display a line plot for growing degree days
  output$growing_temp <- renderPlotly({
    scale <- "gdd_f"
    if(input$scale == 2){
      scale <- "gdd_c"
    }
    ggplot(grow_deg_day(), aes(x = date)) +
      geom_line(aes_string(y = scale)) +
      labs(x = NULL, y = "Degrees")
  })
}

shinyApp(ui = ui, server = server)
