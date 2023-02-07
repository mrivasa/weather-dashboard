library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(fontawesome)
library(readr)
library(dplyr)
library(DT)

library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(jtools)
theme_set(theme_minimal())

# Loading the data
weather <- as.data.frame(read_csv("data/weather.csv",
  col_types = cols(
    date = col_date(format = "%m/%d/%Y"),
    `date and time` = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"),
    time = col_time(format = "%H:%M:%S")
  )
))

# Finding max and min dates from data (needed for the date picker)
min_calendar_date <- min(weather$date, na.rm = TRUE)
max_calendar_date <- max(weather$date, na.rm = TRUE)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Southern.edu Weather"),
  dashboardSidebar(
    dateInput("selected_date",
      label = h4("Select Date"),
      value = max_calendar_date,
      format = "mm/dd/yyyy",
      min = min_calendar_date,
      max = max_calendar_date
    ),
    checkboxInput("show_data", label = "Display data")
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "weather.css"
      )
    ),
    conditionalPanel(
      condition = "output.total_rows > 0",
      wellPanel(
        htmlOutput("display_selected_date")
      ),
      fluidRow(
        valueBoxOutput(width = 3, "temp"),
        valueBoxOutput(width = 3, "humidity"),
        valueBoxOutput(width = 3, "rain"),
        valueBoxOutput(width = 3, "soil")
      ),
      fluidRow(
        column(
          width = 6,
          div(
            plotOutput("line_temp")
          )
        ),
        column(
          width = 6,
          plotOutput("line_soil")
        )
      ),
      conditionalPanel(
        condition = "input.show_data",
        br(),
        fluidRow(
          div(
            id = "dt_container",
            DTOutput("weather_data")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "output.total_rows <= 0",
      alert(
        status = "info",
        tags$b("Note:"), "There is no data available for that date."
      )
    )
  )
)

server <- function(input, output, session) {
  # Filter dataframe based on selcted date
  filtered_weather <- reactive({
    weather[weather$date == input$selected_date, ]
  })

  # This reactive function is used to show/hide panels when data is available
  output$total_rows <- reactive({
    nrow(filtered_weather())
  })

  # Taken from: https://shiny.rstudio.com/articles/dynamic-ui.html
  outputOptions(output, "total_rows", suspendWhenHidden = FALSE)

  # Display selected date
  output$display_selected_date <- renderUI({
    todays_date <- format(input$selected_date, format = "%A, %B %d, %Y")
    HTML(paste(todays_date))
  })

  # Display average temp for the selected date
  output$temp <- renderValueBox({
    avg <- round(mean(filtered_weather()$temp_f, na.rm = TRUE), 0)
    box_color <- "light-blue"
    if (avg >= 80) {
      box_color <- "yellow"
    }
    valueBox(
      paste0(avg, " °F"),
      "Average Temperature",
      icon = icon("temperature-half"),
      color = box_color
    )
  })

  # Display average humidity
  output$humidity <- renderValueBox({
    avg <- round(mean(filtered_weather()$relative_humidity, na.rm = TRUE), 0)
    valueBox(
      paste0(avg, " %"),
      "Average Relative Humidity",
      icon = icon("droplet"),
      color = "olive"
    )
  })

  # Display total rain for the selected date
  output$rain <- renderValueBox({
    total <- sum(filtered_weather()$rain_rate_in_per_hr)
    valueBox(
      paste0(total, " in"),
      "Total Rain",
      icon = icon("cloud-rain"),
      color = "aqua"
    )
  })

  # Display average soil moisture for selected date
  output$soil <- renderValueBox({
    avg <- round(mean(filtered_weather()$soil_moisture_1, na.rm = TRUE), 0)
    valueBox(
      paste0(avg, " cb"),
      "Average Soil Moisture",
      icon = icon("water"),
      color = "olive"
    )
  })

  # Display a line plot for temperature
  output$line_temp <- renderPlot({
    ggplot(filtered_weather(), aes(x = time)) +
      geom_line(aes(y = temp_f), color = "darkblue") +
      geom_line(aes(y = heat_index_f, color = "pink")) +
      labs(x = "Hours", y = "Temperature")
  })

  # Display a line plot for soil moisture
  output$line_soil <- renderPlot({
    ggplot(filtered_weather(), aes(x = time, y = soil_moisture_1)) +
      geom_line() +
      labs(x = "Hours", y = "Moisture")
  })

  # Display filtered data (TODO display data when user checks the box)
  output$weather_data <- renderDT({
    datatable(filtered_weather(), options = list(scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)
