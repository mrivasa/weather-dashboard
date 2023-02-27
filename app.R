# Load libraries and create database connection and data access methods
source("global.r", local = TRUE)

# Finding max and min dates from data (needed for the date picker)
max_calendar_date <- get_max_or_min_date(-1)
min_calendar_date <- get_max_or_min_date(1)

# UI definition
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Southern.edu Weather",
    tags$li(
      actionLink(
        "more_info",
        label = NULL,
        icon = icon("circle-info")
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recent Weather", tabName = "selected_dates", icon = icon("umbrella"), selected = TRUE),
      menuItem("Yearly Comparisons", tabName = "grow_weather", icon = icon("leaf")),
      menuItem("Download Data", tabName = "download_data", icon = icon("download"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "weather.css"
      ),
      tags$script(
        src = "scroll.js"
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
                start = as.Date(max_calendar_date) - 6,
                end = max_calendar_date,
                format = "mm/dd/yyyy",
                min = min_calendar_date,
                max = max_calendar_date
              ),
              checkboxInput("show_data", label = "Display data (scroll down)"),
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
          panel(
            htmlOutput("latest_obs_date", class = "panel-title"),
            infoBoxOutput("latest_temp_box", width = 3),
            infoBoxOutput("latest_hum_box", width = 3),
            infoBoxOutput("latest_rain_box", width = 3),
            infoBoxOutput("latest_soil_box", width = 3)
          ),
          panel(
            tags$div("Selected Date Range", class = "panel-title"),
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
                tags$p(tags$span("Soil Moisture "), icon("circle-info", id = "soil_info_icon"), class = "panel-title"),
                plotlyOutput("line_soil")
              ),
              bsPopover(
                id = "soil_info_icon",
                title = "What is soil moisture?",
                content = paste0("The Davis Instruments sensor measures on a scale of 0 (fully wet) to 200 (fully dry) centibars.  The sensor measures the vacuum created in the soil by the lack of moisture."),
                placement = "right",
                trigger = "click",
                options = list(container = "body")
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
              DTOutput("weather_data"),
              id = "data_panel"
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
        wellPanel(
          fluidRow(
            column(
              width = 4,
              selectInput(
                "year",
                label = "Select Year",
                choices = get_unique_years()
              )
            ),
            column(
              width = 4,
              radioButtons(
                "scale",
                label = "Temperature Scale",
                choices = list("Farenheit" = 1, "Celsius" = 2),
                selected = 1,
                inline = TRUE
              )
            ),
            column(
              width = 12,
              tags$p("Note: Cumulative Growing Degree Days Data are not accurate for 2019 or for February 4, 2020 - December 31, 2020 due to incomplete data.")
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
        ),
        fluidRow(
          column(
            width = 12,
            panel(
              tags$p("Growing Degree Days - Data", class = "panel-title"),
              DTOutput("gdd_data")
            )
          )
        )
      ),
      tabItem(
        tabName = "download_data",
        wellPanel(
          fluidRow(
            column(
              width = 6,
              dateRangeInput("date_range_download",
                label = span("Date Range:", id = "date_range_header"),
                start = as.Date(max_calendar_date) - 6,
                end = max_calendar_date,
                format = "mm/dd/yyyy",
                min = min_calendar_date,
                max = max_calendar_date
              ),
              downloadButton("download", "Download", icon = icon("download"), class = "btn-success")
            )
          )
        )
      )
    )
  )
)

# Server code
server <- function(input, output, session) {
  # Filter dataframe based on selcted date range
  filtered_weather <- reactive({
    date2 <- input$date_range[2] + 1
    query <- stringr::str_interp('{ "date_and_time": { "$gte": "${input$date_range[1]}", "$lte": "${date2}"} }')
    load_data(query, weather_fields)
  }) %>%
    bindCache(input$date_range)

  # This reactive function is used to show/hide panels when data is available
  output$total_rows <- reactive({
    nrow(filtered_weather())
  })

  # Taken from: https://shiny.rstudio.com/articles/dynamic-ui.html
  outputOptions(output, "total_rows", suspendWhenHidden = FALSE)

  # Filter data by selected year
  filtered_year <- reactive({
    query <- stringr::str_interp('{ "year": "${input$year}" }')
    fields <- '{"date_and_time":1, "date": 1, "temp_f": { "$ifNull": ["$temp_f", "$davis_current_observation.temp_in_f"] }, "temp_c": { "$ifNull": ["$temp_c", 0] } }'
    load_data(query, fields)
  })

  # "On-click" event listener for more_info button in header
  observeEvent(input$more_info, {
    showModal(modalDialog(
      includeHTML("www/moreinfo.html"),
      title = "Southern.edu Weather Dashboard",
      easyClose = TRUE,
      footer = modalButton("Got It!")
    ))
  })

  # Group by date. Calculate GDD (daily max + daily min / 2 - base)
  grow_deg_day <- reactive({
    filtered_year() %>%
      group_by(date) %>%
      summarise(
        max_temp = max(temp_f, na.rm = TRUE),
        min_temp = min(temp_f, na.rm = TRUE),
        max_temp_c = max(temp_c, na.rm = TRUE),
        min_temp_c = min(temp_c, na.rm = TRUE),
        gdd_f = ((max_temp + min_temp) / 2 - 50),
        gdd_c = ((max_temp_c + min_temp_c) / 2 - 10),
      ) %>%
      mutate(
        gdd_f = case_when(gdd_f <= 0 ~ 0, gdd_f > 0 ~ gdd_f),
        gdd_c = case_when(gdd_c <= 0 ~ 0, gdd_c > 0 ~ gdd_c),
        cumulative_gdd_f = cumsum(gdd_f),
        cumulative_gdd_c = cumsum(gdd_c)
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

  output$latest_obs_date <- renderUI({
    obs <- get_latest_obs()
    HTML(paste0("Most Recent Observation: ", format(obs$date_and_time, format = "%A, %B %d, %Y %I:%M %p")))
  })

  # Query database to get data with all variables
  download_data <- reactive({
    date2 <- input$date_range_download[2] + 1
    query <- stringr::str_interp('{ "date_and_time": { "$gte": "${input$date_range_download[1]}", "$lte": "${date2}"} }')
    load_data(query)
  })

  # Download data for selected date range (all variables)
  output$download <- downloadHandler(
    filename = function() {
      paste0("data-", input$date_range_download[1], "--", input$date_range_download[2], ".csv")
    },
    content = function(file) {
      write.csv(download_data(), file, row.names = FALSE)
    }
  )

  # --- Latest observation ---
  output$latest_temp_box <- renderInfoBox({
    obs <- get_latest_obs()
    box_color <- "light-blue"
    if (obs$temp_f >= 80) {
      box_color <- "yellow"
    }
    infoBox(
      title = "Temperature",
      value = paste0(obs$temp_f, " °F"),
      color = box_color,
      fill = TRUE,
      icon = icon("temperature-half")
    )
  })

  output$latest_hum_box <- renderInfoBox({
    obs <- get_latest_obs()
    infoBox(
      title = "Relative Humidity",
      value = paste(obs$relative_humidity, " %"),
      icon = icon("droplet"),
      color = "olive",
      fill = TRUE
    )
  })

  output$latest_rain_box <- renderInfoBox({
    obs <- get_latest_obs()
    infoBox(
      title = "Rain",
      value = paste(obs$rain_rate_in_per_hr, " in"),
      subtitle = paste("Day total: ", obs$rain_day_in, " in"),
      icon = icon("cloud-rain"),
      color = "aqua",
      fill = TRUE
    )
  })

  output$latest_soil_box <- renderInfoBox({
    obs <- get_latest_obs()
    infoBox(
      title = "Soil Moisture",
      value = paste(obs$soil_moisture_1, " cb"),
      icon = icon("water"),
      color = "maroon",
      fill = TRUE
    )
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
    ggplot(filtered_weather(), aes(x = date_and_time)) +
      geom_line(aes(y = temp_f, color = "Temperature")) +
      geom_line(aes(y = heat_index_f, color = "Heat Index")) +
      geom_line(aes(y = windchill_f, color = "Windchill")) +
      theme(legend.position = "top") +
      labs(x = NULL, y = "Degrees (°F)", color = "")
  })

  # Display a line plot for soil moisture
  output$line_soil <- renderPlotly({
    ggplot(filtered_weather(), aes(x = date_and_time)) +
      geom_line(aes(y = soil_moisture_1)) +
      labs(x = NULL, y = "Centibars (cb)")
  })

  # Getting all weather data
  all_weather <- reactive({
    fields <- '{"date_and_time":1, "date":1, "soil_moisture_1": "$davis_current_observation.soil_moisture_1"}'
    load_data(qry = NULL, fields)
  })

  # Display daily moisture average for all days of the week
  output$week_soil <- renderPlotly({
    df_temp <- all_weather()
    obs <- get_latest_obs()

    by_day <- df_temp %>%
      group_by(date) %>%
      summarise(avg_moisture = mean(soil_moisture_1))

    day_avg <- round((obs$soil_moisture_1_day_high + obs$soil_moisture_1_day_low)/2, 0)
    #day_avg <- round(mean(the_day$soil_moisture_1, na.rm = TRUE), 0)

    all_time_avg <- round(mean(df_temp$soil_moisture_1, na.rm = TRUE), 0)

    ggplot() +
      geom_col(by_day, mapping = aes(x = date, y = avg_moisture)) +
      geom_hline(aes(yintercept = day_avg, color = "Recent Obs.")) +
      geom_hline(aes(yintercept = all_time_avg, color = "All-time Average")) +
      labs(color = NULL, x = NULL, y = "Centibars (cb)")
  })

  # Display filtered data
  output$weather_data <- renderDT({
    datatable(filtered_weather(), options = list(
      scrollX = TRUE,
      searching = FALSE,
      pagelength = 25
    ))
  })

  # Display a line plot for growing degree days
  output$growing_temp <- renderPlotly({
    scale <- "cumulative_gdd_f"
    if (input$scale == 2) {
      scale <- "cumulative_gdd_c"
    }
    ggplot(grow_deg_day(), aes(x = date)) +
      geom_line(aes_string(y = scale)) +
      labs(x = NULL, y = "Degrees")
  })

  # Displaying GDD data
  output$gdd_data <- renderDT({
    datatable(grow_deg_day(),
      options = list(
        scrollX = TRUE,
        searching = FALSE,
        pagelength = 25
      )
    )
  })
}

shinyApp(ui = ui, server = server)
