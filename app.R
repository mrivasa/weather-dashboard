# Load libraries and create database connection and data access methods
source("global.r", local = TRUE)

# Finding max and min dates from data (needed for the date picker)
max_calendar_date <- get_max_or_min_date(-1)
min_calendar_date <- get_max_or_min_date(1)

# Plot size options
options(repr.plot.width = 5, repr.plot.height = 2)

#--- UI: Page definition
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
  #--- UI: Sidebar (menu options)
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recent Weather", tabName = "selected_dates", icon = icon("umbrella"), selected = TRUE),
      menuItem("Yearly Comparison", tabName = "yearly_comparison", icon = icon("leaf")),
      menuItem("Download Data", tabName = "download_data", icon = icon("download"))
    )
  ),
  #--- UI: Main page
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
    add_loading_state(
      c(".html-widget-output"), # "#latest_temp_box", "#latest_hum_box", "#latest_rain_box", "#latest_soil_box"),
      text = NULL,
      svgColor = "#89a0b3",
      spinner = "pulse",
      timeout = 800,
      backgroundColor = "rgba(30,30,30,0.8)",
      svgSize = "28px",
    ),
    tabItems(
      #--- UI: Recent weather tab
      tabItem(
        tabName = "selected_dates",
        conditionalPanel(
          condition = "output.total_rows > 0",
          panel(
            htmlOutput("latest_obs_date", class = "panel-title obs-title"),
            infoBoxOutput("latest_temp_box", width = 3),
            infoBoxOutput("latest_hum_box", width = 3),
            infoBoxOutput("latest_rain_box", width = 3),
            infoBoxOutput("latest_soil_box", width = 3),
            class = "obs-panel"
          ),
          panel(
            column(
              width = 4,
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
              width = 8,
              br(),
              htmlOutput("display_selected_date"),
              htmlOutput("display_summary")
            )
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
      #--- UI: Yearly comparisons
      tabItem(
        tabName = "yearly_comparison",
        panel(
          fluidRow(
            column(
              width = 4,
              selectInput(
                "year",
                label = "Select Year",
                choices = get_unique_years(),
                selected = year(max_calendar_date)
              ),
            ),
            column(
              width = 4,
              radioButtons(
                "temp_scale",
                label = "Temperature Scale",
                choices = list("Farenheit" = 1, "Celsius" = 2),
                selected = 1,
                inline = TRUE
              )
            ),
            column(
              width = 4,
              radioButtons(
                "precip_scale",
                label = "Precipitation Scale",
                choices = list("Inches" = 1, "Centimeters" = 2),
                selected = 1,
                inline = TRUE
              )
            )
          )
        ),
        alert(
          stat = "info",
          tags$p("Note: Cumulative Yearly Precipitation is not accurate for 2019
              or for February 4, 2020 - December 31, 2020 due to incomplete data.
              30 year average was calculated from data obtained from the National
              Weather Service's weather station at the Chattanooga Airport.")
        ),
        fluidRow(
          column(
            width = 12,
            panel(
              tags$p("Average Temperature", class = "panel-title"),
              plotlyOutput("avg_temp")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            panel(
              tags$p("Average Cumulative Precipitation", class = "panel-title"),
              plotlyOutput("avg_precip")
            )
          )
        )
      ),
      #--- UI: Download data
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
  #--- "On-click" event listener for more_info button in header to show modal with more info
  observeEvent(input$more_info, {
    showModal(modalDialog(
      includeHTML("www/moreinfo.html"),
      title = "Southern.edu Weather Dashboard",
      easyClose = TRUE,
      footer = modalButton("Got It!")
    ))
  })

  #--- This reactive function and "outputOptions" is used to show/hide panels based on data availability
  #--- Taken from: https://shiny.rstudio.com/articles/dynamic-ui.html
  output$total_rows <- reactive({
    nrow(filtered_weather())
  })
  outputOptions(output, "total_rows", suspendWhenHidden = FALSE)

  #--- Filter dataframe based on selcted date range
  #--- Caching reactive result based on selected date range
  filtered_weather <- reactive({
    date2 <- input$date_range[2] + 1
    query <- stringr::str_interp('{ "date_and_time": { "$gte": "${input$date_range[1]}", "$lte": "${date2}"} }')
    load_data(query, weather_fields)
  }) %>%
    bindCache(input$date_range)

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

  #--- Display date of the most recent observation
  output$latest_obs_date <- renderUI({
    obs <- get_latest_obs()
    HTML(paste0("Most Recent Observation: ", format(obs$date_and_time, format = "%A, %B %d, %Y %I:%M %p")))
  })

  #--- Display selected date range
  output$display_selected_date <- renderUI({
    start_date <- format(input$date_range[1], format = "%A, %B %d, %Y")
    end_date <- format(input$date_range[2], format = "%A, %B %d, %Y")
    HTML(sprintf("[%s] to [%s]", start_date, end_date))
  })

  #--- Display how many observatios are in the selected date range
  #--- along with how many days are included in the selected date range
  output$display_summary <- renderUI({
    total_days <- difftime(input$date_range[2] + 1, input$date_range[1], units = "days")
    HTML(sprintf("%s days | %s observations", total_days, nrow(filtered_weather())))
  })

  #--- Query database to get data with all variables
  #--- used in the download option
  download_data <- reactive({
    date2 <- input$date_range_download[2] + 1
    query <- stringr::str_interp('{ "date_and_time": { "$gte": "${input$date_range_download[1]}", "$lte": "${date2}"} }')
    load_data(query)
  })

  #--- Download data for selected date range (all variables)
  #--- this is the "Download Data" tab
  output$download <- downloadHandler(
    filename = function() {
      paste0("data-", input$date_range_download[1], "--", input$date_range_download[2], ".csv")
    },
    content = function(file) {
      write.csv(download_data(), file, row.names = FALSE)
    }
  )

  # --- Latest observation boxes
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

  #--- Display average temp for the selected date
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

  #--- Display average humidity
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

  #--- Display total rain for the selected date
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

  #--- Display average soil moisture for selected date
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

  #--- Display a line plot for temperature
  output$line_temp <- renderPlotly({
    ggplot(filtered_weather(), aes(x = date_and_time)) +
      geom_line(aes(y = temp_f, color = "Temperature")) +
      geom_line(aes(y = heat_index_f, color = "Heat Index")) +
      geom_line(aes(y = windchill_f, color = "Windchill")) +
      theme(legend.position = "top") +
      labs(x = NULL, y = "Degrees (°F)", color = "")
  })

  #--- Display a line plot for soil moisture
  output$line_soil <- renderPlotly({
    ggplot(filtered_weather(), aes(x = date_and_time)) +
      geom_line(aes(y = soil_moisture_1)) +
      labs(x = NULL, y = "Centibars (cb)")
  })

  #--- Getting all weather data
  all_weather <- reactive({
    fields <- '{"date_and_time":1, "date":1, "soil_moisture_1": "$davis_current_observation.soil_moisture_1"}'
    load_data(qry = NULL, fields)
  })

  #--- Display daily moisture over time
  output$week_soil <- renderPlotly({
    df_temp <- all_weather()
    obs <- get_latest_obs()

    by_day <- df_temp %>%
      group_by(date) %>%
      summarise(avg_moisture = mean(soil_moisture_1))

    day_avg <- round((obs$soil_moisture_1_day_high + obs$soil_moisture_1_day_low) / 2, 0)

    all_time_avg <- round(mean(df_temp$soil_moisture_1, na.rm = TRUE), 0)

    ggplot() +
      geom_col(by_day, mapping = aes(x = date, y = avg_moisture)) +
      geom_hline(aes(yintercept = day_avg, color = format(obs$date, format = "%A, %B %d, %Y"))) +
      geom_hline(aes(yintercept = all_time_avg, color = "All-time Average")) +
      labs(color = NULL, x = NULL, y = "Centibars (cb)")
  })

  #--- Display data returned from selected date
  output$weather_data <- renderDT({
    datatable(filtered_weather(), options = list(
      scrollX = TRUE,
      searching = FALSE,
      pagelength = 25
    ))
  })

  #--- Filter nws data, group by day and calculate 30 yr averages.
  #--- Fix date format. Calcualte cumulative precipitation.
  nws_avg <- reactive({
    nws %>%
      mutate(DATE = as.Date(DATE, format = "%m/%d/%Y")) %>%
      mutate(month = month(DATE), day = day(DATE)) %>%
      group_by(month, day) %>%
      summarise(
        temp_avg = mean(TAVG, na.rm = TRUE),
        precip_avg = mean(PRCP, na.rm = TRUE)
      ) %>%
      mutate(calc_date = paste(month, day, input$year, sep = "/")) %>%
      mutate(calc_date = as.Date(calc_date, format = "%m/%d/%Y")) %>%
      mutate(cum_precip = cumsum(precip_avg))
  }) # cum_sum resets monthly.  Change this if using line.

  #--- Display a line plot for average temperature
  output$avg_temp <- renderPlotly({
    daily_avg_temp <- filtered_year() %>%
      # mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
      group_by(date) %>%
      summarise(mean_daily_temp_f = mean(temp_f))
    nws_average <- nws_avg()
    temp_choice_1 <- daily_avg_temp$mean_daily_temp_f
    if (input$temp_scale == 2) {
      temp_choice_1 <- ((daily_avg_temp$mean_daily_temp_f - 32) * 5 / 9)
    }
    temp_choice_2 <- nws_average$temp_avg
    if (input$temp_scale == 2) {
      temp_choice_2 <- ((nws_average$temp_avg - 32) * 5 / 9)
    }
    ggplot() +
      geom_line(data = daily_avg_temp, aes(x = date, y = temp_choice_1, color = paste(input$year))) +
      geom_line(data = nws_average, aes(x = calc_date, y = temp_choice_2, color = "30 Year Average")) +
      theme(legend.position = "top") +
      labs(x = NULL, y = "Degrees", color = "")
  })

  #--- Filtering data by selected year data will be cached based on selected year
  filtered_year <- reactive({
    query <- stringr::str_interp('{ "year": "${input$year}" }')
    fields <- '{"_id":0, "date_and_time":1, "date": 1, "temp_f": { "$ifNull": ["$temp_f", "$davis_current_observation.temp_in_f"] }, "temp_c": { "$ifNull": ["$temp_c", 0] }, "rain_day_in": "$davis_current_observation.rain_day_in" }'
    load_data(query, fields)
  }) %>%
    bindCache(input$year)

  #--- Display a line plot for cumulative yearly precipitation
  output$avg_precip <- renderPlotly({
    cum_precipitation <- filtered_year() %>%
      mutate(month = month(date), day = day(date)) %>%
      group_by(month, day) %>%
      summarise(mean_rain_day_in = mean(rain_day_in)) %>%
      mutate(date = paste(month, day, input$year, sep = "/")) %>%
      mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(cum_precip = cumsum(mean_rain_day_in))

    nws_average <- nws_avg()

    precip_choice_1 <- cum_precipitation$cum_precip
    if (input$precip_scale == 2) {
      precip_choice_1 <- (cum_precipitation$cum_precip * 2.54)
    }
    precip_choice_2 <- nws_average$cum_precip
    if (input$precip_scale == 2) {
      precip_choice_2 <- (nws_average$cum_precip * 2.54)
    }

    #    ggplot() +
    #      geom_line(data = cum_precipitation, aes(x = date, y = precip_choice_1, color = "Cumulative Precipitation")) +
    #      geom_line(data = nws_average, aes(x = calc_date, y = precip_choice_2, color = "30 Year Average")) +
    #    theme(legend.position = "top") +
    #    labs(x = NULL, y = "Precipitation", color = "")

    ggplot() +
      geom_col(data = cum_precipitation, aes(x = date, y = precip_choice_1, fill = paste(input$year), alpha = 0.2)) +
      geom_col(data = nws_average, aes(x = calc_date, y = precip_choice_2, fill = "30 Year Average", alpha = 0.15)) +
      geom_col(position = "dodge") +
      theme(legend.position = "top", legend.title = NULL) +
      labs(x = NULL, y = "Precipitation", fill = "")
  })
}

shinyApp(ui = ui, server = server)
