library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(bslib)
library(htmlwidgets)
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
library(config)
library(mongolite)
library(readr)
library(memoise)

# Getting all variables needed to load data from DB
config_file <- config::get(file = "conf/config.yml")
uid <- config_file$uid
pwd <- config_file$pwd
srv <- config_file$srv

# Connection string for mongodb weather database
connection_string <- stringr::str_interp("mongodb://${uid}:${pwd}@${srv}")

# Mongolite client to connect to database and collection containing weather data
weather_coll <- mongo(collection = "sauWeather", db = "weatherdata", url = connection_string)

# JSON to be used in "project" to return all fields from mongodb database
all_fields <- read_file("data/all_fields.json")

# JSON bo be use in "project" to return fields needed for weather details tab
weather_fields <- read_file("data/weather_fields.json")

# Defining 60 mins cache
cache_30mins <- cachem::cache_mem(max_age = 30 * 60)

# Getting data from database using the provided query
# If list of fields is not provided it will returl all fields from database
load_data <- function(qry, return_fields = all_fields) {
    if (is.null(qry)) {
        df <- weather_coll$find(fields = return_fields)
    } else {
        df <- weather_coll$find(query = qry, fields = return_fields)
    }
    df <- df %>% mutate(
        date_and_time = ymd_hms(date_and_time),
        date = as.Date(date, format = "%m/%d/%Y")
    )
    return(df)
}

# This function returns a single date.  Using the sort_direction argument we indicate if max or min.
# sort_direction = -1 for max
# sort_direction = 1 for min
load_date <- function(sort_direction) {
    sort <- stringr::str_interp('{ "date_and_time": ${sort_direction} }')
    df <- weather_coll$find(fields = '{ "date_and_time": 1 }', sort = sort, limit = 1)
    return(ymd_hms(df$date_and_time))
}
# Using cache for loading max and min
get_max_or_min_date <- memoise(load_date)

# Get the most recent observation from database
load_latest_obs <- function() {
    df <- weather_coll$find(fields = weather_fields, sort = '{ "date_and_time": -1 }', limit = 1)
    df <- df %>% mutate(
        date_and_time = ymd_hms(date_and_time),
        #date_and_time = as_datetime(date_and_time, format = "%Y-%m-%dT%H:%M:%S"),
        date = as.Date(date, format = "%m/%d/%Y")
    )
    return(df)
}
get_latest_obs <- memoise(load_latest_obs, cache = cache_30mins)

# Getting list of unique years from the database
unique_years <- function() {
    years <- weather_coll$distinct(key = "year")
    # Removing 2019 and 2022 because of incomplete data
    unique_years <- as.list(years[!(years %in% c("2019", "2022"))])
    return(unique_years)
}
# Using cache for loading unique years
get_unique_years <- memoise(unique_years)
