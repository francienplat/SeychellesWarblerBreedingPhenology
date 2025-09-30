#absolute humidity calculations
library("readxl")
library("dplyr")
library("lubridate")
library("tidyverse")

#load excelfile with worldmet data
WORLDMETDATA<-read.csv("Seychelles_weather1997-2024.csv")

#select relevant columns and filter out NAs
AbsHumidity<-WORLDMETDATA|>
  select(date, air_temp, RH)|>
  filter(!is.na(air_temp))|>
  filter(!is.na(RH))

#exclude days with less than 5 measurements (CAN BE CHANGED TO OTHER MINIMAL N) 
AbsHumidity<-AbsHumidity|>
  mutate(date = as.Date(date)) 
dailyAbsHumidity <- AbsHumidity |>
  group_by(date)|>
  summarise(
    n_measurements = n(),#count of measurements per day
    MeanRH = mean(RH, na.rm = TRUE))|>
  ungroup()
dailyAbsHumidityFiltered <- dailyAbsHumidity|>
  filter(n_measurements >= 5)

# Do the same for temperature (results in the same dates as humidity)
temp<-AbsHumidity|>
  mutate(date = as.Date(date)) 
dailytemp <- temp |>
  group_by(date)|>
  summarise(
    n_measurements = n(),#count of measurements per day
    MeanTemp = mean(air_temp, na.rm = TRUE))|>
  ungroup()
dailytempFiltered <- dailytemp|>
  filter(n_measurements >= 5)

#join humidity and temperature data
AbsHumidData <- inner_join(dailyAbsHumidityFiltered, dailytempFiltered, by = "date")|>
  select(date, MeanRH, MeanTemp)


# Function to calculate absolute humidity (g/m³)
calc_absolute_humidity <- function(MeanTemp, MeanRH) {
  #1: saturation vapor pressure (hPa)
  es <- 6.112 * exp((17.67 * MeanTemp) / (MeanTemp + 243.5))
  
  #2: actual vapor pressure (hPa)
  e <- (MeanRH / 100) * es
  
  #3: absolute humidity (g/m³)
  ah <- (216.74 * e) / (MeanTemp + 273.15)
  
  return(ah)}

AbsHumidData <- AbsHumidData %>%
  mutate(AbsoluteHumidity = calc_absolute_humidity(MeanTemp, MeanRH))


