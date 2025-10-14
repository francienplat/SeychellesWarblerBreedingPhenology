library(dplyr)
library(ggplot2)
library(worldmet)
library(tidyverse)
library(climwin)
library(lubridate)
library(readxl)
library(emmeans)
WORLDMETDOWNLOAD<-read.csv("Seychelles_weather1997-2024.csv")

###############################################################################

###############################################################################
#Max temperature

MaxTemperature <- WORLDMETDOWNLOAD
# date without time means 00:00 of that day
MaxTemperature<-MaxTemperature|>
  filter(!is.na(air_temp))

#of how many days are there data?
# Extract the date part (without time)
MaxTemperature$only_date <- as.Date(MaxTemperature$date)

# Count the number of unique days
# Extract the date part (without time)
MaxTemperature$only_date <- as.Date(MaxTemperature$date)
unique_days <- length(unique(MaxTemperature$only_date))
print(paste("There are", unique_days, "separate days in the dataset."))
#10176 Separate dates in the data set!

#check how many data entries there are for all time stamps
#What if i use time at 9:am
MaxTemperature_9am <- MaxTemperature|>
  filter(!is.na(air_temp))|>
  mutate(datetime = ymd_hms(date, quiet = TRUE))|>
  filter(hour(datetime) == 9) 
#9294 entries

#12:am
MaxTemperature_12pm <- MaxTemperature|>
  filter(!is.na(air_temp))|>
  mutate(datetime = ymd_hms(date, quiet = TRUE))|>
  filter(hour(datetime) == 12)
#9.816 entries

#16:am
MaxTemperature_16pm <- MaxTemperature|>
  filter(!is.na(air_temp))|>
  mutate(datetime = ymd_hms(date, quiet = TRUE))|>
  filter(hour(datetime) == 16)
#8.403

#13 AM
MaxTemperature_13pm <- MaxTemperature|>
  filter(!is.na(air_temp))|>
  mutate(datetime = ymd_hms(date, quiet = TRUE))|>
  filter(hour(datetime) == 13)
#8.427 entries

#14 am
MaxTemperature_14pm <- MaxTemperature|>
  filter(!is.na(air_temp))|>
  mutate(datetime = ymd_hms(date, quiet = TRUE))|>
  filter(hour(datetime) == 14)
#8448 entries

#conclusion: most days have at least 5 datapoints, so average or maximum is fine
#to take. For temperature I will use the max temperature, as that has an effect
#on insects. 

MaxTemperature <- MaxTemperature|>
  mutate(day = as.Date(date))|>
  group_by(day)|>
  summarise(max_temp = max(air_temp, na.rm = TRUE))

MaxTemperature <- MaxTemperature |>
  filter(day<=("2023-12-31"))
#save this
library(writexl)

# write_xlsx(MaxTemperature, path = "MaxTemperature_1997-2023.xlsx")
# in excel data format changed to dd mm jjjj (as in R this did not work)
readxl::read_xlsx("MaxTemperature_1997-2023.xlsx") -> MaxTemperature

#to plot: monthly average temepratures
MaxTemperature_monthly <- MaxTemperature |>
  mutate(month = floor_date(day, "month")) |>
  group_by(month) |>
  summarise(monthly_max_temp = mean(max_temp, na.rm = TRUE), .groups = "drop")


# Plot the monthly average maximum temperatures
ggplot(MaxTemperature_monthly, aes(x = month, y = monthly_max_temp)) +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
  labs(title = "Monthly Average Maximum Temperatures in Seychelles",
       x = "Year",
       y = "Maximum Temperature (°C)") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 0, hjust = 1,))




#linear model
model_monthly_max_temp <- lm(monthly_max_temp ~ month, data = MaxTemperature_monthly)
summary(model_monthly_max_temp)
lm(monthly_max_temp ~ month, data = MaxTemperature_monthly)
head(MaxTemperature_monthly$month)
class(MaxTemperature_monthly$month)

#normality of residuals check!
# 1) Histogram of residuals
hist(residuals(model_monthly_max_temp))

# 2) QQ-plot
qqnorm(residuals(model_monthly_max_temp)); qqline(residuals(model_monthly_max_temp))

# 3) Shapiro-Wilk test (formal)
shapiro.test(residuals(model_monthly_max_temp))


MaxTemperature_monthly$year <- as.numeric(format(MaxTemperature_monthly$month, "%Y"))

modelll<-lm(monthly_max_temp ~ year, data = MaxTemperature_monthly)
summary(modelll)
library(dplyr)

# Add year column
MaxTemperature_monthly <- MaxTemperature_monthly %>%
  mutate(year = as.numeric(format(month, "%Y")))

# 1. Within-year standard deviation
yearly_sd <- MaxTemperature_monthly %>%
  group_by(year) %>%
  summarise(sd_temp = sd(monthly_max_temp),
            range_temp = max(monthly_max_temp) - min(monthly_max_temp))

summary(yearly_sd$sd_temp)   # mean, min, max SD within years
summary(yearly_sd$range_temp)

# 2. Mean temperature per year
yearly_mean <- MaxTemperature_monthly %>%
  group_by(year) %>%
  summarise(mean_temp = mean(monthly_max_temp))

summary(yearly_mean$mean_temp)  # mean, min, max across years
sd(yearly_mean$mean_temp)      # standard deviation across years

#warmest and coldest year
warmest_year <- yearly_mean %>%
  filter(mean_temp == max(mean_temp))
coldest_year <- yearly_mean %>%
  filter(mean_temp == min(mean_temp))



################################################################################
#plotting yearly mean max temperature trends
MaxTemperature_yearly <- MaxTemperature |>
  mutate(year = year(day)) |>
  group_by(year) |>
  summarise(yearly_max_temp = mean(max_temp, na.rm = TRUE), .groups = "drop")
# Plot the yearly average maximum temperatures
ggplot(MaxTemperature_yearly, aes(x = year, y = yearly_max_temp)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Yearly Average Maximum Temperatures in Seychelles",
       x = "Year",
       y = "Maximum Temperature (°C)") +
  theme_minimal()
# Linear regression model for yearly maximum temperatures
model_max_temp <- lm(yearly_max_temp ~ year, data = MaxTemperature_yearly)
summary(model_max_temp)
#significant increase
#test assumptions linear model:
#1.linearity: 
plot(model_max_temp, which = 1)
#2.normality of residuals:
plot(model_max_temp, which = 2)  # Q-Q plot
#3.homoscedasticity:
plot(model_max_temp, which = 3) #lijn is (ongeveer) horizontaal, wat betekent 
#dat de variatie van de residuen constant is.

###########CLIMWIN baseline ###################################################
#climwin 1997-2005
library(climwin)
#load egg-laying data
dfnest <- read_excel("Nestquery.xlsx")

#delete all NW monsoon (Winter season)data
4# Function to get monsoon from a date
monsoon <- function(date) {
  m <- month(date)
  if (m %in% c(12, 1, 2, 3)) {
    return("NW Monsoon")
  } else if (m %in% c(5, 6, 7, 8, 9)) {
    return("SE Monsoon")
  } else {
    return("Transition") }}

#laydate estimate --> day in between earliest and latest date. If only one is known, keep that as estimate.
dfnest <- dfnest |>
  mutate(
    LayDateEarliest = as.Date(LayDateEarliest),
    LayDateLatest = as.Date(LayDateLatest),
    LayDateEst = case_when(
      !is.na(LayDateEarliest) & !is.na(LayDateLatest) ~ as.Date(
        (as.numeric(LayDateEarliest) + as.numeric(LayDateLatest)) / 2,
        origin = "1970-01-01" ),
      is.na(LayDateEarliest) & !is.na(LayDateLatest) ~ LayDateLatest,
      !is.na(LayDateEarliest) & is.na(LayDateLatest) ~ LayDateEarliest,
      TRUE ~ as.Date(NA)))

#remove all NW monsoon data, as we only want data from main breeding season
dfnestSE<-dfnest|>
  mutate(Monsoon = sapply(LayDateEst, monsoon))|>
  filter(Monsoon == "SE Monsoon")

#make sure date is in date class, not a character string
dfnestSE$LayDateEst <- as.Date(dfnestSE$LayDateEst)

#Important: for the cw baseline function, Lay date must be numerical. (days since first of january of that year)
dfnestSE <- dfnestSE |>
  mutate(LayDateNum = as.numeric(LayDateEst - as.Date(paste0(PeriodYear, "-01-01"))))


#dfnestSE<-dfnestSE|>
# mutate(LayDateNum = as.numeric(LayDateEst))|>
#na.omit(dfnestSE$LayDateNum)

#prevent pseudo replication (breeding males, females,helpers, ch, fledgelings are all separately in date, therefore make wide format per nestID (NestName))#only BRM and BrF
breeders_only <- dfnestSE |>
  filter(Status %in% c("BrF", "BrM"))

#make wide format with BrF and BrM ID as columns
duplicates <- breeders_only|>
  count(NestName, Status)|>
  filter(n > 1)

nests_to_remove <- duplicates$NestName

breeders_clean <- breeders_only |>
  filter(!(NestName %in% nests_to_remove))

breeders_wide <- breeders_clean|>
  mutate(RoleLabel = case_when(
    Status == "BrF" ~ "FemaleID",
    Status == "BrM" ~ "MaleID"))|>
  select(NestName, RoleLabel, BirdID,TerritoryID, FieldPeriodID, LayDateEst, LayDateNum,PeriodYear, BreedGroupID) |>
  pivot_wider(names_from = RoleLabel, values_from = BirdID)
#only first nest per year per pair
breeders_wide <- breeders_wide %>%
  group_by(FemaleID, MaleID, PeriodYear)|>
  arrange(LayDateNum) |>  
  slice(1) |>
  ungroup()

#climate data available from 01011997, so 6 months after that: 01-07-1997
breeders_wideyears <- breeders_wide |>
  filter(LayDateEst >= as.Date("1997-07-01")) |>
  filter(LayDateEst <= as.Date("2023-12-31"))

#write_xlsx(breeders_wideyears, path = "breeders_wide_1997-2023.xlsx")
# date format is adjsuted to dd mm yyyy

readxl::read_xlsx("breeders_wide_1997-2023.xlsx") -> breeders_wide
readxl::read_xlsx("MaxTemperature_1997-2023.xlsx") -> MaxTemperature

# median laydate per year 1998-2005  

tempperiod1 <- breeders_wide |>
  filter(PeriodYear %in% 1998:2005)

#median lay date per year
median_laydate <- tempperiod1 |>
  group_by(PeriodYear) |>
  summarise(MedianLayDate = median(LayDateNum, na.rm = TRUE), .groups = "drop")|>
  mutate(MedianDateasDate = as.Date(MedianLayDate - 1, origin = paste0(PeriodYear, "-01-01")))
#write_xlsx(median_laydate, path = "median_laydate_1998-2005.xlsx")                                   
readxl::read_xlsx("median_laydate_1998-2005.xlsx") -> median_laydate                             
# climwin model###############################################################

ResCWYearspoints<-slidingwin(
  xvar = list(Temp = MaxTemperature$max_temp),
  cdate = MaxTemperature$day,
  bdate = median_laydate$MedianDateasDate,
  baseline = lm(MedianLayDate ~ 1, data = median_laydate), 
  stat = c("mean","slope","min", "max","sum"),        
  type = "relative",    
  cinterval = "day",
  range = c(180, 0),   
  func = c("lin","quad"),
  cmissing = "method2")
ResCWYearspoints$combos
plotweights(dataset = ResCWYearspoints[[6]]$Dataset)
RandwinresultsYearspoints<-randwin(repeats = 1000, 
                            xvar = list(Temp = MaxTemperature$max_temp),
                            cdate = MaxTemperature$day,
                            bdate = median_laydate$MedianDateasDate,
                            baseline = lm(MedianLayDate ~ 1, data = median_laydate),
                            stat = "mean",        
                            type = "relative", 
                            cinterval = "day",
                            range = c(180,0),   
                            func = "quad",
                            cmissing = "method2")
save(RandwinresultsYearspoints, file = "RandwinresultsYearspoints.RData")

pvalue(dataset = ResCWYearspoints[[1]]$Dataset, datasetrand = RandwinresultsYearspoints[[1]], metric = "AIC")

plotall(ResCWYearspoints[[6]]$Dataset,
        datasetrand =RandwinresultsYearspoints[[1]],
        bestmodel = ResCWYearspoints[[6]]$BestModel,
        bestmodeldata =ResCWYearspoints[[6]]$BestModelData,
        cw1 = 0.95,
        cw2 = 0.5,
        cw3 = 0.25,
        title = "Temperature effects on average egglaying date 1998-2005",
        arrow = TRUE)

#so the mean temperature between 124 and 112 days before the median egg-laying
#date of each year was related to the median egg-laying date of that year.
#In the following time periods, has the median temperature in this time period
#still an effect on the egg-laying date

#new dataframe with mean temperature of the 124-112 days before the median egg-laying date
#1998-2005

#manually add the yearly median lay date to the MaxTemperature data
readxl::read_xlsx("MaxTemperature_1997-2023.xlsx") -> MaxTemperature

MaxTemperature <- MaxTemperature %>%
  mutate(day = as.Date(day))  
MaxTemperature <- MaxTemperature %>%
  mutate(Year = year(day))
head(MaxTemperature)

MEDIANlay_dates <- tibble(
  Year = c(1998, 1999, 2001, 2002, 2003, 2004, 2005),
  MedianLayDate = c(220, 189, 179, 199, 203, 215, 208)  # day of year
)
# Convert both Year columns to integer
MaxTemperature <- MaxTemperature|>
  mutate(Year = as.integer(Year))

MEDIANlay_dates <- MEDIANlay_dates |>
  mutate(Year = as.integer(as.character(Year)))  # Convert factor to integer

# join them
MaxTemperature <- MaxTemperature|>
  left_join(MEDIANlay_dates, by = "Year")
 
MaxTemperature <- MaxTemperature |>
  filter(!is.na(MedianLayDate))|>
  mutate(doy=yday(day))

# Calculate the start and end dates for the 124-112 days before the median lay date
MaxTemperature <- MaxTemperature |>
  mutate(start_date = MedianLayDate - 124,  
         end_date = MedianLayDate - 112)    
MaxTemperature <- MaxTemperature |>
  filter(doy >= start_date & doy <= end_date)

#mean temperature per year
MaxTemperature <- MaxTemperature %>%
  group_by(Year) %>%
  mutate(MeanMaxTempWindow = mean(max_temp, na.rm = TRUE))

#now regress the mean temperature on the median egg-laying date
model_median_temp <- lm(MedianLayDate ~ MeanMaxTempWindow, data = MaxTemperature)
summary(model_median_temp)
#plot
ggplot(MaxTemperature, aes(x = MeanMaxTempWindow, y = MedianLayDate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Effect of Mean Maximum Temperature on Median Egg-laying Date",
       x = "Mean Maximum Temperature (°C)",
       y = "Median Egg-laying Date (day of year)") +
  theme_minimal()
###############################################################################
#now for the other years 2006-2014
###############################################################################
median_laydate20062014 <- breeders_wide |>
  filter(PeriodYear %in% 2006:2014) |>
  group_by(PeriodYear) |>
  summarise(MedianLayDate = median(LayDateNum, na.rm = TRUE), .groups = "drop")

#manually
readxl::read_xlsx("MaxTemperature_1997-2023.xlsx") -> MaxTemperature

MaxTemperature <- MaxTemperature |>
  mutate(day = as.Date(day))  
MaxTemperature <- MaxTemperature |>
  mutate(Year = year(day))

MEDIANlay_dates20062014 <- tibble(
  Year = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014),
  MedianLayDate = c(201, 214, 202, 201, 204, 210, 212, 213, 211)) 
  
# Convert both Year columns to integer
MaxTemperature <- MaxTemperature |>
mutate(Year = as.integer(Year))
         
MEDIANlay_dates20062014 <- MEDIANlay_dates20062014|>
  mutate(Year = as.integer(as.character(Year)))  # Convert factor to integer
         
# Now do the join
MaxTemperature <- MaxTemperature |>
left_join(MEDIANlay_dates20062014, by = "Year")
         
MaxTemperature <- MaxTemperature |>
filter(!is.na(MedianLayDate))|>
mutate(doy=yday(day))
         
# Calculate the start and end dates for the 124-112 days before the median lay date
MaxTemperature <- MaxTemperature |>
mutate(start_date = MedianLayDate - 124,  
        end_date = MedianLayDate - 112)    
MaxTemperature <- MaxTemperature |>
filter(doy >= start_date & doy <= end_date)
         
#mean temperature per year
MaxTemperature <- MaxTemperature|>
  group_by(Year)|>
mutate(MeanMaxTempWindow = mean(max_temp, na.rm = TRUE))

#now regress the mean temperature on the median egg-laying date
model_median_temp20062014 <- lm(MedianLayDate ~ MeanMaxTempWindow, data = MaxTemperature)
summary(model_median_temp20062014)
#plot
ggplot(MaxTemperature, aes(x = MeanMaxTempWindow, y = MedianLayDate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Effect of Mean Maximum Temperature on Median Egg-laying Date (2006-2014)",
       x = "Mean Maximum Temperature (°C)",
       y = "Median Egg-laying Date (day of year)") +
  theme_minimal()

#R^2 is 0.3379, p value  6.368e-12

#for the last period:
readxl::read_xlsx("MaxTemperature_1997-2023.xlsx") -> MaxTemperature

MaxTemperature <- MaxTemperature %>%
  mutate(day = as.Date(day))  
MaxTemperature <- MaxTemperature %>%
  mutate(Year = year(day))

median_laydate20152023 <- breeders_wide |>
  filter(PeriodYear %in% 2015:2023) |>
  group_by(PeriodYear) |>
  summarise(MedianLayDate = median(LayDateNum, na.rm = TRUE), .groups = "drop")

MEDIANlay_dates20152023 <- tibble(
  Year = c(2015, 2016, 2017, 2018, 2019, 2021, 2022, 2023),
  MedianLayDate = c(201,214,187,183,225,201,224,206)) 

# Convert both Year columns to integer
MaxTemperature <- MaxTemperature %>%
  mutate(Year = as.integer(Year))

MEDIANlay_dates20152023 <- MEDIANlay_dates20152023|>
  mutate(Year = as.integer(as.character(Year)))  # Convert factor to integer

# Now do the join
MaxTemperature <- MaxTemperature |>
  left_join(MEDIANlay_dates20152023, by = "Year")

MaxTemperature <- MaxTemperature |>
  filter(!is.na(MedianLayDate))|>
  mutate(doy=yday(day))

# Calculate the start and end dates for the 124-112 days before the median lay date
MaxTemperature <- MaxTemperature |>
  mutate(start_date = MedianLayDate - 124,  
         end_date = MedianLayDate - 112)    
MaxTemperature <- MaxTemperature |>
  filter(doy >= start_date & doy <= end_date)

#mean temperature per year
MaxTemperature <- MaxTemperature|>
  group_by(Year)|>
  mutate(MeanMaxTempWindow = mean(max_temp, na.rm = TRUE))

#now regress the mean temperature on the median egg-laying date
model_median_temp20152023 <- lm(MedianLayDate ~ MeanMaxTempWindow, data = MaxTemperature)
summary(model_median_temp20152023)
#plot
ggplot(MaxTemperature, aes(x = MeanMaxTempWindow, y = MedianLayDate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Effect of Mean Maximum Temperature on Median Egg-laying Date (2015-2023)",
       x = "Mean Maximum Temperature (°C)",
       y = "Median Egg-laying Date (day of year)") +
  theme_minimal()
#R^2 0.4193
  
  ############################################################################
#now put them all in one model and add an interaction between temperature and
  #time period (factor)
  
# Create time period variable
  readxl::read_xlsx("MaxTemperature_1997-2023.xlsx") -> MaxTemperature
  MaxTemperature <- MaxTemperature %>%
    mutate(day = as.Date(day))  
  MaxTemperature <- MaxTemperature %>%
    mutate(Year = year(day))
  
  medianlaydatesall <- tibble(
    Year = c(1998, 1999, 2001, 2002, 2003, 2004, 2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023),
    MedianLayDate = c(220, 189, 179, 199, 203, 215, 208, 
                      201, 214, 202, 201, 204, 210, 212, 213, 211,
                      201, 214, 187, 183, 225, 201, 224, 206))
  # Convert both Year columns to integer
  MaxTemperature <- MaxTemperature %>%
    mutate(Year = as.integer(Year))
  
  medianlaydatesall <- medianlaydatesall|>
    mutate(Year = as.integer(as.character(Year)))  # Convert factor to integer
  
#join them
  MaxTemperature <- MaxTemperature |>
    left_join(medianlaydatesall, by = "Year")
  
  MaxTemperature <- MaxTemperature |>
    filter(!is.na(MedianLayDate))|>
    mutate(doy=yday(day))
  
# Calculate the start and end dates for the 124-112 days before the median lay date
  MaxTemperature <- MaxTemperature |>
    mutate(start_date = MedianLayDate - 124,  
           end_date = MedianLayDate - 112)    
  MaxTemperature <- MaxTemperature |>
    filter(doy >= start_date & doy <= end_date)
  
#mean temperature per year
  MaxTemperature <- MaxTemperature|>
    group_by(Year)|>
    mutate(MeanMaxTempWindow = mean(max_temp, na.rm = TRUE))
  
 MaxTemperature$TimePeriod <- cut(MaxTemperature$Year, 
                       breaks = c(1997, 2005, 2014, 2023),
                       labels = c("1998-2005", "2006-2014", "2015-2023"))
  
 
#linear model
 model1<-lm(MedianLayDate ~ MeanMaxTempWindow * TimePeriod, data = MaxTemperature)
summary(model1)

par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))
qqnorm(residuals(model1)); qqline(residuals(model1))


####################posthoc####################################################
model1 <- lm(MedianLayDate ~ MeanMaxTempWindow * TimePeriod, data = MaxTemperature)
# get estimated marginal means (simple slopes) per TimePeriod
em_trends <- emtrends(model1, ~ TimePeriod, var = "MeanMaxTempWindow")
em_trends

# pairwise comparisons of slopes
pairs(em_trends)

ggplot(MaxTemperature, aes(x = MeanMaxTempWindow, y = MedianLayDate, color = TimePeriod)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  scale_color_manual(values = c("#117733", "#AA4499", "#DDCC77")) +
  labs(title = "Temporal variation in the effect of temperature on median egg-laying date (1998–2023) ",
  x = "Mean Maximum Temperature during climate window (°C)",
       y = "Median Egg-laying Date (day of year)")
  