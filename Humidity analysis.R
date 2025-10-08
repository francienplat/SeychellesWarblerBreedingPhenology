library(dplyr)
library(ggplot2)
library(worldmet)
library(tidyverse)
library(climwin)
library(lubridate)
library(readxl)
WORLDMETDOWNLOAD<-read.csv("Seychelles_weather1997-2024.csv")

#select humidity
Humidity <- WORLDMETDOWNLOAD |>
  filter(!is.na(RH))|>
  select(date, RH)

#select humidity at 12:00 or otherwise the one closest to 12:00. If there are 2 
#closest, take the mean of those two
Humidity_summary <- Humidity |>
  mutate(
    datetime = ymd_hms(date, quiet = TRUE),
    hour = hour(datetime),
    date_only = as.Date(datetime),
    hour_diff = abs(hour - 12)
  ) |>
  group_by(date_only) |>
  filter(hour_diff == min(hour_diff, na.rm = TRUE)) |>
  summarise(RH = mean(RH, na.rm = TRUE), .groups = "drop") |>
  arrange(date_only)

#yearly median laydates

# Load EGGLAYING data
dfnest <- read_excel("Nestquery.xlsx")

#delete all NW monsoon (Winter season)data
# Function to get monsoon from a date
monsoon <- function(date) {
  m <- month(date)
  if (m %in% c(12, 1, 2, 3)) {
    return("NW Monsoon")
  } else if (m %in% c(5, 6, 7, 8, 9)) {
    return("SE Monsoon")
  } else {
    return("Transition") }}

#laydate estimate --> day in between earliest and latest date. If only one is known, keep that as the estimate.
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
# date format is adjsuted to dd mm yyyy (I had struggles with it in R)

readxl::read_xlsx("breeders_wide_1997-2023.xlsx") -> breeders_wide
#8 punten voor de median laydate per jaar

humidityperiod1 <- breeders_wide |>
  filter(PeriodYear %in% 1998:2005)

#median lay date per year
median_laydate <- humidityperiod1 |>
  group_by(PeriodYear) |>
  summarise(MedianLayDate = median(LayDateNum, na.rm = TRUE), .groups = "drop")|>
  mutate(MedianDateasDate = as.Date(MedianLayDate - 1, origin = paste0(PeriodYear, "-01-01")))
#write_xlsx(median_laydate, path = "median_laydate_1998-2005.xlsx")                                   
readxl::read_xlsx("median_laydate_1998-2005.xlsx") -> median_laydate 

#climwin for 1997-2005
resultsclimwinhumidity<- slidingwin(
  xvar = list(Temp =Humidity_summary$RH),
  cdate = Humidity_summary$date_only,
  bdate = median_laydate$MedianDateasDate,
  baseline = lm(MedianLayDate ~ 1, data = median_laydate), 
  stat = c("mean","slope","min", "max","sum"),        
  type = "relative",    
  cinterval = "day",
  range = c(180, 0),   
  func = c("lin","quad"),
  cmissing = "method2")
resultsclimwinhumidity$combos

RandwinresultsHumidity<-randwin(repeats = 1000, 
                                xvar = list(Temp =Humidity_summary$RH),
                                cdate = Humidity_summary$date_only,
                                bdate = median_laydate$MedianDateasDate,
                                baseline = lm(MedianLayDate ~ 1, data = median_laydate), 
                                stat = "slope",        
                                type = "relative",    
                                cinterval = "day",
                                range = c(180, 0),   
                                func = c("lin"),
                                cmissing = "method2")
save(RandwinresultsHumidity, file = "RandwinresultsHumidity.rds")
plotall(resultsclimwinhumidity[[2]]$Dataset,
        datasetrand =RandwinresultsHumidity[[1]],
        bestmodel = resultsclimwinhumidity[[2]]$BestModel,
        bestmodeldata =resultsclimwinhumidity[[2]]$BestModelData,
        cw1 = 0.95,
        cw2 = 0.5,
        cw3 = 0.25,
        title = "Change in Humidity effects on average egglaying date 1998-2005",
        arrow = TRUE)

pvalue(dataset = resultsclimwinhumidity[[2]]$Dataset, datasetrand = RandwinresultsHumidity[[1]], metric = "AIC")
resultsclimwinhumidity[[2]]$BestModelData
plot(yvar~climate, data=resultsclimwinhumidity[[2]]$BestModelData, 
     xlab = "Humidity", ylab = "Lay Date (days since 1st January)")

###############################################################################
#Linear models of laydate separately per period
# Step 1: Prepare HumidityData
HumidityData <- Humidity_summary %>%
  mutate(
    day = as.Date(date_only),      # strip time part
    Year = year(day),         # extract year as integer
    doy  = yday(day)          # day of year
  )
#combine with median laydate dataset
HumidityData <- HumidityData %>%
  left_join(median_laydate %>% rename(Year = PeriodYear), by = "Year") %>%
  mutate(
    start_doy = MedianLayDate - 71,
    end_doy = MedianLayDate - 60
  )

#filter for correct time period
HumidityData <- HumidityData %>%
  filter(Year >= 1998 & Year <= 2005) %>%
  filter(doy >= start_doy & doy <= end_doy) %>%
  select(day, RH, Year, doy, start_doy, end_doy,MedianLayDate)

#calculate slope
HumiditySlope <- HumidityData %>%
  group_by(Year) %>%
  mutate(
    HumiditySlope = {
      if(n() >= 2) {
        fit <- lm(RH ~ doy, data = cur_data())
        coef(fit)[2]
      } else {
        NA_real_
      }
    }
  )

print(HumiditySlope)
#plot humidityslope vs medianlaydate
ggplot(HumiditySlope, aes(x = HumiditySlope, y = MedianLayDate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Humidity Slope",
    y = "Median Lay Date (days since 1st January)",
    title = "Humidity Slope vs Median Lay Date"
  ) +
  theme_minimal()
lm_modelhumid1 <- lm(MedianLayDate ~ HumiditySlope, data = HumiditySlope)
summary(lm_modelhumid1)

#second period################################################################
#filter for correct time period
humidityperiod2 <- breeders_wide |>
filter(PeriodYear %in% 2006:2014)

#median lay date per year
median_laydate2 <- humidityperiod2 |>
  group_by(PeriodYear) |>
  summarise(MedianLayDate = median(LayDateNum, na.rm = TRUE), .groups = "drop")|>
  mutate(MedianDateasDate = as.Date(MedianLayDate - 1, origin = paste0(PeriodYear, "-01-01")))
                                 

# Step 1: Prepare HumidityData
HumidityData2 <- Humidity_summary %>%
  mutate(
    day = as.Date(date_only),      # strip time part
    Year = year(day),         # extract year as integer
    doy  = yday(day)          # day of year
  )
#combine with median laydate dataset
HumidityData2 <- HumidityData2 %>%
  left_join(median_laydate2 %>% rename(Year = PeriodYear), by = "Year") %>%
  mutate(
    start_doy = MedianLayDate - 71,
    end_doy = MedianLayDate - 60
  )

#filter for correct time period
HumidityData2 <- HumidityData2 %>%
  filter(Year >= 2006 & Year <= 2014) %>%
  filter(doy >= start_doy & doy <= end_doy) %>%
  select(day, RH, Year, doy, start_doy, end_doy,MedianLayDate)

#calculate slope
HumiditySlope2 <- HumidityData2 %>%
  group_by(Year) %>%
  mutate(
    HumiditySlope = {
      if(n() >= 2) {
        fit <- lm(RH ~ doy, data = cur_data())
        coef(fit)[2]
      } else {
        NA_real_
      }
    }
  )

print(HumiditySlope2)
#plot humidityslope vs medianlaydate
ggplot(HumiditySlope2, aes(x = HumiditySlope, y = MedianLayDate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Humidity Slope",
    y = "Median Lay Date (days since 1st January)",
    title = "Humidity Slope vs Median Lay Date"
  ) +
  theme_minimal()
lm_modelhumid2 <- lm(MedianLayDate ~ HumiditySlope, data = HumiditySlope2)
summary(lm_modelhumid2)

####3rd period#################################################################
humidityperiod3 <- breeders_wide |>
  filter(PeriodYear %in% 2015:2023)

#median lay date per year
median_laydate3 <- humidityperiod3 |>
  group_by(PeriodYear) |>
  summarise(MedianLayDate = median(LayDateNum, na.rm = TRUE), .groups = "drop")|>
  mutate(MedianDateasDate = as.Date(MedianLayDate - 1, origin = paste0(PeriodYear, "-01-01")))


# Step 1: Prepare HumidityData
HumidityData3 <- Humidity_summary %>%
  mutate(
    day = as.Date(date_only),      # strip time part
    Year = year(day),         # extract year as integer
    doy  = yday(day)          # day of year
  )
#combine with median laydate dataset
HumidityData3 <- HumidityData3 %>%
  left_join(median_laydate3 %>% rename(Year = PeriodYear), by = "Year") %>%
  mutate(
    start_doy = MedianLayDate - 71,
    end_doy = MedianLayDate - 60
  )

#filter for correct time period
HumidityData3 <- HumidityData3 %>%
  filter(Year >= 2015 & Year <= 2023) %>%
  filter(doy >= start_doy & doy <= end_doy) %>%
  select(day, RH, Year, doy, start_doy, end_doy,MedianLayDate)

#calculate slope
HumiditySlope3 <- HumidityData3 %>%
  group_by(Year) %>%
  mutate(
    HumiditySlope = {
      if(n() >= 2) {
        fit <- lm(RH ~ doy, data = cur_data())
        coef(fit)[2]
      } else {
        NA_real_
      }
    }
  )

print(HumiditySlope3)
#plot humidityslope vs medianlaydate
ggplot(HumiditySlope3, aes(x = HumiditySlope, y = MedianLayDate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Humidity Slope",
    y = "Median Lay Date (days since 1st January)",
    title = "Humidity Slope vs Median Lay Date"
  ) +
  theme_minimal()
lm_modelhumid3 <- lm(MedianLayDate ~ HumiditySlope, data = HumiditySlope3)
summary(lm_modelhumid3)

###############################################################################
#now put them all in one model and add an interaction between Humidity and
#time period (factor)
HumiditySlopeAll <- bind_rows(
  HumiditySlope %>% mutate(Period = "1998-2005"),
  HumiditySlope2 %>% mutate(Period = "2006-2014"),
  HumiditySlope3 %>% mutate(Period = "2015-2023")
)

modelhumidity<-lm(MedianLayDate ~ HumiditySlope * Period, data = HumiditySlopeAll)
summary(modelhumidity)
#posthoc test
# get estimated marginal means (simple slopes) per TimePeriod
em_trends2 <- emtrends(modelhumidity, ~ Period, var = "HumiditySlope")
em_trends2

# pairwise comparisons of slopes
pairs(em_trends2)

############visualisation of the interaction model############################

ggplot(HumiditySlopeAll, aes(x = HumiditySlope, y = MedianLayDate, color = Period)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  scale_color_manual(values = c("#117733", "#AA4499", "#DDCC77")) +
  labs(title = "Temporal variation in the effect of change in humidity on median egg-laying date (1998–2023) ", x = "Humidity Slope",
       y = "Median Egg-laying Date (day of year)")


#Humidity over time during the study period 1997-2023
ggplot(Humidity_summary, aes(x = date_only, y = RH)) +
  geom_line(color = "blue") +
  labs(
    title = "Humidity over time (1997-2024)",
    x = "Date",
    y = "Relative Humidity (%)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Figure thesis: monthly average humidity trend :linear and loess smooth
Humidity_summary %>%
  mutate(month = floor_date(date_only, "month")) %>%
  filter(month >= as.Date("1997-01-01") & month <= as.Date("2023-12-31")) %>%
  group_by(month) %>%
  summarise(mean_RH = mean(RH, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = month, y = mean_RH)) +
  geom_line(color = "black") +
  geom_smooth(color = "orange", method = "lm", se = TRUE) +
  geom_smooth(color = "blue", method = "loess", se = FALSE) +
  labs(
    title = "Monthly Average Humidity (1997–2023)",
    x = "Year",
    y = "Average Relative Humidity (%)"
  ) +
  theme_minimal() +
  scale_x_date( limits = c(as.Date("1997-01-01"), as.Date("2023-12-31")),date_labels = "%Y", date_breaks = "2 year") +   # yearly ticks
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#simple graph with only linear trend line
Humidity_summary %>%
  mutate(month = floor_date(date_only, "month")) %>%
  group_by(month) %>%
  summarise(mean_RH = mean(RH, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = month, y = mean_RH)) +
  geom_line(color = "blue") +
  geom_smooth(color = "red", method = "lm", se = FALSE) +
  labs(
    title = "Monthly Average Humidity (1997-2024)",
    x = "Year",
    y = "Average Relative Humidity (%)"
  ) +
  theme_minimal() +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "5 years",
    limits = as.Date(c("2000-01-01", "2025-01-01"))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


min(Humidity_summary$RH, na.rm = TRUE)
max(Humidity_summary$RH, na.rm = TRUE)
(head(Humidity_summary))

Humidity_summary %>%
  arrange(RH) %>%
  head(10)
#2 outliers!

#linear model humidity over time

lm_modelhumidity <- lm(RH ~ date_only, data = Humidity_summary)
summary(lm_modelhumidity)

#normality of residuals check!
# 1) Histogram of residuals
hist(residuals(lm_modelhumidity))

# 2) QQ-plot
qqnorm(residuals(lm_modelhumidity)); qqline(residuals(lm_modelhumidity))

# 3) Shapiro-Wilk test (formal)
shapiro.test(residuals(lm_modelhumidity))
#too large sample size for shapiro
# Take a random subset of residuals (e.g., 5000 points or fewer)
resid_sub <- sample(residuals(lm_modelhumidity), 5000)

# Run Shapiro-Wilk test on the subset
shapiro.test(resid_sub)
