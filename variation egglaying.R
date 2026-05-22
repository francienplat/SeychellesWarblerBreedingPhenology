#variation in egglaying
library(ggplot2)
library(lubridate)
library(tidyverse)
library(readxl)
# upload dataset from excel
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
    return("Transition")}}

dfnest <- dfnest |>
  mutate(
    LayDateEarliest = as.Date(LayDateEarliest),
    LayDateLatest = as.Date(LayDateLatest),
    LayDateEst = case_when(
      !is.na(LayDateEarliest) & !is.na(LayDateLatest) ~ as.Date(
        (as.numeric(LayDateEarliest) + as.numeric(LayDateLatest)) / 2,
        origin = "1970-01-01"
      ),#If only latest or earliest date is known, or should i let those out?
      is.na(LayDateEarliest) & !is.na(LayDateLatest) ~ LayDateLatest,
      !is.na(LayDateEarliest) & is.na(LayDateLatest) ~ LayDateEarliest,
      TRUE ~ as.Date(NA)))

dfnestSE<- dfnest|>#Only SE monsoon
  mutate(Monsoon = sapply(LayDateEst, monsoon))|>
  filter(Monsoon == "SE Monsoon")



#only BRM and BrF
breeders_only <- dfnestSE |>
  filter(Status %in% c("BrF", "BrM"))

#make wide format with BrF and BrM ID as columns
duplicates <- breeders_only|>
  count(NestName, Status)|>
  filter(n > 1)

nests_to_remove <- duplicates$NestName

breeders_clean <- breeders_only |>
  filter(!(NestName %in% nests_to_remove))

breeders_widevariation <- breeders_clean|>
  mutate(RoleLabel = case_when(
    Status == "BrF" ~ "FemaleID",
    Status == "BrM" ~ "MaleID"))|>
  select(NestName, RoleLabel, BirdID,TerritoryID, FieldPeriodID, LayDateEst,PeriodYear, BreedGroupID) |>
  pivot_wider(names_from = RoleLabel, values_from = BirdID)

#only first nest
breeders_widevariation <- breeders_widevariation %>%
  group_by(FemaleID, MaleID, PeriodYear)|>
  arrange(LayDateEst) |>  
  slice(1) |>
  ungroup()

#take out 1994 and 2024 due to low sample size
breeders_widevariation <- breeders_widevariation |>
  filter(PeriodYear != 1994 & PeriodYear != 2024 & PeriodYear != 1995 & PeriodYear != 1996 & PeriodYear != 1997)


#dayofyear
breeders_widevariation$DayOfYear <- as.numeric(format(breeders_widevariation$LayDateEst, "%j"))

#1995 min max laydate
min(breeders_widevariation$LayDateEst[breeders_widevariation$PeriodYear == 1996], na.rm = TRUE)
max(breeders_widevariation$LayDateEst[breeders_widevariation$PeriodYear == 1996], na.rm = TRUE)


ggplot(breeders_widevariation, aes(x = as.factor(PeriodYear), y = DayOfYear)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Variation in Egg-Laying Dates in Main Breeding Season",
       x = "Year", y = "Day of Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#what is the mean median egg laying date over the years
breeders_widevariation<-breeders_widevariation |>
  group_by(PeriodYear) |>
  mutate(median_doy = median(DayOfYear, na.rm = TRUE)) |>
  arrange(PeriodYear)
breeders_widevariation<- breeders_widevariation |>
  mutate(meanegglayingdate = mean(median_doy, na.rm = TRUE),
            sd_egglayingdate = sd(median_doy, na.rm = TRUE))

#mean egg laying date varied over
breeders_widevariation |>
  group_by(PeriodYear) |>
  mutate(min_med_)


#graph with egg laying peak day over the years, egglaying peak is mode
peak_dates <- breeders_widevariation %>%
  group_by(PeriodYear, LayDateEst) %>%
  summarise(n_nests = n(), .groups = "drop") %>%
  group_by(PeriodYear) %>%
  slice_max(n_nests, n = 1, with_ties = FALSE) %>%
  arrange(PeriodYear)
print(peak_dates)

peak_dates$doy <- as.numeric(format(peak_dates$LayDateEst, "%j"))

ggplot(peak_dates, aes(x = as.numeric(PeriodYear), y = doy)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 2, color = "forestgreen") +
  scale_y_continuous(name = "Dag van het jaar (DOY)", breaks = seq(90, 200, 10)) +
  scale_x_continuous(name = "Jaar", breaks = seq(min(as.numeric(peak_dates$PeriodYear)), max(as.numeric(peak_dates$PeriodYear)), 2)) +
  labs(title = "Peak egg laying date per jaar",
       subtitle = "Gebaseerd op datum waarop meeste nesten gestart zijn",
       caption = "DOY = Day Of Year") +
  theme_minimal()

#linear model
model <- lm(doy ~ PeriodYear, data = peak_dates)
summary(model)
# Add regression line to the plot
ggplot(peak_dates, aes(x = as.numeric(PeriodYear), y = doy)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 2, color = "forestgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(name = "DOY", breaks = seq(90, 200, 10)) +
  scale_x_continuous(name = "Year", breaks = seq(min(as.numeric(peak_dates$PeriodYear)), max(as.numeric(peak_dates$PeriodYear)), 2)) +
  labs(title = "Peak egg laying date per year",
       caption = "DOY = Day Of Year") +
  theme_minimal()

#######################################################################
#mean egglaying date over all years + standard deviation
breeders_widevariation |>
  mutate(DayOfYear = as.numeric(format(LayDateEst, "%j"))) |>
  summarize(mean_doy = mean(DayOfYear, na.rm = TRUE),
            sd_doy = sd(DayOfYear, na.rm = TRUE))

#does median change over the years?
breederswidemedian<-breeders_widevariation |>
  mutate(DayOfYear = as.numeric(format(LayDateEst, "%j"))) |>
  group_by(PeriodYear) |>
  summarize(median_doy = median(DayOfYear, na.rm = TRUE)) |>
  arrange(PeriodYear)

summary(lm(breederswidemedian$median_doy ~ breederswidemedian$PeriodYear))

#does dispersal (IQR) change over the years?
IQRYEar<-breeders_widevariation |>
  mutate(DayOfYear = as.numeric(format(LayDateEst, "%j"))) |>
  group_by(PeriodYear) |>
  summarize(iqr_doy = IQR(DayOfYear, na.rm = TRUE)) |>
  arrange(PeriodYear)

lm(IQRYEar$iqr_doy ~ IQRYEar$PeriodYear)
summary(lm(IQRYEar$iqr_doy ~ IQRYEar$PeriodYear))
