# Number of nests laid per day###############################################
library(readxl)
library(tidyverse)
library(lubridate)

#load data
dfnest <- read_excel("Nestquery.xlsx")

#delete all NW monsoon(Winter season)data
# Function to get monsoon from a date
monsoon <- function(date) {
  m <- month(date)
  if (m %in% c(12, 1, 2, 3)) {
    return("NW Monsoon")
  } else if (m %in% c(5, 6, 7, 8, 9)) {
    return("SE Monsoon")
  } else {
    return("Transition") }}

#laydate estimate -> day in between earliest and latest date. If only one is known, keep that as estimate.
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

#prevent pseudo replication (breeding males, females,helpers, ch, fledgelings are all separately in date, therefore make wide format per nestID (NestName))#only BRM and BrF
breeders_only <- dfnestSE |>
  filter(Status %in% c("BrF", "BrM"))

#take out the duplicates
duplicates <- breeders_only|>
  count(NestName, Status)|>
  filter(n > 1)
nests_to_remove <- duplicates$NestName
breeders_clean <- breeders_only |>
  filter(!(NestName %in% nests_to_remove))

#make wide format, create status column so there are not 2 entries per nest anymore
breeders_wide <- breeders_clean|>
  mutate(RoleLabel = case_when(
    Status == "BrF" ~ "FemaleID",
    Status == "BrM" ~ "MaleID"))|>
  select(NestName, RoleLabel, BirdID,TerritoryID, FieldPeriodID, LayDateEst,PeriodYear, BreedGroupID) |>
  pivot_wider(names_from = RoleLabel, values_from = BirdID)

#only first nest per year per pair
breeders_wide <- breeders_wide %>%
  group_by(FemaleID, MaleID, PeriodYear)|>
  arrange(LayDateEst) |>  
  slice(1) |>
  ungroup()

nrow(breeders_wide %>% filter(LayDateEst == as.Date("1995-06-24")))
nrow(breeders_wide %>% filter(LayDateEst == as.Date("1997-07-07")))


#make sure laydateest is in the right format
breeders_wide <- breeders_wide |>
  mutate(LayDateEst = as.Date(format(LayDateEst, "%Y-%m-%d")))

#count number of nests per day
nests_per_day <- breeders_wide |>
  count(LayDateEst)|>
  arrange(desc(n))








