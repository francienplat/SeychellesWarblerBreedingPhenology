# ChickSuccess

read.csv('pedfate.csv')->pedfate
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(DHARMa)
library(car)

pedfate<-pedfate[,-c(1)]

chicks <- pedfate|>
  filter(
    !is.na(maxclutchsize),
    !is.na(maxfl),
    maxclutchsize > 0,
    maxfl <= maxclutchsize
  ) |>
  rowwise()|>
  mutate(
    chick_data = list(
      tibble(
        chickID = seq_len(maxclutchsize),
        success = c(rep(1, maxfl), rep(0, maxclutchsize - maxfl))
      )
    )
  ) |>
  unnest(chick_data)|>
  ungroup()


View(chicks)
write.csv(chicks, 'chick_success_data.csv')

"%!in%" <-Negate("%in%")

tormv<-c(2001,2020)
chicks<-filter(chicks, chicks$PeriodYear %!in% tormv)

#new time periods:
chicks <- chicks|>
  mutate(TimePeriod = case_when(
    layyear >= 1997 & layyear <= 2010 ~ "1997-2010",
    layyear >= 2011 & layyear <= 2024 ~ "2011-2024"))
chicks <- chicks |>
  filter(!is.na(TimePeriod))#take out years 1995 and 1996

#number of eggs per year
nreggsperyear <- chicks |>
  group_by(layyear) |>
  mutate(eggsperyear = n())|>
  ungroup()

#number of chicks fledged per year
nrfleglingsperyearchicks <- chicks |>
  group_by(layyear) |>
  summarize(fledgedperyear = sum(success))

# View(nrfleglingsperyearchicks)

#merge eggs and fledged per year
together <- left_join(nreggsperyear, nrfleglingsperyearchicks, by='layyear')

# View(together)
#make a graph of average eggs per year and fledged per year during the two periods

histogramdata <- together %>%
  group_by(TimePeriod) %>%
  mutate(
    mean_eggs = mean(eggsperyear, na.rm = TRUE),
    mean_fledglings = mean(fledgedperyear, na.rm = TRUE),
    .groups = "drop"
  )

#make histogram of mean eggs and fledglings per year per time period

histogramdata <- together %>%
  group_by(TimePeriod) %>%
  summarise(
    mean_eggs = mean(eggsperyear, na.rm = TRUE),
    mean_fledglings = mean(fledgedperyear, na.rm = TRUE),
    .groups = "drop"
  )


histogramdata <- together %>%
  group_by(TimePeriod) %>%
  summarise(
    mean_eggs = mean(eggsperyear, na.rm = TRUE),
    se_eggs = sd(eggsperyear, na.rm = TRUE) / sqrt(sum(!is.na(eggsperyear))),
    mean_fledglings = mean(fledgedperyear, na.rm = TRUE),
    se_fledglings = sd(fledgedperyear, na.rm = TRUE) / sqrt(sum(!is.na(fledgedperyear))),
    mean_success = mean(fledgedperyear / eggsperyear, na.rm = TRUE),
    se_success = sd(fledgedperyear / eggsperyear, na.rm = TRUE) / sqrt(sum(!is.na(fledgedperyear / eggsperyear))),
    .groups = "drop"
  )

bars_long <- histogramdata %>%
  pivot_longer(
    cols = c(mean_eggs, mean_fledglings, se_eggs, se_fledglings),
    names_to = c(".value", "variable"),
    names_pattern = "(.*)_(eggs|fledglings)"
  )

# View(bars_long)


# Find a scaling factor to match proportion (0-1) to the bar y-axis
scale_factor <- max(c(histogramdata$mean_eggs, histogramdata$mean_fledglings))

ggplot() +
  # Bars for eggs and fledglings
  geom_col(
    data = bars_long,
    aes(x = TimePeriod, y = mean, fill = variable),
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  geom_errorbar(
    data = bars_long,
    aes(x = TimePeriod, ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  # Line + points for success (scaled)
  geom_line(
    data = histogramdata,
    aes(x = TimePeriod, y = mean_success * scale_factor, group = 1, color = "Proportion success"),
    size = 1.2
  ) +
  geom_point(
    data = histogramdata,
    aes(x = TimePeriod, y = mean_success * scale_factor, color = "Proportion success"),
    size = 3
  ) +
  geom_errorbar(
    data = histogramdata,
    aes(x = TimePeriod,
        ymin = (mean_success - se_success) * scale_factor,
        ymax = (mean_success + se_success) * scale_factor,
        color = "Proportion success"),
    width = 0.2,
  ) +
  scale_y_continuous(
    name = "Mean number of eggs / fledglings (± SE)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Proportion success (± SE)")
  ) +
  scale_fill_manual(
    values = c("eggs" = "orange", "fledglings" = "steelblue"),
    labels = c("Mean eggs", "Mean fledglings")
  ) +
  scale_color_manual(
    values = c("Proportion success" = "forestgreen"),
    labels = c("Proportion success")
  ) +
  labs(
    x = "Time period",
    fill = "",
    color = "",
    title = "Eggs, fledglings, and proportion of success per time period"
  ) +
  theme_minimal(base_size = 14)




#is this the same plot??? 

# Define dodge for consistent positioning
pd <- position_dodge(width = 0.9)

ggplot() +
  # Bars for eggs and fledglings
  geom_col(
    data = bars_long,
    aes(x = TimePeriod, y = mean, fill = variable),
    position = pd,
    width = 0.8
  ) +
  # Error bars on top of bars
  geom_errorbar(
    data = bars_long,
    aes(x = TimePeriod, ymin = mean - se, ymax = mean + se, group = variable),
    position = pd,
    width = 0.2
  ) +
  geom_point(
    data = histogramdata,
    aes(x = TimePeriod, y = mean_success * scale_factor, color = "Proportion success"),
    size = 0.8
  ) +
  geom_errorbar(
    data = histogramdata,
    aes(
      x = TimePeriod,
      ymin = (mean_success - se_success) * scale_factor,
      ymax = (mean_success + se_success) * scale_factor,
      color = "Proportion success"
    ),
    width = 0.2
  ) +
  # Dual Y axis
  scale_y_continuous(
    name = "Mean number of eggs and fledglings per year (± SE)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Proportion success (± SE)")
  ) +
  # Custom colors
  scale_fill_manual(
    values = c("eggs" = "lightblue", "fledglings" = "lightgreen"),
    labels = c("Mean eggs", "Mean fledglings")
  ) +
  scale_color_manual(
    values = c("Proportion success" = "black"),
    labels = c("Proportion success")
  ) +
  labs(
    x = "Time period",
    fill = "",
    color = "",
    title = "Eggs, fledglings, and proportion of success per time period"
  ) +
  theme_minimal(base_size = 16)



#glmer

#add column number of nests per year
chicks <- chicks |>
  group_by(layyear) |>
  mutate(nrnestperyear = n_distinct(NestName)) |>
  ungroup()

#rescale number of nests per year
chicks$nrnestperyear_scaled <- scale(chicks$nrnestperyear)


View(chicks)

chicks2<-filter(chicks, chicks$PeriodYear!=2001)
chicks2<-filter(chicks, chicks$PeriodYear!=2020)

#glm model
glmer_model <- glmer(success ~ TimePeriod + nrnestperyear_scaled + (1|TerritoryID.x) + (1|BrF) + (1|BrM)+(1|NestName),
                     data = chicks,
                     family = binomial)
summary(glmer_model)

#take out variables of no importance
glmer_model_2 <- glmer(success ~ TimePeriod+nrnestperyear_scaled +(1|NestName),
                          data = chicks,
                          family = binomial)
summary(glmer_model_2)

#add territoryID and take out NestID since to many levels with only 1 observation
glmer_model_3 <- glmer(success ~ TimePeriod+nrnestperyear_scaled +(1|TerritoryID.x)+(1|BrF)+(1|BrM),
                      data = chicks,
                      family = binomial)
summary(glmer_model_3)

View(chicks)
tab_model(glmer_model_3)

AIC(glmer_model, glmer_model_2, glmer_model_3)

modest_transformed<-exp(0.34177) / (exp(0.34177) + 1)

CI_min<-exp(0.34177-1.96*0.08709)/ (exp(0.34177-1.96*0.08709)+1)
CI_max<-exp(0.34177+1.96*0.08709)/ (exp(0.34177+1.96*0.08709)+1)

#estimate time period with nrnest/year: -1.37800
#without +nrnestperyear_scaled estimate is -1.3881
#dharma

library(DHARMa)


# Simulate residuals
sim_res <- simulateResiduals(fittedModel = glmer_model_3, n = 1000)

# Plot residuals
plot(sim_res)

# Test for overdispersion
testDispersion(sim_res)

#dharma
simulationOutput <- simulateResiduals(fittedModel = glmer_model_3, plot = TRUE)

testDispersion(simulationOutput)
testUniformity(simulationOutput)
testOutliers(simulationOutput)
testZeroInflation(simulationOutput) #no outliers

#Test for collinearity of fixed effects uing VIF
vif(glmer_model_3) #there is no collinearity between predictors
vif(glmer_model_2)
vif(glmer_model)

#table in sjplot
library(sjPlot)
tab_model(glmer_model_3, show.ci=TRUE, show.se=TRUE, show.stat=TRUE, digits=3)
#plot
library(ggeffects)
library(ggplot2)

p_time <- ggpredict(glmer_model_3, terms = "TimePeriod")

View(p_time)
ggplot(p_time, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  labs(x = "Time Period", 
       y = "Predicted probability of success") +
  theme_minimal()


p_both <- ggpredict(glmer_model_3, terms = c("nrnestperyear_scaled", "TimePeriod"), back_transform = T)
p_both2 <- ggpredict(glmer_model_3, terms = c("nrnestperyear_scaled", "TimePeriod"))

ggplot(p_both, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, colour = NA) +
  labs(x = "Scaled number of nests per year",
       y = "Predicted probability of success",
       colour = "Time Period",
       fill = "Time Period") +
  theme_minimal()




