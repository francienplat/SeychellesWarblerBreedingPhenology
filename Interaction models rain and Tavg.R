library(ggplot2)
library(readxl)
library(car)
library (sjPlot)

readxl::read_excel("C:\\Users\\franc\\Documents\\Master project 1 Seychelles Warbler\\Seychelles Warbler Phenology\\2025_11_25_Nests_per_week_with_climwinData.xlsx")->Nests_per_week_with_climwinData

#rain graph
ggplot(subset(Nests_per_week_with_climwinData,Period=="early"), aes(x=ClimwinRain, y=Total_Nests_per_week))+
  geom_point(col="blue")+
  geom_smooth(method="lm",formula = y~poly(x,2),se=T)+
  geom_point(data=subset(Nests_per_week_with_climwinData,Period=="late"), aes(x=ClimwinRain, y=Total_Nests_per_week,col="red"))+
  geom_smooth(data=subset(Nests_per_week_with_climwinData,Period=="late"), aes(x=ClimwinRain, y=Total_Nests_per_week, col="red"),
              method="lm",formula = y~poly(x,2),se=T)

#pretty
ggplot(Nests_per_week_with_climwinData,
       aes(x = ClimwinRain,
           y = Total_Nests_per_week,
           color = Period)) +
  
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE) +
  
  scale_color_manual(values = c("early" = "blue",
                                "late"  = "red"),
                     name = "Breeding period",
                     labels = c("early" = "Early",
                                "late"  = "Late")) +
  
  labs(x = "Cumulative Rainfall (mm)",
       y = "Total nests per week") +
  
  theme_minimal()


#temperature graph
ggplot(subset(Nests_per_week_with_climwinData,Period=="early"), aes(x=ClimwinTavg, y=Total_Nests_per_week))+
  geom_point(col="blue")+
  geom_smooth(method="lm",formula = y~poly(x,2),se=T)+
  geom_point(data=subset(Nests_per_week_with_climwinData,Period=="late"), aes(x=ClimwinTavg, y=Total_Nests_per_week,col="red"))+
  geom_smooth(data=subset(Nests_per_week_with_climwinData,Period=="late"), aes(x=ClimwinTavg, y=Total_Nests_per_week, col="red"),
              method="lm",formula = y~poly(x,2),se=T)

#pretty
ggplot(Nests_per_week_with_climwinData,
       aes(x = ClimwinTavg,
           y = Total_Nests_per_week,
           color = Period)) +
  
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE) +
  
  scale_color_manual(values = c("early" = "blue",
                                "late"  = "red"),
                     name = "Breeding period",
                     labels = c("early" = "Early",
                                "late"  = "Late")) +
  
  labs(x = "Average Temperature during time window (°C)",
       y = "Total nests per week") +
  
  theme_minimal()



#Interaction model rain

modelRainInteraction<-glm(Total_Nests_per_week ~ ClimwinRain + Period+ # the two main effects
                           ClimwinRain:Period+I(ClimwinRain^2) + # the two "two way" interactions
                           I(ClimwinRain^2):Period, # the "three-way" interaction
                          family = Gamma(link = "log"),
                         data=Nests_per_week_with_climwinData)


summary(modelRainInteraction)

tab_model(
  modelRainInteraction,
  file = "Rain_model_results_gamma.html",
  transform = "exp")

#normality check
qqnorm(resid(modelRainInteraction))
qqline(resid(modelRainInteraction), col="red")

#homoscedasticity)
# Scale-Location plot
plot(modelRainInteraction, which = 1)
plot(modelRainInteraction, which = 2)
plot(modelRainInteraction, which = 3)
plot(modelRainInteraction, which = 4)

par(mfrow = c(2, 2))
plot(modelRainInteraction)
par(mfrow = c(1, 1)) 

ncvTest(modelRainInteraction)     # non-constant variance test
spreadLevelPlot(modelRainInteraction) # visualize homoscedasticity
plot(modelRainInteraction, which = 1:4) 


#Interaction model Tavg
modelTavgInteraction<-glm(Total_Nests_per_week ~ ClimwinTavg + Period+ # the two main effects
                           ClimwinTavg:Period+I(ClimwinTavg^2) + # the two "two way" interactions
                           I(ClimwinTavg^2):Period, # the "three-way" interaction
                         family = Gamma(link = "log"),
                         data=Nests_per_week_with_climwinData)


summary(modelTavgInteraction)
#normality check
qqnorm(resid(modelTavgInteraction))
qqline(resid(modelTavgInteraction), col="red")
tab_model(modelTavgInteraction)

tab_model(
  modelTavgInteraction, transform = "exp",
  file = "Tavg_model_results_gamma.html"
)

#Gamma distribution: use   transform = "exp",

#homoscedasticity)
# Scale-Location plot
plot(modelTavgInteraction, which = 1)
plot(modelTavgInteraction, which = 2)
plot(modelTavgInteraction, which = 3)
plot(modelTavgInteraction, which = 4)
par(mfrow = c(2, 2))
plot(modelTavgInteraction)
par(mfrow = c(1, 1))   # reset
