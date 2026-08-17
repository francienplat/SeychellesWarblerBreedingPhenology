pedfate<-read.csv('pedfate.csv')
nestswithclimwin<-readxl::read_excel('2025_11_25_Nests_per_week_with_climwinData.xlsx')

pedfate<-pedfate[,-c(1)]

library(RODBC)
library(tidyverse)
library(chron)

#what measure as response? 
#lay date as days from start of year? (easiest option - but perhaps not biologically most sensible)


#test whether there are within-individual differences in lay date with age 


DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

tblbirdid<-sqlFetch(swdb, 'tblBirdID', stringsAsFactors=F)


tblbirdid<-tblbirdid[,c("BirdID", "BirthDate")]

femalelaydate<-pedfate
names(femalelaydate)[names(femalelaydate) == 'BrF'] <- 'BirdID'

malelaydate<-pedfate
names(malelaydate)[names(malelaydate) == 'BrM'] <- 'BirdID'

femalelaydate<-left_join(femalelaydate, tblbirdid, by='BirdID')
str(femalelaydate)

femalelaydate$LayDateEarliest<-as.Date(femalelaydate$LayDateEarliest, '%Y-%m-%d')
femalelaydate$BirthDate<-as.Date(femalelaydate$BirthDate, '%y-%m-%d')
femalelaydate$AvgLaydate<-as.Date(femalelaydate$AvgLaydate, '%Y-%m-%d')
#merge, get age, 
#change lay date into days into year 
#within model 

femalelaydate$age_dates<-femalelaydate$AvgLaydate-femalelaydate$BirthDate

femalelaydate$age_year<-round(as.numeric(femalelaydate$age_dates/365))

str(femalelaydate)

library(lubridate)
femalelaydate$layday_yearday<-yday(femalelaydate$AvgLaydate)

#between age centering
AveByInd <- function(x) mean(x)

#within age centering
WithinIndCentr <- function(x) x-mean(x)

#quadratic within age centering 
Quadwithin <- function(x) x^2-mean(x)^2

femalelaydate<-do.call("rbind", as.list(
  by(femalelaydate, femalelaydate[,"BirdID"], transform, AveAge=AveByInd(age_year))))


#within age 
femalelaydate <- do.call("rbind", as.list(
  by(femalelaydate, femalelaydate[,"BirdID"], transform, WithinAge=WithinIndCentr(age_year))))


#within age squared 
femalelaydate <- do.call("rbind", as.list(
  by(femalelaydate, femalelaydate[,"BirdID"], transform, WithinAge2=Quadwithin(age_year))))



nestswithclimwin$yearweek<-isoweek(nestswithclimwin$date)
femalelaydate$yearweek<-isoweek(femalelaydate$AvgLaydate)

names(nestswithclimwin)[names(nestswithclimwin) == 'Year'] <- 'PeriodYear'


Flaydate_clim<-left_join(femalelaydate, nestswithclimwin, by=c('PeriodYear', 'yearweek'))

#remove data for no climate variable 
Flaydate_clim<-filter(Flaydate_clim, Flaydate_clim$PeriodYear>1996)


#remove data for minor seasons 
Flaydate_clim<-filter(Flaydate_clim, Flaydate_clim$yearweek>17)


#model structure 
#lay date ~ within age + between age + quadratic age + quadratic between? + birdID 
library(lme4)
library(lmerTest)
library(car)
library(DHARMa)

Flaydate_clim<-filter(Flaydate_clim, Flaydate_clim$BirdID>0)

Flaydate_clim<-filter(Flaydate_clim, Flaydate_clim$layday_yearday>50)
# hist(Flaydate_clim$layday_yearday)
#needs year? 
tormv<-c(2001,2020)

Flaydate_clim<-filter(Flaydate_clim, Flaydate_clim$PeriodYear %!in% tormv)


# agemod1<-lmer(layday_yearday~WithinAge+ WithinAge2+AveAge+I(AveAge)^2+(1|BirdID), data=Flaydate_clim)
# summary(agemod1)


agemod_temp1<-lmer(layday_yearday~WithinAge*ClimwinTavg + WithinAge2*ClimwinTavg + AveAge +(WithinAge|BirdID), data=Flaydate_clim)

summary(agemod_temp1)
tab_model(agemod_temp1)

agemod_temp2<-lmer(layday_yearday~WithinAge*ClimwinTavg  + AveAge + I(AveAge^2)+
                     WithinAge2*ClimwinTavg +(WithinAge|BirdID), data=Flaydate_clim)

summary(agemod_temp2)


agemod_temp3<-lmer(layday_yearday~(WithinAge)*ClimwinTavg  + AveAge + I(AveAge^2)+
                     (WithinAge|BirdID), data=Flaydate_clim)

summary(agemod_temp3)

# agemod_temp4<-lmer(layday_yearday~(WithinAge)*ClimwinTavg  + AveAge + I(AveAge^2)+
#                      I((age_year^2-AveAge^2))*ClimwinTavg +(WithinAge|BirdID), data=Flaydate_clim)
# 
# summary(agemod_temp4)



vif(agemod_temp1)

agemod_rain1<-lmer(layday_yearday~WithinAge*ClimwinRain + WithinAge2*ClimwinRain + AveAge + (WithinAge|BirdID), data=Flaydate_clim)

summary(agemod_rain1)

# agemod_rain2<-lmer(layday_yearday~WithinAge*ClimwinRain + I((age_year-AveAge)^2) + AveAge +(WithinAge|BirdID), data=Flaydate_clim)
# 
# summary(agemod_rain2)

simulateResiduals(agemod_rain1, plot=T)

agemod_rain2<-lmer(layday_yearday~WithinAge*ClimwinRain + WithinAge2 + AveAge + (WithinAge|BirdID), data=Flaydate_clim)

summary(agemod_rain2)

tab_model(agemod_rain1)
tab_model(agemod_rain2)

agemod_rainfinal<-lmer(layday_yearday~WithinAge +ClimwinRain + WithinAge2 + AveAge +(WithinAge|BirdID), data=Flaydate_clim)

summary(agemod_rainfinal)

library(sjPlot)
tab_model(agemod_rainfinal)

# obsnum<-Flaydate_clim%>%
#   group_by(BirdID)%>%
#   summarise(count=n())
# View(obsnum)
# 
# agedist<-Flaydate_clim%>%
#   group_by(age_year)%>%
#   summarise(count=n())
# 
# nestnum<-Flaydate_clim%>%
#   group_by(NestID)%>%
#   summarise(count=n())
#no duplicate nests! 

# hist(Flaydate_clim$ClimwinTavg)
# View(agedist)

simulateResiduals(agemod_temp1, plot = T)


library(ggeffects)
# tempmodpred<-predict(agemod_temp2, terms=c("(age_year-AveAge)^2"))
# plot(tempmodpred)
# ggplot()


#need fl success model 
#so  fl success ~ laydate + age + age2 + between + helper presence + BirdID+(1|territory ID)  

#binomial model 
Flaydate_clim<-Flaydate_clim%>%
  mutate(fledgesuccess = case_when(propfl_true >0 ~ 1, 
                                   TRUE ~ 0))
Flaydate_clim<-Flaydate_clim%>%
  mutate(helperpresence = case_when(nrH >0 ~ 1, 
                                   TRUE ~ 0))

Flaydate_clim$helperpresence<-as.factor(Flaydate_clim$helperpresence)
 

plot(Flaydate_clim$layday_yearday)

#remove outlier 

# Flaydate_clim$fledgesuccess<-as.factor(Flaydate_clim$fledgesuccess)

Flaydate_clim$layday_yearday_z<-scale(Flaydate_clim$layday_yearday)

# fl_laydatemod<-glmer(fledgesuccess~layday_yearday+WithinAge + WithinAge2+AveAge+
#                        helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
#                      family = binomial(link='logit'), 
#                      data= Flaydate_clim, 
#                      control = glmerControl(optimizer="bobyqa"))
# 
# summary(fl_laydatemod)


#laydate is ztransformed instead of the first one 
# fl_laydatemod2<-glmer(fledgesuccess~layday_yearday_z + WithinAge + WithinAge2 + AveAge +
#                        helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
#                      family = binomial(link='logit'), 
#                      data= Flaydate_clim, control = glmerControl(optimizer="bobyqa"))
# 
# summary(fl_laydatemod2)
# 
# tab_model(fl_laydatemod2)
#completely fine 

# saveRDS(fl_laydatemod2, 'femalefledgesuccessmod.rds')



simulateResiduals(fl_laydatemod2, plot=T)

vif(fl_laydatemod2)


fl_laydatemod_temp<-glmer(fledgesuccess~layday_yearday_z*ClimwinTavg + WithinAge + WithinAge2 + AveAge +
                        helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                      family = binomial(link='logit'), 
                      data= Flaydate_clim, control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e5)))

summary(fl_laydatemod_temp)
simulateResiduals(fl_laydatemod_temp, plot=T)

tab_model(fl_laydatemod_temp)


saveRDS(fl_laydatemod_temp, 'femfledgesuccess_tempmod.rds')
#laydate 

plot(Flaydate_clim$ClimwinTavg, Flaydate_clim$layday_yearday)

#convergence issue needs fixing 


fllaypred<-Flaydate_clim[,c("BirdID","WithinAge", "WithinAge2", "AveAge", "helperpresence",
                            "TerritoryID.x", "fledgesuccess", "layyear","layday_yearday_z" )]

fllaypred<-na.omit(fllaypred)

range(fllaypred$layday_yearday_z)

fledgesuccesspred<-ggpredict(fl_laydatemod_temp, new_data= fllaypred, ci_level = 0.95, terms = "layday_yearday_z [all]", back_transform = T)

View(fledgesuccesspred)

library(cowplot)


ggplot() +
  geom_line(data = fledgesuccesspred, mapping = aes(x = x, y = predicted)) +
  geom_ribbon(data = fledgesuccesspred, mapping = aes(ymin = conf.low, ymax = conf.high, x=x), alpha = 0.3) +
  geom_jitter(data = fllaypred, mapping = aes(x = layday_yearday_z, y = fledgesuccess), width=0.05, height=0.05) +
  xlab('Lay Date scaled (days into the year)') +
  ylab('Fledging success') +
  theme_classic(base_size = 14)+labs(title='')
#look at the interaction 
#laydate and temp on fl success 







#rein
library(optimx)

fl_laydatemod_rain<-glmer(fledgesuccess~layday_yearday_z*ClimwinRain + WithinAge + WithinAge2 + AveAge +
                            helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                          family = binomial(link='logit'), 
                          data= Flaydate_clim)

summary(fl_laydatemod_rain)
library(performance)
check_convergence(fl_laydatemod_rain)


#remove interaction
fl_laydatemod_rain2<-glmer(fledgesuccess~layday_yearday_z+ClimwinRain + WithinAge + WithinAge2 + AveAge +
                            helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                          family = binomial(link='logit'), 
                          data= Flaydate_clim, control = glmerControl(optimizer="bobyqa"))

summary(fl_laydatemod_rain2)

tab_model(fl_laydatemod_rain)
tab_model(fl_laydatemod_rain2)


simulateResiduals(fl_laydatemod_rain2, plot=T)


saveRDS(fl_laydatemod_rain2, 'fem_fledgesuccessrainmod.rds')


###################################
#trying same model with age and lifespan 
# reprodata<-read.csv('repro_28_5.csv')
# 
# alrafr<-unique(reprodata[,c("BirdID", 'minreproage', 'maxreproage')])
# 
# Flaydate_clim<-left_join(Flaydate_clim, alrafr, by='BirdID')
# 
# agemod_temp5<-lmer(layday_yearday~(age_year)*ClimwinTavg  + maxreproage + 
#                      I((age_year)^2)*ClimwinTavg +(age_year|BirdID), data=Flaydate_clim)
# 
# summary(agemod_temp5)

#samplesize halved 
#nope 


ggplot(Flaydate_clim, aes(x=ClimwinTavg, y=ClimwinRain))+geom_point()+stat_smooth(method='lm')
cor(Flaydate_clim$ClimwinTavg, Flaydate_clim$ClimwinRain, use = 'complete.obs')
  #claude solution for plotting 


# Build a grid that respects the relationship between WithinAge and WithinAge2
age_seq <- seq(min(Flaydate_clim$WithinAge), max(Flaydate_clim$WithinAge), length.out = 50)

# clim_vals <- c(mean(Flaydate_clim$ClimwinTavg, na.rm=T) - sd(Flaydate_clim$ClimwinTavg, na.rm=T),
#                mean(Flaydate_clim$ClimwinTavg, na.rm=T),
#                mean(Flaydate_clim$ClimwinTavg, na.rm=T) + sd(Flaydate_clim$ClimwinTavg, na.rm=T))

clim_vals<-c(-0.454, -0.302, -0.213,-0.146, 0.250)

pred_grid <- expand.grid(
  WithinAge  = age_seq,
  ClimwinTavg = clim_vals,
  AveAge     = mean(Flaydate_clim$AveAge)
) %>%
  mutate(WithinAge2 = WithinAge^2 - AveAge^2)  
# recalculate WithinAge2 to stay consistent with your function

pred_griddf <- predict(agemod_temp1, newdata = pred_grid, re.form = NA, se.fit=T)

fpred_grid<- transform(pred_grid, mod_fit=pred_griddf$fit, mod_se=pred_griddf$se.fit)

# pred_griddf <- predict(agemod_temp1, newdata = pred_grid, re.form = NA, se.fit=T)


library(viridis)
fagelayplot<-ggplot(fpred_grid, aes(x = WithinAge, y = mod_fit, colour = factor(ClimwinTavg))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin=(mod_fit-1.96*mod_se), ymax=(mod_fit+1.96*mod_se), fill=factor(ClimwinTavg)), 
              alpha=0.3,color=NA, show.legend = F)+
  labs(x = "Within-individual age deviation", y = "Predicted lay date", colour = "Change in temperature", title = '(a) Female lay date with age')+
  theme_classic(base_size = 14)+scale_colour_viridis_d(option='plasma')+
  scale_fill_viridis_d(option='plasma')

View(pred_grid)


ggplot(fpred_grid, aes(x = ClimwinTavg, y = mod_fit, colour = factor(WithinAge))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin=(mod_fit-1.96*mod_se), ymax=(mod_fit+1.96*mod_se), fill=factor(WithinAge)), 
              alpha=0.3,color=NA, show.legend = F)+
  labs(x = "Temperature", y = "Predicted lay date", colour = "Age", title = 'Female lay date with age')+
  theme_classic(base_size = 14)+scale_colour_viridis_d(option='plasma')+scale_fill_viridis_d(option='plasma')



tab_model(agemod_temp1)
#needs CI and bigger range of values 

#other plot 

# flaydayage<-Flaydate_clim[,c('BirdID', 'WithinAge', 'ClimwinTavg','WithinAge2', 'AveAge', 'layday_yearday')]
# 
# flaydayage<-na.omit(flaydayage)
# 
# predtest<-predict(agemod_temp1, newdata=flaydayage, se.fit=T)
# 
# 
# flaydayage<-transform(flaydayage, mod_fit=predtest$fit, mod_se=predtest$se.fit)
# 

summary(Flaydate_clim$ClimwinRain)

frain_vals<-c(-106.382, -4.077, 0.522, 5.792, 88.250)

frain_pred_grid <- expand.grid(
  WithinAge  = age_seq,
  ClimwinRain = frain_vals,
  AveAge     = mean(Flaydate_clim$AveAge)
) %>%
  mutate(WithinAge2 = WithinAge^2 - AveAge^2)  
# recalculate WithinAge2 to stay consistent with your function

rainpred_griddf <- predict(agemod_rainfinal, newdata = frain_pred_grid, re.form = NA, se.fit=T)

frain_pred_grid<- transform(frain_pred_grid, mod_fit=rainpred_griddf$fit, mod_se=rainpred_griddf$se.fit)

# pred_griddf <- predict(agemod_temp1, newdata = pred_grid, re.form = NA, se.fit=T)

ggplot(frain_pred_grid, aes(x = WithinAge, y = mod_fit, colour = factor(ClimwinRain))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin=(mod_fit-1.96*mod_se), ymax=(mod_fit+1.96*mod_se), fill=factor(ClimwinRain)), 
              alpha=0.3,color=NA, show.legend = F)+
  labs(x = "Within-individual age deviation", y = "Predicted lay date", colour = "Rain", title = 'Female lay date with age')+
  theme_classic(base_size = 14)+scale_colour_viridis_d(option='plasma')+scale_fill_viridis_d(option='plasma')

View(pred_grid)


flaytemppred <- ggpredict(
  fl_laydatemod_temp,
  terms = c("layday_yearday_z",
            "ClimwinTavg [-0.454, -0.302, -0.213, -0.146,  0.250]")
)

flaytempplot<-ggplot(flaytemppred,
       aes(x = x, y = predicted,
           colour = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = NA) +
  labs(x = "Lay day (scaled Julian days)",
       y = "Probability of nesting success",
       colour = "Change in temperature",
       fill = "Change in temperature") +
  theme_classic(base_size = 14)+labs(title='(A) Females')+scale_fill_viridis_d(option='plasma')+
  scale_colour_viridis_d(option='plasma')






#############






#################################################
#males
malelaydate<-left_join(malelaydate, tblbirdid, by='BirdID')

malelaydate$LayDateEarliest<-as.Date(malelaydate$LayDateEarliest, '%Y-%m-%d')
malelaydate$BirthDate<-as.Date(malelaydate$BirthDate, '%y-%m-%d')
malelaydate$AvgLaydate<-as.Date(malelaydate$AvgLaydate, '%Y-%m-%d')
#merge, get age, 
#change lay date into days into year 
#within model 

malelaydate$layday_yearday<-yday(malelaydate$AvgLaydate)


malelaydate$age_dates<-malelaydate$AvgLaydate-malelaydate$BirthDate

malelaydate$age_year<-round(as.numeric(malelaydate$age_dates/365))

malelaydate<-do.call("rbind", as.list(
  by(malelaydate, malelaydate[,"BirdID"], transform, AveAge=AveByInd(age_year))))


#within age 
malelaydate <- do.call("rbind", as.list(
  by(malelaydate, malelaydate[,"BirdID"], transform, WithinAge=WithinIndCentr(age_year))))


#within age squared 
malelaydate <- do.call("rbind", as.list(
  by(malelaydate, malelaydate[,"BirdID"], transform, WithinAge2=Quadwithin(age_year))))


# View(nestswithclimwin)

malelaydate$yearweek<-isoweek(malelaydate$AvgLaydate)


Mlaydate_clim<-left_join(malelaydate, nestswithclimwin, by=c('PeriodYear', 'yearweek'))


Mlaydate_clim<-filter(Mlaydate_clim, Mlaydate_clim$PeriodYear>1996)


#remove data for minor seasons 
Mlaydate_clim<-filter(Mlaydate_clim, Mlaydate_clim$yearweek>17)


Mlaydate_clim<-filter(Mlaydate_clim, Mlaydate_clim$BirdID>0)

Mlaydate_clim<-filter(Mlaydate_clim, Mlaydate_clim$layday_yearday>50)

# plot(Mlaydate_clim$layday_yearday)

Mlaydate_clim<-filter(Mlaydate_clim, Mlaydate_clim$PeriodYear %!in% tormv)
# View(Mlaydate_clim)
# male_agemod<-lmer(layday_yearday~WithinAge+ WithinAge2+AveAge+I(AveAge)^2+(1|BirdID), data=Mlaydate_clim)
# 
# summary(male_agemod)

male_agemod_temp1<-lmer(layday_yearday~WithinAge*ClimwinTavg + WithinAge2*ClimwinTavg + 
                          AveAge  +(WithinAge|BirdID), data=Mlaydate_clim)

summary(male_agemod_temp1)

tab_model(male_agemod_temp1)
# male_agemod_temp2<-lmer(layday_yearday~(WithinAge)*ClimwinTavg  + AveAge + I(AveAge^2)+
#                      I((age_year-AveAge)^2)*ClimwinTavg +(WithinAge|BirdID), data=Mlaydate_clim)

# summary(male_agemod_temp2)


male_agemod_rain1<-lmer(layday_yearday~WithinAge*ClimwinRain + WithinAge2*ClimwinRain + 
                     AveAge +(WithinAge|BirdID), data=Mlaydate_clim)

summary(male_agemod_rain1)

male_agemod_rain_final<-lmer(layday_yearday~WithinAge+ClimwinRain + WithinAge2+
                          AveAge +(WithinAge|BirdID), data=Mlaydate_clim)

summary(male_agemod_rain_final)

tab_model(male_agemod_rain1)
tab_model(male_agemod_rain2)

male_agemod_rain2<-lmer(layday_yearday~WithinAge*ClimwinRain + WithinAge2+
                          AveAge  +(WithinAge|BirdID), data=Mlaydate_clim)

summary(male_agemod_rain2)

tab_model(male_agemod_rain2)
################

Mlaydate_clim<-Mlaydate_clim%>%
  mutate(fledgesuccess = case_when(propfl_true >0 ~ 1, 
                                   TRUE ~ 0))
Mlaydate_clim<-Mlaydate_clim%>%
  mutate(helperpresence = case_when(nrH >0 ~ 1, 
                                    TRUE ~ 0))

Mlaydate_clim$helperpresence<-as.factor(Mlaydate_clim$helperpresence)

Mlaydate_clim$layday_yearday_z<-scale(Mlaydate_clim$layday_yearday)

Mlaydate_clim$fledgesuccess<-as.factor(Mlaydate_clim$fledgesuccess)
View(Mlaydate_clim)

# m_fsuccess<-glmer(fledgesuccess~layday_yearday_z + WithinAge + WithinAge2 + AveAge +
#                            helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
#                          family = binomial(link='logit'), 
#                          data= Mlaydate_clim, control = glmerControl(optimizer="bobyqa"))
# 
# summary(m_fsuccess)
library(performance)
# check_convergence(m_fsuccess)

# simulateResiduals(m_fsuccess, plot=T)
# 
# tab_model(m_fsuccess)
#well fuck you 

# mlaypred<-Mlaydate_clim[,c("BirdID","WithinAge", "WithinAge2", "AveAge", "helperpresence",
#                             "TerritoryID.x", "fledgesuccess", "layyear","layday_yearday_z" )]
# 
# mlaypred<-na.omit(mlaypred)
# 
# range(mlaypred$layday_yearday_z)

# mfledgesuccesspred<-ggpredict(m_fsuccess, new_data= mlaypred, ci_level = 0.95, terms = "layday_yearday_z [all]", back_transform = T)

# View(fledgesuccesspred)

library(cowplot)

# mlaypred$fledgesuccess<-as.numeric(mlaypred$fledgesuccess)
# 
# mlaypred$fledgesuccess[mlaypred$fledgesuccess==1]<-0
# mlaypred$fledgesuccess[mlaypred$fledgesuccess==2]<-1

# ggplot() +
#   geom_line(data = mfledgesuccesspred, mapping = aes(x = x, y = predicted)) +
#   geom_ribbon(data = mfledgesuccesspred, mapping = aes(ymin = conf.low, ymax = conf.high, x=x), alpha = 0.3) +
#   geom_jitter(data = mlaypred, mapping = aes(x = layday_yearday_z, y = fledgesuccess), width=0.05, height=0.05) +
#   xlab('Lay Date scaled (days into the year)') +
#   ylab('Fledging success') +
#   theme_classic(base_size = 14)+labs(title='Male fledging success')

# modelchecker(m_fsuccess)

m_laydatemod_temp<-glmer(fledgesuccess~layday_yearday_z*ClimwinTavg + WithinAge + WithinAge2 + AveAge +
                            helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                          family = binomial(link='logit'), 
                          data= Mlaydate_clim, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1e5)))

summary(m_laydatemod_temp)

tab_model(m_laydatemod_temp)

simulateResiduals(m_laydatemod_temp, plot=T)

m_laydatemod_temp2<-glmer(fledgesuccess~layday_yearday_z*ClimwinTavg + WithinAge  + AveAge +
                           helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                         family = binomial(link='logit'), 
                         data= Mlaydate_clim, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1e5)))

summary(m_laydatemod_temp2)
tab_model(m_laydatemod_temp2)



saveRDS(m_laydatemod_temp, 'male_fledgesuccesstempmod.rds')

m_laydatemod_rain<-glmer(fledgesuccess~layday_yearday_z * ClimwinRain + WithinAge + WithinAge2 + AveAge +
                           helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                         family = binomial(link='logit'), 
                         data= Mlaydate_clim, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1e5)))

summary(m_laydatemod_rain)

tab_model(m_laydatemod_rain)
m_laydatemod_rain2<-glmer(fledgesuccess~layday_yearday_z + ClimwinRain + WithinAge  
                          + WithinAge2+ AveAge +
                           helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                         family = binomial(link='logit'), 
                         data= Mlaydate_clim, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1e5)))

summary(m_laydatemod_rain2)

tab_model(m_laydatemod_rain2)


simulateResiduals(m_laydatemod_rain, plot=T)

m_laydatemod_rain3<-glmer(fledgesuccess~layday_yearday_z + ClimwinRain + WithinAge  + AveAge +
                            helperpresence+(1|BirdID)+(1|TerritoryID.x)+(1|layyear), 
                          family = binomial(link='logit'), 
                          data= Mlaydate_clim, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1e5)))

summary(m_laydatemod_rain3)

##########
#Plot 

pred <- ggpredict(
  m_laydatemod_temp2,
  terms = c("layday_yearday_z",
            "ClimwinTavg [-0.454,-0.302,-0.213,-0.146,0.250]")
)

malelaydatetempplot<-ggplot(pred,
       aes(x = x, y = predicted,
           colour = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = NA) +
  labs(x = "Lay day (scaled Julian days)",
       y = "Probability of nesting success",
       colour = "Change in temperature",
       fill = "Change in temperature") +
  theme_classic(base_size = 14)+labs(title='(B) Males')+
  scale_fill_viridis_d(option = 'plasma')+
  scale_colour_viridis_d(option='plasma')


library(ggpubr)

ggarrange(flaytempplot, malelaydatetempplot, common.legend=T, legend = 'right')


mlaydatetemp_pred_grid <- expand.grid(
  WithinAge  = age_seq,
  ClimwinTavg = clim_vals,
  AveAge     = mean(Mlaydate_clim$AveAge)
) 

# recalculate WithinAge2 to stay consistent with your function

pred_griddf <- predict(agemod_rain1, newdata = pred_grid, re.form = NA, se.fit=T)

frain_pred_grid<- transform(frain_pred_grid, mod_fit=pred_griddf$fit, mod_se=pred_griddf$se.fit)

# pred_griddf <- predict(agemod_temp1, newdata = pred_grid, re.form = NA, se.fit=T)

ggplot(frain_pred_grid, aes(x = WithinAge, y = mod_fit, colour = factor(ClimwinRain))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin=(mod_fit-1.96*mod_se), ymax=(mod_fit+1.96*mod_se), fill=factor(ClimwinTavg)), 
              alpha=0.3,color=NA, show.legend = F)+
  labs(x = "Within-individual age deviation", y = "Predicted lay date", colour = "Temperature", title = 'Female lay date with age')+
  theme_classic(base_size = 14)+scale_colour_viridis_d(option='plasma')+scale_fill_viridis_d(option='plasma')









#############################
#
male_age_seq <- seq(min(Mlaydate_clim$WithinAge), max(Mlaydate_clim$WithinAge), length.out = 50)

summary(Mlaydate_clim$ClimwinTavg)
male_clim_vals<-c(-0.454 , -0.302, -0.213,-0.146, 0.250)

Male_pred_grid <- expand.grid(
  WithinAge  = male_age_seq,
  ClimwinTavg = male_clim_vals,
  AveAge     = mean(Mlaydate_clim$AveAge)
) %>%
  mutate(WithinAge2 = WithinAge^2 - AveAge^2)  
# recalculate WithinAge2 to stay consistent with your function

maletemppred <- predict(male_agemod_temp1, newdata = Male_pred_grid, re.form = NA, se.fit=T)

Male_pred_grid<-transform(Male_pred_grid, mod_fit=maletemppred$fit, mod_se=maletemppred$se.fit)

magelayplot<-ggplot(Male_pred_grid, aes(x = WithinAge, y = mod_fit, colour = factor(ClimwinTavg))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin=(mod_fit-1.96*mod_se), ymax=(mod_fit+1.96*mod_se), fill=factor(ClimwinTavg)), 
              alpha=0.3,color=NA, show.legend = F)+
  labs(x = "Within-individual age deviation", y = "Predicted lay date", colour = "Change in temperature", title = '(b) Male lay date with age')+
  theme_classic(base_size = 14)+scale_colour_viridis_d(option='plasma')+scale_fill_viridis_d(option='plasma')


ggarrange(fagelayplot, magelayplot, common.legend = T, legend = 'right', font.label = 24)

#######
tempchangetest<-lm(layday_yearday~PeriodYear, data=Flaydate_clim)

summary(tempchangetest)
Flaydate_clim$PeriodYear

plot(Flaydate_clim$PeriodYear, Flaydate_clim$layday_yearday)



temprainlm<-lm(fledgesuccess~ ClimwinRain*ClimwinTavg, data=Flaydate_clim)
summary(temprainlm)
vif(temprainlm)

#########
#new tables 
#male and female age model temperature 
tab_model(agemod_temp1, male_agemod_temp1, digits = 2)


tab_model(agemod_rainfinal, male_agemod_rain2, digits = 2, pred.labels = T)


#male and female age model, rainfall 
tab_model(agemod_rain1, male_agemod_rain1, digits=2)

#male and female fl success, temp
tab_model(fl_laydatemod_temp, m_laydatemod_temp, digits=2)

#male and female fl success, rainfall 
tab_model(fl_laydatemod_rain, m_laydatemod_rain, digits=2)


tab_model(fl_laydatemod_temp)
#supplementary tables for age rain female 
tab_model(agemod_rain2, agemod_rainfinal, digits=2)

#supplementary tables for age rain male 
tab_model(male_agemod_rain3, male_agemod_rain2, digits=2)

#supplementary of male fl success square removed. 
tab_model(m_laydatemod_temp2)

#supplementary female rain fl success 
tab_model(fl_laydatemod_rain2)

#supplementary male rain fl success 
tab_model(m_laydatemod_rain2, m_laydatemod_rain3, digits=2)


# claire is habing a breakdown
#table 5
tab_model(agemod_rainfinal, male_agemod_rain_final, digits=2)

#table 6
tab_model(fl_laydatemod_temp, m_laydatemod_temp2, digits=2)

#table 7
tab_model(fl_laydatemod_rain2, m_laydatemod_rain3, digits=2)

#S4
tab_model(agemod_rain1,agemod_rain2, digits=2)

# summary(agemod_rain2)
#S5
tab_model(male_agemod_rain1, male_agemod_rain2, digits=2)

#S6
tab_model(m_laydatemod_temp, digits=2)

#S7
tab_model(fl_laydatemod_rain, digits=2)

#S8
tab_model(m_laydatemod_rain,m_laydatemod_rain2, digits=2)

# claire's breakdown ends
