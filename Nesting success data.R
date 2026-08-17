library(RODBC)
library(tidyverse)

"%!in%" <-Negate("%in%")

DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

nestdata<-sqlQuery(swdb, 'SELECT tblNestInfo.*, tblBreedGroupLocation.FieldPeriodID, tblBreedGroupLocation.TerritoryID, tblFieldPeriodIDs.Island, tblFieldPeriodIDs.PeriodYear
FROM tblFieldPeriodIDs INNER JOIN (tblBreedGroupLocation INNER JOIN tblNestInfo ON tblBreedGroupLocation.BreedGroupID = tblNestInfo.BreedGroupID) ON tblFieldPeriodIDs.FieldPeriodID = tblBreedGroupLocation.FieldPeriodID;
', stringsAsFactors=F)

nestdata<-nestdata[,-c(20:22)]
nestdata<-filter(nestdata, nestdata$Island=='CN')

breedgroup<-sqlQuery(swdb, "SELECT tblBreedStatus.BreedGroupID, tblBreedStatus.BirdID, tblBreedStatus.Status, tblBreedGroupLocation.TerritoryID, tblBreedGroupLocation.FieldPeriodID, tblFieldPeriodIDs.Island
FROM tblFieldPeriodIDs INNER JOIN (tblBreedGroupLocation INNER JOIN tblBreedStatus ON tblBreedGroupLocation.BreedGroupID = tblBreedStatus.BreedGroupID) ON tblFieldPeriodIDs.FieldPeriodID = tblBreedGroupLocation.FieldPeriodID
WHERE (((tblFieldPeriodIDs.Island)='CN'));"
, stringsAsFactors=F)

breedgroup<-arrange(breedgroup, BreedGroupID)

library(data.table)

#make breed group horizontal (only include brf and brm and FL) 
#for each group, get brf, brm, nr of helpers, nr of fl and nr of other subordinates 


breedgroupinfo<-data.frame()
for(i in unique(breedgroup$BreedGroupID)){
  onegroup<-breedgroup%>%filter(BreedGroupID == i )  #may return duplicate statuses
  BrF <- ifelse(length(onegroup$BirdID[onegroup$Status == "BrF"]) > 0, 
                onegroup$BirdID[onegroup$Status == "BrF"][1], NA)
  BrM <- ifelse(length(onegroup$BirdID[onegroup$Status == "BrM"]) > 0, 
                onegroup$BirdID[onegroup$Status == "BrM"][1], NA)
  nrFL<-nrow(subset(onegroup, Status %in% c('FL','OFL'), select = BirdID))
  nrH<-nrow(subset(onegroup, Status=='H', select = BirdID))
  nrSub<-nrow(subset(onegroup, Status %!in% c('BrF','BrM','H','FL','CH','EGG','OFL'), select = BirdID))
  TerritoryID<-unique(onegroup$TerritoryID)
  FieldPeriodID<-unique(onegroup$FieldPeriodID)
  groupinfo<-data.frame(BreedGroupID = i, BrF=BrF, BrM=BrM, nrFL=nrFL, nrH=nrH, nrSub=nrSub, TerritoryID=TerritoryID, FieldPeriodID=FieldPeriodID)
  breedgroupinfo<-rbind(groupinfo, breedgroupinfo)
}

# length(unique(breedgroup$BreedGroupID))

breedgroupinfo<- arrange(breedgroupinfo, BreedGroupID)
 


#nest fate 

nestdata<-nestdata[,-c(8,9,12,13)]
nestdata<-nestdata[,-c(11,13,15)] 
#proportion fledged - nr fledglings divided by clutch size 
nestdata$propFledged<-nestdata$NoFledglings / nestdata$ClutchSize
#prop fledged based on brood size  - largely similar except very few discrepancies 
nestdata$propFledged_brd<-nestdata$NoFledglings / nestdata$BroodSize
nestdata$propFledged_brd[is.nan(nestdata$propFledged_brd)]<-0

#average lay date 
nestdata$LayDateEarliest<-as.Date(nestdata$LayDateEarliest, '%Y-%m-%d')
nestdata$LayDateLatest<-as.Date(nestdata$LayDateLatest, '%Y-%m-%d')

nestdata<- nestdata%>%
  rowwise()%>%
  mutate(AvgLaydate = mean(c(LayDateEarliest,LayDateLatest), na.rm=T))
nestdata$layyear<-str_sub(nestdata$AvgLaydate,1,4)


#remove nests with NA lay date earliest and latest 
nestdata<-nestdata[!with(nestdata, is.na(LayDateEarliest) & is.na(LayDateLatest)),]

#3092 nests

#add time period 
nestdata<-nestdata%>%
  mutate(TimePeriod = case_when( layyear < 2011 ~ "1997-2010", 
                                 layyear > 2010 ~ '2011-2024'))

#there are two nests in 2024 

# nestdata<-nestdata %>% filter(layyear<2024) #remove 2024 


#merge nest data with breed group data 

nestdata<-left_join(nestdata,breedgroupinfo, by=c('BreedGroupID'))

clean_nestdata<-nestdata

#checking with pedigree 90 day survival 

# ars90day<-read.csv('ars_90day.csv', stringsAsFactors = F)  #but this is the parent though..  and ars 
# ars90day<-ars90day[,-c(1)]

#read pedigree
ped<-read.csv('updated pedigree.csv', stringsAsFactors = F, sep=';')
#filter pedigree genetic  confidence 
ped<-filter(ped, ped$GenDadConfidence >=80 & ped$GenMumConfidence >=80)


pedfate<-left_join(clean_nestdata,ped, by='NestID')


lifespan<-read.csv('lifespan_28_5_24.csv', stringsAsFactors = F)
lifespan<-lifespan[,-c(1)]
pedfate_lifespan<-left_join(pedfate, lifespan, by='BirdID')


#what the FUCK does this do 
#these are all the nests that arent in the pedigree right, because no bird ID linked to nest
pedfate_checkID<-pedfate_lifespan%>%group_by(NestID)%>%
  mutate(nrpedrow= case_when(is.na(BirdID)~ 0, TRUE ~ n()
  ))

#number of fledglings according to the pedigree 
pedfate_checkID<-pedfate_checkID%>%
  group_by(NestID)%>%
  mutate(nrpedfl = sum(newlifespan > 20 ))

View(pedfate_checkID)
#but should check with original nest right 

pedfate_fls<-pedfate_checkID%>%
  mutate(maxfl = case_when(is.na(nrpedfl)~NoFledglings,
                           !is.na(nrpedfl)~max(NoFledglings, nrpedfl)))

#what about birds without a nest ID in the pedigree 


#but this worked somehow 
pedfate_clutch<-pedfate_fls%>%
  mutate(maxclutchsize = max(nrpedrow, ClutchSize, BroodSize))


#
#141 rows with NA max nr of fledglings 
pedfate_clutch$maxfl[is.na(pedfate_clutch$maxfl)]<-0

#some rows have no fledglings in both nest and ped data, but status assignment for breedgroups shows fl statuses 

pedfate_clutch$propfl_true<-pedfate_clutch$maxfl / pedfate_clutch$maxclutchsize

View(pedfate_clutch)
#nan for nests that have 0 eggs 

pedfate_clutch<-pedfate_clutch[,-c(29:36)]
pedfate_clutch<-pedfate_clutch[,-c(27:29)]
pedfate_clutch<-unique(pedfate_clutch)


test<-pedfate_clutch%>%
  group_by(NestID)%>%
  summarise(n=n())

pedfate_clutch<-as.data.frame(pedfate_clutch)

whyna<-filter(pedfate_clutch, is.na(pedfate_clutch$propfl_true))
#0 divided by 0 



#no more duplicate rows 

write.csv(pedfate_clutch, 'pedfate.csv')

#nests that have no fledglings but have birds from the pedigree 
#there are also more rows when joined so multiple birds from same nest  

#how do you know if its the same bird? maybe its the same bird but not assigned to the nest? 

#create new row in the df 
#if nr of fledglings is 0 and bird ID is not NA, assign mismatch 
# 
# test<-pedfate %>%
#   mutate(mismatch= case_when(NoFledglings==0 & !is.na(BirdID) & newlifespan>20 ~ 'Mismatch',
#                              NoFledglings==0 & !is.na(BirdID) & newlifespan<20 ~ 'Match',
#                              NoFledglings==0 & is.na(BirdID)~ 'Match'))



#remember to check with the pedigree or bird ID table as well to see if there are any 
#birds that we missed but ringed retrospectively 
#check that they survived 90 days  (sys_stat per field period) or 17 days 



