library(RODBC)
library(tidyverse)

"%!in%" <-Negate("%in%")

DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

nestdata<-sqlFetch(swdb, 'tblNestInfo', stringsAsFactors=F)

nestdata<-nestdata[,c(1:19)]

breedgroup<-sqlQuery(swdb, 'SELECT tblBreedStatus.BreedGroupID, tblBreedStatus.BirdID, tblBreedStatus.Status, tblBreedGroupLocation.TerritoryID, tblBreedGroupLocation.FieldPeriodID
FROM tblBreedGroupLocation INNER JOIN tblBreedStatus ON tblBreedGroupLocation.BreedGroupID = tblBreedStatus.BreedGroupID;
', stringsAsFactors=F)

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

#average lay date 
nestdata$LayDateEarliest<-as.Date(nestdata$LayDateEarliest, '%Y-%m-%d')
nestdata$LayDateLatest<-as.Date(nestdata$LayDateLatest, '%Y-%m-%d')

nestdata<- nestdata%>%
  rowwise()%>%
  mutate(AvgLaydate = mean(c(LayDateEarliest,LayDateLatest), na.rm=T))
nestdata$layyear<-str_sub(nestdata$AvgLaydate,1,4)


#remove nests with NA lay date earliest and latest 
nestdata<-nestdata[!with(nestdata, is.na(LayDateEarliest) & is.na(LayDateLatest)),]

#3309 nests

#add time period 
nestdata<-nestdata%>%
  mutate(TimePeriod = case_when( layyear < 2006 ~ "1997-2005", 
                                 layyear > 2005 & layyear < 2015 ~ '2006-2014', 
                                 layyear > 2014 ~ '2015-2023'))

#there are two nests in 2024 

nestdata<-nestdata %>% filter(layyear<2024) #remove 2024 


#checking with pedigree 90 day survival 

ars90day<-read.csv('ars_90day.csv', stringsAsFactors = F)  #but this is the parent though..  and ars 
ars90day<-ars90day[,-c(1)]

#read pedigree
ped<-read.csv('updated pedigree.csv', stringsAsFactors = F, sep=';')
#filter pedigree genetic  confidence 
ped<-filter(ped, ped$GenDadConfidence >=80 & ped$GenMumConfidence >=80)


pedfate<-left_join(nestdata,ped, by='NestID')

#nests that have no fledglings but have birds from the pedigree 
#there are also more rows when joined so multiple birds from same nest 



#remember to check with the pedigree or bird ID table as well to see if there are any 
#birds that we missed but ringed retrospectively 
#check that they survived 90 days  (sys_stat per field period) or 17 days 



