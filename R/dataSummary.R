PackageList =c("plyr","dplyr","tidyverse","lubridate","censusapi","forcats","lemon","reshape","ggplot2")
NewPackages=PackageList[!(PackageList %in% 
                            installed.packages()[,"Package"])]
if(length(NewPackages)) install.packages(NewPackages,repos = "http://cran.us.r-project.org")
lapply(PackageList,require,character.only=TRUE)#array function

options(tibble.print_max = Inf, tibble.print_min = 20) 
set.seed(1) #Always set the seed for reproducibility

knit_print.data.frame <- lemon_print
source("taxiData.R")
#Load records from taxi data source and store as a dataframe
data1m <- taxiData(5000000)
df <- data.frame(data1m$rawData)
rm(data1m) #Drop old data
df$dropoff_community_area<-as.factor(df$dropoff_community_area)
df$dropoff_centroid_location.coordinates<-as.character(df$dropoff_centroid_location.coordinates)
df$pickup_centroid_location.coordinates<-as.character(df$pickup_centroid_location.coordinates)
df$trip_total<-as.numeric(df$trip_total)
df$tolls<-as.numeric(df$tolls)
df$extras<-as.numeric(df$extras)

#Select only the columns we will be using
colKeep<-c("company","pickup_community_area","dropoff_community_area","fare","tolls","extras","tips","payment_type","taxi_id","trip_miles","trip_seconds","trip_end_timestamp","trip_start_timestamp","trip_total","pickup_census_tract","dropoff_census_tract","data.table")
df<-df[, (names(df) %in% colKeep)]

#% of Records that are NA in each column
summaryNA <- as.data.frame(sapply(df,function(x) sum(is.na(x)))/nrow(df)*100)
#% of records that are zero in each column
summaryZero <- as.data.frame(colSums(df==0,na.rm=TRUE)/nrow(df)*100)

cat("Summary of data with NA records in each column:")
summaryNA

#Drop all NA records for the following columns
naFilter<-c("fare","tolls","extras","tips","trip_total","trip_seconds","trip_miles","trip_end_timestamp","dropoff_community_area","pickup_community_area")
dfClean<-df[complete.cases(df[,naFilter]),]

#Drop records where miles, seconds, fare <=0
dfClean<-dfClean[dfClean$trip_miles>0,]
dfClean<-dfClean[dfClean$trip_seconds>0,]
dfClean<-dfClean[dfClean$fare>0,]
dfClean<-dfClean[dfClean$trip_total>0,]

cat("After dropping NA and zero records: ", round(100*(1-(nrow(dfClean)/nrow(df))),2),"% of records dropped")

numericCols <- c("fare","tolls","extras","tips","trip_total","trip_seconds","trip_miles")
meltData <- melt(dfClean[,numericCols])

dfClean2 <- dfClean
dfClean2 <- dfClean2[dfClean2$fare>=3.25,]
dfClean2 <- dfClean2[dfClean2$trip_total>=3.25,]
dfClean2 <- dfClean2[dfClean2$fare<=225,]
dfClean2 <- dfClean2[dfClean2$extras<=225,]
dfClean2 <- dfClean2[dfClean2$trip_seconds>=30,]
dfClean2 <- dfClean2[dfClean2$trip_miles<=100,]

cat("After additional filtering ", round(100*(1-(nrow(dfClean2)/nrow(df))),2),"% of all records have been dropped")

#look at distribution of cab companies
dfClean2 %>% dplyr::group_by(company) %>% dplyr::summarize(percent_trips=round(100*n()/nrow(dfClean2),5),
  mean_fare = round(mean(fare,na.rm=TRUE),2) )%>% arrange(desc(percent_trips))
dfClean2$company <- fct_explicit_na(dfClean2$company,"NA")
topCompany <- dfClean2 %>% dplyr::group_by(company) %>% tally(sort=TRUE)
companyKeep <- topCompany[topCompany$n>(.001*nrow(dfClean2)),]
#Add level for other companies with low frequency in data
levels(dfClean2$company) <- c(levels(dfClean2$company),"Other")
dfClean2$company[dfClean2$company] <- "Other"

CurrentColumn=dfClean2$company 
Thres_Low=floor(.001*nrow(dfClean2))
CurrentColumn_Table=table(CurrentColumn) #Tabulate the frequency
levels(CurrentColumn)[CurrentColumn_Table<=Thres_Low]="Other"
dfClean2$company=CurrentColumn 
droplevels(dfClean2$company)

##Generate a distribtion after adding our new level

dfClean2 %>% dplyr::group_by(company) %>% dplyr::summarize(percent_trips=round(100*n()/nrow(dfClean2),5),
  mean_fare = round(mean(fare,na.rm=TRUE),2) )%>% arrange(desc(percent_trips))

#Load census data by Chicago Community Area
ccaData <- read.csv("ChicagoCCAData.csv")
ccaData$CCA.Code<-as.factor(ccaData$CCA.Code)
ccaData$GEOG<-as.factor(ccaData$GEOG)
#Look up name of pickup CCA and add to df
dfClean2<- dfClean2 %>% left_join(select(ccaData,GEOG,CCA.Code),by=c("pickup_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'GEOG'] <- 'pickup_area'
#Look up name of dropoff CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,GEOG,CCA.Code),by=c("dropoff_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'GEOG'] <- 'dropoff_area'
#Look up population>16yo of pickup CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,POP_16OV,CCA.Code),by=c("pickup_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'POP_16OV'] <- 'Pop16PlusPickup'
#Look up population>16yo of dropoff CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,POP_16OV,CCA.Code),by=c("dropoff_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'POP_16OV'] <- 'Pop16PlusDropoff'
#Look up Unemp of pickup CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,MEDINC,CCA.Code),by=c("pickup_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'MEDINC'] <- 'MedIncomePickup'
#Look up Unemp of dropoff CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,MEDINC,CCA.Code),by=c("dropoff_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'MEDINC'] <- 'MedIncomeDropoff'
#Look up %Commuters who drive of pickup CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,PercentDrive,CCA.Code),by=c("pickup_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'PercentDrive'] <- 'PercentDrivePickup'
#Look up %Commuters who drive of dropoff CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,PercentDrive,CCA.Code),by=c("dropoff_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'PercentDrive'] <- 'PercentDriveDropoff'
#Look up % Unemp of pickup CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,PercentUnemp,CCA.Code),by=c("pickup_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'PercentUnemp'] <- 'PercentUnempPickup'
#Look up % Unemp of dropoff CCA and add to dfClean2
dfClean2<- dfClean2 %>% left_join(select(ccaData,PercentUnemp,CCA.Code),by=c("dropoff_community_area"="CCA.Code"))
names(dfClean2)[names(dfClean2) == 'PercentUnemp'] <- 'PercentUnempDropoff'

#Add levels for new factors:
#For our two unemp vectors: 0-5%,>5-10%,>10-15%,>15-20%,>20-25%, >25%
#For population: 0-10k, >10-20K, >20-30k, 30-50k, 50-75k, >75k
#For % MedInc: 0-25k, >25-40k, >40-55k, >55-70k, >70k
#For % Drive: 0-40%, >40-60%, >60-80%, >80%

#Assign factor levels for unemployment
#Create new columns for factors
dfClean2$PercentUnempPickupF <- as.factor(dfClean2$PercentUnempPickup)
dfClean2$PercentUnempDropoffF <- as.factor(dfClean2$PercentUnempDropoff)
Thres1=5
Thres2=10
Thres3=15
Thres4=20
Thres5=25

colsToChange=c("PercentUnempPickupF","PercentUnempDropoffF")

for (i in 1:2){
  CurrentColumn=dfClean2[[colsToChange[i]]]                    #Extraction of column

  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn))) 
  levels(CurrentColumn)[CurrentColumn_Table$y<=Thres1]="0-5%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres1 & CurrentColumn_Table$y<=Thres2 ]=">5-10%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres2 & CurrentColumn_Table$y<=Thres3 ]=">10-15%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres3 & CurrentColumn_Table$y<=Thres4 ]=">15-20%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres4 & CurrentColumn_Table$y<=Thres5 ]=">20-25%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres5]=">25%"

  dfClean2[[colsToChange[i]]]=CurrentColumn                    #Plug-back to the data.frame
}


#Assign factor levels for unemployment
#Create new columns for factors
dfClean2$Pop16PlusPickupF <- as.factor(dfClean2$Pop16PlusPickup)
dfClean2$Pop16PlusDropoffF <- as.factor(dfClean2$Pop16PlusDropoff)
Thres1=20000
Thres2=50000
Thres3=60000
Thres4=70000
Thres5=80000
Thres6=90000

colsToChange=c("Pop16PlusPickupF","Pop16PlusDropoffF")

for (i in 1:2){
  CurrentColumn=dfClean2[[colsToChange[i]]]                    #Extraction of column

  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn))) 
  levels(CurrentColumn)[CurrentColumn_Table$y<=Thres1]="0-20k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres1 & CurrentColumn_Table$y<=Thres2]=">20-50K"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres2 & CurrentColumn_Table$y<=Thres3]=">50-60k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres3 & CurrentColumn_Table$y<=Thres4]=">60-70k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres4 & CurrentColumn_Table$y<=Thres5]=">70-80k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres5 & CurrentColumn_Table$y<=Thres6]=">80-90k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres6]=">90k"

  dfClean2[[colsToChange[i]]]=CurrentColumn                    #Plug-back to the data.frame
}

#Assign factor levels for Median Income
#Create new columns for factors
dfClean2$MedIncomePickupF <- as.factor(dfClean2$MedIncomePickup)
dfClean2$MedIncomeDropoffF <- as.factor(dfClean2$MedIncomeDropoff)
Thres1=25000
Thres2=40000
Thres3=55000
Thres4=70000
Thres5=85000
Thres6=100000

colsToChange=c("MedIncomePickupF","MedIncomeDropoffF")

for (i in 1:2){
  CurrentColumn=dfClean2[[colsToChange[i]]]                    #Extraction of column

  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn))) 
  levels(CurrentColumn)[CurrentColumn_Table$y<=Thres1]="0-25k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres1 & CurrentColumn_Table$y<=Thres2]=">25-40K"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres2 & CurrentColumn_Table$y<=Thres3]=">40-55k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres3 & CurrentColumn_Table$y<=Thres4]=">55-70k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres4 & CurrentColumn_Table$y<=Thres5]=">70-85k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres5 & CurrentColumn_Table$y<=Thres6]=">85-100k"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres6]=">100k"

  dfClean2[[colsToChange[i]]]=CurrentColumn                    #Plug-back to the data.frame
}


#Assign factor levels for percent of commuters who drive
#Create new columns for factors
dfClean2$PercentDrivePickupF <- as.factor(dfClean2$PercentDrivePickup)
dfClean2$PercentDriveDropoffF <- as.factor(dfClean2$PercentDriveDropoff)
Thres1=40
Thres2=60
Thres3=80
Thres4=90
Thres5=95
Thres6=100

colsToChange=c("PercentDrivePickupF","PercentDriveDropoffF")

for (i in 1:2){
  CurrentColumn=dfClean2[[colsToChange[i]]]                    #Extraction of column

  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn))) 
  levels(CurrentColumn)[CurrentColumn_Table$y<=Thres1]="0-40%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres1 & CurrentColumn_Table$y<=Thres2]=">40-60%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres2 & CurrentColumn_Table$y<=Thres3]=">60-80%"
  CurrentColumn_Table=data.table(x=levels(CurrentColumn),y=as.numeric(levels(CurrentColumn)))
  levels(CurrentColumn)[CurrentColumn_Table$y>Thres6]=">80%"

  dfClean2[[colsToChange[i]]]=CurrentColumn                    #Plug-back to the data.frame
}

#Add some additional columns
dfClean2$startDate <- lubridate::date(dfClean2$trip_start_timestamp)
dfClean2$endDate <- lubridate::date(dfClean2$trip_end_timestamp)
dfClean2$startDay <- lubridate::wday(dfClean2$trip_start_timestamp,label=TRUE)
dfClean2$startMonth <- lubridate::month(dfClean2$trip_start_timestamp,label=TRUE)
dfClean2$startHour <- lubridate::hour(str_replace(dfClean2$trip_start_timestamp,"T"," "))

dfClean2$hourlyRate<-dfClean2$trip_total/(dfClean2$trip_seconds/(60*60))
dfClean2$avgMPH<-dfClean2$trip_miles/(dfClean2$trip_seconds/(60*60))
dfClean2$tipOutcome<-ifelse(dfClean2$tips>0,1,0)

dfClean2$startDay<-factor(dfClean2$startDay,ordered=FALSE)
dfClean2$startMonth<-factor(dfClean2$startMonth,ordered=FALSE)
dfClean2$startHour<-as.factor(dfClean2$startHour)
