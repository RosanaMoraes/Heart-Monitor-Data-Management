install.packages(c("chron", "lubridate", "xts", "dplyr", "reshape2", "plyr", "XML", "xml2"))

library(chron)
library(lubridate)
library(chron)
library(xts)
library(reshape2)
library(plyr)
library(XML)
library(xml2)
library(dplyr)
library(stringr)
library(tidyr)

#M.Ditmer_12/20/2017; updated by Peter Leimgruber 07/23/2018 & Qiongyu Huang 10/16/2018 for use with Maned Wolf Project at SCBI

###Linq data
#set the WD to wherever the LINQ .xml files are stored

setwd("C:/Users/NogueiradeMoraesR/Dropbox (Smithsonian)/Maned wolf Heart Monitor Project/Data & Analysis/MW Heart Monitor R Project")

data_export_dir<- "C:/Users/NogueiradeMoraesR/Dropbox (Smithsonian)/Maned wolf Heart Monitor Project/Data & Analysis/MW Heart Monitor R Project/Extracted_data"
## where we want to save the data to

file_list <- list.files(pattern = ".xml") 
file_list


#Peter, modified code to only read xml files

animal_name<-list.files("C:/Users/NogueiradeMoraesR/Dropbox (Smithsonian)/Maned wolf Heart Monitor Project/Data & Analysis/MW Heart Monitor R Project", pattern = ".xml") #"C:/Users/NogueiradeMoraesR/Dropbox (Smithsonian)/Maned wolf Heart Monitor Project/Data & Analysis/R Projects/MW Heart Monitor Data Project/XML Files") #Peter, modified code to only read xml files
animal_name<- str_extract(animal_name, "[a-zA-Z]+" )
animal_name
# the name of the individuals in the loop

ImplantDateTime_vec <- c("2018-06-26 10:06:30", "2018-06-26 11:48:30", "2018-06-27 09:32:30", "2018-06-27 11:13:30", "2018-07-24 09:52:30", "2018-07-24 11:29:45")  ###### animal specific starting time
## need to provide the inplantation time for each animal, need to change the time when data is given
## the order HAS TO MATCH with the file_list

#check on all of the Linq ID's in a given folder before doing full loop..../Only have the latest download in the directory
#or run manually

#######################################  Manual XML File Read  ################################################

for (i in (1 : length(file_list))) {

test.file<- file_list[i]


datax<-xmlParse(test.file)  #enter the xml file name you want to analyze
data<-xmlToList(datax) #convert xml as a list
serial<-as.vector(unlist(data$.attrs[2]))
#print(data)
print(serial)


####################################  Run loop to combine all xml files in directory into a single analysis ##########################
####################################  Use this when comparing or combining animals for analysis             ##########################
####################################  Serial numbers are IDs for animals                                    ##########################

### convert the xml to a list

# for(i in 1:length(file_list)){
# dataX<-xmlParse(file_list[i])}
# data<-xmlToList(dataX)
# serial<-as.vector(unlist(data$.attrs[2]))
# print(i)
# print(serial)

# for(i in 1:length(file_list)){
# dataX<-xmlParse(file_list[i])}



#############################################  Listing of all data fields available from XML files ##############################

###Data available for Reveal XT
###Source:
### $SingleVariableTrend$TrendData
##[1]StartDateTime
##[2]TrendData 

###Data available for LNQ11
###Source:
### $SingleVariableTrend$TrendData
##[1]TimeStamp
##[2]AverageImpedanceAtStartOfFourHourInterval
##[3]TemperatureAtStartOfFourHourIntervalCelsius
##[4]TwoMinuteHRAvg
##[5]NumActiveMinutesEachFifteenMinutePeriod
##[6]XAxisAccelAvgData
##[7]YAxisAccelAvgData
##[8]ZAxisAccelAvgData
##[9]PostureChangeCounts
##[10]PostureCounts



################################################################################################################################
ImpedData<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[2]) ##[2]is from above list
ImpedData <- as.character(ImpedData)
ImpedData<-strsplit(ImpedData, split = " ")
ImpedData<-as.numeric(unlist(ImpedData))
ImpedData<-as.data.frame(ImpedData)

####################################################### Read and Store Heart Rate Data (2 min bpm)   ##########################

Hrdata<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[4])##[4]is from above list
Hrdata <- as.character(Hrdata)
Hrdata<-strsplit(Hrdata, split = " ")
Hrdata<-as.numeric(unlist(Hrdata))
HrdataT<-as.data.frame(Hrdata)

####################################################### Read and Store Activity Data (15 min)   ###############################

###Active Minutes data (15 min avg)
ActMindata<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[5])##[5]is from above list
ActMindata <- as.character(ActMindata)
ActMindata<-strsplit(ActMindata,split = " ")
ActMindata<-as.numeric(unlist(ActMindata))

###X-axis accelerometer data (15 min avg)
XAccel<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[6])##[6]is from above list
XAccel <- as.character(XAccel)
XAccel<-strsplit(XAccel,split = " ")
XAccel<-as.numeric(unlist(XAccel))

###Y-axis accelerometer data (15 min avg)
YAccel<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[7])##[7]is from above list
YAccel <- as.character(YAccel)
YAccel<-strsplit(YAccel,split = " ")
YAccel<-as.numeric(unlist(YAccel))

###Z-axis accelerometer data (15 min avg)
ZAccel<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[8])##[8]is from above list
ZAccel <- as.character(ZAccel)
ZAccel<-strsplit(ZAccel,split = " ")
ZAccel<-as.numeric(unlist(ZAccel))

###Posture Change Counts data (15 min avg)
PostChg<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[9])##[9]is from above list
PostChg<- as.character(PostChg)
PostChg<-strsplit(PostChg,split = " ")
PostChg<-as.numeric(unlist(PostChg))

###Posture Counts data (15 min avg)
PostCt<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[10])##[10]is from above list
PostCt<- as.character(PostCt)
PostCt<-strsplit(PostCt,split = " ")
PostCt<-as.numeric(unlist(PostCt))

ActdataT<-as.data.frame(cbind(ActMindata,XAccel,YAccel,ZAccel,PostChg,PostCt))
head(ActdataT)

####################################################### Read and Store Temperature Data (4 hrs)   ###############################

##Temperature at start of 4 hour period degrees Celcius
TempData<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[3])##[3]is from above list
TempData<- as.character(TempData)
TempData<-strsplit(TempData,split = " ")
TempData<-as.numeric(unlist(TempData))


########################################### Read Start and End Time and Create Time Stamp for Dataframes ########################


## Extract and convert Timestamp for temperature and impedance data (start of 4 h period)
## Read from list (XML)
Ts<-as.vector(data$SensorMeasurementTrend$HourlyMeasurementSetTrend$AggregatedData[1])##[1]is from above list
Ts <- as.character(Ts)
n<-20   # No of characters for each individual time stamp in the list 
Time4H<-substring(Ts,seq(1,nchar(Ts),n),seq(n,nchar(Ts)+n-1,n)) #parse the individual time stamps based on character length 
Time4H
Time4H<-as.POSIXct(unlist(Time4H), tz="UTC") #convert characters to a time POSIXct time object #make sure the time zone is correct (EDT) 
TEMPct<-as.data.frame(cbind(TempData,Time4H)) #add timestamp to temperature data
TEMPct$Time4H<-as.POSIXct(Time4H)
IMPEDct<-as.data.frame(cbind(ImpedData,Time4H)) #add timestamp to impedance data
IMPEDct$Time4H<-as.POSIXct(Time4H)

## Create Timestamp for Heart Rate Data (2 min intervals)
head(Time4H)  #Make sure the start time is the same you have recorded on your data sheet when the device was first started (not implantation)
tail(Time4H)  #End time should be the beginning time of the last complete 4 hour interval before download
EndTime <- Time4H[(length(Time4H))] + 14400  #add 4 hours because the last 2 min interval is within the 4 hours
EndTime
EndTime<-data.frame(EndTime)
nobs<-1:nrow(HrdataT) #assess how often HR was recorded
length(nobs)
EndTimeRep<-rep(EndTime$EndTime,nrow(HrdataT))
EndTimeRep<-data.frame(EndTimeRep)
EndTimeRep2 <-ymd_hms((EndTimeRep$EndTimeRep -   (120*nobs)), tz="UTC") # if time zone isn't specified this reverts to UTC
EndTimeRep2
EndTimeAllS<-sort(EndTimeRep2)
HRct<-cbind(HrdataT,EndTimeAllS)
head(HRct)
tail(HRct)

##  Create Timestamp for the activity data
Actnobs<-1:nrow(ActdataT)

ActEndTimeRep<-rep(EndTime$EndTime,nrow(ActdataT))
ActEndTimeRep<-data.frame(ActEndTimeRep)
ACTEndTimeRep2 <-ymd_hms((ActEndTimeRep$ActEndTimeRep -   (900*Actnobs)),tz = "UTC") #
ACTEndTimeAllS<-sort(ACTEndTimeRep2)
ACTct<-cbind(ActdataT,ACTEndTimeAllS)
head(ACTct)
tail(ACTct)

########################################################  extract device number and add to Data Frames, use as animal ID #############
serial<-as.vector(unlist(data$.attrs[2]))
serial<-as.factor(serial)
HRct$serial<-serial
ACTct$serial<-serial
TEMPct$serial<-serial
IMPEDct$serial <-serial

########################################################  Manually Enter Implant Date to Remove Records From Before    ###############

ImplantDateTime <- ImplantDateTime_vec[i]  #replace with date you need to have

ImplantDateTime <- as.POSIXct(ImplantDateTime) + (72*60*60)  # add 3 days to remove effects from implantation 

#subset data to exclude all date collected before StartDate
HR <- subset(HRct, EndTimeAllS >= as.POSIXct(ImplantDateTime)) %>%
  mutate(name= animal_name[i])

ACT <- subset(ACTct, ACTEndTimeAllS >= as.POSIXct(ImplantDateTime))%>%
  mutate(name= animal_name[i])

TEMP <- subset(TEMPct, Time4H >= as.POSIXct(ImplantDateTime))%>%
  mutate(name= animal_name[i])

IMPED <- subset(IMPEDct, Time4H >= as.POSIXct(ImplantDateTime))%>%
  mutate(name= animal_name[i])

#########################################################  Write data to CSV  ######################################################
write.csv(HR, paste0(data_export_dir,"/extract_", animal_name[i],"_HR.csv") )
write.csv(ACT, paste0(data_export_dir,"/extract_", animal_name[i],"_ACT.csv"))
write.csv(TEMP, paste0(data_export_dir,"/extract_", animal_name[i],"_TEMP.csv"))
write.csv(IMPED, paste0(data_export_dir,"/extract_", animal_name[i],"_IMPED.csv"))
}
#####  combine tables #####

## combine HR data


## combine ACT data
file_list_ACT <- list.files(data_export_dir, pattern = "ACT", full.names=TRUE) #Peter, modified code to only read xml files
file_list_ACT

ACT_list<- list()
for (i in (1 : length(file_list_ACT))){
  ACT_list[[i]]<- read.csv(file_list_ACT[i])}

ACT_combined<- bind_rows(ACT_list)

write.csv(ACT_combined, "ACT_combined.csv")

## combine TEMP data
file_list_TEMP <- list.files(data_export_dir, pattern = "TEMP", full.names=TRUE) #Peter, modified code to only read xml files
file_list_TEMP

TEMP_list<- list()
for (i in (1 : length(file_list_TEMP))){
  TEMP_list[[i]]<- read.csv(file_list_TEMP[i])}

TEMP_combined<- bind_rows(TEMP_list)

write.csv(TEMP_combined, "TEMP_combined.csv")

## combine IMPED data
file_list_IMPED <- list.files(data_export_dir, pattern = "IMPED", full.names=TRUE) #Peter, modified code to only read xml files
file_list_IMPED

IMPED_list<- list()
for (i in (1 : length(file_list_IMPED))){
  IMPED_list[[i]]<- read.csv(file_list_IMPED[i])}

IMPED_combined<- bind_rows(IMPED_list)

write.csv(IMPED_combined, "IMPED_combined.csv")

## Add coluns Day & Hour to HR combined file
HRsplitEndTime<- separate(HR_combined, col=EndTimeAllS, c("day", "hour"), sep=" ", remove=FALSE)%>%
  mutate(day= as.Date(day))

HR_combined<- HRsplitEndTime

write.csv(HR_combined, "HR_combined.csv")


## Add coluns Day & Hour to ACT combined file
ACTsplitEndTime<- separate(ACT_combined, col=ACTEndTimeAllS, c("day", "hour"), sep=" ", remove=FALSE)%>%
  mutate(day= as.Date(day))

ACT_combined<- ACTsplitEndTime

write.csv(ACT_combined, "ACT_combined.csv")

## Add coluns Day & Hour to TEMP combined file
TEMPsplitEndTime<- separate(TEMP_combined, col=Time4H, c("day", "hour"), sep=" ", remove=FALSE)%>%
  mutate(day= as.Date(day))

TEMP_combined<- TEMPsplitEndTime

write.csv(TEMP_combined, "TEMP_combined.csv")


## Add coluns Day & Hour to IMPED combined file
IMPEDsplitEndTime<- separate(IMPED_combined, col=Time4H, c("day", "hour"), sep=" ", remove=FALSE)%>%
  mutate(day= as.Date(day))

IMPED_combined<- IMPEDsplitEndTime

write.csv(IMPED_combined, "IMPED_combined.csv")
