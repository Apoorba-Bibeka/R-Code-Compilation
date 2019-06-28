#File Name:  DataCleanPDSL.R
#Created by: Apoorba Bibeka
#Creation date: June 18 2019
#Date Modified : June
#Purpose:To clean shoulder lane closure data and get % of time shoulder in use each hour of hte day
#Last executed:
Sys.time()

#1 Housekeeping
#********************************************************************************************************************
ls()
rm(list=ls())
ls()

#2 Load Libraries
#********************************************************************************************************************
library(readxl)
library(data.table)
library(xts)
library(zoo)
library(writexl)
#3 Read Data
#********************************************************************************************************************
dir1="C:/Users/abibeka/OneDrive - Kittelson & Associates, Inc/Documents/PartTimeShoulderUse-Pete"
setwd(dir1)
dat<- read_excel("PDSL event log.xlsx")
dat<-data.table(dat)
str(dat)


#4. Data Cleaning
#********************************************************************************************************************
#Checking if it is only one device
table(dat$Deviceid)
# Get msg's for diff lanes
dat[,c("m1","m2","m3"):=tstrsplit(Message,' ',keep=c(1,2,3))]
# Getting the categories for 1st msg
table(dat$m1)  
# "Lane" category needs to be combined with 2nd message (Delimiters are not consistent)
# "Use" category needs to be combined with 2nd message (Delimiters are not consistent)
dat[m1=='Lane'|m1=='Use'|m1=='Merge',m1:=paste(toupper(trimws(m1)),"_",toupper(trimws(m2)),sep="")]
table(dat$m1)  
#Remove Description == "LCS CLEARED"
table(dat$Description)
dat=dat[Description=="LCS DEPLOYED"]
table(dat$Description)
table(dat$m1)  #All the "None" are removed from shoulder use message

#5. Get clean time series data
#********************************************************************************************************************
data<-dat[,.(Description,m1,m2,m3)]
dates<-dat$Eventdate
# Use xts() to create smith
dat1 <- xts(x = data, order.by =dates)
#Get On, Off instead of all the status given for Shoulder Lane
dat1$ShLnStat<-dat1$m1
#Assign all UNKNOWN, VSA or NONE as NA and then backfill the NAs
mask0= toupper(dat1$ShLnStat) %in% c("VSA","UNKNOWN","NONE")
dat1[mask0,"ShLnStat"] = rep(NA,sum(mask0))
sum(is.na(dat1$ShLnStat))
#Fill NA with last known value
dat2<-na.locf(dat1)
sum(is.na(dat2$ShLnStat))
#Assign all DARK and LANE_CLOSED as 0 
mask=toupper(dat2$ShLnStat) %in% c("DARK","LANE_CLOSED")
dat2[mask,"ShLnStat"] = rep(0,sum(mask))
#Assign all LANE_OPEN, LANE_CLOSED_AHEAD, USE_CAUTION and MERGE_RIGHT as 1 
mask1=toupper(dat2$ShLnStat) %in% c("LANE_OPEN","LANE_CLOSED_AHEAD","USE_CAUTION","MERGE_RIGHT")
dat2[mask1,"ShLnStat"] = rep(1,sum(mask1))
#Check If the above worked
table(dat2$ShLnStat)
sum(is.na(dat2$ShLnStat))
dat3<-dat2$ShLnStat
dat3[order(index(dat3))]
head(dat3)
#Create boundary time events for each hour. Need for summation.
rm(list=c("dates","data"))
dates<-seq(round(min(index(dat2)),"hours"),round(max(index(dat2)),"hours"), by = "hours")
data<-rep(NA, length(dates))
LHSda1 <- xts(order.by=dates)
length(LHSda1["2014"])
#Insert the boundary values to orignal data
MerDa<-merge(LHSda1,dat3,join="outer")
head(MerDa)
MerDa2<-na.locf(MerDa)
head(MerDa2,100)
tail(MerDa2,20)
sum(is.na(MerDa2)) #1st value should be NA so good
#CHECKING:
head(lag(MerDa2,k=-1))
tail(lag(MerDa2,k=-1))

#Create a data with lag to get status diffs:
MerDa3<-lag(MerDa2,k=-1)
colnames(MerDa3)<-"LagStat"
MerDa3<-merge(MerDa2,MerDa3,join="inner")
#Create data table with lead lag columns
MerDa4<-data.table(coredata(MerDa3))
MerDa4$dt<-index(MerDa3)
str(MerDa4)
MerDa4[,LAGdt:=lapply(.SD, function(x) c(tail(as.character(x), -1L),NA)),.SDcols=c("dt")]
MerDa4[,LAGdt:=as.POSIXct(LAGdt,format="%Y-%m-%d %H:%M:%OS",tz='UTC')]
MerDa4[,TmDiff:=difftime(LAGdt,dt,units="mins")]
#If the Lane was open at the starting of an interval. It was open at the end also
#If a lane was closed at the starting of an interval. It was closed at the end also
#Take intervals where lane was open at start (ShLnStat= ==1)
MerDa4[,TmON:=TmDiff*ShLnStat]
MerDa4[,Hr:=hour(dt)]
MerDa4[,FullDate:=tstrsplit(dt," ",keep=1)]
MerDa4[,CmSumON:=sum(na.omit(TmON)),by=list(Hr,FullDate)]
MerDa5<-unique(MerDa4,by=c("Hr","FullDate"))
MerDa5[,PerTimeOn:=round(as.numeric(CmSumON)*100/60,2)]
table(MerDa5$CmSumON)
MerDa6<-MerDa5[,.(dt,FullDate,Hr,CmSumON,PerTimeOn)]

#Get # of time Unknown
table(dat$m1)
UnkDAT<-dat[,.(Eventdate,m1)]
UnkDAT[,Unkn:=ifelse(toupper(m1)=="UNKNOWN",1,0)]
UnkDAT[,Hr:=hour(Eventdate)]
UnkDAT[,FullDate:=tstrsplit(Eventdate," ",keep=1)]
UnkDAT[,CmSumUNK:=sum(na.omit(Unkn)),by=list(Hr,FullDate)]
UnkDAT<-unique(UnkDAT,by=c("Hr","FullDate"))
UnkDAT<-UnkDAT[,.(Hr,FullDate,CmSumUNK)]
#Merge the data with % time ON with # of UNKNOWNS
MerDa6<-merge(MerDa6,UnkDAT,by=c("Hr","FullDate"),all.x=TRUE,sort=FALSE)
MerDa6[,CmSumUNK:=na.fill(CmSumUNK,0)]
setnames(MerDa6,"Hr","Start-Hr")

#TESTING
#********************************************************************************************************************
str(MerDa6)
table(diff(MerDa6$`Start-Hr`))  # Should be 0 or -23 (Midnight - 11 pm)
c(min(MerDa6$PerTimeOn),max(MerDa6$PerTimeOn))
c(min(MerDa6$CmSumON),max(MerDa6$CmSumON))

#Write to excel
#********************************************************************************************************************
file="PDSL_Log_Clean.xlsx"
write_xlsx(MerDa6, file)