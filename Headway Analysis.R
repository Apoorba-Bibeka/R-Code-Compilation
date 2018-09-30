# Script for processing the headways
#File Name: headway_summary
#Created by: Apoorba Bibeka
#Creation date:7 Feb 2016
#Purpose:To get the flow rate at a section 
#Last executed:
Sys.time()

#1 Housekeeping
ls()
rm(list=ls())
ls()


#Set the current directory to the directory where all the data files are kept
#setwd("/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/PerfMeasures/Dot_Mer")

setwd("C:/Users/a-bibeka/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/PerfMeasures/Dot_Mer")

#Load data.table package for fread to work
library(data.table)

#fread by default create data table. Change the data table to data frame
options(datatable.fread.datatable=FALSE)

#Find and store the files which contains length in their names
files<-list.files(pattern=".mer")


#Skip the first row   fread_  skip=1
#File has header 		fread header=true

#i index of list li. Stores the number of the senario
i<-1

#li is a list to hold all the file numerical names 
li<-list()

#loop over files to extract all the raw data files and save them in a data frame (pla_len_1, ....)
for(file in files){
		no=gsub("[^0-9]","",file)
		fi_name=paste("flow_rate",no,sep="")
		assign(paste("flow_rate",no,sep=""),fread(file,header=TRUE, skip=1,sep=";"))
		li[[i]]<-no
i<-i+1
}   

#List all the objects in the R workspace 
ls()

flow_summary <-data.frame()


library(plyr)

#Iterate over all the files 

i<-1
final_data<-data.frame()
for(i in 1:length(li)){
		#extract file name
		fi_name=paste("flow_rate",li[[i]],sep="")
		#convert file name to data frame
		data_f<-(get(fi_name))
		
		colnames(data_f)[2]<-"time_stamp"
		colnames(data_f)[1]<-"Measurem."
		#Calculation for Non Left lane 
		time_stamp<-data_f[data_f$Measurem.!=3,"time_stamp"]
		time_stamp<-time_stamp[(time_stamp!=-1)]
		time_stamp <-sort(time_stamp)
		
	
		bi<-seq(900,4500,300)
		bin<-findInterval(time_stamp,   bi)

		headway<-diff(time_stamp)
		
		dat<-aggregate(headway,list(bin[-1]),mean)
		dat<-data.frame(bi,dat$x)
		dat$Scenario<-paste("Scenario",li[[i]],sep="_")
		dat$Lane<-1
		colnames(dat)<-c("Interval","avg_headway","Scenario","Lane")
		dat$flow<-(1/dat$avg_headway)*3600
		dat<-dat[,c("Scenario","Lane","Interval","avg_headway","flow")]
    
		#dat<-data.table(dat)
		#dat<-dat[,max_flow:=max(flow),by="Scenario"]
    #dat<-unique(dat,by="Scenario")
    #dat[,Interval:=NULL]
    #dat[,avg_headway:=NULL]
    #dat[,flow:=NULL]
    #dat<-data.frame(dat)
		final_data<-rbind(final_data,dat)

    
		
		time_stamp<-NULL
		dat<-NULL
		bi<-NULL
		bin<-NULL
		headway<-NULL
		
		# Calculation for left lane 
		time_stamp<-data_f[data_f$Measurem.==3,"time_stamp"]
		time_stamp<-time_stamp[(time_stamp!=-1)]
		time_stamp <-sort(time_stamp)
		
		
		bi<-seq(900,4500,300)
		bin<-findInterval(time_stamp,   bi)
		headway<-diff(time_stamp)
		
		
		dat<-aggregate(headway,list(bin[-1]),mean)
		dat<-data.frame(bi,dat$x)
		dat$Scenario<-paste("Scenario",li[[i]],sep="_")
		dat$Lane<-3
		colnames(dat)<-c("Interval","avg_headway","Scenario","Lane")
		dat$flow<-(1/dat$avg_headway)*3600
		dat<-dat[,c("Scenario","Lane","Interval","avg_headway","flow")]
  
		final_data<-rbind(final_data,dat)
		
		time_stamp<-NULL
		dat<-NULL
		bi<-NULL
		bin<-NULL
		headway<-NULL
		
		#Calculation for all lanes 
		time_stamp<-data_f[,"time_stamp"]
		time_stamp<-time_stamp[(time_stamp!=-1)]
		time_stamp <-sort(time_stamp)
		
		
		bi<-seq(900,4500,300)
		bin<-findInterval(time_stamp,   bi)
		headway<-diff(time_stamp)
		
		
		dat<-aggregate(headway,list(bin[-1]),mean)
		dat<-data.frame(bi,dat$x)
		dat$Scenario<-paste("Scenario",li[[i]],sep="_")
		dat$Lane<-123
		colnames(dat)<-c("Interval","avg_headway","Scenario","Lane")
		dat$flow<-(1/dat$avg_headway)*3600
		dat<-dat[,c("Scenario","Lane","Interval","avg_headway","flow")]
		
		#dat<-data.table(dat)
		#dat<-dat[,max_flow:=max(flow),by="Scenario"]
		#dat<-unique(dat,by="Scenario")
		#dat[,Interval:=NULL]
		#dat[,avg_headway:=NULL]
		#dat[,flow:=NULL]
		#dat<-data.frame(dat)
		final_data<-rbind(final_data,dat)
		
		
		
		time_stamp<-NULL
		dat<-NULL
		bi<-NULL
		bin<-NULL
		headway<-NULL
		
		
	}
	
		#input the list of sen
	list_sen<-fread("List_of_scenarios.csv",header=T)
	final_data<-merge(final_data,list_sen, by.x="Scenario")

		x= final_data
		#file="/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results/Flow_rate_summary.csv"
	  file="C:/Users/a-bibeka/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results/Flow_rate_summary.csv"
		write.table(x,file,append=F,row.names=F,sep=",")




