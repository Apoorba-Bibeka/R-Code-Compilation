#File Name: LDV_Combined_reception_R_script
#Created by: Apoorba Bibeka
#Creation date: 9 Feb 2016
#Purpose:To process VISSIM vehicle records into MOVES bin
#Last executed:
Sys.time()

#1 Housekeeping
ls()
rm(list=ls())
ls()


#Set the current directory to the directory where all the data files are kept
setwd("/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/PerfMeasures")
#setwd("C:/Users/a-bibeka/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/PerfMeasures")
#Load data.table package for fread to work
library(data.table)

#fread by default create data table. Change the data table to data frame
options(datatable.fread.datatable=FALSE)

#Find and store the files which contains length in their names
files<-list.files(pattern="moves_process")


#Skip the first row   fread_  skip=1
#File has header 		fread header=true


final_data<-data.frame()



#Import the emission rates from moves
Emission_rate<-read.csv("Emission_Rates_HDV.csv", header = TRUE, sep = ",")

#Rename the columns of Emission_rate
colnames(Emission_rate)<-c("op_Mode","polID","rate")

#Change op_Mode from 999 to 0
Emission_rate[Emission_rate$op_Mode==999,"op_Mode"]<-0


#Change the polutant names 
decode<-read.csv("decode.csv", header = TRUE, sep = ",")


#loop over files to extract all the raw data files and save them in a data frame (pla_len_1, ....)
for(file in files){
		no=gsub("[^0-9]","",file)
		assign(paste("moves_process",no,sep=""),fread(file,header=TRUE, skip=1))

		fi_name=paste("moves_process",no,sep="")
		#convert file name to data frame
		vehicle_record <-(get(fi_name))


		rm(list=paste("moves_process",no,sep=""))

# Structure of vehicle record
#str(vehicle_record)

# Columns present in the data 
#names(vehicle_record)

#Rename Column names
colnames(vehicle_record)<-c("vehID","simtime_sec","SPEED","ACCELERATION","status","front_gap","rear_gap","reception")

#Create a subset for only HGV 
#HGV_record<-subset(vehicle_record, vehicle_record$status==201 || vehicle_record$status==202 ||vehicle_record$status==203 ||vehicle_record$status==204)


#vehicle_record<-vehicle_record[,c("vehID","simtime_sec","SPEED","ACCELERATION","status","front_gap","rear_gap","reception")]


#Change MPH to m/s2 and ft/s2 to m/s2  if US units are used in VISSIM 
vehicle_record$SPEED<-vehicle_record$SPEED* 0.44704
vehicle_record$ACCELERATION <- vehicle_record$ACCELERATION* 0.3048


#function to calculate A for STP calculation
fun_stp_A <- function(weight) {
  if(weight<=14000 & weight>=8500)
    s_A<-(weight* 0.0996/2204.6)
  else if(weight <=33000 & weight>14000)
    s_A<-(weight*0.0875/2204.6)
  else if(weight>33000)
    s_A<-(weight*0.0661/2204.6)
  else
    s_A<--1 #Some error has occured 
  
  s_A
}


#Assume all trucks weigh 33000 lb
vehicle_record$WEIGHT<-33000

#Add column for calculating A for STP for Heavy vehicles (Ignore LHV for now)									
vehicle_record$A_stp<-mapply(fun_stp_A,vehicle_record$WEIGHT)

#Add column for calculating B for STP for HV
vehicle_record$B_stp<-0

#function to calculate C for STP calculation
fun_stp_C <- function(weight) {
  #ignore the weight term 
  #all three category have same equation because same frontal area is being used for HGV, MGV etc
  #Moves power calculation is wrong
  #It gives C in terms of WATT but says that it is in Kilo Watt 
  if(weight<=14000 & weight>=8500)
    s_C<-(12.5*0.65*1.225)/(2*1000)
  else if(weight <=33000 & weight>14000)
    s_C<-(12.5*0.65*1.225)/(2*1000)
  else if(weight>33000)
    s_C<-(12.5*0.65*1.225)/(2*1000)
  else
    s_C<--1 #Some error has occured 
  
  s_C
}

#Add column for calculating C for STP for HV
vehicle_record$C_stp<-mapply(fun_stp_C,vehicle_record$WEIGHT)

#functions to calculate the reduction in drag coefficient for vehicle in platooning
rear_cd_coef <- function(front_gap_ft, rear_veh_len) {

       # get the reduced drag coef (Cd/Cd0) of the rear veh
       # based on the front gap distance (ft) and rear_veh_len (ft)
       # Use Table 3.1.a of PATH report
       # assume no more reduction at 4 car lengths

       spc_ratio <- front_gap_ft/rear_veh_len
       

       if(spc_ratio > 0 & spc_ratio < 0.35)
              cd_coef <- 0.73
       else if (spc_ratio >= 0.35 & spc_ratio < 0.53)
              cd_coef <- 0.73 + (spc_ratio-0.35)/(0.53-0.35)*(0.6441-0.73)
       else if (spc_ratio >=0.53 & spc_ratio < 0.98)
              cd_coef <- 0.6441 + (spc_ratio-0.53)/(0.98-0.53)*(0.7529-0.6441)
       else if (spc_ratio >=0.98 & spc_ratio < 2)
              cd_coef <- 0.7529 + (spc_ratio-0.98)/(2-0.98)*(1-0.7529)
       else if (spc_ratio >=2 | front_gap_ft == -1)
              # too large gap or no leader
              cd_coef <- 1
       else
              # unknown NA gives error 
              cd_coef <- 1

       cd_coef
}

lead_cd_coef <- function(rear_gap_ft, lead_veh_len) {

       # get the reduced drag coef (Cd/Cd0) of the lead veh
       # based on the rear gap distance (ft) and lead_veh_len (ft)
       # Use Table 3.2.a of PATH report
       # assume no more reduction at 2 car lengths

       spc_ratio <- rear_gap_ft/lead_veh_len
       

       if(spc_ratio > 0 & spc_ratio < 0.35)
              cd_coef <- 0.7124
       else if (spc_ratio >= 0.35 & spc_ratio < 0.53)
              cd_coef <- 0.7124 + (spc_ratio-0.35)/(0.53-0.35)*(0.7-0.7124)
       else if (spc_ratio >=0.53 & spc_ratio < 0.98)
              cd_coef <- 0.7 + (spc_ratio-0.53)/(0.98-0.53)*(0.8877-0.7)
       else if (spc_ratio >=0.98 & spc_ratio < 2)
              cd_coef <- 0.8877 + (spc_ratio-0.98)/(2-0.98)*(1-0.8877)
       else if (spc_ratio >=2 | rear_gap_ft == -1)
              # too large gap or no leader
              cd_coef <- 1
       else
              # unknown
              cd_coef <- 1

       cd_coef
}


#Take some length of HGV / Can later obtain this from VISSIM 
vehicle_record$veh_len<-59
vehicle_record$C_reduction<-1
#Calculate the reduction factor for C 
		#202 is a leader
		vehicle_record[vehicle_record$status==202,"C_reduction"]<-mapply(lead_cd_coef,vehicle_record[vehicle_record$status==202,"rear_gap"] ,vehicle_record[vehicle_record$status==202,"veh_len"])
	
		#203 is a follower
		vehicle_record[vehicle_record$status==203,"C_reduction"]<-mapply(rear_cd_coef,vehicle_record[vehicle_record$status==203,"front_gap"] ,vehicle_record[vehicle_record$status==203,"veh_len"])


		#204 is a member
		vehicle_record[vehicle_record$status==204,"C_reduction"]<-mapply(rear_cd_coef,vehicle_record[vehicle_record$status==204,"front_gap"] ,vehicle_record[vehicle_record$status==204,"veh_len"])




		#Calculate C_stp*reduction factor
	  vehicle_record$C_stp<-vehicle_record$C_stp*vehicle_record$C_reduction

		
		
		#Calculate STP for HGV
		vehicle_record$STP<-(vehicle_record$A_stp*vehicle_record$SPEED+vehicle_record$B_stp*(vehicle_record$SPEED^2)+vehicle_record$C_stp*(vehicle_record$SPEED^3)+(vehicle_record$WEIGH*0.000453592)*vehicle_record$SPEED*(vehicle_record$ACCELERATION))/17.1
		
  

#function to get the operating mode 
opmode_fun<-function(stp,speed,acc){
				
				#change speed to Mph
				speed = speed/0.44704
				
				if (acc<=-2)
						opMode<-0
				else if (speed==0)
						opMode<-1
				else if(stp<0 & speed<25 &speed >=1)
						opMode<-11
				else if(stp>=0 & stp<3 & speed>=1 & speed<25)
						opMode<-12
				else if(stp>=3 &stp<6 & speed>=1 &speed<25)
						opMode<-13
				else if(stp>=6 & stp<9 &speed>=1 & speed<25)
						opMode<-14
				else if(stp>=9 & stp<12 &speed>=1 &speed<25)
						opMode<-15
				else if(stp>=12 &speed>=1 &speed<25)
						opMode<-16
				else if(stp<0 &speed>=25 &speed<50)
						opMode<-21
				else if(stp>=0 &stp<3 &speed>=25 &speed<50)
						opMode<-22
				else if(stp>=3 &stp<6 &speed>=25 &speed<50)
						opMode<-23
				else if(stp>=6 &stp<9 & speed>=25 & speed <50)
						opMode<-24
				else if(stp>=9 &stp<12 & speed>=25 & speed <50)
						opMode<-25
				else if(stp>=12 & stp<18 & speed>=25 & speed <50)
						opMode<-27
				else if(stp>=18 & stp<24 & speed>=25 & speed <50)
						opMode<-28
				else if(stp>=24 & stp<30 & speed>=25 & speed <50)
						opMode<-29
				else if(stp>=30 & speed>=25 & speed <50)
						opMode<-30
				else if(stp<6 & speed>=50)
						opMode<-33
				else if(stp>=6 & stp<12 & speed>=50)
						opMode<-35
				else if(stp>=12 & stp<18 & speed>=50)
						opMode<-37
				else if(stp>=18 & stp<24 & speed>=50 )
						opMode<-38
				else if(stp>=24 & stp<30 & speed>=50)
						opMode<-39
				else if(stp>=30 & speed>=50)
				 		opMode<-40
				else 
						#error
						opMode<--1
						
				opMode
}


#Put the values into bins  
vehicle_record$op_Mode<-mapply(opmode_fun,vehicle_record[,"STP"],vehicle_record[,"SPEED"],vehicle_record[,"ACCELERATION"])


#Test 
#vehicle_record<-vehicle_record[vehicle_record$ACCELERATION>=0,]
#




#Find the distance travelled by vehicle (MI)  
	#(1 sec is the time step at which data is recorded
vehicle_record$dist<- 1* (vehicle_record$SPEED/3600)/0.44704

#vehicle_record<-vehicle_record[,c("vehID","op_Mode","dist")]


#Merge vehicle_record and Emission_rate
LDV_EM <-merge(vehicle_record,Emission_rate)



LDV_EM$Emissions<-LDV_EM$dist*LDV_EM$rate


LDV_EM<-LDV_EM[order(LDV_EM$vehID,LDV_EM$op_Mode,LDV_EM$polID,decreasing=FALSE),]



#Keep an output table to view later 
#testing the opmode distribution
#HG_out<-LDV_EM


LDV_EM<-LDV_EM[,c("vehID","op_Mode","polID","Emissions","dist")]


# Initialize the data.table package
library(data.table)

LDV_ems<-data.table(LDV_EM)

#delete LDV_EM
rm(LDV_EM)


# aggregate with 2 variables
LDV_ems[, Total_em := sum(Emissions), by = list(vehID, polID)]



#TEST
#aggregate distance


LDV_ems[, Total_dist := sum(dist), by = list(vehID, polID)]
LDV_ems[, Avg_em:=Total_em/Total_dist, by = list(vehID, polID) ]
LDV_ems$Total_em<-NULL
LDV_ems$Total_dist<-NULL
LDV_ems$dist<-NULL

#end test 

#Remove the column for emissions
LDV_ems$Emissions<-NULL
LDV_ems$op_Mode<-NULL


#Remove all the redundant rows with same Total_em value
LDV_ems<-unique(LDV_ems,by=c("vehID","polID"))


#Change the polutant names 
LDV_ems<-merge(LDV_ems,decode, by="polID")

#Remove Polutant ID
LDV_ems$polID<-NULL

LDV_ems<-data.frame(LDV_ems)

#load reshape library 
library(reshape)

LDV_wide<- reshape(LDV_ems, 
  timevar = "Pol",
  idvar = c("vehID"),
  direction = "wide")
  
   
rm("LDV_ems")
#===========================================================================================================
#===========================================================================================================

#get the mean speed and root mean acceleration for vehicles 
sum_speed<-vehicle_record[,c("vehID","SPEED","ACCELERATION")]
sum_speed$SPEED<-sum_speed$SPEED/0.44704
sum_speed<-data.table(sum_speed)
setkey(sum_speed,vehID)
sum_speed[, avg_speed := mean(SPEED), by ="vehID"]
sum_speed[,std_speed:=sd(SPEED),by="vehID"]
sum_speed[,avg_acc:=mean(ACCELERATION), by ="vehID"]
sum_speed[,std_acc:=sd(ACCELERATION),by="vehID"]
sum_speed<-unique(sum_speed,by="vehID")



# get the average front gap of followers and members
sum_frn<-vehicle_record[,c("vehID","status","front_gap")]
sum_frn<-data.table(sum_frn)
setkey(sum_frn,vehID)
sum_frn[status==203|status==204, avg_front_gap := mean(front_gap), by ="vehID"]
sum_frn[status==203|status==204, stdev_front_gap := sd(front_gap,na.rm=TRUE), by ="vehID"]
sum_frn<-sum_frn[status==203|status==204,]
sum_frn<-unique(sum_frn,by="vehID")


#get the per time as leader 
summary_pla<-vehicle_record[,c("vehID","status")]
summary_pla<-data.table(summary_pla)
setkey(summary_pla,vehID)
summary_pla[,no:=.N,by= list(vehID,status)]
summary_pla<-unique(summary_pla,by=c("vehID","status"))
summary_pla[,travel_time:=sum(no),by="vehID"]
summary_pla[status!=201,time_platoon:=sum(no),by="vehID"]
summary_pla[status==202,per_time_leader:=100*sum(no)/travel_time,by="vehID"]

summary_pla<-summary_pla[status==202,.(vehID,travel_time,time_platoon,per_time_leader)]


#get the per time as follower 
summary_fol<-vehicle_record[,c("vehID","status")]
summary_fol<-data.table(summary_fol)
setkey(summary_fol,vehID)
summary_fol[,no0:=.N,by= list(vehID,status)]
summary_fol<-unique(summary_fol,by=c("vehID","status"))
summary_fol[,travel_time1:=sum(no0),by="vehID"]
summary_fol[status!=201,time_platoon:=sum(no0),by="vehID"]
summary_fol[status==203|status==204,per_time_follower:=100*sum(no0)/travel_time1,by="vehID"]
summary_fol<-summary_fol[status==203,.(vehID,per_time_follower)]



#get secondary details
summary_rec<-vehicle_record[,c("vehID","reception")]
summary_rec<-data.table(summary_rec)
setkey(summary_rec,vehID)



if(summary_rec[reception==1,.N]==0){
  summary_rec[,per_rec:=100]
  summary_rec<-unique(summary_rec,by="vehID")
}else{
  summary_rec<-summary_rec[reception!=-99]
  summary_rec[,no1:=na.omit(.N),by=list(vehID,reception)]
  summary_rec<-unique(summary_rec,by=c("vehID","reception"))
  summary_rec[,total_rec:=sum(no1),by="vehID"]
  summary_rec[reception==1,per_rec:=100*sum(no1)/total_rec,by="vehID"]
  summary_rec<-summary_rec[reception==1,]
}


summary_pla<-merge(summary_pla,sum_speed,by="vehID")
summary_pla<-merge(summary_pla,sum_frn, by="vehID")
summary_pla<-merge(summary_pla,summary_fol, by="vehID")
summary_pla<-merge(summary_pla,summary_rec, by="vehID")

summary_pla<-summary_pla[,.(vehID,avg_front_gap,stdev_front_gap,travel_time,time_platoon,per_time_leader,per_time_follower,per_rec,avg_speed,std_speed,avg_acc,std_acc)]

summary_pla<-data.frame(summary_pla)


summary_pla<-merge(summary_pla,LDV_wide,all=TRUE,by="vehID")
summary_pla$Scenario<-paste("Scenario",no,sep="_")


summary_pla<-summary_pla[summary_pla$travel_time>1000,]


final_data<-rbind(final_data,summary_pla)

rm("summary_pla")
rm("summary_fol")
rm("summary_rec")
rm("sum_speed")
rm("sum_frn")
rm("vehicle_record")

	}

	#input the list of sen
	list_sen<-fread("List_of_scenarios.csv",header=T)
	final_data<-merge(final_data,list_sen, by.x="Scenario")

	#file="C:/Users/a-bibeka/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results/sum_reception.csv"
	file="/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results/sum_reception.csv"
	write.table(final_data,file,sep=",",append = F,row.names=FALSE)





#write.csv(LDV_wide, file=paste("/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results/Summary_",fi_name,".csv",sep=""),row.names=FALSE)	
	



#testing the opmode distribution
#write.table(HG_out,file="opmode_with_red.csv",sep=',',append=F, row.names = F)






