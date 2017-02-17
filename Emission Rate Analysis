# Script for processing the processed emission table
#File Name: platoon_len
#Created by: Apoorba Bibeka
#Creation date: 16 Feb 2016
#Purpose:To get final results from processed data 
#Last executed:
Sys.time()

#1 Housekeeping
ls()
rm(list=ls())
ls()



#Load lattice package to plot graphs 
#install.packages("lattice")
library(lattice)
#install.packages("latticeExtra")
library(latticeExtra)

#Set the current directory to the directory where all the data files are kept
getwd()
setwd("C:/Users/a-bibeka/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results")

setwd("/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results")

#Load data.table package for fread to work
library(data.table)

file1= "sum_reception.csv"

#fread create data table
options(datatable.fread.datatable=TRUE)

#data_emis is a data table 
data_emis<-fread(file1,header=TRUE)

#structure of data_semis
str(data_emis)



fileB="Base_case.csv"

#base is a data table 
base<-fread(fileB,header=TRUE)

#structure of data_semis
str(base)

base[,Lane_change_mode:=NULL]
base[,Cacc_gap_dist:=NULL]
base[,min_time_follow:=NULL]
base[,Transmission_power:=NULL]
base[,Scenario:=NULL]
base[,travel_time:=NULL]
base[,no:=NULL]
base[,status:=NULL]

setnames(base,"Avg_em.Total.Gaseous.Hydrocarbons","Base_Hydrocarbon")
setnames(base,"Avg_em.Carbon.Monoxide..CO.","Base_CO")
setnames(base,"Avg_em.Oxides.of.Nitrogen..NOx.","Base_NOx")
setnames(base,"Avg_em.Methane..CH4.","Base_CH4")
setnames(base,"Avg_em.Atmospheric.CO2","Base_CO2")
setnames(base,"avg_speed","base_avg_speed")
setnames(base,"std_speed","base_std_speed")
setnames(base,"avg_acc","base_avg_acc")
setnames(base,"std_acc","base_std_acc")

setnames(data_emis,"Avg_em.Total Gaseous Hydrocarbons","T.Hydrocarbon")
setnames(data_emis,"Avg_em.Carbon Monoxide (CO)","T.CO")
setnames(data_emis,"Avg_em.Oxides of Nitrogen (NOx)","T.NOx")
setnames(data_emis,"Avg_em.Methane (CH4)","T.CH4")
setnames(data_emis,"Avg_em.Atmospheric CO2","T.CO2")


data_emis<-merge(data_emis,base,by=c("mpr","Volume","vehID"))

data_emis[,per_red_CO2:=((T.CO2-Base_CO2)*100/Base_CO2)]
data_emis[,per_red_CO:=((T.CO-Base_CO)*100/Base_CO)]
data_emis[,per_red_NOx:=((T.NOx-Base_NOx)*100/Base_NOx)]
data_emis[,per_red_Hydrocarbon:=((T.Hydrocarbon-Base_Hydrocarbon)*100/Base_Hydrocarbon)]
data_emis[,per_red_CH4:=((T.CH4-Base_CH4)*100/Base_CH4)]

#str(data_emis)

data_emis<-subset(data_emis,,c("vehID","Volume","mpr","avg_front_gap","Lane_change_mode",
                               "Cacc_gap_dist","Transmission_power","Max_plat_len","stdev_front_gap",
                               "travel_time","time_platoon","per_time_follower","avg_speed","base_avg_speed","std_speed","base_std_speed","avg_acc","base_avg_acc","std_acc","base_std_acc",
                               "per_red_CO2","per_red_CO","per_red_NOx","per_red_Hydrocarbon",
                               "per_red_CH4","T.CO2","Base_CO2","T.CO","Base_CO","T.NOx","Base_NOx","T.CH4","Base_CH4","T.Hydrocarbon","Base_Hydrocarbon"))



x= data_emis
file="Final_HGV.csv"
write.table(x, file,sep=",", append = F,row.names=FALSE)




file="Final_HGV.csv"

#base is a data table 
data_emis<-fread(file,header=TRUE)


data_emis$Cacc_gap_dist<-as.factor(data_emis$Cacc_gap_dist)
levels(data_emis$Cacc_gap_dist)<-c("Aggresive", "Conservative")


data_emis$Lane_change_mode<-as.factor(data_emis$Lane_change_mode)
levels(data_emis$Lane_change_mode)[]<-c("CACC Left Lane", "Free Lane Selection")

data_emis$Volume<-as.factor(data_emis$Volume)







#********************************************************************************************************************************************************
#Graphs for Emissions 
#**********************************************************************************************************************************************************


# Function for CACC Gap Distribution 
plot_gap<-function(y,m,yl){
  bwplot(y~factor(Cacc_gap_dist)|factor(Lane_change_mode)+ factor(paste(mpr," %CV",sep=""))+factor(Volume),data = data_emis,
         main=m, xlab="CACC Gap Distribution",ylab=yl,
         layout=c(8,2),scales=list(cex=1,x=list(rot=45)),par.strip.text=list(cex=0.5))
}
#Function for Transmission power plots 
plot_trans<-function(y,m,yl){
  bwplot(y~factor(Transmission_power)|factor(Lane_change_mode)+ factor(paste(mpr," %CV",sep=""))+factor(Volume),data = data_emis,
         main=m, xlab="Transmission Power",ylab=yl,layout=c(8,2),par.strip.text=list(cex=0.5))
}
#Function for Scatter plot of CO2 Vs AVG Front Gap at different factor of mpr
plot_avg<-function(y,m,yl){
  xyplot(y~avg_front_gap|factor(paste(mpr," %CV",sep=""))+factor(Volume),data=data_emis,main=m,xlab="Average Front Gap (ft)",ylab=yl,layout=c(4,2))
}
#Function for Scatter plot of CO2 Vs Std Dev Front Gap at different factor of mpr
plot_std<-function(y,m,yl){
  xyplot(y~stdev_front_gap|factor(paste(mpr," %CV",sep=""))+factor(Volume),data=data_emis,main=m,xlab="St.Dev Front Gap (ft)",ylab=yl,layout=c(4,2))
}
#Functon for Scatter plot of CO2 Vs percent time spent as follower at different factor of mpr
plot_rec<-function(y,m,yl){
  xyplot(y~per_time_follower|factor(paste(mpr," %CV",sep=""))+factor(Volume),data=data_emis,main=m,xlab="Percent Time Spent as Follower",ylab=yl,layout=c(4,2))
}
#Functon for Scatter plot of CO2 Vs avg speed at different factor of mpr
plot_spd<-function(y,m,yl){
  xyplot(y~avg_speed|factor(paste(mpr," %CV",sep=""))+factor(Volume),data=data_emis,main=m,xlab="Avg Speed",ylab=yl,layout=c(4,2))
}

#Function for lane change dist 
Plot_lane<-function(y,m,yl){
  bwplot(y~factor(Lane_change_mode)|factor(paste(mpr," %CV",sep=""))+factor(Volume),data=data_emis,group=`Lane_change_mode`,main=m, 
         xlab= "Lane Change Mode",ylab=yl,layout=c(4,2),scales=list(cex=1,x=list(rot=45)))
}

#Open a pdf file to store the histograms
pdf("r_graphs.pdf")

# CACC Gap Distribution plots
plot_gap(data_emis[,per_red_CO2],"Percent Change in CO2 Vs CACC Gap Distribution ","Percent Change in CO2 (%)")
plot_trans(data_emis[,`per_red_CO2`],"Percent Change in CO2 Vs Transmission Power","Percent Change in CO2 (%)")
Plot_lane(data_emis[,`per_red_CO2`],"Percent Change in CO2 Vs Lane Change Mode","Percent Change in CO2 (%)")
plot_avg(data_emis[,`per_red_CO2`],"Percent Change in CO2 Vs  Average Front Gap","Percent Change in CO2 (%)")
plot_std(data_emis[,`per_red_CO2`],"Percent Change in CO2 Vs  St.Dev Front Gap","Percent Change in CO2 (%)")
plot_rec(data_emis[Max_plat_len==2,`per_red_CO2`],"Percent Change in CO2 Vs Percent Time Spent as Follower","Percent Change in CO2 (%)")



# CACC Gap Distribution plots
plot_gap(data_emis[,per_red_CO],"Percent Change in CO Vs CACC Gap Distribution ","Percent Change in CO (%)")
plot_trans(data_emis[,`per_red_CO`],"Percent Change in CO Vs Transmission Power","Percent Change in CO (%)")
Plot_lane(data_emis[,`per_red_CO`],"Percent Change in CO Vs Lane Change Mode","Percent Change in CO (%)")
plot_avg(data_emis[,`per_red_CO`],"Percent Change in CO Vs  Average Front Gap","Percent Change in CO (%)")
plot_std(data_emis[,`per_red_CO`],"Percent Change in CO Vs   St.Dev Front Gap","Percent Change in CO (%)")
plot_rec(data_emis[,`per_red_CO`],"Percent Change in CO Vs Percent Time Spent as Follower","Percent Change in CO (%)")



# CACC Gap Distribution plots
plot_gap(data_emis[,per_red_NOx],"Percent Change in NOx Vs CACC Gap Distribution ","Percent Change in NOx (%)")
plot_trans(data_emis[,`per_red_NOx`],"Percent Change in NOx Vs Transmission Power","Percent Change in NOx (%)")
Plot_lane(data_emis[,`per_red_NOx`],"Percent Change in NOx Vs Lane Change Mode","Percent Change in NOx (%)")
plot_avg(data_emis[,`per_red_NOx`],"Percent Change in NOx Vs  Average Front Gap","Percent Change in NOx (%)")
plot_std(data_emis[,`per_red_NOx`],"Percent Change in NOx Vs   St.Dev Front Gap","Percent Change in NOx (%)")
plot_rec(data_emis[,`per_red_NOx`],"Percent Change in NOx Vs Percent Time Spent as Follower","Percent Change in NOx (%)")

# CACC Gap Distribution plots
plot_gap(data_emis[,per_red_Hydrocarbon],"Percent Change in Hydrocarbon Vs CACC Gap Distribution ","Percent Change in Hydrocarbon (%)")
plot_trans(data_emis[,`per_red_Hydrocarbon`],"Percent Change in Hydrocarbon Vs Transmission Power","Percent Change in Hydrocarbon (%)")
Plot_lane(data_emis[,`per_red_Hydrocarbon`],"Percent Change in Hydrocarbon Vs Lane Change Mode","Percent Change in Hydrocarbon (%)")
plot_avg(data_emis[,`per_red_Hydrocarbon`],"Percent Change in Hydrocarbon Vs  Average Front Gap","Percent Change in Hydrocarbon (%)")
plot_std(data_emis[,`per_red_Hydrocarbon`],"Percent Change in Hydrocarbon Vs   St.Dev Front Gap","Percent Change in Hydrocarbon (%)")
plot_rec(data_emis[,`per_red_Hydrocarbon`],"Percent Change in Hydrocarbon Vs Percent Time Spent as Follower","Percent Change in Hydrocarbon (%)")




plot_spd(data_emis[,`per_red_CO2`],"Percent Change in CO2 Vs Average Speed","Percent Change in CO2 (%)")
plot_spd(data_emis[,`per_red_CO`],"Percent Change in CO Vs Average Speed","Percent Change in CO (%)")
plot_spd(data_emis[,`per_red_NOx`],"Percent Change in NOx Vs Average Speed","Percent Change in NOx (%)")
plot_spd(data_emis[,`per_red_Hydrocarbon`],"Percent Change in Hydrocarbon Vs Average Speed","Percent Change in Hydrocarbon (%)")



graphics.off()


da<-data_emis
da[,Avg_Change_CO2:=mean(per_red_CO2),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]
da[,Avg_Change_CO:=mean(per_red_CO),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]
da[,Avg_Change_NOx:=mean(per_red_NOx),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]
da[,Avg_Change_Hydrocarbon:=mean(per_red_Hydrocarbon),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]

da[,Std_Change_CO2:=sd(per_red_CO2),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]
da[,Std_Change_CO:=sd(per_red_CO),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]
da[,Std_Change_NOx:=sd(per_red_NOx),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]
da[,Std_Change_Hydrocarbon:=sd(per_red_Hydrocarbon),by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist")]

da<-unique(da,by=c("Volume","mpr","Lane_change_mode","Transmission_power","Cacc_gap_dist"))

da<-da[,.(Volume,mpr,Lane_change_mode,Transmission_power,
          Cacc_gap_dist,Avg_Change_CO2,Std_Change_CO2,Avg_Change_CO,Std_Change_CO,
          Avg_Change_NOx,Std_Change_NOx,Avg_Change_Hydrocarbon
          ,Std_Change_Hydrocarbon)]

write.table(da, "Emission_Final_Summary.csv",sep=",", append = F,row.names=FALSE)

#=================================================================================================================================================
#Graphs for platoon len 
#=================================================================================================================================================

pdf("platoon.pdf")

file2="Pla_len_summary.csv"
data_pla_len<-fread(file2,header=TRUE)


#data_pla_len$Cacc_gap_dist<-as.factor(data_pla_len$Cacc_gap_dist)
#levels(data_pla_len$Cacc_gap_dist)[2]<-"Conservative"


data_pla_len$Lane_change_mode<-as.factor(data_pla_len$Lane_change_mode)
levels(data_pla_len$Lane_change_mode)[]<-c("CACC Left Lane", "Free Lane Selection")


str(data_pla_len)
# Platoon Length   Distribution W.r.t to trnansmission power
histogram(~`Mean`|factor(Transmission_power),data = data_pla_len,strip=strip.custom(factor.levels=c("Transmission Power 100","Transmission Power 250")),
          main="Platoon Length Distribution (Transmission Power)", xlab="Mean Platoon Length",ylab="Percent",layout=c(1,2))

# Platoon Length Distribution W.r.t to Lane change mode
histogram(~`Mean`|factor(Lane_change_mode),data = data_pla_len,
          main="Platoon Length Distribution (Lane Change Mode)", xlab="Mean Platoon Length",ylab="Percent",layout=c(1,2))


# Platoon Length Distribution W.r.t to Lane change mode
histogram(~`Mean`|factor(mpr),data = data_pla_len,strip=strip.custom(factor.levels=c("10%","30%","50%","70%")),
          main="Platoon Length Distribution (Market Penetration Rate)", xlab="Mean Platoon Length",ylab="Percent",layout=c(1,4))

#Mean platoon size vs Mean number of platoons 
xyplot(`Mean`~(avg_pla_no/25.4)|factor(Lane_change_mode)+factor(Transmission_power),data=data_pla_len,main="Mean Platoon Length Vs Mean No of Platoon",xlab="Mean Number of Platoon(Platoons/mile)",ylab="Mean Platoon Length",layout=c(2,2))

graphics.off()

#data_pla_len[,Quartile_1N:=mean(Quartile_1),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]
data_pla_len[,MedianN:=mean(Median),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]
#data_pla_len[,Quartile_3N:=mean(Quartile_3),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]
data_pla_len[,`85th Percentile Platoon Len`:=mean(Percentile_85),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]
data_pla_len[,MeanN:=mean(Mean),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]
data_pla_len[,StdevN:=mean(Stdev),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]
data_pla_len[,avg_pla_noN:=mean(avg_pla_no),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]
data_pla_len[,std_pla_noN:=mean(std_pla_no),by=c("Volume","mpr","Lane_change_mode","Transmission_power")]

data_pla_len<-unique(data_pla_len,by=c("Volume","mpr","Lane_change_mode","Transmission_power"))

data_pla_len<-data_pla_len[,.(mpr,Volume,Lane_change_mode,Transmission_power,MedianN,`85th Percentile Platoon Len`,MeanN,StdevN,avg_pla_noN,std_pla_noN)]


data_pla_len<-data_pla_len[order(Volume,mpr,Lane_change_mode,Transmission_power)]
setnames(data_pla_len,c("mpr","Lane_change_mode","Transmission_power","MedianN",
                       "MeanN","StdevN","avg_pla_noN","std_pla_noN"),
         c("% CV", "Lane Setting","Trans Power","Median Platoon Len",
            "Mean Platoon Len","St.Dev Platoon Len","Avg No of Platoon","St.Dev No of Platoon"))

write.table(data_pla_len, "Fin_platoon_len.csv",sep=",", append = F,row.names=FALSE)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Graphs for flow rate 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

file3="Flow_rate_summary.csv"
data_flow<-fread(file3,header=TRUE)

data_flow<-data_flow[min_time_follow!="NA",]

data_fl_red<-data_flow[Lane==123]
data_fl_red<-data_fl_red[,mean_flow:=mean(flow),by=c("Volume","mpr")]
data_fl_red<-data_fl_red[,Q85_flow:=quantile(flow,.85),by=c("Volume","mpr")]
data_fl_red<-unique(data_fl_red,by=c("Volume","mpr"))

pdf("Flow.pdf")

bwplot(flow~factor(Cacc_gap_dist)|factor(Lane_change_mode)+ factor(paste(mpr," %CV",sep=""))+factor(Volume),data = data_flow,
       scales=list(cex=1,x=list(rot=45)),main="Flow Rate Vs CACC Gap Distribution",xlab="CACC Gap Distribution",ylab="Flow Rate(veh/hr)")


xyplot(mean_flow+Q85_flow~factor(mpr)|factor(paste("Volume =",Volume,"Veh/Hr",sep=" ")),data=data_fl_red
       ,col=c("black","black"),pch=c(2,3),
       key = list(text=list(c("Mean Flow Rate","85th Percentile Flow Rate")),points=list(col=c("black","black"),pch=c(2,3)))
       , xlab="% CV", ylab="Flow Rate(Veh/Hr)",main="Flow Rate Vs % CV")

graphics.off()


da_f<-data_flow

da_f[,flow1:=quantile(flow,.85),by=c("Volume","mpr","Lane_change_mode","Lane")]
da_f<-da_f[!(Lane_change_mode=="Free Lane Selection"&(Lane==1|Lane==3)),]
da_f<-unique(da_f,by=c("Volume","mpr","Lane_change_mode","Lane"))

da_f[Lane==123,`Lane Gr`:="All Lanes"]
da_f[Lane==1,`Lane Gr`:="Non-Left Lanes"]
da_f[Lane==3,`Lane Gr`:="Left Lane"]
da_f<-da_f[,.(Volume,mpr,Lane_change_mode,`Lane Gr`,flow1)]
da_f<-da_f[order(Volume,mpr,Lane_change_mode)]
setnames(da_f,c("mpr","Lane_change_mode","flow1"),c("% CV", "Lane Setting","85th Percentile Flow Rate"))

write.table(da_f, "flow_fin_table.csv",sep=",", append = F,row.names=FALSE)


data_fl_red<-data_flow
data_fl_red<-data_fl_red[Lane==123]
write.table(data_fl_red, "flow_jmp_table.csv",sep=",", append = F,row.names=FALSE)

sink("fow_rate.log",append=FALSE ,type=c("output","message"))

results=lm(Q85_flow~+factor(`Lane_change_mode`)+factor(Cacc_gap_dist)+factor(mpr)+
             factor(Transmission_power)+factor(Volume)+factor(Volume)*factor(mpr),data=data_fl_red)
summary(results)

results=lm(Q85_flow~+factor(`Lane_change_mode`)+,da ta=data_fl_red)
summary(results)

results=lm(Q85_flow~+factor(`Cacc_gap_dist`),data=data_fl_red)
summary(results)

results=lm(Q85_flow~+factor(`Transmission_power`),data=data_fl_red)
summary(results)

results=lm(Q85_flow~+factor(`mpr`)+factor(Volume)+factor(Volume)*factor(mpr),data=data_fl_red)
summary(results)

results
summary(results)

sink()   
sink(file=NULL)



#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
#Summary for Speed and acceleration of data collection point 
#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
file4="speed_dataCol.csv"
data_spd<-fread(file4,header=TRUE)



data_spd[Lane_change_mode=="",Lane_change_mode:="VISSIM Default"]

  bwplot(avg_speed~factor(`type`)|factor(Lane_change_mode),data = data_spd,
         main="Average Speed Vs Lane Change Mode", xlab="Vehicle Type",ylab="Average Speed(mph)",layout=c(3,1))


  data_spd<-data_spd[min_time_follow!=""]
  data_spd[,avg_speed:=mean(na.omit(avg_speed)),by=c("mpr","Volume","type","Lane","Lane_change_mode")]
  data_spd[,`85th Percentile Speed`:=mean(na.omit(speed_Q85)),by=c("mpr","Volume","type","Lane","Lane_change_mode")]
  data_spd[,avg_acc:=mean(na.omit(avg_acc)),by=c("mpr","Volume","type","Lane","Lane_change_mode")]
  data_spd[,`85th Percentile Acc`:=mean(na.omit(acc_Q85)),by=c("mpr","Volume","type","Lane","Lane_change_mode")]
  data_spd[,std_speed:=mean(na.omit(std_speed)),by=c("mpr","Volume","type","Lane","Lane_change_mode")]
  data_spd[,std_acc:=mean(na.omit(std_acc)),by=c("mpr","Volume","type","Lane","Lane_change_mode")]

  data_spd[Lane_change_mode=="Free Lane Selection",avg_speed:=mean(na.omit(avg_speed)),by=c("mpr","Volume","type")]
  data_spd[Lane_change_mode=="Free Lane Selection",`85th Percentile Speed`:=mean(na.omit(speed_Q85)),by=c("mpr","Volume","type")]
  data_spd[Lane_change_mode=="Free Lane Selection",avg_acc:=mean(na.omit(avg_acc)),by=c("mpr","Volume","type")]
  data_spd[Lane_change_mode=="Free Lane Selection",`85th Percentile Acc`:=mean(na.omit(acc_Q85)),by=c("mpr","Volume","type")]
  data_spd[Lane_change_mode=="Free Lane Selection",std_speed:=mean(na.omit(std_speed)),by=c("mpr","Volume","type")]
  data_spd[Lane_change_mode=="Free Lane Selection",std_acc:=mean(na.omit(std_acc)),by=c("mpr","Volume","type")]
  
  data_spd[Lane_change_mode=="Free Lane Selection",Lane:=0]
  data_spd<-unique(data_spd,by=c("mpr","Volume","Lane_change_mode","Lane","type"))
  data_spd[Lane==3&Lane_change_mode=="CACC Left Lane",`Lane Gr`:="Left"]
  data_spd[Lane==1&Lane_change_mode=="CACC Left Lane",`Lane Gr`:="Non-Left Lanes"]
  data_spd[Lane_change_mode=="Free Lane Selection"&Lane==0,`Lane Gr`:="All Lanes"]
  data_spd<-data_spd[,.(mpr,Volume,Lane_change_mode,`Lane Gr`,type,avg_speed,`85th Percentile Speed`,
                        std_speed,avg_acc,`85th Percentile Acc`,std_acc)]
  
  data_spd<-data_spd[order(Volume,mpr,Lane_change_mode,`Lane Gr`,type)]
  
  setnames(data_spd,c("mpr","Volume","Lane_change_mode","avg_speed","std_speed","avg_acc","std_acc","type"),c("% CV", "Volume","Lane Setting",
                            "Avg Speed", "St.Dev Speed","Avg Acc", "St.Dev Acc","Vehicle Type"))

 
  write.table(data_spd, "speed_fin_table.csv",sep=",", append = F,row.names=FALSE)
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Regression
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

sink("test.log",append=FALSE ,type=c("output","message"))

#pairwise.t.test(data_emis$per_red_CO2,c(data_emis$mpr,data_emis$Volume),p.adjust="none")


results=lm(`per_red_CO2`~avg_front_gap+stdev_front_gap+per_time_follower+factor(Volume)+factor(Lane_change_mode)+factor(Transmission_power)+factor(mpr),data=data_emis)
results
summary(results)

pdf("a.pdf")
plot(fitted(results),residuals(results))
qqnorm(residuals(results))
graphics.off()
sink()   
sink(file=NULL)
