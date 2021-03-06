#Using Data Tables to carry out various operations in R 

#Load data.table package 
library(data.table)

#fread create data table. If this option is false than a data.frame is created
options(datatable.fread.datatable=TRUE)

#Read a file using data table. File has header
x<-fread(fileName,header=TRUE)

#Create a data frame
dt_fr<-data.frame(dt_fr)

#Changing a data frame to data table
dt_tab<-data.table(dt_fr)

#Example of Aggregation using data table 

