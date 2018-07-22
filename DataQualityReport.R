setwd("G:\\Capstone")
getwd()
data<-read.csv(file.choose(),na.strings = "","NA"," ")
dim(data)

columns<-colnames(data)
columns<-as.data.frame(columns)
names(columns)<-"VariableName"

v<-list()
for(i in colnames(data))
    v[i]<-class(data[,i])
v<-as.data.frame(v)
v<-t(v)
v<-as.data.frame(v)
names(v)<-"DataType"

li<-rep(nrow(data),ncol(data))
li<-as.data.frame(li)
names(li)<-"NoOfRecords"

ur<-sapply(data, function(x)length(unique(x)))
ur<-as.data.frame(fg)
names(ur)<-"UniqueRecords"

da<-sapply(data,function(y)sum(! is.na(y)))
da<-as.data.frame(da)
names(da)<-"DataAvailable"
dataframe<-data.frame(columns,v,li,ur,da)

dataframe$AvailablePercent<-dataframe$DataAvailable/dataframe$NoOfRecords

dp<-sapply(data, function(y)sum(is.na(y)))
dp<-as.data.frame(dp)
names(dp)<-"Missing"
dataframe<-cbind(dataframe,dp)

dataframe$MissingPercent<-dataframe$Missing/dataframe$NoOfRecords


ds<-sapply(data, function(y)ifelse(class(y)=="factor","NA",min(y,na.rm=TRUE)))
ds<-as.data.frame(ds)
names(ds)<-"Minimum"
dataframe<-cbind(dataframe,ds)

dm<-sapply(data, function(y)ifelse(class(y)=="factor","NA",max(y,na.rm=TRUE)))
dm<-as.data.frame(dm)
names(dm)<-"Maximum"
dataframe<-cbind(dataframe,dm)

dme<-sapply(data, function(y)ifelse(class(y)=="factor","NA",mean(y,na.rm=TRUE)))
dme<-as.data.frame(dme)
names(dme)<-"Mean"
dataframe<-cbind(dataframe,dme)

for(i in c(5,10,25,50,75,90,95)){
  d5<-sapply(data, function(y)ifelse(class(y)=="factor"|class(y)=="character","NA",quantile(y,probs=i/100,na.rm=TRUE)))
  d5<-as.data.frame(d5)
  names(d5)<-paste(i,"th Percentile")
  dataframe<-cbind(dataframe,d5)
}


write.csv(dataframe,file="Capstone_Data_Quality_Report.csv",na="NA",row.names = FALSE)




