library(dplyr)
setwd("G:\\Capstone")
getwd()
data<-read.csv("telecomfinal.csv",na.strings =c("","NA"," "))
dim(data)
colnames(data)
x=which(colnames(data) %in% c("dwlltype","dwllsize","mailordr","occu1","numbcars","retdays","wrkwoman","solflag","proptype","mailresp","cartype","children","div_type","income"))
data<-data[,-c(x)]
summary(data)

data$dummy_crclscod<-as.integer(data$crclscod)
data$dummy_asl_flag<-as.integer(data$asl_flag)
data$dummy_prizm_social_one<-as.integer(data$prizm_social_one)
data$dummy_area<-as.integer(data$area)
data$dummy_refurb_new<-as.integer(data$refurb_new)
data$dummy_hnd_webcap<-as.integer(data$hnd_webcap)
data$dummy_marital<-as.integer(data$marital)
data$dummy_ethnic<-as.integer(data$ethnic)
data$dummy_car_buy<-as.integer(data$car_buy)
data$dummy_csa<-as.integer(data$csa)

y=which(colnames(data) %in% c("crclscod","asl_flag","prizm_social_one","area","refurb_new","hnd_webcap","marital","ethnic","car_buy","csa"))

data_New<-data[,-c(y)]
dim(data_New)

#Outlier Imputation
box<-boxplot(data_New$mou_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$mou_Mean == 12206.75)
data_New$mou_Mean[index]=7667.75

box<-boxplot(data_New$totmrc_Mean)
ins=which(data_New$totmrc_Mean<0)
data_New$totmrc_Mean[ins]<-0

box<-boxplot(data_New$rev_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$rev_Range == 13740.54)
data_New$rev_Range[index]=4585.46

box<-boxplot(data_New$mou_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$mou_Range == 43050)
data_New$mou_Range[index]=14413

box<-boxplot(data_New$change_mou)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$change_mou == 31219.250)
data_New$change_mou[index]=4480.000

box<-boxplot(data_New$drop_blk_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$drop_blk_Mean > 411 & data_New$drop_blk_Mean <412)
data_New$change_mou[index]=350
index=which(data_New$drop_blk_Mean > 489 & data_New$drop_blk_Mean <490)
data_New$change_mou[index]=380

box<-boxplot(data_New$drop_vce_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$drop_vce_Range == 313)
data_New$drop_vce_Range[index]=235

box<-boxplot(data_New$owylis_vce_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$owylis_vce_Range == 699)
data_New$owylis_vce_Range[index]=566

box<-boxplot(data_New$mou_opkv_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$mou_opkv_Range == 3501.49)
data_New$mou_opkv_Range[index]=2850
index=which(data_New$mou_opkv_Range == 4783.67)
data_New$mou_opkv_Range[index]=2930

box<-boxplot(data_New$months)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$totcalls)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$eqpdays)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$custcare_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$custcare_Mean>208 & data_New$custcare_Mean<209)
data_New$mou_opkv_Range[index]=140
index=which(data_New$custcare_Mean>272.9 & data_New$custcare_Mean<273.1)
data_New$mou_opkv_Range[index]=149
index=which(data_New$custcare_Mean>327 & data_New$custcare_Mean<327.4)
data_New$mou_opkv_Range[index]=157
index=which(data_New$custcare_Mean>365 & data_New$custcare_Mean<366)
data_New$mou_opkv_Range[index]=165

box<-boxplot(data_New$callwait_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$iwylis_vce_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$iwylis_vce_Mean==404.00)
data_New$iwylis_vce_Mean[index]=351
index=which(data_New$iwylis_vce_Mean>519.3 & data_New$iwylis_vce_Mean<519.4)
data_New$iwylis_vce_Mean[index]=355

box<-boxplot(data_New$callwait_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$callwait_Range==192)
data_New$callwait_Range[index]=178

box<-boxplot(data_New$iwylis_vce_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)
index=which(data_New$iwylis_vce_Mean==404.00)
data_New$iwylis_vce_Mean[index]=351

index=which(data_New$iwylis_vce_Mean>519.3 & data_New$iwylis_vce_Mean<519.4)
data_New$iwylis_vce_Mean[index]=355

box<-boxplot(data_New$callwait_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$callwait_Range==192)
data_New$callwait_Range[index]=178


box<-boxplot(data_New$ccrndmou_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$ccrndmou_Range==1590)
data_New$ccrndmou_Range[index]=720

box<-boxplot(data_New$adjqty)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)


box<-boxplot(data_New$ovrrev_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$ovrrev_Mean>744 & data_New$ovrrev_Mean<744.1)
data_New$ovrrev_Mean[index]=610

index=which(data_New$ovrrev_Mean>890 & data_New$ovrrev_Mean<890.8)
data_New$ovrrev_Mean[index]=620

index=which(data_New$ovrrev_Mean>896 & data_New$ovrrev_Mean<896.1)
data_New$ovrrev_Mean[index]=630

box<-boxplot(data_New$rev_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$rev_Mean>1223.3 & data_New$rev_Mean<1223.4)
data_New$rev_Mean[index]=1010

index=which(data_New$rev_Mean>3843.2 & data_New$rev_Mean<3843.3)
data_New$rev_Mean[index]=1030

box<-boxplot(data_New$ovrmou_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$ovrmou_Mean==4320.75)
data_New$ovrmou_Mean[index]=3850

box<-boxplot(data_New$comp_vce_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$plcd_vce_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$avg3mou)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$avg3mou==7270)
data_New$avg3mou[index]=6440

index=which(data_New$avg3mou==7716)
data_New$avg3mou[index]=6690

box<-boxplot(data_New$avgmou)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$avgmou==6329.40)
data_New$avgmou[index]=5303.3

index=which(data_New$avgmou==7040.13)
data_New$avgmou[index]=5407.98

box<-boxplot(data_New$avg3qty)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$avg3qty==3261)
data_New$avg3qty[index]=3004

box<-boxplot(data_New$avgqty)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$avg6mou)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$avg6mou==6504)
data_New$avg6mou[index]=5635

index=which(data_New$avg6mou==7217)
data_New$avg6mou[index]=5813

box<-boxplot(data_New$avg6qty)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$age1)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$age2)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$models)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$hnd_price)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$actvsubs)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$uniqsubs)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$forgntvl)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$opk_dat_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$opk_dat_Mean > 245 & data_New$opk_dat_Mean < 246)
data_New$avg6mou[index]=186

index=which(data_New$opk_dat_Mean > 247 & data_New$opk_dat_Mean < 248)
data_New$avg6mou[index]=197

index=which(data_New$opk_dat_Mean > 252 & data_New$opk_dat_Mean < 253)
data_New$avg6mou[index]=208

index=which(data_New$opk_dat_Mean > 278 & data_New$opk_dat_Mean < 279)
data_New$avg6mou[index]=219

index=which(data_New$opk_dat_Mean == 304)
data_New$avg6mou[index]=228

index=which(data_New$opk_dat_Mean == 308)
data_New$avg6mou[index]=232

box<-boxplot(data_New$mtrcycle)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$truck)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$roam_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$roam_Mean == 3685.2000)
data_New$roam_Mean[index]=1150

box<-boxplot(data_New$recv_sms_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$recv_sms_Mean > 517)
data_New$recv_sms_Mean[index]=160

box<-boxplot(data_New$blck_dat_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$blck_dat_Mean > 122 & data_New$blck_dat_Mean < 123)
data_New$blck_dat_Mean[index]=75

index=which(data_New$blck_dat_Mean > 413)
data_New$blck_dat_Mean[index]=81

box<-boxplot(data_New$mou_pead_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$mou_pead_Mean > 618 & data_New$mou_pead_Mean < 619)
data_New$mou_pead_Mean[index]=321.9876

index=which(data_New$mou_pead_Mean > 1036 & data_New$mou_pead_Mean < 1037)
data_New$mou_pead_Mean[index]=334

box<-boxplot(data_New$da_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$da_Mean > 159)
data_New$da_Mean[index]=76

box<-boxplot(data_New$da_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$da_Range ==67.32)
data_New$da_Range[index]=58.43

index=which(data_New$da_Range ==74.25)
data_New$da_Range[index]=59.42

box<-boxplot(data_New$datovr_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$datovr_Mean >242)
data_New$datovr_Mean[index]=161

box<-boxplot(data_New$datovr_Range)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$drop_dat_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$drop_dat_Mean >207)
data_New$drop_dat_Mean[index]=99

box<-boxplot(data_New$drop_vce_Mean)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

box<-boxplot(data_New$adjmou)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$adjmou ==211204)
data_New$adjmou[index]=175465

index=which(data_New$adjmou >232855)
data_New$adjmou[index]=178465

box<-boxplot(data_New$totrev)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$totrev ==19754.85)
data_New$totrev[index]=16079

index=which(data_New$totrev ==27321.50)
data_New$totrev[index]=17134

box<-boxplot(data_New$adjrev)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$adjrev ==19432.46)
data_New$adjrev[index]=16132.78

index=which(data_New$adjrev ==27071.30)
data_New$adjrev[index]=17092.78

box<-boxplot(data_New$avgrev)
ins=which(ntile(box$out,100)==100)
sort(box$out[ins],decreasing = TRUE)

index=which(data_New$avgrev ==902.38)
data_New$avgrev[index]= 751.35

head(data_New)

un<-unique(data_New$dummy_hnd_webcap)

data$hnd_webcap

#Categorical Variables profiling
#Treatment of na values in Categorical variables

datC1<-data_New%>%count(churn,levels=dummy_hnd_webcap)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_hnd_webcap%in%datC1$levels)%>%count(dummy_hnd_webcap))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_hnd_webcap",nrow(datC1))

View(datC1)

index<-which(is.na(data_New$dummy_hnd_webcap))
data_New$dummy_hnd_webcap[index]<-2
unique(data_New$dummy_hnd_webcap)


datC1<-data_New%>%count(churn,levels=dummy_prizm_social_one)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_prizm_social_one%in%datC1$levels)%>%count(dummy_prizm_social_one))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_prizm_social_one",nrow(datC1))

View(datC1)
index<-which(is.na(data_New$dummy_prizm_social_one))
data_New$dummy_prizm_social_one[index]<-4
unique(data_New$dummy_prizm_social_one)

datC1<-data_New%>%count(churn,levels=dummy_area)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_area%in%datC1$levels)%>%count(dummy_area))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_area",nrow(datC1))

View(datC1)
index<-which(is.na(data_New$dummy_area))
data_New$dummy_area[index]<-15
unique(data_New$dummy_area)

datC1<-data_New%>%count(churn,levels=dummy_refurb_new)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_refurb_new%in%datC1$levels)%>%count(dummy_refurb_new))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_refurb_new",nrow(datC1))

View(datC1)
index<-which(is.na(data_New$dummy_refurb_new))
data_New$dummy_refurb_new[index]<-1
unique(data_New$dummy_refurb_new)

datC1<-data_New%>%count(churn,levels=dummy_marital)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_marital%in%datC1$levels)%>%count(dummy_marital))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_marital",nrow(datC1))

View(datC1)
index<-which(is.na(data_New$dummy_marital))
data_New$dummy_marital[index]<-4
unique(data_New$dummy_marital)


datC1<-data_New%>%count(churn,levels=dummy_ethnic)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_ethnic%in%datC1$levels)%>%count(dummy_ethnic))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_ethnic",nrow(datC1))

View(datC1)
index<-which(is.na(data_New$dummy_ethnic))
data_New$dummy_ethnic[index]<-9
unique(data_New$dummy_ethnic)

datC1<-data_New%>%count(churn,levels=dummy_car_buy)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_car_buy%in%datC1$levels)%>%count(dummy_car_buy))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_car_buy",nrow(datC1))

View(datC1)
index<-which(is.na(data_New$dummy_car_buy))
data_New$dummy_car_buy[index]<-1
unique(data_New$dummy_car_buy)

datC1<-data_New%>%count(churn,levels=dummy_csa)%>%filter(churn==1)
datC1$N<-unclass(data_New%>%filter(dummy_csa%in%datC1$levels)%>%count(dummy_csa))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("dummy_csa",nrow(datC1))

View(datC1)
index<-which(is.na(data_New$dummy_csa))
data_New$dummy_csa[index]<-731
unique(data_New$dummy_csa)

#Treatment of na values in continuous variables

index<-which(is.na(data_New$mou_Mean))
data_New1<-data_New[-c(index),]

data_check<-data_New1[,c("mou_Mean","totmrc_Mean","rev_Range","mou_Range","ovrrev_Mean","rev_Mean","ovrmou_Mean","roam_Mean","da_Mean","da_Range","datovr_Mean","datovr_Range")]
summary(data_check)

data_New1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(data_New1%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(data_New1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat45$LessThan<-unclass(data_New1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat45$varname<-rep("change_mou",nrow(dat45))

View(dat45)

index<-which(is.na(data_New1$change_mou))
data_New2<-data_New1[-c(index),]

index<-which(is.na(data_New2$eqpdays))
data_New3<-data_New2[-c(index),]

data_New4<-na.omit(data_New3)

data_New4<-data_New4[,-which(colnames(data_New4)=="Customer_ID")]

data_New4$Completion_Percentage<-data_New4$comp_vce_Mean/data_New4$totcalls
index<-which(is.na(data_New4$Completion_Percentage))
data_New4$Completion_Percentage[index]<-0

#Training/Testing data samples
set.seed(200)
index<-sample(nrow(data_New4),0.80*nrow(data_New4),replace=F)
train<-data_New4[index,]
test<-data_New4[-index,]
dim(test)

######Modeling

#*******************Logistic Regrssion********************

Mod<-glm(churn~.,data=train,family=binomial)
summary(Mod)
Mod<-glm(churn~drop_blk_Mean+months+ovrrev_Mean+avgmou+avg6mou+actvsubs+dummy_crclscod+dummy_area+totmrc_Mean+mou_Range+models+uniqsubs+drop_vce_Mean+dummy_asl_flag+dummy_refurb_new+dummy_ethnic+Completion_Percentage+change_mou+eqpdays+avgqty+age1+hnd_price+dummy_hnd_webcap,data=train,family=binomial)
step(Mod,direction="backward")

pred<-predict(Mod,type="response",newdata=test)
predicted <- Mod$fitted.values
View(predicted)

predbkt<-ifelse(predicted>0.5,1,0)
table(predbkt,train$churn)

library(gplots)
library(ROCR)

#ROC curve analysis
library(ROCR)
pred<-prediction(predicted,train$churn)
View(pred)
perf <- performance(pred,"acc")
View(perf)

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") 
plot(perf,col="red")
abline(0,1, lty = 8, col = "blue")

# Area under the curve
auc<-performance(pred,"auc")
auc

result<-glm(churn ~ months+ovrrev_Mean+avgmou+actvsubs+totmrc_Mean+mou_Range+models+uniqsubs+drop_vce_Mean+dummy_asl_flag+dummy_refurb_new+dummy_ethnic+Completion_Percentage+change_mou+eqpdays+age1+hnd_price,data=train,family=binomial)

summary(result)

confint(result)

# ******************Random Forest*****************
library(randomForest)
library(caret)
rf<-randomForest(as.factor(churn) ~ months + ovrrev_Mean + avgmou + avg6mou + actvsubs + 
                   totmrc_Mean + mou_Range + models + uniqsubs + drop_vce_Mean + 
                   dummy_asl_flag + dummy_refurb_new + dummy_ethnic + Completion_Percentage + 
                   change_mou + eqpdays + age1 + hnd_price,data=train,mtry=5)
print(rf)



#Evaluation of variable importance
importance(rf)
varImpPlot(rf)

#Prediction and Calculation of Performance Metrics
pred1=predict(rf,type = "prob")

library(ROCR)
perf = prediction(pred1[,2], train$churn)

# Area under curve
auc = performance(perf, "auc")
auc

# True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=8,col="gray")

library(car)
vif(rf)
varImpPlot(rf)
#Answer 1
#eqpdays, change_mou,c_percent,mou_Range,avgmou

#******************Kernel SVM*********************************
library(kernlab)
kpca = kpca(~months + ovrrev_Mean + avgmou + avg6mou + actvsubs + 
              totmrc_Mean + mou_Range + models + uniqsubs + drop_vce_Mean + 
              dummy_asl_flag + dummy_refurb_new + dummy_ethnic + Completion_Percentage + 
              change_mou + eqpdays + age1 + hnd_price, data = train, kernel = 'rbfdot', features = 10)
train_pca = as.data.frame(predict(kpca, train))
train_pca$churn = train$churn
test_pca = as.data.frame(predict(kpca, test))
test$churn = test$churn

library(e1071)
classifier = svm(formula = churn ~ months + ovrrev_Mean + avgmou + avg6mou + actvsubs + 
                   totmrc_Mean + mou_Range + models + uniqsubs + drop_vce_Mean + 
                   dummy_asl_flag + dummy_refurb_new + dummy_ethnic + Completion_Percentage + 
                   change_mou + eqpdays + age1 + hnd_price,
                 data = train,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
index=which(colnames(test)=="churn")
y_pred = predict(classifier, newdata = test[,-44])

# Making the Confusion Matrix
cm = table(test[, 44], y_pred)


#Grid Search
library(caret)
classifier = train(form = churn ~ drop_blk_Mean+months+ovrrev_Mean+avgmou+avg6mou+actvsubs+dummy_crclscod+dummy_area+totmrc_Mean+mou_Range+models+uniqsubs+drop_vce_Mean+dummy_asl_flag+dummy_refurb_new+dummy_ethnic+Completion_Percentage+change_mou+eqpdays+avgqty+age1+hnd_price+dummy_hnd_webcap , data = train, method = 'svmRadial')
classifier
classifier$bestTune

#*************************Decision Tree**************************************

install.packages(rattle)
library(rpart)
library("rattle")
library(rpart.plot)
library(RColorBrewer)
classifier = rpart(formula =churn~months+ovrrev_Mean+avgmou+actvsubs+totmrc_Mean+mou_Range+models+uniqsubs+drop_vce_Mean+dummy_asl_flag+dummy_refurb_new+dummy_ethnic+Completion_Percentage+change_mou+eqpdays+age1+hnd_price,
                   data=train)

mod1<-rpart(churn~months+ovrrev_Mean+avgmou+actvsubs+totmrc_Mean+mou_Range+models+uniqsubs+drop_vce_Mean+dummy_asl_flag+dummy_refurb_new+dummy_ethnic+c_percent+change_mou+eqpdays+age1+hnd_price,
           data=train,control=rpart.control(cp=0.002,maxdepth=7),method="class",parms=list(split="gini"))

plot(mod1, margin=0.1, main="Classification Tree for Telecom")
text(mod1, use.n=TRUE, all=TRUE, cex=.7)

fancyRpartPlot(mod1)

printcp(mod1)
plotcp(mod1, minline = TRUE)

mod1<-prune(mod1,cp= 0.035)

fancyRpartPlot(mod1)

#Confusion Matrix
actual<-train$churn
predicted<-predict(mod1,type = "class")

head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

library(caret)
confusionMatrix(predicted,train$churn,positive="1")

#kappa metric
kappa2(data.frame(actual,predicted))

#ROC curve analysis
library(ROCR)
pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)
