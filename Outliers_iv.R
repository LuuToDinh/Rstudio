library(ggplot2)
library(readr)
library(lubridate)

setwd("D:/BTL")
par=read.csv(file="12-04_2021.csv",header = TRUE)#nhap file

chil=subset(par, par$Humi>-1 , c(2,3,6,7,2,3))
#loc bo NA, và chuyen cot 2 3 6 7 2 vào chil

chil$date.1=as.Date(chil$date.1) 
#chuye chuoi sang thoi gian không có gio, phút, giây
chil$date=as.POSIXct(chil$date)
#chuyen chuoi sang thoi gian có gio, phút, giây

chil$day=strftime(chil$date.1,"%d") #tao cot ngày tu cot date

chil$day = as.numeric(chil$day)#chuyen kieu time sang numeric

day5=subset(chil,chil$day==5,c(1,2,3,4,6,7))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 5

day7=subset(chil,chil$day==7,c(1,2,3,4,6,7))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 7

day9=subset(chil,chil$day==9,c(1,2,3,4,6,7))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 9

day10=subset(chil,chil$day==10,c(1,2,3,4,6,7))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 10

chil$deviceid = as.numeric(as.factor(chil$deviceid))
#chuyen sang kieu numeric de de phân loai

################################################$$$$$
#devices
dvthree = subset(chil,chil$deviceid==3,)
dvone = subset(chil,chil$deviceid==1,)
dvtwo = subset(chil,chil$deviceid==2,)
############################################

###############################################$$$$$$$$$$$$
result=quantile(dvone$Humi, na.rm = FALSE)
#tim trung vi

ulimit= result[4] + 1.5*(result[4] - result[2])
#can tren

llimit= result[2] - 1.5*(result[4] - result[2])
#can duoi

result= subset(dvone, Humi > ulimit | Humi < llimit)
#khoang outliers
###############################################

result$date=hour(result$date) #chuyen thoi gian sang gio de ve do thi

hours=c(1:24)
hours=as.factor(hours)

###################################################$$$$$$$$$$$
counts=c(nrow(subset(result,date==0)),nrow(subset(result,date==1)), nrow(subset(result,date==2)),
         nrow(subset(result,date==3)),nrow(subset(result,date==4)),nrow(subset(result,date==5)),
         nrow(subset(result,date==6)),nrow(subset(result,date==7)),nrow(subset(result,date==8)),
         nrow(subset(result,date==9)),nrow(subset(result,date==10)),nrow(subset(result,date==11)),
         nrow(subset(result,date==12)),
         nrow(subset(result,date==13)),nrow(subset(result,date==14)),nrow(subset(result,date==15)),
         nrow(subset(result,date==16)),nrow(subset(result,date==17)),nrow(subset(result,date==18)),
         nrow(subset(result,date==19)),nrow(subset(result,date==20)),nrow(subset(result,date==21)),
         nrow(subset(result,date==22)),nrow(subset(result,date==23)))
###############################################

chart=data.frame(hours,counts)#gop thanh 1 dataframe

ggplot(data=chart,aes(x= hours,y=counts)) +
  geom_histogram(stat='identity', fill= 'blue',color = 'green')+
  labs(x='Time',y='Outliers',title= 'Humidity of Outliers of device SS0866971111458415, month 12th')+ 
  geom_text(aes(label = counts), vjust = -0.25)+
  theme_bw()
#bieu do the hien nhiet do va do am outliers (thay các ngày tuong ung)