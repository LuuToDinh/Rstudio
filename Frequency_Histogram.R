library(ggplot2)
library(readr)
library(dplyr)


setwd("D:/BTL")

par=read.csv(file="10-02_2021.csv",header = TRUE)#nhap file

chil=subset(par,par$Temp>0&par$Humi>0,c(2,3,6,7,2))
#loc bo NA, và chuyen cot 2 3 6 7 2 vào chil

chil$date.1=as.Date(chil$date.1) 
#chuye chuoi sang thoi gian không có gio, phút, giây
chil$date=as.POSIXct(chil$date)
#chuyen chuoi sang thoi gian có gio, phút, giây

chil$day=strftime(chil$date.1,"%d") #tao cot ngày tu cot date

chil$day = as.numeric(chil$day)#chuyen kieu time sang numeric

day5=subset(chil,chil$day==5,c(1,2,3,4,6))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 5

day7=subset(chil,chil$day==7,c(1,2,3,4,6))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 7

day9=subset(chil,chil$day==9,c(1,2,3,4,6))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 9

day10=subset(chil,chil$day==10,c(1,2,3,4,6))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 10

count = c(nrow(subset(day5,Temp>0 & Humi>0)),nrow(subset(day7,Temp>0 & Humi>0)),
          nrow(subset(day9,Temp>0 & Humi>0)),nrow(subset(day10,Temp>0 & Humi>0)))
#dem sô c???t m???i ngày

dayofmonth=c(  as.Date(day10$date[1]) + 1,as.Date(day9$date[1]) + 1, 
               as.Date(day7$date[1]) + 1 ,as.Date(day5$date[1]) + 1)
#ngày có giò, phút, giây -> không có giò phút giây

dayofmonth = as.factor(as.character(dayofmonth))#chuyen ve factor de de xu ly

caculate= data.frame(count, dayofmonth)
#d???o thành dataframe mói

ggplot(data=caculate,aes(x= dayofmonth, y= count)) +#d???nh dang x và y
  geom_histogram(stat='identity',fill="blue", color = 'green', binwidth = 0.75)+
  geom_text(aes(label = count), vjust = -0.25)+#hien so trên dau moi cot
  labs(x='Time',y='Count',title= 'Frequency Histogram' )