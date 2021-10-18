library(ggplot2)
library(readr)

par=read.csv(file="11-03_2021.csv",header = TRUE)#nhap file

chil=subset(par,par$Temp>-1,c(2,3,6,7,2))
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

ggplot(data = day10, aes(x= as.POSIXct(date), 
y=Temp, colour=deviceid))+geom_line()+labs(title = "Temp Day 10",
x = "Time", y = "Humi")
#bieu do day5 trên tung thiet bi