library(ggplot2)
library(readr)

setwd("D:/BTL")
par=read.csv(file="12-04_2021.csv",header = TRUE)#nhap file

chil=subset(par, par$Temp>0 & par$Humi>0, c(2,3,6,7,2))
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

setQ1=subset(chil,chil$Temp<quantile(chil$Temp,c(0.5)))
#vùng c???a t???p d??? liêu Q1 c???a ngày 7

setQ3=subset(chil,chil$Temp>quantile(chil$Temp,c(0.5)))
#vùng c???a t???p d??? liêu Q3 c???a ngày 7

valueQ1=quantile(setQ1$Temp,c(0.5))
#t??? vùng dã cho l???c ra giá tr??? trung v??? c???a Q1

valueQ3=quantile(setQ3$Temp,c(0.5))
#t??? vùng dã cho l???c ra giá tr??? trung v??? c???a Q3

outTempday10=subset(chil,chil$Temp <= valueQ1 - 1.5*(valueQ3-valueQ1)
                   & chil$Temp >= valueQ3 + 1.5*(valueQ3-valueQ1),)
#outliers nhi???t d??? c???a ngày 7

ggplot(data=outTempday10,aes(x= as.POSIXct(date),y=Temp)) +
  geom_histogram(color = 'red',stat='identity')+
  labs(x='Time',y='Temp',title= 'Histogram of Tempurature')
#bi???u d??? th??? hi???n nhi???t d??? outlies ngày 7