library(ggplot2)
library(readr)

par=read.csv(file="10-02_2021.csv",header = TRUE)#nhap file

chil=subset(par,par$Temp>0&par$Humi>0,c(2,3,6,7,2))
#loc bo NA, và chuyen cot 2 3 6 7 2 vào chil

chil$date.1=as.Date(chil$date.1) 
#chuye chuoi sang thoi gian không có gio, phút, giây
chil$date=as.POSIXct(chil$date)
#chuyen chuoi sang thoi gian có gio, phút, giây

chil$day=strftime(chil$date.1,"%d") #tao cot ngày tu cot date

chil$day = as.numeric(chil$day)#chuyen kieu time sang numeric

day5=subset(chil,chil$day==5,c(1,2,3,4,6,2))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 5

day7=subset(chil,chil$day==7,c(1,2,3,4,6,2))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 7

day9=subset(chil,chil$day==9,c(1,2,3,4,6,2))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 9

day10=subset(chil,chil$day==10,c(1,2,3,4,6,2))
#nhap cot 1 2 3 4 6 vào day5, dong thoi loc (chi chon) ngày 10

as.factor(day5$deviceid)
#dùng d??? ki???m tra s??? thi???t b??? khác nhau (b???ng levels)
#T??? levels dã cho ta có: day5 -> 2 thi???t b???
#day 7 -> 2 thi???t b???
#day 9 -> 2 thi???t b???
#day 10 -> 2 thi???t b???

day5$deviceid = as.numeric(as.factor(day5$deviceid))
day7$deviceid = as.numeric(as.factor(day7$deviceid))
day9$deviceid = as.numeric(as.factor(day9$deviceid))
day10$deviceid = as.numeric(as.factor(day10$deviceid))
#chuy???n sang ki???u numeric d??? d??? phân lo???i

#T??? 4 ngày, ta có 8 thi???t b??? -> 8 bi???n

#day 5
dv1day5 = subset(day5,day5$deviceid==1,)
dv2day5 = subset(day5,day5$deviceid==2,)

#day7
dv1day7 = subset(day7,day7$deviceid==1,)
dv2day7 = subset(day7,day7$deviceid==2,)

#day9
dv1day9 = subset(day9,day9$deviceid==1,)
dv2day9 = subset(day9,day9$deviceid==2,)

#day10
dv1day10 = subset(day10,day10$deviceid==1,)
dv2day10 = subset(day10,day10$deviceid==2,)

ggplot(data = dv1day10, aes(x= as.POSIXct(date), 
                            y=Humi, colour=deviceid.1,as.factor(F)))+geom_line()+labs(title = "Humidity of Device 1 day 10",
                                                                                      x = "Time", y = "Humi",col="device 1")+
                            scale_color_manual(values = "#1b98e0")#d???i màu du???ng
#bieu do day5 trên tung thiet bi (tuong ung 8 thiet b??? nhu trên, data)