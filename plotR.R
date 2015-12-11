
w<-read.table(file = "household_power_consumption.txt",header = TRUE, sep = ";", stringsAsFactors = FALSE)
w_clean<-w[complete.cases(w),]
w_clean$Time<-strptime(paste(w_clean$Date,w_clean$Time),format = "%d/%m/%Y %H:%M:%S")
p1<-strptime("01/02/2007 00:00:00",format = "%d/%m/%Y %H:%M:%S")
p2<-strptime("02/02/2007 23:59:59",format = "%d/%m/%Y %H:%M:%S")
q<-subset(w_clean, w_clean$Time >= p1 & w_clean$Time <= p2)
#q<-subset(w_clean, w_clean$Time >= p1 & w_clean$Time <= p2, select = w_clean$Global_active_power)

#1
q$Global_active_power<-as.numeric(q$Global_active_power)
hist(q$Global_active_power)

#2
with(q,plot(q$Time,q$Global_active_power,type="l"))

#3
q$Sub_metering_1<-as.numeric(q$Sub_metering_1)
q$Sub_metering_2<-as.numeric(q$Sub_metering_2)
plot(q$Time,q$Sub_metering_1,type="l", col="red")
lines(x= q$Time,q$Sub_metering_2)
lines(x= q$Time,q$Sub_metering_3)

#4
layout(matrix(1:4, ncol = 2))
plot(q$Time,q$Sub_metering_1,type="l", col="red")
plot(q$Time,q$Sub_metering_1,type="l", col="red")
plot(q$Time,q$Sub_metering_1,type="l", col="red")
plot(q$Time,q$Sub_metering_1,type="l", col="red")





#z<-read.table(file = "household_power_consumption.txt",header = TRUE, sep = ";", nrows = 10,colClasses = c(rep("factor",2), rep("numeric",7)))
#w<-read.table(file = "household_power_consumption.txt",header = TRUE, sep = ";", stringsAsFactors = FALSE, nrows = 10)
#strptime(paste(z$Date,z$Time),format = "%d/%m/%Y %H:%M:%S")
