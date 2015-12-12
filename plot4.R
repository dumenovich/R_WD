
#read data 
w<-read.table(file = "household_power_consumption.txt",header = TRUE, sep = ";", stringsAsFactors = FALSE)
w_clean<-w[complete.cases(w),]
#convert data/time
w_clean$Time<-strptime(paste(w_clean$Date,w_clean$Time),format = "%d/%m/%Y %H:%M:%S")
#select range
p1<-strptime("01/02/2007 00:00:00",format = "%d/%m/%Y %H:%M:%S")
p2<-strptime("02/02/2007 23:59:59",format = "%d/%m/%Y %H:%M:%S")
q<-subset(w_clean, w_clean$Time >= p1 & w_clean$Time <= p2)

Sys.setlocale("LC_ALL","English")

q$Global_active_power<-as.numeric(q$Global_active_power)
q$Global_reactive_power<-as.numeric(q$Global_reactive_power)
q$Sub_metering_1<-as.numeric(q$Sub_metering_1)
q$Sub_metering_2<-as.numeric(q$Sub_metering_2)
q$Voltage<-as.numeric(q$Voltage)

#4
layout(matrix(1:4, ncol = 2))
with(q,plot(q$Time,q$Global_active_power,type="l",ylab = "Global Active Power (kilowatts)", xlab = ""))

plot(q$Time,q$Sub_metering_1,type="l",xlab = "",ylab = "Energy sub metering")
lines(x= q$Time,q$Sub_metering_2, col="red")
lines(x= q$Time,q$Sub_metering_3, col="blue")
legend("topright",legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"),lty=1,cex =0.7,bty = "n",y.intersp = 0.6,x.intersp = 0.5)

with(q,plot(q$Time,q$Voltage, type="l", xlab="datetime", ylab = "Voltage"))

with(q,plot(q$Time,q$Global_reactive_power, type="l", xlab="datetime", ylab = "Global_reactive_power"))

dev.copy(png,"plot4.png", width=480, height=480)

dev.off()