
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

q$Sub_metering_1<-as.numeric(q$Sub_metering_1)
q$Sub_metering_2<-as.numeric(q$Sub_metering_2)

#3
par(mfrow = c(1,1))
plot(q$Time,q$Sub_metering_1,type="l",xlab = "",ylab = "Energy sub metering")
lines(x= q$Time,q$Sub_metering_2, col="red")
lines(x= q$Time,q$Sub_metering_3, col="blue")
legend("topright",legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"),lty=1,cex = 0.7)
dev.copy(png,"plot3.png", width=480, height=480)

dev.off()