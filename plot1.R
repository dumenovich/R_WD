
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

#1
par(mfrow = c(1,1))
hist(q$Global_active_power,xlab = "Global Activity Power (kilowatts)",ylab = "Frequency",main = "Global Active Power", col = "orangered")
dev.copy(png,"plot1.png", width=480, height=480)

dev.off()


