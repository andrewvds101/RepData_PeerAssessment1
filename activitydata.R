## Set working directory and load necessary packages
setwd("/Users/Andrew/Library/Mobile Documents/com~apple~CloudDocs/Reproducible research/Assignment 1/RepData_PeerAssessment1")
library(dplyr)
library(reshape2)
## Read in data
activity <- read.csv("activity.csv")
activity$date <- as.Date(as.character(activity$date),format = "%Y-%m-%d")
# Aggregate into daily data
steps <- group_by(activity,date) %>% summarise(TotalSteps=sum(steps,na.rm=TRUE))

# Plot histogram of total steps each day
hist(steps$TotalSteps,main = "Total Daily Steps",xlab = "Steps",col = "red")

daily <- group_by(activity,interval) %>% summarise(AvgSteps=mean(steps,na.rm=TRUE))
##plot(daily$interval,daily$TotalSteps,type = "l",main = "Average number of steps across 5 minute intervals",xlab = "5 minute interval",ylab = "Average number of steps")

activity2 <- activity
for(i in 1:length(activity2$steps)) {
        if(is.na(activity2$steps[i])) {
                activity2$steps[i]=daily$AvgSteps[activity2$interval[i]==daily$interval]
        }
}
sum(is.na(activity2$steps))

steps2 <- group_by(activity2,date) %>% summarise(TotalSteps=sum(steps))
hist(steps2$TotalSteps,main = "Total Daily Steps",xlab = "Steps",col = "green")

for(j in 1:length(activity2$steps)) {
        if(weekdays(activity2$date[j])=="Saturday" | weekdays(activity2$date[j])=="Sunday") {
                activity2$dayInd[j] <- "Weekend"
        } else activity2$dayInd[j] <- "Weekday"
}
head(activity2)
activity2$dayInd <- as.factor(activity2$dayInd)
activityWd <- activity2[activity2$dayInd=="Weekday",]
dailyWd <- group_by(activityWd,interval) %>% summarise(AvgSteps=mean(steps))

activityWe <- activity2[activity2$dayInd=="Weekend",]
dailyWe <- group_by(activityWe,interval) %>% summarise(AvgSteps=mean(steps))

activity3 <- merge(dailyWd,dailyWe,by="interval")
names(activity3) <- c("interval","Weekdays","Weekends")

activity4 <- cbind(activity3$interval,melt(activity3[,2:3]))
names(activity4) <- c("interval","dayInd","AvgSteps")
activity4$dayInd <- as.factor(activity4$dayInd)
