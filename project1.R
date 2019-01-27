##read in data####
activity <- read.csv("activity.csv", header=T)
summary(activity)
str(activity)
#change date from factor to date
activity$date <- as.Date(activity$date, format= "%Y-%m-%d")
#aggregate activity by date, taking the sum
###question 1 #####
#aggregate total steps per day
daily_steps <- aggregate(steps~date, data = activity, sum, na.rm=TRUE)#removes na

#histogram of total steps each day
hist(daily_steps$steps, 
     main= "Histogram of Total Number of Steps each Day with Mean Value",
     border = "blue",
     col="green",
     breaks= seq(0,25000, by=2500),
     xlim = c(0, 25000),
     xlab = "Total Steps per Day", 
     ylim = c(0, 20))

abline(v=mean(daily_steps$steps),
            col="royalblue",
            lwd=2)
text(x=mean(daily_steps$steps),
     y=20,
     labels = paste("Mean=", 
                    round(mean(daily_steps$steps), 2)),
     pos = 2)
legend(x="topright",
       "Mean",
       col= "royalblue",
       lwd = 2, 
       cex=0.8,
       text.font = 2,
       box.lty = 0)

#histogram showing median
hist(daily_steps$steps, 
     main= "Histogram of Total Number of Steps each Day with Median Value",
     border = "blue",
     col="green",
     breaks= seq(0,25000, by=2500),
     xlim = c(0, 25000),
     xlab = "Total Steps per Day",
     ylim= c(0, 20))
abline(v=median(daily_steps$steps),
            col="red",
            lwd=2)
text(x=median(daily_steps$steps),
     y=20,
     labels = paste("Median=", 
                    round(median(daily_steps$steps), 2)),
     pos = 2)
legend(x="topright",
       "Median",
       col= "red",
       lwd = 2, 
       cex=0.8,
       text.font = 2,
       box.lty = 0)
#median of total steps each day
b <-median(daily_steps$steps, na.rm = T)
#print median results
print(paste("The median total number of steps each day is", b))
#mean of total steps each day
c<-mean(daily_steps$steps, na.rm = T)
#print mean results
print(paste("The mean total number of steps each day is", c))

####question 2####
summary(activity$interval)
str(activity$interval)
library(dplyr)

##without padding/changing interval to a time
avg_steps <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
#plot for 5 min interval average daily steps
plot(row.names(avg_steps), avg_steps, type="l", xlab="5-minute intervals for 24 hours",
     ylab= "Average Number of Steps", main="Average Daily Activity Pattern in 5-minute Intervals", col="blue")

#find max steps (use aggregate to get the full row of data)
ts <-aggregate(steps ~ interval,activity, mean)
max_steps <-ts[which.max(ts$steps),]
print(max_steps)


###question 3: impute missing data####
#Calculate and report total number of missing values
missing_data<-sum(is.na(activity))
print(paste("The number of missing values is", missing_data))
#devise a strategy for filling in missing data
#replace NA's with mean for each 5 min interval in time_series dataframe from ts aggregate of steps
ts <-aggregate(steps ~ interval,activity, mean)
#df1$B <- ifelse(is.na(df1$B) == TRUE, df2$B[df2$A %in% df1$A], df1$B)  
activity$steps_imputed <- ifelse(is.na(activity$steps) == TRUE, ts$steps[ts$interval %in% activity$interval], activity$steps)   
#split into a new dataset with no misisng values
interval_imputed <-activity %>%
  select(date, interval, steps_imputed)
summary(interval_imputed)
#mean on the dataset without missing values
#first total(sum) of steps for each day
new_daily_steps <- aggregate(steps_imputed~date, data = interval_imputed, sum)

#mean of interval imputed daily steps
b3<-mean(new_daily_steps$steps_imputed)
#print mean of imputed daily steps
print(paste("The mean of steps from the imputed dataset using mean of 5-min interval is", b3, "While the mean of the original dataset is", c))
#print difference in mean between original data set and imputed data set
print(paste("The difference between the two means is", b3-c))
#median of interval imputed daily steps
b4<-median(new_daily_steps$steps_imputed)
#print median of imputed daily steps
print(paste("The median of steps from the imputed dataset is", b4, "While the mean of the original dataset is", b))
#print difference between imputed and original median
print(paste("The difference between the two median values is", b4-b))

##filling in the missing values using the daily mean- the mean and median change
#get the mean of the steps on each date
mean_daily_steps <- aggregate(steps~date, data=activity, mean)
#replace NA's with mean of daily steps 
activity$mean_steps <- ifelse(is.na(activity$steps)==TRUE, mean_daily_steps$steps[mean_daily_steps$date %in% activity$date], activity$steps)
mean3<- aggregate(mean_steps~date, data=activity, sum)
c2<- mean(mean3$mean_steps)
print(paste("The mean of steps from the imputed dataset using daily means is", c2, "While the mean of the original dataset is", c))
print(paste("The difference between the two means is", c2-c))
c3<-median(mean3$mean_steps)
print(paste("The median of steps from the imputed dataset is", c3, "While the mean of the original dataset is", b))
print(paste("The difference between the two median values is", c3-b))
###histogram of imputed dataset
#mean histogram
hist(new_daily_steps$steps_imputed, 
     main= "Histogram of Total Number of Steps each Day for Imputed Dataset",
     border = "blue",
     col="green",
     breaks= seq(0,25000, by=2500),
     xlim = c(0, 25000),
     xlab = "Total Steps per Day",
     ylim = c(0,30))
abline(v=mean(new_daily_steps$steps_imputed),
       col="royalblue",
       lwd=2)
text(x=mean(new_daily_steps$steps_imputed),
     y=28,
     labels = paste("Mean=", 
                    round(mean(new_daily_steps$steps_imputed), 2)),
     pos = 2)
legend(x="topright",
       "Mean",
       col= "royalblue",
       lwd = 2, 
       cex=0.8,
       text.font = 2,
       box.lty = 0)
#median histogram
hist(new_daily_steps$steps_imputed, 
     main= "Histogram of Total Number of Steps each Day for Imputed Dataset",
     border = "blue",
     col="green",
     breaks= seq(0,25000, by=2500),
     xlim = c(0, 25000),
     xlab = "Total Steps per Day",
     ylim = c(0,30))
abline(v=median(new_daily_steps$steps_imputed),
       col="red",
       lwd=2)
text(x=median(new_daily_steps$steps_imputed),
     y=28,
     labels = paste("Median=", 
                    round(median(new_daily_steps$steps_imputed), 2)),
     pos = 2)
legend(x="topright",
       "Median",
       col= "red",
       lwd = 2, 
       cex=0.8,
       text.font = 2,
       box.lty = 0)
#####question 4: Is there a difference between weekday and weekend activity?#######
class(activity$date)
#change date to weekday value
activity$weekdays <- weekdays(activity$date, abbreviate = FALSE)
#change weekdays character class to factor
activity$weekdays <- factor(activity$weekdays)
#check levels
levels(activity$weekdays)
#rename all levels in order shown above to weekend or weekday
levels(activity$weekdays)<- c("Weekday", "Weekday", "Weekend", "Weekend","Weekday","Weekday","Weekday")
#get average steps for weekdays and weekends in 5 min interval
steps_day_type <- aggregate(steps ~interval + weekdays, data=activity, mean)

#plot daily steps for weekend, weekday using ggplot
library(ggplot2)
ggplot( steps_day_type, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(weekdays ~ .) +
  xlab("5-minute interval over 24 hours") + 
  ylab("Frequency")
