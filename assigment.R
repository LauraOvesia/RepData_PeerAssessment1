setwd("~/Desktop/corseraR/ReproducibleR")
data <- read.csv("activity.csv")
str(data)
summary(data)
total_steps_by_day <- aggregate(steps ~ date, data, sum)
str(total_steps_by_day)
hist(total_steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
rmean <- mean(total_steps_by_day$steps)
rmean
##[1] 10766.19
rmedian <- median(total_steps_by_day$steps)
rmedian
##[1] 10765


steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval 
##[1] 835


summary(data)
incomplete <- length(which(is.na(data$steps)))
incomplete
##[1] 2304
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
str(imputed_data)
head(imputed_data)
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(total_steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
rmean.i <- mean(steps_by_day_i$steps)
 rmean.i 
##[1] 10589.69
rmedian.i <- median(steps_by_day_i$steps)
rmedian.i
##[1] 10766.19
mean_diff <- rmean.i - rmean
mean_diff
##[1] -176.4949
med_diff <- rmedian.i - rmedian
med_diff 
##[1] 1.188679
total_diff <- sum(steps_by_day_i$steps) - sum(total_steps_by_day$steps)

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
imputed_data$week_days = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + week_days, imputed_data, mean)
str(imputed_data)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$week_days, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")