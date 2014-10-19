## Set Working Directory to the data folder having the activity file
setwd("Data Science/data/")

## Read the data file 
activity <- read.csv("activity.csv")

## Remove missing values 'NA'
activitycompact <- subset(activity, activity$steps != 'NA')

## Aggregate the data by date
activitybydate <- aggregate(activitycompact$steps,by=list(activitycompact$date),FUN=sum)

## Rename headers
names(activitybydate) <- c("date","steps")

## Plot the histogram
hist(activitybydate$steps, col="blue", main ="Number of steps taken each day", 
      xlab="Steps", ylab="Frequency")

## Calculate the mean
mean(activitybydate$steps,na.rm=TRUE)

## Calculate the median
median(activitybydate$steps,na.rm=TRUE)

## Activity by Interval Mean
activitybyinterval <- aggregate(activitycompact$steps,by=list(activitycompact$interval),FUN=mean)

## Rename headers
names(activitybyinterval) <- c("interval","steps")

## Time series plot
plot(activitybyinterval$steps ~ activitybyinterval$interval, type = "l", 
    col="red", main = "Average daily activity pattern", 
    xlab="5 min interval",ylab="average number of steps")

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
subset(activitybyinterval, activitybyinterval$steps == max(activitybyinterval$steps))

## Imputing missing values
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nrow(activity[activity$steps == 'NA',])

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## Create a new dataset that is equal to the original dataset but with the missing data filled in.

activityfilled <- merge(x=activity,y=activitybyinterval,by.x="interval",by.y="interval")

names(activityfilled) <- c("interval","steps","date","stepsperinterval")

for (i in 1:nrow(activityfilled))
{ if (is.na(activityfilled[i,]$steps)) {activityfilled[i,2] = activityfilled[i,4]}
}

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?

activityfilledbydate <- aggregate(activityfilled$steps,by=list(activityfilled$date),FUN=sum)

names(activityfilledbydate) <- c("date","steps")

hist(activityfilledbydate$steps, col="blue", main ="Number of steps taken each day", 
     xlab="Steps", ylab="Frequency")

## Calculate the mean
mean(activityfilledbydate$steps,na.rm=TRUE)

## Calculate the median
median(activityfilledbydate$steps,na.rm=TRUE)

activityfilled$date <- as.Date(activityfilled$date)
activityfilled$day <- weekdays(activityfilled$date)
for (i in 1:nrow(activityfilled)) 
{ 
  if(activityfilled[i,5] == "Saturday") { activityfilled$daylevel[i] = "Weekend"} 
  else if (activityfilled[i,5] == "Sunday") {activityfilled$daylevel[i] = "Weekend"} 
  else {activityfilled$daylevel[i] = "Weekday"} 
}
activityfilled$daylevel <- factor(activityfilled$daylevel)


activityfilledbyday <- aggregate(activityfilled$steps,by=list(activityfilled$interval,activityfilled$daylevel),FUN=mean)
names(activityfilledbyday) <- c("interval","daylevel","steps")

qplot(activityfilledbyday$interval,activityfilledbyday$steps, data = activityfilledbyday,facets = .~daylevel, xlab = "5 min interval", ylab= "average steps") +geom_line() 
