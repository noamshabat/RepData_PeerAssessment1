# library
library(ggplot2)
library(dplyr)

getData <- function() {
  zipfile = 'activity.zip'
  csvfile = 'activity.csv'
  if (!file.exists(csvfile)) {
    unzip(zipfile)
  }
  activity <- read.table(csvfile, header=T, sep=',', colClasses = c(
    "numeric", "Date", "numeric"
  ))
  activity
}

activity <- getData()

sum(is.na(activity$steps))

intervalMean <- activity[!is.na(activity$steps),] %>%
  group_by(interval) %>%
  summarise(steps=mean(steps), .groups='drop')

activity$steps[is.na(activity$steps)] <- # insert into all NA values
  sapply(activity$interval[is.na(activity$steps)], # select interval of na values
         # return the mean value for each interval.
         function (x) { intervalMean[intervalMean$interval==x,'steps'] })

activity$steps <- unlist(activity$steps)
# function that returns true when weekend.
isWeekend <- function(x) {
  weekdays(x) %in% c("Sunday", "Saturday")
}
# add factor variable
activity$daytype <- factor(ifelse(isWeekend(activity$date), "WeekEnd", "WeekDay")) 
activity$day <- weekdays(activity$date)

meanByType <- activity %>%
  group_by(interval, daytype) %>%
  summarise(steps=mean(steps))

show(ggplot(meanByType, aes(x=interval, y=steps, group=daytype)) + 
       geom_line(color="blue") + 
       facet_grid(rows=vars(daytype)) + 
       geom_hline(yintercept = 100))