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

dailySteps <- activity[!is.na(activity$steps),] %>%
  group_by(date) %>%
  summarise(steps=sum(steps))

ggplot(dailySteps, aes(x=date)) + 
  geom_histogram(aes(weight=steps), bins=length(unique(dailySteps$date)))


