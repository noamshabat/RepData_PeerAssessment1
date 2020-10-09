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

activity[is.na(activity$steps),'steps'] <- 0

dailyPattern <- activity[!is.na(activity$steps),] %>%
  group_by(interval) %>%
  summarise(steps=mean(steps), .groups='drop')


show(ggplot(dailyPattern, aes(x=interval, y=steps)) + 
  geom_line())
