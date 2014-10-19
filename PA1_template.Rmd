---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


This document completes Peer Assessment #1.

### Load up some necessary packages

```{r}
library(dplyr)
library(ggplot2)
```

### First section - Loading and preprocessing the data


1. Load the data

```{r}
setwd("C:/Users/rrreuter/Google Drive/DataScienceClass/Class5_reproducible research")
data <- read.csv("./activity.csv")
```

Basic summary of the raw data:
        
```{r}       
summary(data)
```


### Mean steps
An interesting thing to know about this dataset is something about the normal number of steps taken per day.

The mean and median steps per day are:

```{r}
date.group <- group_by(data, date)
date.group.mean <- summarise(date.group, 
        mean = mean(steps, na.rm=TRUE),
        median = median(steps, na.rm=TRUE),
        sum = sum(steps, na.rm=TRUE))
```

A histogram of total steps per day:
```{r}
hist(date.group.mean$sum)
```


### Activity patterns
Understanding activity patterns might be useful too.  So we can plot the activity over the day (averaged across all days), and look for the maximum activity.

```{r}
interval.group <- group_by(data, interval)
interval.group.means <- summarise(interval.group, 
                                mean = mean(steps, na.rm=TRUE))
plot(mean ~ interval, data=interval.group.means, type="l")
```

The interval of the the day with the maximum activity can be found:

```{r}
interval.group.means[which.max(interval.group.means$mean), ]
```

###Missing values
There are several instances of missing values in this dataset.  

```{r}
sum(is.na(data))
```

To begin investigating if these missing values may impart any bias into the analysis, the mean value for the time interval is substitued for the NAs.

```{r}
fixed.data <- data
fixed.data.merge <- merge(fixed.data, interval.group.means)
for (i in 1:(length(fixed.data.merge$steps))) {
        
        if (is.na(fixed.data.merge$steps[i])) {
                fixed.data.merge$steps[i] <- fixed.data.merge$mean[i]
        }

}

sum(is.na(fixed.data.merge))
```

Re-calculating the mean and median steps for each day now contrasts with what was calculated on the raw data prior.
```{r}
fixed.date.group <- group_by(fixed.data.merge, date)
fixed.date.group.means <- summarise(fixed.date.group, 
        mean = mean(steps, na.rm=TRUE),
        median = median(steps, na.rm=TRUE),
        sum = sum(steps, na.rm=TRUE))
fixed.date.group.means
hist(fixed.date.group.means$sum)
```

Imputing the missing data removed the NAs from the data set, which make the distribution of total steps per day much more normally shaped. 

###Weekday effects on activity
Type of day may affect activity.  To investigate, a variable is created to denote type of day.

```{r}
type.data <- fixed.data.merge
type.data$date <- as.Date(type.data$date)
type.data$day <- weekdays(type.data$date)
type.data$day.type <- "weekday"
for (i in 1:(length(type.data$day))) {
        if (type.data$day[i] == "Saturday") {
                type.data$day.type[i] <- "weekend"
}
}
for (i in 1:(length(type.data$day))) {
        if (type.data$day[i] == "Sunday") {
                type.data$day.type[i] <- "weekend"
}
}
```

Then a plot of the activity in weekday vs. weekends can be created to illustrate that the activity pattern is dependent on the day type; activity is greater early in the day on weekdays.

```{r}
type.group <- group_by(type.data, interval, day.type)
type.group.means <- summarise(type.group, 
                                mean = mean(steps, na.rm=TRUE))
ggplot(type.group.means, aes(y=mean, x=interval)) + geom_line(shape=1) + facet_grid(day.type ~ .)

```
