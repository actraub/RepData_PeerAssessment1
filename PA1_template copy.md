---
title: "PA1_template"
author: "Aaron Traub"
date: "May 25, 2016"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Reproducable Data Assignment 1 (week 2)
### By Aaron Traub

## Summary
This document is an exploration of steps monitored by an activity tracker. The data has been collected at 5 minute intervals aross 61 days tracking the number of steps taken. 

Due to a large number of NAs in the dataset, the data is replaced with the average for that interval across all of the days. This data is graphed.

Lastly, I plot the difference between weekends and weekdays which shows that weekends are slightly less active for the subject.

Dataset: Activity monitoring data [52K]
Forked from: (http://github.com/rdpeng/RepData_PeerAssessment1). 
**Note:** dataset is download from the repo on 25 May 2016

##Data Processing
### Load R Libraries
``` {r libs} 
library(chron)
library (dplyr)
```
### Loading and preprocessing the data
```{r load-data }
    
    # Load data
    zipPath <- "./activity.zip"
    filePath <- "./activity.csv"
    unzip(zipPath)
    activity <- read.csv(filePath, header = TRUE)
    
    # convert date column to a date object
    activity$date <- as.Date(activity$date, format = "%Y-%m-%d" )
```

### Calculating date summary values
```{r summaryCalc}
    # Calculate values
    meanSteps <- activity %>% group_by(date) %>% summarize  (mean(steps, na.rm =TRUE)) 
    names(meanSteps) <- c("date","steps")
    
    medianSteps <- activity %>% group_by(date) %>% summarize  (median(steps, na.rm =TRUE))
   
    sumSteps <- activity %>% group_by(date) %>% summarize  (sum(steps, na.rm =TRUE)) 
    names(sumSteps) <- c("date","steps")
    
       
```
###Histogram of the total number, mean & median steps taken each day
``` {r histMeanSteps}
    hist(sumSteps$steps, breaks= 15, col="pink",xlab="# of Steps", main="Figure 1: Daily Steps ")
    print (meanSteps)
    print (medianSteps)
```


###Time series plot of the average number of steps taken at given intervals
``` {r fivemin}
    meanStepsInt <- activity %>% group_by(interval) %>% summarize (mean(steps, na.rm =TRUE)) 
    names(meanStepsInt) <- c("interval","mean")
    plot(meanStepsInt, type="l", ylab="# of Steps", xlab="5 Minute Interval", col="cornflowerblue",  main="Figure 2: Daily Steps for Each Increment")
    max(meanStepsInt) 
```
##Imputing missing values
I imputed missing values by filling in NA values with intrval average across days. Data was missing in large chunks likely caused by not wearing the device for portions a day in general. To have the most accurate data, it was more likely that an interval's mean would be more accurate (users will likely perform common tasks ast common times during the day) than daily average. 


``` {r impute }
    numNA <- sum(is.na(activity)) 
    # meanNA <- mean(is.na(activity$steps))
    activityCount <- nrow(activity)
    # make a second variable
    activity2 <- activity

   
    print ("Summary numbers by interval including mean")
    str (meanStepsInt)
    summary (meanStepsInt)
    max(meanStepsInt)
    
    activity3 <- merge (activity2, tbl_df(meanStepsInt), by="interval" )   
    names(activity3)[4] <- "intMean"
    activity3$newSteps <- mapply (ifelse , is.na(activity3$steps), activity3$intMean, activity3$steps)
    
    #output
    str (activity2)
    print (meanStepsInt[max(meanStepsInt$mean),])
   
    activity4 <- activity3 %>%
        arrange (newSteps) %>% 
        group_by(date) %>% 
        summarize  (mean(newSteps, na.rm =TRUE))
        names(activity4) <- c("date", "mean")
    
    hist(activity4$mean, breaks= 15, col="pink", main="Figure 3: Daily Steps imputed")

```
Number of NA values: **`r numNA`**  
Number of observations: **`r activityCount`**  
maximum Interval: **`r str (meanStepsInt)`**  

## Difference between weekends & weekdays
### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

``` {r days}

# subset for weekends and weekdays

    activityWeekend <- activity3 %>%
        arrange (newSteps) %>% 
        filter (is.weekend(activity3$date)) %>% 
        group_by(interval) %>% 
        summarize (mean(newSteps, na.rm =TRUE))
        names(activityWeekend) <- c("interval", "mean")
        
    activityWeekdays <- activity3 %>%
        arrange (newSteps) %>% 
        filter (!is.weekend(activity3$date)) %>% 
        group_by(interval) %>% 
        summarize (mean(newSteps, na.rm =TRUE))
        names(activityWeekdays) <- c("interval", "mean")
```
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends 

``` {r panelplot}

    par(mfrow=c(1,2))

    plot(activityWeekdays$interval, activityWeekdays$mean, main="Weekday intervals", type="l", ylab="Step Mean", xlab="Interval",  ylim=c(0,275))
    abline(h=mean(activityWeekdays$mean), col="purple")

    plot(activityWeekend$interval, activityWeekend$mean, main="Weekend Intervals", type="l", ylab="Step Mean", xlab="Interval",  ylim=c(0,275))
    abline(h=mean(activityWeekend$mean), col="purple")

```

###Final Note
The weekend and week day average steps across days if nearly identical. The Interval average on weekends had a greater range. It would be more effective imputing data if the weekday and weekend interval mean were imputed from individual subsets. Further analysis could be done on individual day of the week.

###--The End--



