---
title: "PA1 TEMPLATE"
output: html_notebook
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Reproducible Research: Peer assessment 1


```{r chunk1}
knitr::opts_chunk$set(
  warning = TRUE, # show warnings during codebook generation
  message = FALSE, # show messages during codebook generation
  error = TRUE, # do not interrupt codebook generation in case of errors,
  # usually better for debugging
  echo = TRUE  # show R code
)


```


``` {r chk1}
library(dplyr)
library(ggplot2)
```

## 0. Loading and pre-processing the data


```{r chk2}

if(!file.exists('activity.csv')){
  unzip('activity.zip')
  }

myactivity<-read.csv("activity.csv")
```


## 1. What is mean total number of steps taken per day
### 1.1. calculate the total number of steps per day and plotting the histogram.

```{r chk3}

mydata<-myactivity[!is.na(myactivity$steps),]%>%
  select(date,steps)%>%
  group_by(mydate=as.factor(date))%>%
  summarise(TotalSteps=sum(as.double(steps)),avgSteps=mean(as.double(steps)),medSteps=median(steps))

png("plot1.png",width=480,height=480)

hist(mydata$TotalSteps,
       xlab= "Frequency",
        ylab="Total steps", 
        main = "Total number of steps taken each day",
        col ="blue")


dev.off()
```
### 1.2. Report the mean and the median of total number of steps taken per day.

```{r chk4}
StepsMean=mean(mydata$TotalSteps)
StepsMedian=median(mydata$TotalSteps)
print(c(StepsMean , StepsMedian))
```

### 1.3. What is the average daily activity pattern?

```{r chk5}

myAvgStep<-myactivity[!is.na(myactivity$steps),]%>%
  select(interval,steps)%>%
  group_by(myinterval=as.factor(interval))%>%
  summarise(avgSteps=mean(as.double(steps)))
```
## 2. Make a time series plot (i.e.type = "l") of the 5-minute interval and the average number of steps taken, averaged across all days

```{r chk6}

png("plot2.png",width=480,height=480)
plot(myAvgStep$avgSteps,
     type="l",
     xlab= "5 min",
     ylab="Average across all days", 
     main = "Average number of steps taken per interval",
     col ="red")
dev.off()
```

### 2.1. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r chk7}

IntervalMax<-myAvgStep[myAvgStep$avgSteps==max(myAvgStep$avgSteps),1]
print(IntervalMax)
```


## 3. Imputing missing values
### 3.1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA)

```{r chk8}

missingInput=myactivity[!complete.cases(myactivity),]
print(nrow(missingInput))

```

## 4.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated
####  strategy consist of replacing NA by average interval, we create new factor variable (interval) by converting integer to factor.
 

```{r chk9}
myjoin_0<-mutate(myactivity,myinterval=as.factor(interval))
myjoin<-left_join(myjoin_0,myAvgStep,by="myinterval")
myjoin[!complete.cases(myjoin),"steps"]<-as.integer(myjoin[!complete.cases(myjoin),"avgSteps"])
myactivity_new<-select(myjoin,names(myactivity))
```

## 5.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```{r chk10}
mydata<-myactivity_new[!is.na(myactivity_new$steps),]%>%select(date,steps)%>%group_by(mydate=as.factor(date))%>%summarise(TotalSteps=sum(as.double(steps)))

png("plot3.png",width=480,height=480)

hist(mydata$TotalSteps,
     xlab= "Frequency",
     ylab="Total steps", 
     main = "Total number of steps taken each day with adjusted value",
     col ="green")
dev.off()
```

### 5.1. Do these values differ from the estimates from the first part of the assignment? 

```{r chk11}

StepsMean_new=mean(mydata$TotalSteps)
StepsMedian_new=median(mydata$TotalSteps)
print(c(StepsMean_new , StepsMedian_new))
```
### 5.2. What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r chk12}

impact_mean = StepsMean_new-StepsMean
impact_median =StepsMedian_new-StepsMedian

print (c(impact_mean, impact_median))
```
## 6. Are there differences in activity patterns between weekdays and weekends?

### 6.1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r chk13}
myactivity_new<-mutate(myactivity_new,myday=as.factor(ifelse(weekdays(as.Date(date)) %in% c("samedi","dimanche"),"Weekend","Weekday")))

```
### 6.2. Make a panel plot containing a time series plot (type = "l") of the 5-minute interval and 
### the average number of steps taken, averaged across all weekday days or weekend days.

```{r chk14}

mydata<-group_by(myactivity_new,myday,interval)%>%summarise(avgSteps=mean(steps))

png("plot4.png")

ggplot(mydata,aes(interval,avgSteps))+
  geom_line()+
  facet_grid(myday~.)+
  labs(x= "5-minute interval",y = "average number of steps")+
  labs(title="Average number of steps weekday and weekend")

dev.off()
```
