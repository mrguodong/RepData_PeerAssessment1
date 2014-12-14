##======================================================================
## Module Name  :       PA1_Template
##                      Plot6.R
## Created on   :       12/12/2014
## Created by   :       Dong Guo
## 
## Output       :       
## History:
##      12/12/2014   Created
##======================================================================
library(data.table)
library(ggplot2)

setwd("C:\\GTemp\\R\\RepData_PeerAssessment1")
df <- read.csv(".\\activity\\activity.csv")
dt <- data.table(df)
dt$date <- as.Date(dt$date)
#dt[is.na(dt[,steps]),1] <- 0 
s_d <- dt[, sum(steps,na.rm=TRUE),by = date]
mn_d <- dt[, mean(steps,na.rm=TRUE),by = date]

# 1. Make a histogram of the total number of steps taken each day
ggplot(s_d,aes(x=s_d$date,y=V1))+ geom_bar(stat='identity',aes_string(x=NULL)) + xlab("Date") + 
        ylab("Steps(Sum)") +
        scale_x_date()
        
# 2. Calculate and report the mean and median total number of steps taken per day
mn <- mean(s_d$V1,na.rm=TRUE)
md <- median(s_d$V1,na.rm=TRUE)

# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
mn_i <- dt[, mean(steps,na.rm=TRUE),by = interval]
setnames(mn_i,"V1","Average_Steps")
plot(mn_i$interval,mn_i$Average_Steps,type="l")
colnames(mn_i) <- c("Interval","Average_Steps")
head(mn_i)

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
mn_i[which.max(mn_i$Average_Steps),]

#Imputing missing values
#1. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
nrow(dt[is.na(dt[,steps]),])

#2. Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.
getNAMean <- function(idate,steps){
        if(!is.na(steps)){
                return (steps)
        }else{
                return (mn_d[date==idate,V1])        
        }        
}
getNAMeanIterval <- function(inInterval,steps){
        if(!is.na(steps)){
                return (steps)
        }else{
                return (mn_i[Interval==inInterval,Average_Steps])        
        }        
}

    
#3. Create a new dataset that is equal to the original dataset 
#but with the missing data filled in.
dt2 <- df
dt2$steps <- mapply(getNAMeanIterval,dt2$interval,dt2$steps)

#4. Make a histogram of the total number of steps taken each day 
#   and Calculate and report the mean and median total number of steps taken per day. 
#   Do these values differ from the estimates from the first part of the assignment? 
#   What is the impact of imputing missing data on the estimates of the total daily 
#   number of steps?
dt2 <- data.table(dt2)
dt2$date <- as.Date(dt2$date)
s_d2 <- dt2[, sum(steps,na.rm=TRUE),by = date]
ggplot(s_d2,aes(x=s_d2$date,y=V1)) + geom_bar(stat='identity')+ xlab("Date") + 
        ylab("Steps(Sum)") +
        scale_x_date()
mn2 <- mean(s_d2$V1,na.rm=TRUE)
md2 <- median(s_d2$V1,na.rm=TRUE)


#Create a new factor variable in the dataset with two levels -- "weekday" 
#and "weekend" indicating whether a given date is a weekday or weekend day.
getWeekday <- function(idate){
        if(weekdays(idate) =="Saturday" || weekdays(idate) =="Sunday"){
                return ("Weekend")
        }else{
                return ("Weekday")
        }
}
dt3 <- dt2
dt3$Days <- mapply(getWeekday,as.Date(dt3$date))
mn_dt3 <- dt3[, mean(steps,na.rm=TRUE),by = c("interval","Days")]
setnames(mn_dt3,"V1","Average_Steps")
ggplot(mn_dt3,aes(interval,Average_Steps)) + geom_line()+ facet_grid(Days ~ .)



