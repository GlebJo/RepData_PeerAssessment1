# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(sqldf)
```

```
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: RSQLite.extfuns
```

```r
library(lattice)
library(ggplot2)

dd<-read.table(unzip("./activity.zip"), header=TRUE, sep=",")
dd<-sqldf("select *, strftime('%w',date) as wdn from dd")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```

```r
dd<-sqldf("select *, case when wdn between 1 and 5 then 'Weekday' else 'Weekend' end as wd from dd")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```


## What is mean total number of steps taken per day?

```r
rr1<-sqldf("select date, wdn, wd, sum(steps) as sumsteps, avg(steps) as avgsteps from dd where steps is not null group by date, wdn, wd")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```
- ***Make a histogram of the total number of steps taken each day***

```r
par(mfrow=c(1,1))
dev=png
hist(as.numeric(rr1$sumsteps), col="red", main="Total number of steps taken each day", xlab="Steps by day", ylab="Days count")
```

```
## Error in hist(as.numeric(rr1$sumsteps), col = "red", main = "Total number of steps taken each day", : object 'rr1' not found
```

- ***Calculate and report the*** **mean** ***and*** **median** ***total number of steps taken per day***  

```r
summary(as.numeric(rr1[!is.na(rr1$sumsteps),"sumsteps"]))
```

```
## Error in summary(as.numeric(rr1[!is.na(rr1$sumsteps), "sumsteps"])): error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'rr1' not found
```
As you can see **mean** and **median** values are the same in this case: **10800 steps**.


```r
boxplot(as.numeric(rr1$sumsteps), col="red")
```

```
## Error in boxplot(as.numeric(rr1$sumsteps), col = "red"): object 'rr1' not found
```


## What is the average daily activity pattern?

- ***Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)***


```r
rr2<-sqldf("select interval, avg(steps) as avgsteps from dd where steps is not null group by interval")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```

```r
xyplot(avgsteps~interval, data=rr2, type="l")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'rr2' not found
```
- ***Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?***

```r
sqldf("select interval, avgsteps from rr2 where avgsteps=(select max(avgsteps) from rr2)")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```
Maximum number of steps (**206.2**) in this pattern found at **835** interval.

## Imputing missing values

- ***Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)***


```r
sqldf("select count(*) as [NA count] from dd where steps is null")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```
- ***Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.***

Now lets replace NAs with average values of activity pattern from previous study

- ***Create a new dataset that is equal to the original dataset but with the missing data filled in.***


```r
dd1<-sqldf("select a.date, a.wdn, a.wd, a.interval, case when a.steps is null then b.avgsteps else a.steps end as steps from dd a inner join rr2 b on a.interval = b.interval ")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```

```r
rr3<-sqldf("select date, wdn, wd, sum(steps) as sumsteps, avg(steps) as avgsteps from dd1 group by date, wdn, wd")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```
- ***Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?***


```r
par(mfrow=c(1,2))
hist(as.numeric(rr1$sumsteps), col="red", main="Total number of steps taken each day (No NAs)", xlab="Steps by day", ylab="Days count")
```

```
## Error in hist(as.numeric(rr1$sumsteps), col = "red", main = "Total number of steps taken each day (No NAs)", : object 'rr1' not found
```

```r
hist(as.numeric(rr3$sumsteps), col="blue", main="Total number of steps taken each day (Fill NAs)", xlab="Steps by day", ylab="Days count")
```

```
## Error in hist(as.numeric(rr3$sumsteps), col = "blue", main = "Total number of steps taken each day (Fill NAs)", : object 'rr3' not found
```

```r
par(mfrow=c(1,1))
rr4<-sqldf("select 'No NAs' as type,  * from rr1 union all select 'Fill NAs' as type,  * from rr3 order by type desc")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```

```r
boxplot(rr4$sumsteps ~ rr4$type, col=c("blue", "red"))
```

```
## Error in eval(expr, envir, enclos): object 'rr4' not found
```

Mean & median values (No NAs)

```r
summary(rr1$sumsteps)
```

```
## Error in summary(rr1$sumsteps): error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'rr1' not found
```
Mean & median values (Fill NAs)

```r
summary(rr3$sumsteps)
```

```
## Error in summary(rr3$sumsteps): error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'rr3' not found
```

## Are there differences in activity patterns between weekdays and weekends?


```r
rr5<-sqldf("select wd, interval, avg(steps) as avgsteps from dd1 group by wd,interval")
```

```
## Error in if (.allows_extensions(db)) {: missing value where TRUE/FALSE needed
```

```r
summary(rr5)
```

```
## Error in summary(rr5): error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'rr5' not found
```

```r
xyplot(avgsteps~interval|wd , data = rr5, type="l", layout = c(1,2) )
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'rr5' not found
```
