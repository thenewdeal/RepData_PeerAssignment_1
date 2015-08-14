#########################################################################
# Reproducible Research (course project 1)

# Loading and preprocessing the data
cls = c("integer", "character", "integer")
df <- read.csv("activity.csv", head=TRUE, colClasses=cls, na.strings="NA")
head(df)

df$date <- as.Date(df$date)
df_ign <- subset(df, !is.na(df$steps))
#########################################################################
### What is mean total number of steps taken per day?
dailysum <- tapply(df_ign$steps, df_ign$date, sum, na.rm=TRUE, simplify=T)
dailysum <- dailysum[!is.na(dailysum)]

hist(x=dailysum,
     col="red",
     breaks=20,
     xlab="Daily Total Steps",
     ylab="Frequency",
     main="The Distribution of Daily Total Steps (regardless missing data)")
mean(dailysum)
median(dailysum)
#########################################################################
## What is the average daily activity pattern?
int_avg <- tapply(df_ign$steps, df_ign$interval, mean, na.rm=TRUE, simplify=T)
df_ia <- data.frame(interval=as.integer(names(int_avg)), avg=int_avg)

with(df_ia,
     plot(interval,
          avg,
          type="l",
          xlab="5-Minute Intervals",
          ylab="Average Number of Steps Across All Days"))

max_steps <- max(df_ia$avg)
df_ia[df_ia$avg == max_steps, ]
#########################################################################
## Imputing missing values
sum(is.na(df$steps))
df_impute <- df
ndx <- is.na(df_impute$steps)
int_avg <- tapply(df_ign$steps, df_ign$interval, mean, na.rm=TRUE, simplify=T)
df_impute$steps[ndx] <- int_avg[as.character(df_impute$interval[ndx])]

new_dailysum <- tapply(df_impute$steps, df_impute$date, sum, na.rm=TRUE, simplify=T)

hist(x=new_dailysum,
     col="red",
     breaks=20,
     xlab="Daily Steps",
     ylab="Frequency",
     main="The Total Distribution of Daily Steps (imputted missing data)")

mean(new_dailysum)
median(new_dailysum)
#########################################################################
# Are there differences in activity patterns between weekdays and weekends?
# helper function to decide if a day is a week day or not
is_weekday <- function(d) {
  wd <- weekdays(d)
  ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(df_impute$date, is_weekday)
df_impute$wk <- as.factor(wx)
head(df_impute)

wk_df <- aggregate(steps ~ wk+interval, data=df_impute, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(wk), layout = c(1, 2), xlab="Interval", ylab="Number of Steps", type="l", lty=1, data=wk_df)

