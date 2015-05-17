# repResearch_project1.r

# Copyright statement: none
# Author date comment: Sandyn Skudneski 5/17/2105
# File description
#   purpose: Explore dataset, discover patterns and feed code to rmarkdown/knitr
#             
#   inputs:  personal activity data [343 kb unzipped]
#             - 1 table, x rows, 3 columns
#   outputs: plots and code chunks for rmarkdown/knitr
#            
# Function definitions: none
#
# Dev SysSpec: Dell Latitude E6430, Win 7 Pro 64bit, R 3.2.0, RStudio 0.98.1103
# Platform:    x86_64-w64-mingw32/x64 (64-bit)

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(Amelia)

zipfile <- "repdata_data_activity.zip"
file <- "activity.csv"
setwd("~/R/Coursera/RepResearch/Project1/")
unzip(zipfile)
df <- tbl_df(read.csv(file, stringsAsFactors = FALSE)) 
# given that we'll work from df several times I'll add comlums to it as needed
# to standardize things
# convert column data types and add full date/time column
df$steps <- as.numeric(df$steps)
df$date <- ymd(df$date)
df$intervalA <- sprintf("%04d", df$interval)
df$intervalA <- format(strptime(df$intervalA, format="%H%M"), 
                       format = "%H:%M") # note, this is a chr vector
df <- mutate(df, dateTime = ymd_hm(paste(date, intervalA))) # in dateTime format
df <- mutate(df, weekday = wday(dateTime, label = TRUE, abbr = TRUE))

df1 <- df[complete.cases(df), ] # df1 is only complete cases

# What is mean total number of steps taken per day?
result1 <-
    df1 %>%
    group_by(date) %>%
    summarize(totalDailySteps = sum(steps))
meanResult1 <- round(mean(result1$totalDailySteps))
medResult1 <- round(median(result1$totalDailySteps))

g <- ggplot(result1, aes(x=totalDailySteps))
g + geom_bar(binwidth = 5000, fill = "blue", color = "black") + 
    xlim(0, 25000) +
    labs(x = "Steps per Day", 
         y = "Total Days") +
    ggtitle("Total Days by_group Steps per Day") + 
    theme(plot.title = element_text(size = 20, colour="blue")) +
    annotate("text", x = 21000, y = 29, 
             label = paste("   mean steps/day: ", meanResult1)) + 
    annotate("text", x = 21000, y = 27, 
             label = paste("median steps/day: ", medResult1))

# What is the average daily activity pattern?
result2 <-
    df1 %>%
    group_by(interval) %>%
    summarize(meanStepsPerInterval = mean(steps))

g2 <- ggplot(result2, aes(x = interval, y = meanStepsPerInterval))
g2 + geom_line(colour = "red", size = .7) + 
     labs(x = "Time Interval", y = "Steps per Time Interval") +
     ggtitle("Mean Daily Activity Pattern")

# Imputing missing values
# Determine number of incomplete measurements
incompleteCases <- sum(!complete.cases(df))

# Fill in data for missing time intervals
# Note: I had some much trouble graphing that I did not complete this section
#       if I'd have had been able I would have taken the average per interval
#       for the interval.
mutate(df, steps_est, meanStepsPerInterval)
for (i in seq(nrow(df))) {
    if (is.na(df[i, "steps"])) {
        df$steps_est[i] <- df[i, "average_steps"]
    }
    else {
        data$steps_revised[i] <- data[i, "steps"]
    }
}

# Are there differences in activity patterns between weekdays and weekends?
library(timeDate)
df1$dayType <- "weekday" # add "weekends" to all and...
df1$dayType[isWeekend(df$dateTime)] <- "weekend" # change what needs it
df1$dayType <- as.factor(df$dayType) # set them up as a factor

result4 <-
    df %>%
    group_by(interval, dayType) %>%
    summarize(totalPerDayType = sum(steps))

g4 <- ggplot(result4, aes(interval, totalPerDayType, facets = dayType))
g4 + geom_line(aes(colour = dayType), size = .7) +
     labs(x = "Interval", y="Steps per Interval") + 
     ggtitle("Comparative Daily Activity Pattern")