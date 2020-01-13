

#checking to see if data folders exists
if(!file.exists("data")) {
        dir.create("data")
}


#install necessary packages
library(ggplot2) #for visualizations                      
library(readr) #for read_csv so dates are formatted properly
library(dplyr)



#Set working directa to where my data files are
setwd("~/R/RepData_PeerAssessment1/data")
#read the activity.csv data into df
df <- read_csv("activity.csv")

# Look at first rows of  df
head(df)
summary(df)
#remove NA from steps
df1 <- df %>% 
        select(steps,date, interval) %>%
        filter(steps != "NA")

#group and sum by date 
df2 <- df1  %>%
        group_by(date) %>%
        summarise(day_total = sum(steps))
#alternate for grouping

df3 <-df1 %>% group_by(date) %>% filter(steps>0)
head (df3)


#average staps per day
avg_steps_per_day <- mean(df2$day_total)
avg_steps_per_day

#median steps per day
median_steps <- median(df2$day_total)
median_steps

#Create the histogram

steps_histo <- ggplot(data = df2, aes(day_total)) +
        geom_histogram(bins = 15, fill="blue")

steps_histo
summary(df2)

#creating the time series
time_series <- ggplot(df1, aes(x=interval, y=steps)) +
        geom_line(color = "blue") + 
        xlab("Interval")
#display the timeseries visualization
time_series

# remove the NAs
df_no_NA <- df %>%
        select(everything()) %>%  # replace to your needs
        summarise_all(list(sum(is.na(.))))
head(df_no_NA)

