library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)

apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())
ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

day_data <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_data)

ggplot(day_data, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
month_day <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())
ggplot(month_day, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by day and Month") +
  scale_y_continuous(labels = comma)

dayofweek_data <- data_2014 %>%
  group_by(dayofweek) %>%
  dplyr::summarize(Total = n()) 
datatable(dayofweek_data)

month_dayofweek <- data_2014 %>%
  group_by(dayofweek, month, day) %>%
  dplyr::summarize(Total = n())

ggplot(month_dayofweek, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by day and Month") +
  scale_y_continuous(labels = comma)
