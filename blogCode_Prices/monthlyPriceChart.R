library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# raw_data <- read.csv("//Users//cmohan//Dropbox//blogData//DataMiner-Export_2016-01-16-123508.csv") %>%
raw_data <- read.csv("C:\\Users\\cmohan\\Dropbox\\blogData\\DataMiner-Export_2016-01-16-123508.csv") %>%
  select(-VERSION,-ZONE,-PNODEID,-PNODETYPE,-PRICINGTYPE,-H2DST) %>%
  mutate(PUBLISHDATE = mdy(PUBLISHDATE)) %>%
  gather(hour,value,-PUBLISHDATE,-PNODENAME) %>%
  mutate(variable = as.numeric(substring(hour,2)) - 1,
         Date = mdy_hms(paste(month(PUBLISHDATE),day(PUBLISHDATE),year(PUBLISHDATE),hour,0,0))) %>%
  select(Date,PNODENAME,value) %>%
  arrange(Date) %>%
  na.omit()

monthly <- raw_data %>%
  mutate(month = month(Date),
         year = year(Date)) %>%
  filter(year >= 2012) %>%
  select(month, year, PNODENAME, value) %>%
  group_by(month,year,  PNODENAME) %>%
  summarize(value = mean(value, na.rm = TRUE))
#   spread(PNODENAME,value) %>%
#   mutate(date = ymd(paste(year,month,"1",sep ="-"))) %>%
#   select(-year, -month)
#
ggplot(monthly,aes(x= as.factor(month), y=value)) +
  geom_boxplot(aes(fill = as.factor(month))) +
  facet_grid(.~PNODENAME)

week_of_month <- function(date) {
  first_day <- wday(ymd(paste(year(date), month(date), "1",sep = "-")))
  (first_day + day(date) - 1) %/% 7
}


week_number <- raw_data %>%
  filter(month(Date) == 2) %>%
  mutate(week = week_of_month(floor_date(ymd_hms(Date),unit = "day")),
         month = month(Date),
         year = year(Date)) %>%
  filter(year >= 2012) %>%
  select(week, year, PNODENAME, value) %>%
  group_by(week,year,  PNODENAME) %>%
  summarize(value = mean(value, na.rm = TRUE))

ggplot(week_number,aes(x= as.factor(week), y=value)) +
  geom_boxplot(aes(fill = as.factor(week))) +
  facet_grid(.~PNODENAME)

day_number <- raw_data %>%
  filter(month(Date) == 2) %>%
  mutate(day = mday(Date),
         week = week_of_month(floor_date(ymd_hms(Date),unit = "day")),
         month = month(Date),
         year = year(Date)) %>%
  filter(year >= 2012) %>%
  select(day,week, year, PNODENAME, value) %>%
  group_by(day,year,week,  PNODENAME) %>%
  summarize(value = mean(value, na.rm = TRUE))

ggplot(day_number,aes(x= as.factor(day), y=value)) +
  geom_boxplot(aes(fill = as.factor(week), outlier.colour = as.factor(week))) +
  facet_grid(.~PNODENAME)





