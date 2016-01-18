library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

raw_data <- read.csv("//Users//cmohan//Dropbox//blogData//DataMiner-Export_2016-01-16-123508.csv") %>%
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








