library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


Sys.setenv(TZ='UTC')

#Scalar build
raw_data <- read.csv('C:\\Users\\cmohan\\Desktop\\DataMiner-Export_2016-01-12-westhub.csv') %>%
  select(-VERSION,-ZONE,-PNODEID,-PNODETYPE,-PRICINGTYPE,-H2DST) %>%
  mutate(PUBLISHDATE = mdy(PUBLISHDATE)) %>%
  gather(hour,value,-PUBLISHDATE,-PNODENAME) %>%
  mutate(variable = as.numeric(substring(hour,2)) - 1,
         Date = mdy_hms(paste(month(PUBLISHDATE),day(PUBLISHDATE),year(PUBLISHDATE),hour,0,0))) %>%
  select(Date,value) %>%
  arrange(Date) %>%
  na.omit()


  filtered_data  <- raw_data %>%
    mutate(value = as.numeric(value),
           type = ".",
           type = ifelse(wday(Date) %in% seq(2,6) & hour(Date) %in% seq(7,22),'OnPeak','OffPeak'),
           type = ifelse(wday(Date) %in% c(1,7), 'WeekEnd',type)
    )

  types <- c('OnPeak','OffPeak','WeekEnd')

    out <- list()
        # for each month
        for(m in 1:12){
        # for each type
        for (t in 1:3 ){
          fdata <- filter(filtered_data,type == types[t],month(Date) == m)

          by_year <- split(fdata, year(fdata$Date))
          pct_of_mean <- lapply(by_year, function(x)  mutate(x,value = value/mean(value)))
          s <- do.call(rbind, pct_of_mean)

          name <- paste(types[t],':', month(m, label = TRUE),sep="")


          out[[name]] <- s
        }

      }

    all <- do.call(rbind,out)





  scalars <- arrange(all) %>%
             mutate(month = month(Date),hour = hour(Date))%>%
             group_by(month,hour,type) %>%
             summarize(scalar = mean(value)) %>%
             ungroup()



  ggplot(mutate(scalars, month = as.factor(month), hour = as.factor(hour)), aes(x = hour, y = scalar,group = month)) +
    geom_line(aes(colour = month)) +
    scale_y_continuous(breaks = seq(-2,2,by = .25),'Scalar') +
    facet_grid(type~.) +
    theme_bw()

  dates <- data.frame(date = seq(ymd_h('2016-08-01-0'),ymd_h('2016-08-10-23'), by = 'hours')) %>%
    mutate(month = month(date),
           hour = hour(date),
           type = ".",
           type = ifelse(wday(date) %in% seq(2,6) & hour(date) %in% seq(7,22),'OnPeak','OffPeak'),
           type = ifelse(wday(date) %in% c(1,7), 'WeekEnd',type))

  forwardPrice <- data.frame(month = 8, type = c('OnPeak','OffPeak','WeekEnd'),price = c(52,35,32))

  fcast <- Reduce(left_join,list(dates,scalars, forwardPrice)) %>%
    mutate(fcast_price = scalar * price)

  ggplot(fcast, aes(x= date, y = fcast_price)) +
    geom_line() +
    theme_bw()

