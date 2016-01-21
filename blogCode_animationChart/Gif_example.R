library(dplyr)
#library(magrittr)
library(lubridate)
library(ggplot2)
#library(gridExtra)
library(animation)
# library(reshape2)
library(tidyr)

buys<-c('7/13/2012','8/15/2012','9/14/2012',
        '10/15/2012','11/15/2012','12/14/2012',
        '1/15/2013','2/15/2013','3/15/2013',
        '4/15/2013','5/15/2013','6/14/2013',
        '7/15/2013','8/15/2013','9/13/2013',
        '10/15/2013','11/15/2013','12/13/2013',
        '1/15/2014','2/14/2014','3/14/2014',
        '4/15/2014','5/15/2014','6/13/2014'
)



rawdata <- read.csv("C:\\Users\\cmohan\\Desktop\\Junk Drawer\\13_14.csv") %>%
  select(Date,July_13_June_14_fw,July_14_June_15_fw) %>%
  mutate(Date = mdy(Date)) %>%
  filter(Date > ymd('2012-06-29')) %>%
  mutate(buy = as.character(ifelse(Date %in% mdy(buys),1,0)),
         July_13_June_14_fw = July_13_June_14_fw,
         July_14_June_15_fw = July_14_June_15_fw) %>%
  gather(variable,value,-Date,-buy) %>%
  na.omit()

draw.chart<-function(cutoff){

  cutoff <- ymd('2014-06-30')

  a<-ggplot(filter(rawdata,Date <= cutoff),aes(x=Date, y=value, group = variable)) +
    geom_line() +
    geom_hline(yintercept  = filter(rawdata,Date <= cutoff &
                                      buy == 1 &
                                      variable == 'July_13_June_14_fw') %>%
                 summarize(mean = mean(value, na.rm=TRUE)) %>% use_series(.,mean),
               colour = "red", geom_text(aes(label = "2013 Avg."))
    ) +
    geom_hline(yintercept  = filter(rawdata,Date <= cutoff &
                                      buy == 1 &
                                      variable == 'July_14_June_15_fw') %>%
                 summarize(mean = mean(value, na.rm=TRUE)) %>% use_series(.,mean),
               colour = "blue", geom_text(aes(label = "2013 Avg."))
    ) +
    geom_point(aes(size=buy,alpha = buy, color = variable, fill = buy)) +
    scale_colour_manual(values = c("red","blue")) +
    ylim(35,50) +
    xlim( ymd('2012-06-29'),ymd('2014-06-30')) +
    annotate("text", x =ymd('2013-12-01'), y = 50,
             label = paste("Average Price 2013-2014: $", as.character(round(filter(rawdata,Date <= cutoff &
                                                                                     buy == 1 &
                                                                                     variable == 'July_13_June_14_fw') %>%
                                                                              summarize(mean(value, na.rm=TRUE)),
                                                                            digits=2)),sep =" "),colour = 'red',hjust =0) +
    annotate("text", x =ymd('2013-12-01'), y = 49,
             label = paste("Average Price 2014-2015: $", as.character(round(filter(rawdata,Date <= cutoff &
                                                                                     buy == 1 &
                                                                                     variable == 'July_14_June_15_fw') %>%
                                                                              summarize(mean(value, na.rm=TRUE)),
                                                                            digits=2)),sep =" "), colour = 'blue', fontface = 'bold',hjust =0)+
    annotate("text", x =ymd('2013-12-01'), y = 48,
             label = paste("Date: ",filter(rawdata,Date <= cutoff) %>% summarize(date = last(Date))%>%
                             extract2(.,1),sep=" " ),hjust =0, fontface = 'bold') +

    theme_bw(base_size = 15) +
    theme(legend.position="none") +
    ggtitle("Simulation of Monthly Buying") +
    ylab("$/MWh") +

    geom_point(data = data.frame(x=ymd('2012-07-29 UTC'),y=50,variable = 'July_13_June_14_fw'),aes(x,y),color = 'red',size=7) +
    annotate("text", x =ymd('2012-11-01 UTC'), y = 50,label = "Buy Points 2013-2014") +

    geom_point(data = data.frame(x=ymd('2012-07-29 UTC'),y=48,variable = 'July_14_June_15_fw'),aes(x,y),color = 'blue',size=7) +
    annotate("text", x =ymd('2012-11-01 UTC'), y = 48,label = "Buy Points 2014-2015")

  print(a)

}

draw.chart(ymd('2014-06-30'))

trace.animate <- function() {
  lapply(seq(ymd('2012-06-29 UTC'),ymd('2014-06-30'),by = '2 day'), function(i) {
    draw.chart(ymd(i))
  })
}


saveVideo(trace.animate(),interval=.05, video.name="C:\\Users\\cmohan\\Desktop\\buypoint2.mp4",
          ani.width = 800,
          ani.height = 372)
#saveHTML(trace.animate(),interval=.05)
