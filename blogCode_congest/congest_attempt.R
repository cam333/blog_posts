
raw <- read.csv("C://users//cmohan//desktop//DataMiner-Export_2016-01-27_subset.csv") %>%
  mutate(PNODEID = as.factor(PNODEID)) %>%
  mutate(PUBLISHDATE = mdy(PUBLISHDATE))

s2s_pnodes <- read.csv("C://users//cmohan//desktop//sourse_sink_pnodes.csv")

utc_refPrice <- read.csv("C://users//cmohan//desktop//up-to-congestion-reference-price-JAN-2015.csv") %>%
  mutate(s2s_path = paste(sourcepnodeid,sinkpnodeid,sep="_"))

con_data <- raw %>%
  filter(PNODEID %in% s2s_pnodes$pnodeid)


test <- con_data %>%
  filter(PNODEID %in% c(51205,51214), PRICINGTYPE %in% c("CongLMP","LossLMP")) %>%
  select(-VERSION,-ZONE,-PNODETYPE,-PNODEID,-PRICINGTYPE) %>%
  group_by(PUBLISHDATE,PNODENAME) %>%
  summarize_each(funs(sum)) %>%
  gather(hour,value,-PUBLISHDATE,-PNODENAME) %>%
  mutate(variable = as.numeric(substring(hour,2)) - 1,
         Date = mdy_hms(paste(month(PUBLISHDATE),day(PUBLISHDATE),year(PUBLISHDATE),variable,0,0))) %>%
  select(Date,PNODENAME,value) %>%
  spread(PNODENAME,value) %>%
  mutate(diff = BRANDONSH - DOVER) %>%
  group_by(month(Date)) %>%
  summarize(avg = mean(diff))


mdy_hms(paste(month(ymd('2015-01-31 UTC')),day(ymd('2015-01-31 UTC')),year(ymd('2015-01-31 UTC')),23,0,0))





