# plot climate data 

# let's plot the values and see what the trends for long drought look like 

# to load in the files as data frames, use function RHESSysIOinR::read_clim('file_name') 
# here we are using source to bring in the function rather than load the entire package 
source("climate/read_clim.R")

# load tidyverse for ggplot 
library(tidyverse)
library(ggpmisc)

# read sb clim data 
sbrain = read_clim("climate/sb_long_drought.rain")
sbtmin = read_clim("climate/sb_long_drought.tmin")
sbtmax = read_clim("climate/sb_long_drought.tmax")
# read sc clim data 
scrain = read_clim("climate/sc_long_drought.rain")
sctmin = read_clim("climate/sc_long_drought.tmin")
sctmax = read_clim("climate/sc_long_drought.tmax")

# get avg temperature and combine temperature locations
sbtemp = inner_join(sbtmin, sbtmax) %>% 
  mutate(tmean = (tmin+tmax)/2,
         location="Santa Barbara")

sctemp = inner_join(sctmin, sctmax) %>% 
  mutate(tmean=(tmin+tmax)/2,
         location="Santa Cruz")

alltemps = rbind(sbtemp, sctemp)

# aggregate to annual average temps 
alltemps_ann = rbind(sbtemp, sctemp) %>% 
  group_by(location, year) %>% 
  summarise(temp = mean(tmean))

# save as csv 
#write.csv(alltemps, "temperature_daily.csv")
#write.csv(alltemps_ann, "temperature_ann_mean.csv")

# plot temperatures 

# monthly min temp with linear trend line 
alltemps %>% group_by(month, year, location) %>% 
  summarise(tmin = mean(tmin)) %>% 
  ggplot(aes(x=year, y=tmin, col=month)) + 
  geom_point() + scale_colour_viridis_c() + 
  facet_grid("location") +
  stat_poly_eq(use_label(c("eq", "R2"))) + 
  stat_poly_line()

# annual 
alltemps %>% 
  group_by(year, location) %>% 
  summarise(tmin=mean(tmin), tmax=mean(tmax)) %>% 
  ggplot() + 
  geom_point(aes(x=year, y=tmin, col=location, shape="tmin")) + 
  geom_point(aes(x=year, y=tmax, col=location, shape="tmax")) +
  geom_smooth(aes(x=year, y=tmin, col=location, linetype = "tmin"),
              method='lm') +
  geom_smooth(aes(x=year, y=tmax, col=location, linetype = "tmax"),
              method='lm') +
  labs(y="avg. temperature")

# rain 

# combine locations and aggregate to year 
sbrain$location = "Santa Barbara"
scrain$location = "Santa Cruz"

annrain <- rbind(sbrain, scrain) %>% 
  group_by(wy, location) %>% 
  summarise(total_rain = sum(rain)) 


summary(annrain)

# plot 
annrain%>% 
  ggplot(aes(wy, y=total_rain, col=location)) + 
  geom_col()

annrain%>% 
  ggplot(aes(wy, y=total_rain, col=location)) + 
  geom_line() + 
  geom_point()

# add drought indicator 
library(SPEI) # package for drought index functions 

annrain <- annrain %>% group_by(location) %>% 
  mutate(spi = spi(total_rain, scale=1)$fitted)

ggplot(annrain, aes(x=wy, y=spi, col=location)) +
  geom_hline(aes(yintercept=0)) + 
  geom_line() + geom_point()

# column plot 
ggplot(annrain) + 
  geom_hline(aes(yintercept=0)) +
  geom_col(aes(x=wy, y=spi,fill=location), position='dodge')
  