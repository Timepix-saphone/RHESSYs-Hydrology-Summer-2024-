### workflow to read in Cal-Adapt long drought downloads
# read more about data here: https://cal-adapt.org/data/download/
### combine early and late century values, save as RHESSys clim file 

library(RHESSysIOinR)
library(tidyverse)
source("climate/read_clim.R")

setwd("~/Documents/SB_LongDrought_CalAdapt/")

# note precip is in mm, need to convert to meters for RHESSys 
# load data 
prcp_early <- read.csv("downloads/pr_day_drought_HadGEM2-ES_rcp85_early_century.csv") %>% 
  rename(prcp_mm = pr_day_drought_HadGEM2.ES_rcp85_early_century)
prcp_late <- read.csv("downloads/pr_day_drought_HadGEM2-ES_rcp85_late_century.csv") %>% 
  rename(prcp_mm = pr_day_drought_HadGEM2.ES_rcp85_late_century)

# combine all dates 
prcp_cent <- rbind(prcp_early, prcp_late)

# convert to meters 
prcp_cent$meters = prcp_cent$prcp_mm/1000

# get start 
start_time = prcp_early$time[1] 
start_date = unlist(str_split(start_time, " "))[1]
wr_date = gsub("-", " ", start_date)
# start clim file 
write_lines(paste(wr_date, 1), "long_drought.rain", append = F)

# tells R not to use scientific notation when writing 
options(scipen = 999)
# write the values 
pm_v = prcp_cent$meters
write_lines(pm_v, "long_drought.rain", append = T)

#############################
## repeat with temperatures 
#############################

# temperature seems to be in kelvins, convert to celsius 
# made function to repeat with temperatures, label is either "min" or "max" 
make_temp_file <- function(label){
  
  early <- read.csv(paste("downloads/tas", label,"_day_drought_HadGEM2-ES_rcp85_early_century.csv",sep="")) 
  names(early) <- c("date", "temp")
  late <- read.csv(paste("downloads/tas", label,"_day_drought_HadGEM2-ES_rcp85_late_century.csv",sep="")) 
  names(late) <- c("date", "temp")
  
  all_cent = rbind(early, late)
  
  # convert to celsius 
  all_cent$temp_c = all_cent$temp - 273.15
  
  # get start 
  start_time = all_cent$date[1] 
  start_date = unlist(str_split(start_time, " "))[1]
  wr_date = gsub("-", " ", start_date)
  
  # start clim file 
  file_name = paste("long_drought.t",label, sep="")
  write_lines(paste(wr_date, 1), file_name, append = F)

  # write the values 
  tmp_v <- round(all_cent$temp_c, 4)
  write_lines(tmp_v, file_name, append = T)
}

# run function 
make_temp_file("min")
make_temp_file("max")


