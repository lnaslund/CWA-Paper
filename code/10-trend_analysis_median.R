library(tidyverse)
library(lubridate)
library(spsurvey)
library(ggplot2)
library(ggrepel)

## Defining function to analyze trends median concentraions

median_trends_function<-function(data,nutrient){
  
  years<-unique(data$single_year)
  
  state_medians<-data.frame()
  
  for (i in 1:length(years)){
    data1<-data[data$single_year==years[i],]
    medians<-cont_analysis(data1,vars = nutrient,subpops = "state",xcoord="lon",ycoord="lat",statistics = "Pct") 
    medians2<-medians$Pct
    medians3<-medians2[medians2$Statistic=="50Pct",]
    medians3$single_year=years[i]
    state_medians<-rbind(state_medians,medians3)
  }
  
  states<-unique(state_medians$Subpopulation)
  
  trends<-data.frame()
  for (i in 1:length(states)){
    one_state<-state_medians[state_medians$Subpopulation==states[i],]
    model<-lm(Estimate~single_year,one_state)  
    model_sum<-summary(model)
    output<-data.frame(state=states[i],trend=coef(model)[2],trend.se=model_sum$coefficients[2,2])
    trends<-rbind(trends,output)
  }
  trends
  
}


############## Recast year function
############## This is needed because the trend analysis function estimates a variance for each sampling year
############## thus, treating the 2008/2009 sampling efforts as discrete events reduces the sample size,
############## and becomes problematic in some state/ year combinations

alter_year <- function(x){
  if(x=="2009"){
    return("2008")
  }
  if(x=="2014"){
    return("2013")
  }
  if(x=="2019"){
    return("2018")
  }
  else{
    return(x)
  }
}

#############  Stream nitrate trend analysis
stream_nitrate<-read.csv("data/clean_data/clean_stream_nitrate.csv")

stream_nitrate$siteID<-stream_nitrate$site
stream_nitrate$year<-year(ymd(stream_nitrate$date))
stream_nitrate$single_year <- year(ymd(sapply(as.character(stream_nitrate$year), alter_year), truncated = 2L))
stream_nitrate$Wyear <- stream_nitrate$single_year - min(stream_nitrate$single_year)

# summary of the number of observations per survey year per state
year_no3_state_n <- stream_nitrate %>% group_by(state, Wyear) %>% dplyr::summarize(count=n())

# trend analysis function weights the estimates based on the design of the survey (stratified random sampling)
difference_no3<-median_trends_function(stream_nitrate,"no3") 

write.csv(difference_no3, "data/clean_data/difference_no3_median.csv", row.names =F)

############### stream ammonium 

stream_ammonium<-read.csv("data/clean_data/clean_stream_nh4.csv")

stream_ammonium$siteID<-stream_ammonium$site
stream_ammonium$year<-year(ymd(stream_ammonium$date))
stream_ammonium$single_year <- year(ymd(sapply(as.character(stream_ammonium$year), alter_year), truncated = 2L))
stream_ammonium$Wyear <- stream_ammonium$single_year - min(stream_ammonium$single_year)

year_nh4_state_n <- stream_ammonium %>% group_by(state, Wyear) %>% dplyr::summarize(count=n())

difference_nh4<-median_trends_function(stream_ammonium,"nh4")

write.csv(difference_nh4, "data/clean_data/difference_nh4_median.csv", row.names =F)

############### stream total nitrogen

stream_tn<-read.csv("data/clean_data/clean_stream_tn.csv")

stream_tn$siteID<-stream_tn$site
stream_tn$year<-year(ymd(stream_tn$date))
stream_tn$single_year <- year(ymd(sapply(as.character(stream_tn$year), alter_year), truncated = 2L))
stream_tn$Wyear <- stream_tn$single_year - min(stream_tn$single_year)


difference_tn<-median_trends_function(stream_tn,"tn")

write.csv(difference_tn, "data/clean_data/difference_tn_median.csv", row.names =F)

############### stream total phosphorus

stream_tp<-read.csv("data/clean_data/clean_stream_tp.csv")


stream_tp$siteID<-stream_tp$site
stream_tp$year<-year(ymd(stream_tp$date))
stream_tp$single_year <- year(ymd(sapply(as.character(stream_tp$year), alter_year), truncated = 2L))
stream_tp$Wyear <- stream_tp$single_year - min(stream_tp$single_year)

difference_tp<-median_trends_function(stream_tp,"tp")

write.csv(difference_tp, "data/clean_data/difference_tp_median.csv", row.names =F)

############# lake TP

lake_tp<-read.csv("data/clean_data/clean_lake_tp.csv")
#### in 2012 all of the TP samples from CA were below the limit of detection, so they all have the same
#### values 

lake_tp<-lake_tp[lake_tp$state!="CA",]


lake_tp$year<-year(ymd(lake_tp$date))
lake_tp$single_year <- year(ymd(sapply(as.character(lake_tp$year), alter_year), truncated = 2L))
lake_tp$Wyear <- lake_tp$single_year - min(lake_tp$single_year)

difference_tp_lake<-median_trends_function(lake_tp,"tp")

write.csv(difference_tp_lake, "data/clean_data/difference_tp_lake_median.csv", row.names =F)


################ lake TN

lake_tn<-read.csv("data/clean_data/clean_lake_tn.csv")

lake_tn$year<-year(ymd(lake_tn$date))
lake_tn$single_year <- year(ymd(sapply(as.character(lake_tn$year), alter_year), truncated = 2L))
lake_tn$Wyear <- lake_tn$single_year - min(lake_tn$single_year)

difference_tn_lake<-median_trends_function(lake_tn,"tn")

write.csv(difference_tn_lake, "data/clean_data/difference_tn_lake_median.csv", row.names =F)



