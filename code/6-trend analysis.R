library(tidyverse)
library(lubridate)
library(spsurvey)
library(ggplot2)
library(ggrepel)
library(cowplot)

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

############ Run weighted and unweighted linear regression and write results to csv
clean_data <- list()
estimate_df <- list()
model_compare_plot <- list()

clean_data$nutrients <- c("Stream Nitrate", "Stream Ammonium", "Stream TN", "Stream TP", "Lake TN", "Lake TP")
clean_data$abbr <- c("no3", "nh4", "tn", "tp", "tn", "tp")
clean_data$estimate_df_names <- c("difference_no3", "difference_nh4", "difference_tn", "difference_tp", "difference_tn_lake", "difference_tp_lake")
dfs <- list(read.csv("data/clean_data/clean_stream_nitrate.csv"), read.csv("data/clean_data/clean_stream_nh4.csv"), read.csv("data/clean_data/clean_stream_tn.csv"), 
            read.csv("data/clean_data/clean_stream_tp.csv"), read.csv("data/clean_data/clean_lake_tn.csv"), read.csv("data/clean_data/clean_lake_tp.csv"))
clean_data$dfs <- dfs


for(i in 1:length(clean_data$nutrients)){
  
  # standardize columns and add ordinal year
  temp <- clean_data$dfs[[i]]
  
  if(!("siteID" %in% names(temp))){
    temp$siteID <- temp$site
  }
  
  temp$year <- year(ymd(temp$date))
  temp$single_year <- year(ymd(sapply(as.character(temp$year), alter_year), truncated = 2L))
  temp$Wyear <- temp$single_year - min(temp$single_year)
  
  clean_data$dfs[[i]] <- temp
  
  # wlr model
  wlr.model <-trend_analysis(temp, vars_cont = clean_data$abbr[i], subpops = "state", model_cont="SLR", xcoord="lon", ycoord="lat", year="single_year") 
  wlr.summary <- wlr.model$contsum
  
  # slr model
  output_build <- data.frame()
  for(j in 1:length(levels(as.factor(temp$state)))){
    est <- lm(as.formula(paste(clean_data$abbr[i], "~ Wyear")), data= temp[temp$state==levels(as.factor(temp$state))[j],])$coeff[2]
    est_std <- summary(lm(as.formula(paste(clean_data$abbr[i], "~ Wyear")), data= temp[temp$state==levels(as.factor(temp$state))[j],]))$coeff[[4]]
    slr_intercept <- lm(as.formula(paste(clean_data$abbr[i], "~ Wyear")), data= temp[temp$state==levels(as.factor(temp$state))[j],])$coeff[1]
    output <- data.frame(state=levels(as.factor(temp$state))[j], SLR_Trend_Estimate=est, SLR_Trend_Error=est_std, SLR_Intercept_Estimate=slr_intercept)
    output_build <- rbind(output_build, output)
  }
    
  # combine wlr and slr estimates and compare
   estimate_df[[i]] <- wlr.summary %>% select(Subpopulation, Trend_Estimate, Trend_Std_Error,Intercept_Estimate) %>% 
     dplyr::rename("state" = "Subpopulation", "WLR_Trend_Estimate" = "Trend_Estimate","WLR_Trend_Error"="Trend_Std_Error", "WLR_Intercept_Estimate" = "Intercept_Estimate") %>% 
      left_join(output_build, by = "state") %>% 
      mutate(diff_wlr_slr = round(abs(WLR_Trend_Estimate-SLR_Trend_Estimate), 6), sign_switch_wlr_slr = (WLR_Trend_Estimate >0 & SLR_Trend_Estimate <0)| (WLR_Trend_Estimate <0 & SLR_Trend_Estimate >0)) %>% 
      select(state, WLR_Trend_Estimate, SLR_Trend_Estimate, diff_wlr_slr, sign_switch_wlr_slr, WLR_Intercept_Estimate, SLR_Intercept_Estimate, WLR_Trend_Error, SLR_Trend_Error) 
   
   
   model_compare_plot[[i]] <- ggplot(estimate_df[[i]], aes(SLR_Trend_Estimate,WLR_Trend_Estimate))+
     geom_smooth(method="lm")+
     geom_point()+
     geom_abline(slope = 1, linetype="dashed")+
     geom_text_repel(size=5, aes(label=state))+
     labs(x="Unweighted Trend Estimate", y="Weighted Trend Estimate")+
     theme_bw()+
     theme(axis.title = element_text(size=24, color='black', face="bold"), axis.text = element_text(size=14, color='black'), strip.text.y = element_text(
       size = 14, color = "black", face = "bold"
     ))
   
   write.csv(estimate_df[[i]], paste0("data/clean_data/", clean_data$estimate_df_names[i], ".csv"), row.names =F)

}

clean_data$estimate_df <- estimate_df
clean_data$model_compare_plot <- model_compare_plot

##################### saving figures

tiff(filename="./figures/methods.compare.streams.tiff",units="in",res=600,width=16,height=12,compression="lzw")
plot_grid(clean_data$model_compare_plot[[1]],clean_data$model_compare_plot[[2]],clean_data$model_compare_plot[[3]],clean_data$model_compare_plot[[4]],ncol=2,labels="AUTO",label_x=0.12,label_y=0.98,label_size=25)
dev.off()

tiff(filename="./figures/methods.compare.lakes.tiff",units="in",res=600,width=16,height=6,compression="lzw")
plot_grid(clean_data$model_compare_plot[[5]],clean_data$model_compare_plot[[6]],ncol=2,labels="AUTO",label_x=0.12,label_y=0.98,label_size=25)
dev.off()

################## How correlated are the different nutrient trends

corr_df <- clean_data$estimate_df[[1]]  %>% filter(is.na(WLR_Trend_Error)==F) %>% select(state, WLR_Trend_Estimate) %>% dplyr::rename("Stream Nitrate" = "WLR_Trend_Estimate")

for(i in 2:length(clean_data$nutrients)){
  temp <- clean_data$estimate_df[[i]]  %>% filter(is.na(WLR_Trend_Error)==F) %>% select(state, WLR_Trend_Estimate) 
  names(temp) <- c("state", clean_data$nutrients[i])
  corr_df <- corr_df %>% left_join(temp, by="state")
}

correlation.matrix.trends<-cor(corr_df[,2:7],method="pearson",use="complete.obs")
write.csv(correlation.matrix.trends,file="results/correlation.matrix.nutrient.trends.csv")

########### Plots of trends

coloring_bars<-function(mean.trend,se.trend){
  temp<-data.frame(mean.trend,se.trend)
  temp$bar.color<-"black"
  for (i in 1:nrow(temp)){
    if (temp$mean.trend[i]-temp$se.trend[i]>0){temp$bar.color[i]<-"Red"}
    if (temp$mean.trend[i]+temp$se.trend[i]<0){temp$bar.color[i]<-"Blue"}
  }
  return(temp$bar.color)
}

trend_plot <- list()

for(i in 1:length(clean_data$nutrients)){
  build <- estimate_df[[i]] %>% filter(is.na(WLR_Trend_Error)==F)
  
  build$state <- factor(build$state,levels=build$state[order(build$WLR_Trend_Estimate)])
  build$bar_color<-coloring_bars(build$WLR_Trend_Estimate,build$WLR_Trend_Error)
  
  nm <- clean_data$nutrients[i]
  
  trend_plot[[i]] <- ggplot(build , aes(x=WLR_Trend_Estimate,y=state,color=bar_color))+
    geom_point()+
    geom_errorbarh(aes(xmin=WLR_Trend_Estimate-WLR_Trend_Error,xmax=WLR_Trend_Estimate+WLR_Trend_Error))+
    theme_classic()+
    geom_vline(xintercept = 0,linetype="dashed")+
    xlab(bquote(.(nm) ~ "("*mu*g~L^-1~"year"^-1*")"))+ 
    ylab("")+
    theme(text = element_text(size=20))+
    scale_color_manual(values = c("gray50","blue","red"))+
    theme(legend.position = "none")
}

jpeg(filename="./figures/all_streams.jpeg",units="in",res=600,width=16,height=12)
plot_grid(trend_plot[[1]], trend_plot[[2]], trend_plot[[3]], trend_plot[[4]],ncol=4,labels="AUTO",label_x=0.9,label_y=0.95)
dev.off()

jpeg(filename="./figures/all_lakes.jpeg",units="in",res=600,width=8,height=12)
plot_grid(trend_plot[[5]],trend_plot[[6]],ncol=2,labels="AUTO",label_x=0.9,label_y=0.95)
dev.off()

rm(list=ls())
