
library(ggplot2)
library(cowplot)
library(Rmisc)

########### Plots of trends
########### Trend slopes
difference_no3 <- read.csv("./data/clean_data/difference_no3_median.csv")
difference_nh4 <- read.csv("./data/clean_data/difference_nh4_median.csv")
difference_tn <- read.csv("./data/clean_data/difference_tn_median.csv")
difference_tp <- read.csv("./data/clean_data/difference_tp_median.csv")
difference_tn_lake <- read.csv("./data/clean_data/difference_tn_lake_median.csv")
difference_tp_lake <- read.csv("./data/clean_data/difference_tp_lake_median.csv")
###
difference_no3$type<-"stream no3"
difference_nh4$type<-"stream nh4"
difference_tn$type<-"stream TN"
difference_tp$type<-"stream TP"
difference_tn_lake$type<-"Lake TN"
difference_tp_lake$type<-"Lake TP"

median_trends<-bind_rows(difference_no3,difference_nh4,difference_tn,difference_tp,
                         difference_tn_lake,difference_tp_lake)

coloring_bars<-function(mean.trend,se.trend){
  temp<-data.frame(mean.trend,se.trend)
  temp$bar.color<-"black"
  for (i in 1:nrow(temp)){
    if (temp$mean.trend[i]-temp$se.trend[i]>0){temp$bar.color[i]<-"Red"}
    if (temp$mean.trend[i]+temp$se.trend[i]<0){temp$bar.color[i]<-"Blue"}
  }
  return(temp$bar.color)
}


difference_no3<-difference_no3[complete.cases(difference_no3),]
difference_no3$state<-factor(difference_no3$state,levels=difference_no3$state[order(difference_no3$trend)])
difference_no3$bar_color<-coloring_bars(difference_no3$trend,difference_no3$trend.se)

stream_no3_plot<-ggplot(difference_no3,aes(x=trend,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=trend-trend.se,xmax=trend+trend.se))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("NO3 trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_no3_plot

difference_nh4$state<-factor(difference_nh4$state,levels=difference_nh4$state[order(difference_nh4$trend)])
difference_nh4$bar_color<-coloring_bars(difference_nh4$trend,difference_nh4$trend.se)

stream_nh4_plot<-ggplot(difference_nh4,aes(x=trend,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=trend-trend.se,xmax=trend+trend.se))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("NH4 trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_nh4_plot

difference_tn<-difference_tn[complete.cases(difference_tn),]
difference_tn$state<-factor(difference_tn$state,levels=difference_tn$state[order(difference_tn$trend)])
difference_tn$bar_color<-coloring_bars(difference_tn$trend,difference_tn$trend.se)


stream_tn_plot<-ggplot(difference_tn,aes(x=trend,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=trend-trend.se,xmax=trend+trend.se))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TN trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_tn_plot

difference_tp$state<-factor(difference_tp$state,levels=difference_tp$state[order(difference_tp$trend)])
difference_tp$bar_color<-coloring_bars(difference_tp$trend,difference_tp$trend.se)

stream_tp_plot<-ggplot(difference_tp,aes(x=trend,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=trend-trend.se,xmax=trend+trend.se))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TP trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_tp_plot

jpeg(filename="./figures/all_streams_medians.jpeg",units="in",res=600,width=16,height=12)
plot_grid(stream_tn_plot,stream_tp_plot,stream_no3_plot,stream_nh4_plot,ncol=4,labels="AUTO",label_x=0.9,label_y=0.95)
dev.off()

#############

difference_tn_lake<-difference_tn_lake[complete.cases(difference_tn_lake),]
difference_tn_lake$state<-factor(difference_tn_lake$state,levels=difference_tn_lake$state[order(difference_tn_lake$trend)])
difference_tn_lake$bar_color<-coloring_bars(difference_tn_lake$trend,difference_tn_lake$trend.se)

lake_tn_plot<-ggplot(difference_tn_lake,aes(x=trend,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=trend-trend.se,xmax=trend+trend.se))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TN trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
lake_tn_plot

difference_tp_lake$state<-factor(difference_tp_lake$state,levels=difference_tp_lake$state[order(difference_tp_lake$trend)])
difference_tp_lake$bar_color<-coloring_bars(difference_tp_lake$trend,difference_tp_lake$trend.se)


lake_tp_plot<-ggplot(difference_tp_lake,aes(x=trend,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=trend-trend.se,xmax=trend+trend.se))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TP trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
lake_tp_plot


jpeg(filename="./figures/all_lakes_median.jpeg",units="in",res=600,width=8,height=12)
plot_grid(lake_tn_plot,lake_tp_plot,ncol=2,labels="AUTO",label_x=0.9,label_y=0.95)
dev.off()


####################### Median trends vs. mean trends

difference_no3.mean <- read.csv("./data/clean_data/difference_no3.csv")
difference_nh4.mean <- read.csv("./data/clean_data/difference_nh4.csv")
difference_tn.mean <- read.csv("./data/clean_data/difference_tn.csv")
difference_tp.mean <- read.csv("./data/clean_data/difference_tp.csv")
difference_tn_lake.mean <- read.csv("./data/clean_data/difference_tn_lake.csv")
difference_tp_lake.mean <- read.csv("./data/clean_data/difference_tp_lake.csv")

difference_no3.mean$type<-"stream no3"
difference_nh4.mean$type<-"stream nh4"
difference_tn.mean$type<-"stream TN"
difference_tp.mean$type<-"stream TP"
difference_tn_lake.mean$type<-"Lake TN"
difference_tp_lake.mean$type<-"Lake TP"

mean_trends<-bind_rows(difference_no3.mean,difference_nh4.mean,difference_tn.mean,
                       difference_tp.mean,difference_tp.mean,difference_tn_lake.mean,
                       difference_tp_lake.mean)



compare_mm<-merge(median_trends,mean_trends,by=c("type","state"))


compare_facet<-ggplot(compare_mm,aes(x=SLRW_Trend_Estimate,y=trend))+geom_point()+geom_smooth(method="lm",se=FALSE)+
  xlab(expression("Mean trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab(expression("Median trend ("*mu*g~L^-1~"year"^-1*")"))+
  facet_wrap(~type)

jpeg(filename="./figures/compare_trend_methods.jpeg",units="in",res=600,width=8,height=8)
compare_facet
dev.off()



model_summaries<-data.frame()
for (i in 1:6){
  one_type<-compare_mm[compare_mm$type==unique(compare_mm$type)[i],]
  model<-lm(trend~SLRW_Trend_Estimate,one_type)  
  sum_model<-summary(model)  
  
  output<-data.frame(type=unique(compare_mm$type)[i],intercept=sum_model$coefficients[1,1],
                     intercept.se=sum_model$coefficients[1,2],slope=sum_model$coefficients[2,1],
                     slope.se=sum_model$coefficients[2,2],r.squared=sum_model$r.squared)
  
  model_summaries<-rbind(model_summaries,output)
}

write.csv(model_summaries,file="./results/compare_median_mean_trends.csv")
