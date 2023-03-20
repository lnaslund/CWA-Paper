library(lubridate)
library(lme4)
library(cowplot)
library(MuMIn)
library(Rmisc)
library(tidyverse)
library(spsurvey)
library(ggridges)
library(ggrepel)
library(reshape2)
library(MuMIn)

set.seed(207)

########### Trend slopes
difference_no3 <- read.csv("./data/clean_data/difference_no3_median.csv")
difference_nh4 <- read.csv("./data/clean_data/difference_nh4_median.csv")
difference_tn <- read.csv("./data/clean_data/difference_tn_median.csv")
difference_tp <- read.csv("./data/clean_data/difference_tp_median.csv")
difference_tn_lake <- read.csv("./data/clean_data/difference_tn_lake_median.csv")
difference_tp_lake <- read.csv("./data/clean_data/difference_tp_lake_median.csv")

########################## Modeling -- loading variables first
loading.alt<-read.csv("./data/predictor_data/alternative_loading_predictors.csv")


#### Stream NO3

stream.no3<-merge(difference_no3,loading.alt,by="state")

stream.no3.global<-lm(trend~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                        fertilizer+fertilizer.delta+feed+feed.delta,stream.no3,na.action = "na.fail")
#options(na.action = "na.fail")
best<-dredge(stream.no3.global)
subset(best,delta<2)
best.stream.no3<-lm(trend~feed+feed.delta,stream.no3)

### Stream NH4
stream.nh4<-merge(difference_nh4,loading.alt,by="state")

stream.nh4.global<-lm(trend~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                        fertilizer+fertilizer.delta+feed+feed.delta,stream.nh4,na.action = "na.fail")
best<-dredge(stream.nh4.global)
subset(best,delta<2)

best.stream.nh4<-lm(trend~feed+fertilizer,data=stream.nh4)

### stream TP
stream.tp<-merge(difference_tp,loading.alt,by="state")

stream.tp.global<-lm(trend~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                        fertilizer+fertilizer.delta+feed+feed.delta,stream.tp,na.action = "na.fail")
best<-dredge(stream.tp.global)
subset(best,delta<2)
best.stream.tp<-lm(trend~ag.delta,data=stream.tp)

### stream TN
stream.tn<-merge(difference_tn,loading.alt,by="state")

stream.tn.global<-lm(trend~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                       fertilizer+fertilizer.delta+feed+feed.delta,stream.tn,na.action = "na.fail")
best<-dredge(stream.tn.global)
subset(best,delta<2)

best.stream.tn<-lm(trend~feed+feed.delta,data=stream.tn)
summary(best.stream.tn)

### lake TP
lake.tp<-merge(difference_tp_lake,loading.alt,by="state")

lake.tp.global<-lm(trend~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                     fertilizer+fertilizer.delta+feed+feed.delta,lake.tp,na.action = "na.fail")
best<-dredge(lake.tp.global)
subset(best,delta<2)
best.lake.tp<-lm(trend~ag+fertilizer.delta+undeveloped,data=lake.tp)
summary(best.lake.tp)



### lake TN
lake.tn<-merge(difference_tn_lake,loading.alt,by="state")

lake.tn.global<-lm(trend~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                     fertilizer+fertilizer.delta+feed+feed.delta,lake.tn,na.action = "na.fail")

best<-dredge(lake.tn.global)
subset(best,delta<2)

best.lake.tn<-lm(trend~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta,lake.tn)
summary(best.lake.tn)

#################################### Fitting policy variables.

state.areas<-read.csv("./data/predictor_data/state_areas.csv")
state.areas$states<-state.areas$State
state.areas$state.jurs.land<-state.areas$state.jurs.land*1000*0.00404686 ## converting from 1000 acres to km2

nutrient_criteria<-read.csv("./data/predictor_data/regulations.csv")
nutrient_criteria_short<-data.frame(states=nutrient_criteria$state,lake.p.criteria=nutrient_criteria$plakes.aggregate,
                                    lake.n.criteria=nutrient_criteria$n.lakes.aggregate,stream.n.criteria=
                                      nutrient_criteria$nstream.aggregate,stream.p.criteria=nutrient_criteria$pstream.aggregate)
nutrient.criteria.melt<-melt(nutrient_criteria_short)

s319<-read.csv("./data/predictor_data/summarized_319.csv") %>% 
  dplyr::rename("states"="State") %>% 
  inner_join(state.areas, by="states") %>% 
  mutate(total.money.sq.km= Total.State.Money/state.jurs.land)
s319$states<-factor(s319$states,levels=s319$states[order(s319$total.money.sq.km)])

tmdl<-read.csv("./data/predictor_data/TMDL_data_summary.csv") %>% inner_join(state.areas,by="states") %>% mutate(tmdl.sites.sq.km=tally/state.jurs.land)
tmdl$states <-factor(tmdl$states,levels=tmdl$states[order(tmdl$tmdl.sites.sq.km)])

####### merge_predictors
loading.pca$states<-loading.pca$state

# TODO: Figure out why all states don't have loading scores
# The ones that don't are AK and HI which aren't included in the NLCD 
# Make this clear in the text if you do show AK and HI in the figures but don't include them in the models
policy.loading <- full_join(tmdl, s319, by =c("states", "State", "total.area.th.acre","federal.land.th.acre", "tribal.land.th.acre", "state.jurs.land")) %>% 
  dplyr::select(-State) %>% 
  full_join(nutrient_criteria_short, by = "states") # %>% 
  #full_join(loading.pca %>% dplyr::select(-State, -state), by = "states") %>% dplyr::rename("state"= "states")

# Why doesn't PA have tmdl data? Not sure, I've dropped it from analysis lower down
policy.loading$tmdl.z<-(policy.loading$tmdl.sites.sq.km-mean(policy.loading$tmdl.sites.sq.km, na.rm=T))/sd(policy.loading$tmdl.sites.sq.km, na.rm=T)
policy.loading$money.z<-(policy.loading$total.money.sq.km-mean(policy.loading$total.money.sq.km))/sd(policy.loading$total.money.sq.km)
policy.loading$lake.criteria.p.z<-(policy.loading$lake.p.criteria-mean(policy.loading$lake.p.criteria))/sd(policy.loading$lake.p.criteria)
policy.loading$lake.criteria.n.z<-(policy.loading$lake.n.criteria-mean(policy.loading$lake.n.criteria))/sd(policy.loading$lake.n.criteria)
policy.loading$stream.n.criteria.z<-(policy.loading$stream.n.criteria-mean(policy.loading$stream.n.criteria))/sd(policy.loading$stream.n.criteria)
policy.loading$stream.p.criteria.z<-(policy.loading$stream.p.criteria-mean(policy.loading$stream.p.criteria))/sd(policy.loading$stream.p.criteria)

policy.loading$state<-policy.loading$states
####################### modeling the effects of policy

# stream nitrate
difference_no3<-left_join(stream.no3, policy.loading, by="state")

stream.nitrate.policy.wlr<-data.frame()

nsims<-10000

for (i in 1:nsims){
  d<-difference_no3
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$trend,d$trend.se)
  

  model.money.wlr<-lm(slope.wlr~feed+feed.delta+total.money.sq.km,data=d)
  
  model.money.wlr.z<-lm(slope.wlr~feed+feed.delta+money.z,data=d)
  
  
  model.criteria.wlr<-lm(slope.wlr~feed+feed.delta+stream.n.criteria,data=d)
  
  model.criteria.wlr.z<-lm(slope.wlr~feed+feed.delta+stream.n.criteria.z,data=d)
  
  
  model.tmdl.wlr<-lm(slope.wlr~feed+feed.delta+tmdl.sites.sq.km,data=d)
  
  model.tmdl.wlr.z<-lm(slope.wlr~feed+feed.delta+tmdl.z,data=d)
  
  
  null.wlr<-lm(slope.wlr~feed+feed.delta,data=d)
  s.null.wlr<-summary(null.wlr)

  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  model.tmdl.wlr.z.summary<-summary(model.tmdl.wlr.z)
  model.money.wlr.z.summary<-summary(model.money.wlr.z)
  model.criteria.wlr.z.summary<-summary(model.criteria.wlr.z)
  

  

  model.tmdl.wlr.summary<-summary(model.tmdl.wlr)
  model.money.wlr.summary<-summary(model.money.wlr)
  model.criteria.wlr.summary<-summary(model.criteria.wlr)
  
  wlr.models.output<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                                ceof=c(coef(model.tmdl.wlr)[4],
                                       coef(model.money.wlr)[4],
                                       coef(model.criteria.wlr)[4]),
                                coef.se=c(model.tmdl.wlr.summary$coefficients[4,2],
                                          model.money.wlr.summary$coefficients[4,2],
                                          model.criteria.wlr.summary$coefficients[4,2]))


  stream.nitrate.policy.wlr<-rbind(stream.nitrate.policy.wlr,wlr.models.output)



}






#### Stream ammonium

stream.ammonium.slopes.predictors<-merge(stream.nh4, policy.loading,by="state")

stream.ammonium.policy.wlr<-data.frame()


for (i in 1:nsims){
  d<-stream.ammonium.slopes.predictors
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$trend,d$trend.se)

  
  model.money.wlr<-lm(slope.wlr~feed+fertilizer+total.money.sq.km,data=d)

  
  model.criteria.wlr<-lm(slope.wlr~feed+fertilizer+stream.n.criteria,data=d)

  
  model.tmdl.wlr<-lm(slope.wlr~feed+fertilizer+tmdl.sites.sq.km,data=d)

  
  null.wlr<-lm(slope.wlr~feed+fertilizer,data=d)

  
  null.wlr.summary<-summary(null.wlr)
  

  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  

  

  

  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[4],
                                coef(model.money.wlr)[4],
                                coef(model.criteria.wlr)[4]),
                         coef.se=c(wlr.tmdl.summary$coefficients[4,2],
                                   wlr.money.summary$coefficients[4,2],
                                   wlr.criteria.summary$coefficients[4,2]))
  stream.ammonium.policy.wlr<-rbind(stream.ammonium.policy.wlr,output.wlr)
  
  

  }




### stream TN

stream.tn.slopes.predictors<-merge(stream.tn,policy.loading,by="state")

stream.tn.policy.slr<-data.frame()
stream.tn.policy.wlr<-data.frame()

stream.tn.model.selction.wlr<-data.frame()
stream.tn.model.selction.slr<-data.frame()

stream.tn.z.scores.slr<-data.frame()
stream.tn.z.scores.wlr<-data.frame()

null.model.fit.stream.tn<-data.frame()
for (i in 1:nsims){
  
  d<-stream.tn.slopes.predictors
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$trend,d$trend.se)

  
  model.money.wlr<-lm(slope.wlr~feed+feed.delta+total.money.sq.km,data=d)

  
  model.criteria.wlr<-lm(slope.wlr~feed+feed.delta+stream.n.criteria,data=d)

  
  model.tmdl.wlr<-lm(slope.wlr~feed+feed.delta+tmdl.sites.sq.km,data=d)

  
  null.wlr<-lm(slope.wlr~feed+feed.delta,data=d)

  
  null.wlr.summary<-summary(null.wlr)
  

  

  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  stream.tn.model.selction.wlr<-rbind(stream.tn.model.selction.wlr,model.selction.wlr)


  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                     coef=c(coef(model.tmdl.wlr)[4],
                            coef(model.money.wlr)[4],
                            coef(model.criteria.wlr)[4]),
                     coef.se=c(wlr.tmdl.summary$coefficients[4,2],
                               wlr.money.summary$coefficients[4,2],
                               wlr.criteria.summary$coefficients[4,2]))
  
  stream.tn.policy.wlr<-rbind(stream.tn.policy.wlr,output.wlr)
  
  
  
  
}



## stream tp

stream.tp.slopes.predictors<-merge(stream.tp, policy.loading,by="state")

stream.tp.policy.wlr<-data.frame()
stream.tp.model.selction.wlr<-data.frame()

stream.tp.policy.slr<-data.frame()
stream.tp.model.selction.slr<-data.frame()

stream.tp.z.scores.slr<-data.frame()
stream.tp.z.scores.wlr<-data.frame()

null.coef.stream.tp<-data.frame()

for (i in 1:nsims){
  
  d<-stream.tp.slopes.predictors
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$trend,d$trend.se)

  
  model.money.wlr<-lm(slope.wlr~fertilizer+fertilizer.delta+total.money.sq.km,data=d)

  
  model.criteria.wlr<-lm(slope.wlr~fertilizer+fertilizer.delta+stream.n.criteria,data=d)

  
  model.tmdl.wlr<-lm(slope.wlr~fertilizer+fertilizer.delta+tmdl.sites.sq.km,data=d)

  
  null.wlr<-lm(slope.wlr~fertilizer+fertilizer.delta,data=d)

  
  null.wlr.summary<-summary(null.wlr)
  

  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[4],
                                coef(model.money.wlr)[4],
                                coef(model.criteria.wlr)[4]),
                         coef.se=c(wlr.tmdl.summary$coefficients[4,2],
                                   wlr.money.summary$coefficients[4,2],
                                   wlr.criteria.summary$coefficients[4,2]))
  
  

  
  
  stream.tp.policy.wlr<-rbind(stream.tp.policy.wlr,output.wlr)

  

  
  
  
}



## lake tp

lake.tp.slopes.policy<-merge(lake.tp,policy.loading,by="state")

lake.tp.policy.slr<-data.frame()
lake.tp.policy.wlr<-data.frame()

lake.tp.model.selction.slr<-data.frame()
lake.tp.model.selction.wlr<-data.frame()

lake.tp.z.scores.slr<-data.frame()
lake.tp.z.scores.wlr<-data.frame()

lake.tp.z.scores.slr<-data.frame()
lake.tp.z.scores.wlr<-data.frame()

null.coef.lake.tp<-data.frame()

for (i in 1:nsims){
  
  d<-lake.tp.slopes.policy
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$trend,d$trend.se)

  

  model.money.wlr<-lm(slope.wlr~ag.delta+feed.delta+undeveloped.delta+total.money.sq.km,data=d)

  
  model.criteria.wlr<-lm(slope.wlr~ag.delta+feed.delta+undeveloped.delta+lake.n.criteria,data=d)

  
  model.tmdl.wlr<-lm(slope.wlr~ag.delta+feed.delta+undeveloped.delta+tmdl.sites.sq.km,data=d)

  
  null.wlr<-lm(slope.wlr~ag.delta+feed.delta+undeveloped.delta+undeveloped,data=d)

  

  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[5],
                                coef(model.money.wlr)[5],
                                coef(model.criteria.wlr)[5]),
                         coef.se=c(wlr.tmdl.summary$coefficients[5,2],
                                   wlr.money.summary$coefficients[5,2],
                                   wlr.criteria.summary$coefficients[5,2]))
  
   lake.tp.policy.wlr<-rbind(lake.tp.policy.wlr,output.wlr)
}



summarySE(lake.tp.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")

## lake tn

lake.tn.slopes.policy<-merge(lake.tn,policy.loading,by="state")

lake.tn.policy.slr<-data.frame()
lake.tn.policy.wlr<-data.frame()

lake.tn.model.selction.slr<-data.frame()
lake.tn.model.selction.wlr<-data.frame()

lake.tn.z.scores.slr<-data.frame()
lake.tn.z.scores.wlr<-data.frame()

lake.tn.z.scores.slr<-data.frame()
lake.tn.z.scores.wlr<-data.frame()

null.coef.lake.tn<-data.frame()

for (i in 1:nsims){
  
  d<-lake.tn.slopes.policy
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$trend,d$trend.se)

  
  
  model.money.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer+total.money.sq.km,data=d)

  model.criteria.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer+lake.n.criteria,data=d)

  model.tmdl.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer+tmdl.sites.sq.km,data=d)
 
  null.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer,data=d)

  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[5],
                                coef(model.money.wlr)[5],
                                coef(model.criteria.wlr)[5]),
                         coef.se=c(wlr.tmdl.summary$coefficients[5,2],
                                   wlr.money.summary$coefficients[5,2],
                                   wlr.criteria.summary$coefficients[5,2]))
  
  lake.tn.policy.wlr<-rbind(lake.tn.policy.wlr,output.wlr)
}



##### merging all the policy data to make plots

######################### z scored predictors

lake.tn.policy.wlr$nutrient_type<-"Lake TN"
lake.tp.policy.wlr$nutrient_type<-"Lake TP"
stream.tn.policy.wlr$nutrient_type<-"Stream TN"
stream.tp.policy.wlr$nutrient_type<-"Stream TP"
stream.nitrate.policy.wlr$nutrient_type<-"Stream Nitrate"
stream.ammonium.policy.wlr$nutrient_type<-"Stream Ammonium"

non_z_summary<-bind_rows(lake.tn.policy.wlr,lake.tp.policy.wlr,stream.tn.policy.wlr,stream.tp.policy.wlr,
                         stream.nitrate.policy.wlr,stream.ammonium.policy.wlr)
non_z_summary_new<-data.frame(policy=non_z_summary$policy,nutreint_type=non_z_summary$nutrient_type,new_value=rnorm(nrow(non_z_summary),non_z_summary$coef,non_z_summary$coef.se))
non_z_melt<-melt(non_z_summary_new)

summarized.non.z<-aggregate(value~policy+nutreint_type,non_z_melt,quantile,c(0.025,0.5,0.975),na.rm=TRUE)

write.csv(summarized.non.z,"./results/non_z_scored_estiamtes_MEDIAN_supplement.csv")



