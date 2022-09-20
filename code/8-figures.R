library(ggplot2)
library(cowplot)
library(Rmisc)

####################### Plotting policy variable ranges

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

state_tmdl_plot<-ggplot(tmdl,aes(x=tmdl.sites.sq.km,y=states))+geom_point(size=2)+
  theme_bw()+xlab(expression("TMDL sites visited (2-year-cycle"^-1~"km"^-2*")"))+
  ylab("")+theme(text = element_text(size=20))

state_319_plot<-ggplot(s319,aes(x=total.money.sq.km,y=states))+geom_point(size=2)+
  theme_bw()+xlab(expression("319 Dollars (Dollars"~"km"^-2*")"))+
  ylab("")+theme(text = element_text(size=20))

state_nutrient_criteria <- ggplot(nutrient.criteria.melt %>% 
                                    mutate(variable = recode(variable, "lake.p.criteria"="Lake P", "lake.n.criteria"="Lake N", "stream.n.criteria"="Stream N", "stream.p.criteria"="Stream P")),
                                  aes(x=value,y=states))+
  geom_bar(stat="identity", position = position_dodge())+
  theme_bw()+
  facet_grid(~variable) + 
  ylab("") + 
  xlab("Nutrient Criteria Score")+ 
  theme(text = element_text(size=20))

jpeg(filename="./figures/predictors.jpeg",units="in",res=600,width=17,height=12)
plot_grid(state_319_plot,state_tmdl_plot,state_nutrient_criteria,ncol=3,labels="AUTO", label_size = 24)
dev.off()

########################Plotting coefficients of loading variable.names

loading<-read.csv("./data/clean_data/loading_variables.csv")

rsq<-loading[loading$variable=="rsq",]
aggregate(value~nutrient_type,rsq,mean) ### median r2 values of models

loading2<-loading[loading$variable!="rsq",]

loading.means<-aggregate(value~variable+nutrient_type,loading2,mean)
loading.lower<-aggregate(value~variable+nutrient_type,loading2,quantile,0.025)
names(loading.lower)[names(loading.lower)=="value"]<-"lower.ci"
loading.upper<-aggregate(value~variable+nutrient_type,loading2,quantile,0.975)
names(loading.upper)[names(loading.upper)=="value"]<-"upper.ci"

loading3<-merge(loading.means,loading.upper,by=c("variable","nutrient_type"))
loading.summary<-merge(loading3,loading.lower,by=c("variable","nutrient_type"))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00")

loading.plot<-ggplot(loading.summary,aes(x=value,y=variable,color=nutrient_type))+
  geom_point(size=4,position=position_dodge(width=0.3), aes(shape=nutrient_type), stroke = 2)+
  geom_errorbarh(aes(xmin=lower.ci,xmax=upper.ci),height=0.001,position=position_dodge(width=0.3), size=1)+
  theme_classic()+
  geom_vline(xintercept = 0,linetype="dashed")+ylab("")+xlab("Parameter Estimate")+
  theme(text = element_text(size = 20),legend.position = c(0.2,0.9),legend.text.align = 0)+
  scale_color_manual(values = cbbPalette,name="",labels=c('Lake TN', 'Lake TP',
                                                          expression("Stream NH"[4]),
                                                          expression("Stream NO"[3]),
                                                          "Stream TN",
                                                          "Stream TP"))+
  scale_shape_manual(values = c(2,16,17,23,15,1),name="",labels=c('Lake TN', 'Lake TP',
                                                                    expression("Stream NH"[4]),
                                                                    expression("Stream NO"[3]),
                                                                    "Stream TN",
                                                                    "Stream TP"))+
  scale_y_discrete(labels=c("urban.delta" = expression(Delta*"Urban"), "urban" = "Urban",
                            "undeveloped" = "Undeveloped","pop.delta"=expression(Delta*"Population"),
                            "fertilizer.delta"=expression(Delta*"Fertilizer"),
                            "feed.delta"=expression(Delta*"Animal feed"),
                            "feed"="Animal Feed","ag.delta"=expression(Delta*"Agriculture"),
                            "ag"="Agriculture"))
loading.plot


jpeg(filename="./figures/loading_coef.jpeg",units="in",res=600,width=8,height=10)
loading.plot
dev.off()


###################### plotting z scored coefficients

zs<-read.csv("./data/clean_data/z_scores_estimates.csv")

z.means<-aggregate(value~policy+nutrient_type,zs,mean)
z.lower<-aggregate(value~policy+nutrient_type,zs,quantile,0.025)
names(z.lower)[names(z.lower)=="value"]<-"lower.ci"
z.upper<-aggregate(value~policy+nutrient_type,zs,quantile,0.975)
names(z.upper)[names(z.upper)=="value"]<-"upper.ci"

zs2<-merge(z.means,z.lower,by=c("nutrient_type","policy"))
z.summary<-merge(zs2,z.upper,by=c("nutrient_type","policy"))

policy.plot<-ggplot(z.summary,aes(x=value,y=nutrient_type,color=policy))+
  geom_point(size=4,position=position_dodge(width=0.3), aes(shape=policy))+
  geom_errorbarh(aes(xmin=lower.ci,xmax=upper.ci),height=0.0001,position=position_dodge(width=0.3), size=1)+
  theme_classic()+
  geom_vline(xintercept = 0,linetype="dashed")+ylab("")+xlab("Parameter Estimate")+
  theme(text = element_text(size = 20),legend.position = c(0.2,0.95))+
  scale_color_manual(values = cbbPalette,name="",labels=c('319 funding', 'Nutrient Criteria',"TMDL site visits"))+
  scale_shape_manual(values=c(15,16,17),name="",labels=c('319 funding', 'Nutrient Criteria',"TMDL site visits"))
policy.plot

jpeg(filename="./figures/policy_coef.jpeg",units="in",res=600,width=8,height=10)
policy.plot
dev.off()

###########

nz<-read.csv("./data/clean_data/non_z_scored_estimates.csv")

summarized.non.z<-aggregate(value~policy+nutrient_type,nz,quantile,c(0.025,0.5,0.975))

write.csv(summarized.non.z,"./results/non_z_scored_estimates_summarized.csv", row.names = F)
