library(tidyverse)
library(here)
library(cowplot)
library(lme4)
library(car)
library(MuMIn)
library(lmSupport)
library(viridis)
theme_set(theme_cowplot())
root_path <- here()

#read in data

#fix this first file - does not have expected info
subj_dist_long <- read.csv(here(root_path,"analysis","cogsci2020","processed_data","Grid_subject_distance_item_pairs.csv"))
ratings_pairs <- read.csv(here(root_path,"analysis","cogsci2020","processed_data","ratings_item_pairs.csv"))

#rating pair correlations
cor(ratings_pairs[,c("dist_valence","dist_arousal","dist_pos","dist_neg")])

#  average distance item pair by group
dist_long_byGroup <- subj_dist_long %>%
  group_by(age_bin,age_group,items,sort) %>%
  summarize(N=n(),average_dist=mean(dist),ci_dist=qt(0.975, N-1)*sd(dist,na.rm=T)/sqrt(N))
#join with rating pairs
dist_long_byGroup <- dist_long_byGroup %>%
  left_join(ratings_pairs)

#join with rating pairs: Tina added this in 3-18-20 to make code below work
subj_dist_long <- subj_dist_long %>%
  left_join(ratings_pairs)



#### subject level analysis valence, arousal, emotion category ####
compute_AIC_decrement_Valence_Arousal_EC <- function(data,age_category) {
  #fit models
  full_m <- lmer(dist~1+dist_valence+dist_arousal+emotion_pair_same+(1+dist_valence+dist_arousal+emotion_pair_same|subject),data=filter(data,age_bin==age_category))
  minusV_m <- lmer(dist~1+dist_arousal+emotion_pair_same+(1+dist_arousal+emotion_pair_same|subject),data=filter(data,age_bin==age_category))
  minusA_m <- lmer(dist~1+dist_valence+emotion_pair_same+(1+dist_valence+emotion_pair_same|subject),data=filter(data,age_bin==age_category))
  minusEC_m <- lmer(dist~1+dist_valence+dist_arousal+(1+dist_valence+dist_arousal|subject),data=filter(data,age_bin==age_category))
  full_model_AIC=AIC(full_m)
  minus_models=c(AIC(minusV_m),AIC(minusA_m),AIC(minusEC_m))
  minus_models_name=c("minus_valence","minus_arousal","minus_EC")
  temp <- data_frame(
    age_bin=age_category,
    model_type="valence_arousal_ec",
    full_model_AIC=full_model_AIC,
    minus_model_name=minus_models_name,
    minus_model_AIC=minus_models) %>%
    mutate(AIC_decrement=minus_model_AIC-full_model_AIC)
  return(temp)
}



d <- compute_AIC_decrement_Valence_Arousal_EC(subj_dist_long,"adults")



age_bins <- as.character(unique(subj_dist_long$age_bin))
aic_v_a_ec <- data.frame()
for (age_bin in age_bins) {
  temp <- compute_AIC_decrement_Valence_Arousal_EC(subj_dist_long,age_bin)
  aic_v_a_ec <- bind_rows(aic_v_a_ec,temp)
}

ggplot(aic_v_a_ec,aes(minus_model_name,AIC_decrement,  fill=minus_model_name))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  scale_fill_brewer(palette="Set1",
                    name = "Model Predictor",
                    limits=c("minus_valence","minus_arousal","minus_EC"),
                    breaks=c("minus_valence","minus_arousal","minus_EC"),
                    labels=c("Valence","Arousal","Same Emotion Category"))+
  scale_x_discrete(
    limits=c("minus_valence","minus_arousal","minus_EC"),
    breaks=c("minus_valence","minus_arousal","minus_EC"),
    labels=c("Valence","Arousal","Same Emotion Category"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("AIC Fit Decrement")+
  facet_wrap(~age_bin)


# library(boot)
# foo <- boot(filter(dist_long_byGroup,age_bin=="adults"),function(data,indices)
#   summary(lm(average_dist~1+dist_valence+dist_arousal,data[indices,]))$r.squared,R=10000)
# foo$t0
# quantile(foo$t,c(0.025,0.975))

full_m <- lmer(dist~1+dist_valence+dist_arousal+emotion_pair_same+(1+dist_valence+dist_arousal+emotion_pair_same|subject),data=filter(subj_dist_long,age_bin=="adults"))
minusV_m <- lmer(dist~1+dist_arousal+emotion_pair_same+(1+dist_arousal+emotion_pair_same|subject),data=filter(subj_dist_long,age_bin=="adults"))
minusA_m <- lmer(dist~1+dist_valence+emotion_pair_same+(1+dist_valence+emotion_pair_same|subject),data=filter(subj_dist_long,age_bin=="adults"))
minusEC_m <- lmer(dist~1+dist_valence+dist_arousal+(1+dist_valence+dist_arousal|subject),data=filter(subj_dist_long,age_bin=="adults"))
full_model_AIC <- AIC(full_m)
minus_models <- c(AIC(minusV_m),AIC(minusA_m),AIC(minusEC_m))
minus_models_name <- c("minus_valence","minus_arousal","minus_EC")
temp <- data_frame(
  age_bin="adults",
  model_type="valence_arousal_ec",
  full_model_AIC=full_model_AIC,
  minus_model_name=minus_models_name,
  minus_model_AIC=minus_models) %>%
  mutate(AIC_decrement=minus_model_AIC-full_model_AIC)

ggplot(temp,aes(minus_model_name,AIC_decrement,  fill=minus_model_name))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  scale_fill_brewer(palette="Set1",
    name = "Model Predictor",
    limits=c("minus_valence","minus_arousal","minus_EC"),
    breaks=c("minus_valence","minus_arousal","minus_EC"),
    labels=c("Valence","Arousal","Same Emotion Category"))+
  scale_x_discrete(
    limits=c("minus_valence","minus_arousal","minus_EC"),
    breaks=c("minus_valence","minus_arousal","minus_EC"),
    labels=c("Valence","Arousal","Same Emotion Category"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
    ylab("AIC Fit Decrement")
  



#### predicting from valence, arousal, emotion category ####
#kids, 3 to 4
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 4 to 5
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 5 to 6
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 6 to 7
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m)
vif(m)
modelEffectSizes(m)

#adults
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m)
vif(m)
modelEffectSizes(m)

#### predicting from pos/neg, arousal, emotion category ####
# formula for subject-wise model
# m <- lmer(dist~dist_valence+(1+dist_valence|subject),data=filter(subj_dist_long,age_bin=="6 to 7"))
# summary(m)
# r.squaredGLMM(m)

#kids, 3 to 4
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 4 to 5
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 5 to 6
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 6 to 7
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m)
vif(m)
modelEffectSizes(m)

#adults
# m <- lm(average_dist~dist_valence+dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="adults"))
# vif(m)
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m)
vif(m)
modelEffectSizes(m)

# vif(m)
m <- lm(average_dist~1+dist_valence,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m)
modelEffectSizes(m)

m <- lm(average_dist~1+dist_valence+dist_arousal,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m)
modelEffectSizes(m)

library(boot)
foo <- boot(filter(dist_long_byGroup,age_bin=="adults"),function(data,indices)
  summary(lm(average_dist~1+dist_valence+dist_arousal,data[indices,]))$r.squared,R=10000)
foo$t0
quantile(foo$t,c(0.025,0.975))

compute_RSQ_decrement_Valence_Arousal_EC <- function(data,age_category) {
  #fit models
  full_m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(data,age_bin==age_category))
  minusV_m <- lm(average_dist~dist_arousal+emotion_pair_same,data=filter(data,age_bin==age_category))
  minusA_m <- lm(average_dist~dist_valence+emotion_pair_same,data=filter(data,age_bin==age_category))
  minusEC_m <- lm(average_dist~dist_valence+dist_arousal,data=filter(data,age_bin==age_category))
  full_model_rsq <- summary(full_m)$r.squared
  minus_models_rsq=c(summary(minusV_m)$r.squared,summary(minusA_m)$r.squared,summary(minusEC_m)$r.squared)
  minus_models_name=c("minus_valence","minus_arousal","minus_EC")
  temp <- data_frame(
    age_bin=age_category,
    model_type="valence_arousal_ec",
    full_model_rsq=full_model_rsq,
    minus_model_name=minus_models_name,
    minus_model_rsq=minus_models_rsq) %>%
    mutate(Rsq_decrement=full_model_rsq-minus_model_rsq)
  return(temp)
}
compute_deltaRSQ_Valence_Arousal_EC <- function(data,age_category) {
  #fit models
  full_m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(data,age_bin==age_category))
  full_model_rsq <- summary(full_m)$r.squared
  delta_rsq=modelEffectSizes(full_m)$Effects[2:4,4]
  delta_rsq_ci=list(
    bootstrap_deltarsqr_ci(
      filter(data,age_bin==age_category),
      "average_dist~1+dist_valence+dist_arousal+emotion_pair_same",
      2),
    bootstrap_deltarsqr_ci(
      filter(data,age_bin==age_category),
      "average_dist~1+dist_valence+dist_arousal+emotion_pair_same",
      3),
    bootstrap_deltarsqr_ci(
      filter(data,age_bin==age_category),
      "average_dist~1+dist_valence+dist_arousal+emotion_pair_same",
      4)
  )
  ci_lower_list <- c(delta_rsq_ci[[1]][1],delta_rsq_ci[[2]][1],delta_rsq_ci[[3]][1])
  ci_upper_list <- c(delta_rsq_ci[[1]][2],delta_rsq_ci[[2]][2],delta_rsq_ci[[3]][2])
                 
  minus_models_name=c("minus_valence","minus_arousal","minus_EC")
  temp <- data_frame(
    age_bin=age_category,
    model_type="valence_arousal_ec",
    full_model_rsq=full_model_rsq,
    minus_model_name=minus_models_name,
    delta_rsq=delta_rsq,
    delta_rsq_lower=ci_lower_list,
    delta_rsq_upper=ci_upper_list) 
  return(temp)
}
compute_AICLM_decrement_Valence_Arousal_EC <- function(data,age_category) {
  #fit models
  full_m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(data,age_bin==age_category))
  minusV_m <- lm(average_dist~dist_arousal+emotion_pair_same,data=filter(data,age_bin==age_category))
  minusA_m <- lm(average_dist~dist_valence+emotion_pair_same,data=filter(data,age_bin==age_category))
  minusEC_m <- lm(average_dist~dist_valence+dist_arousal,data=filter(data,age_bin==age_category))
  full_model_aic <- AIC(full_m)
  minus_models_aic=c(AIC(minusV_m),AIC(minusA_m),AIC(minusEC_m))
  minus_models_name=c("minus_valence","minus_arousal","minus_EC")
  temp <- data_frame(
    age_bin=age_category,
    model_type="valence_arousal_ec",
    full_model_aic=full_model_aic,
    minus_model_name=minus_models_name,
    minus_model_aic=minus_models_aic) %>%
    mutate(AIC_decrement=minus_model_aic-full_model_aic)
  return(temp)
}

age_bins <- as.character(unique(subj_dist_long$age_bin))
rsq_v_a_ec <- data.frame()
aic_v_a_ec <- data.frame()
drsq_v_a_ec <- data.frame()
for (age_bin in age_bins) {
  temp <- compute_RSQ_decrement_Valence_Arousal_EC(dist_long_byGroup,age_bin)
  rsq_v_a_ec <- bind_rows(rsq_v_a_ec,temp)
  temp_aic <- compute_AICLM_decrement_Valence_Arousal_EC(dist_long_byGroup,age_bin)
  aic_v_a_ec <- bind_rows(aic_v_a_ec,temp_aic)
  temp_drsqr <- compute_deltaRSQ_Valence_Arousal_EC(dist_long_byGroup,age_bin)
  drsq_v_a_ec <- bind_rows(drsq_v_a_ec,temp_drsqr)
}

ggplot(drsq_v_a_ec,aes(minus_model_name,delta_rsq,  fill=minus_model_name))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  geom_errorbar(aes(ymin=delta_rsq_lower,ymax=delta_rsq_upper),width=0.1)+
  scale_fill_brewer(palette="Set1",
                    name = "Model Predictor",
                    limits=c("minus_valence","minus_arousal","minus_EC"),
                    breaks=c("minus_valence","minus_arousal","minus_EC"),
                    labels=c("Valence","Arousal","Same Emotion Category"))+
  scale_x_discrete(
    limits=c("minus_valence","minus_arousal","minus_EC"),
    breaks=c("minus_valence","minus_arousal","minus_EC"),
    labels=c("Valence","Arousal","Same Emotion Category"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Delta R Squared")+
  facet_wrap(~age_bin,nrow=1)+
  theme(legend.position=c(0.1,0.8))
ggsave(here(root_path,"analysis","cogsci2020","plots","drsqr_dimensions_valence_arousal_ec.jpg"),width=7,height=7)

ggplot(rsq_v_a_ec,aes(minus_model_name,Rsq_decrement,  fill=minus_model_name))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  scale_fill_brewer(palette="Set1",
                    name = "Model Predictor",
                    limits=c("minus_valence","minus_arousal","minus_EC"),
                    breaks=c("minus_valence","minus_arousal","minus_EC"),
                    labels=c("Valence","Arousal","Same Emotion Category"))+
  scale_x_discrete(
    limits=c("minus_valence","minus_arousal","minus_EC"),
    breaks=c("minus_valence","minus_arousal","minus_EC"),
    labels=c("Valence","Arousal","Same Emotion Category"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Fit Decrement")+
  facet_wrap(~age_bin,nrow=1)+
  theme(legend.position=c(0.1,0.8))

ggplot(aic_v_a_ec,aes(minus_model_name,AIC_decrement,  fill=minus_model_name))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  scale_fill_brewer(palette="Set1",
                    name = "Model Predictor",
                    limits=c("minus_valence","minus_arousal","minus_EC"),
                    breaks=c("minus_valence","minus_arousal","minus_EC"),
                    labels=c("Valence","Arousal","Same Emotion Category"))+
  scale_x_discrete(
    limits=c("minus_valence","minus_arousal","minus_EC"),
    breaks=c("minus_valence","minus_arousal","minus_EC"),
    labels=c("Valence","Arousal","Same Emotion Category"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("AIC Fit Decrement")+
  facet_wrap(~age_bin,nrow=1)+
  theme(legend.position=c(0.1,0.8))

bootstrap_rsqr_ci <- function(data, model_formula,samples=10000){
  library(boot)
  foo <- boot(data,function(data,indices)
    summary(lm(model_formula,data[indices,]))$r.squared,R=samples)
  foo$t0
  quantile(foo$t,c(0.025,0.975))
}

bootstrap_deltarsqr_ci <- function(data, model_formula,predict_num,samples=10000){
  library(boot)
  foo <- boot(data,function(data,indices)
    modelEffectSizes(lm(model_formula,data[indices,]))$Effects[predict_num,4],R=samples)
  foo$t0
  quantile(foo$t,c(0.025,0.975))
}

t <- bootstrap_rsqr_ci(filter(dist_long_byGroup,age_bin=="adults"),
                 "average_dist~1+dist_valence+dist_arousal+emotion_pair_same")
t <- bootstrap_deltarsqr_ci(filter(dist_long_byGroup,age_bin=="adults"),
                       "average_dist~1+dist_valence+dist_arousal+emotion_pair_same",
                       2)

compute_deltaRSQ_model <- function(data,age_category, predictors, dv,samples=10000) {
  prediction_formula <- paste0(predictors,sep="",collapse="+")
  model_formula <- paste(dv,"~", prediction_formula,sep=" ")
  print(model_formula)
  #fit models
  full_m <- lm(model_formula,data=filter(data,age_bin==age_category))
  full_model_rsq <- summary(full_m)$r.squared
  delta_rsq=modelEffectSizes(full_m)$Effects[2:(length(predictors)+1),4]
  delta_rsq_ci=list()
  for (i in 1:length(predictors)) {
    delta_rsq_ci[[i]] <-  bootstrap_deltarsqr_ci(
      filter(data,age_bin==age_category),
      model_formula,
      i+1,
      samples=samples)
  }
  ci_lower_list <- c()
  ci_upper_list <- c()
  
  for (j in 1:length(predictors)) {
    ci_lower_list <- c(ci_lower_list,delta_rsq_ci[[j]][1])
    ci_upper_list <- c(ci_upper_list,delta_rsq_ci[[j]][2])
  }
  
  minus_models_name=predictors
  
  temp <- data_frame(
    age_bin=age_category,
    full_model_rsq=full_model_rsq,
    minus_model_name=minus_models_name,
    delta_rsq=delta_rsq,
    delta_rsq_lower=ci_lower_list,
    delta_rsq_upper=ci_upper_list) 
  return(temp)
}

set.seed(100)


#### valence, arousal, emotion category ####
age_bins <- as.character(unique(subj_dist_long$age_bin))
drsq_v_a_ec <- data.frame()
for (age_bin in age_bins) {
  temp_drsqr <- compute_deltaRSQ_model(dist_long_byGroup,age_bin,predictors=c("dist_valence","dist_arousal","emotion_pair_same"),dv="average_dist")
  drsq_v_a_ec <- bind_rows(drsq_v_a_ec,temp_drsqr)
}
write.csv(drsq_v_a_ec,here(root_path,"analysis","cogsci2020","processed_data","drsqr_v_a_ec.csv"),row.names=F)

pA <- ggplot(drsq_v_a_ec,aes(minus_model_name,delta_rsq,  fill=minus_model_name))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  geom_errorbar(aes(ymin=delta_rsq_lower,ymax=delta_rsq_upper),width=0.1)+
  # scale_fill_brewer(palette="Set2",
  #                   name = "Model Predictor",
  #                   limits=c("dist_valence","dist_arousal","emotion_pair_same"),
  #                   breaks=c("dist_valence","dist_arousal","emotion_pair_same"),
  #                   labels=c("Valence","Arousal","Same Emotion Category"))+
  scale_fill_viridis_d(
                    name = "Model Predictor",
                    limits=c("dist_valence","dist_arousal","emotion_pair_same"),
                    breaks=c("dist_valence","dist_arousal","emotion_pair_same"),
                    labels=c("Valence","Arousal","Same Emotion Category"))+
  scale_x_discrete(
    limits=c("dist_valence","dist_arousal","emotion_pair_same"),
    breaks=c("dist_valence","dist_arousal","emotion_pair_same"),
    labels=c("Valence","Arousal","Same Emotion Category"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18),
        legend.text =element_text(size=20),
        legend.title =element_text(size=22,face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Delta R Squared")+
  facet_wrap(~age_bin,nrow=1)+
  theme(legend.position=c(0.1,0.8))
ggsave(here(root_path,"analysis","cogsci2020","plots","drsqr_dimensions_valence_arousal_ec.jpg"),width=7,height=7)


pB <- ggplot(unique(select(drsq_v_a_ec,age_bin,full_model_rsq)),aes(age_bin,full_model_rsq,  fill=age_bin))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Model R Squared")+
  xlab("Age Group")+
theme(legend.position=c(0.1,0.8))
plot_grid(pB,pA)
ggsave(here(root_path,"analysis","cogsci2020","plots","drsqr_dimensions_valence_arousal_ec_wModelRSquared.jpg"),width=12,height=7)


#### pos, neg, arousal, emotion category ####
age_bins <- as.character(unique(subj_dist_long$age_bin))
drsq_pn_a_ec <- data.frame()
for (age_bin in age_bins) {
  temp_drsqr <- compute_deltaRSQ_model(dist_long_byGroup,age_bin,predictors=c("dist_pos","dist_neg","dist_arousal","emotion_pair_same"),dv="average_dist")
  drsq_pn_a_ec <- bind_rows(drsq_pn_a_ec,temp_drsqr)
}
write.csv(drsq_pn_a_ec,here(root_path,"analysis","cogsci2020","processed_data","drsqr_pn_a_ec.csv"),row.names=F)


pA <- ggplot(drsq_pn_a_ec,aes(minus_model_name,delta_rsq,  fill=minus_model_name))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  geom_errorbar(aes(ymin=delta_rsq_lower,ymax=delta_rsq_upper),width=0.1)+
  scale_fill_brewer(palette="Set1",
                    name = "Model Predictor",
                    limits=c("dist_pos","dist_neg","dist_arousal","emotion_pair_same"),
                    breaks=c("dist_pos","dist_neg","dist_arousal","emotion_pair_same"),
                    labels=c("Positive","Negative","Arousal","Same Emotion Category"))+
  scale_x_discrete(
    limits=c("dist_pos","dist_neg","dist_arousal","emotion_pair_same"),
    breaks=c("dist_pos","dist_neg","dist_arousal","emotion_pair_same"),
    labels=c("Positive","Negative","Arousal","Same Emotion Category"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Delta R Squared")+
  facet_wrap(~age_bin,nrow=1)+
  theme(legend.position=c(0.1,0.8))
ggsave(here(root_path,"analysis","cogsci2020","plots","drsqr_dimensions_posneg_arousal_ec.jpg"),width=7,height=7)


pB <- ggplot(unique(select(drsq_pn_a_ec,age_bin,full_model_rsq)),aes(age_bin,full_model_rsq,  fill=age_bin))+
  geom_bar(stat="identity",color="black")+
  geom_hline(yintercept=0,color="black")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Model R Squared")+
  xlab("Age Group")+
  theme(legend.position=c(0.1,0.8))
plot_grid(pB,pA)
ggsave(here(root_path,"analysis","cogsci2020","plots","drsqr_dimensions_posneg_arousal_ec_wModelRSquared.jpg"),width=12,height=7)


#### comparing Valence vs. splitting pos+neg dimensions ####

#just valence vs. pos/neg models
#kids, 3 to 4
m_v <- lm(average_dist~dist_valence,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 4 to 5
m_v <- lm(average_dist~dist_valence,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 5 to 6
m_v <- lm(average_dist~dist_valence,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 6 to 7
m_v <- lm(average_dist~dist_valence,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m_pn)
anova(m_v,m_pn)

#adults
m_v <- lm(average_dist~dist_valence,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m_pn)
anova(m_v,m_pn)

#full model comparison
#kids, 3 to 4
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 4 to 5
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 5 to 6
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 6 to 7
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m_pn)
anova(m_v,m_pn)

#adults
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m_pn)
anova(m_v,m_pn)


