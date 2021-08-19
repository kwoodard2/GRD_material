library(MuMIn)
library(lmSupport)
library(viridis)
library(tidyverse)
library(here)
library(cowplot)
library(lme4)
library(car)
library(lmerTest)
library(boot)
theme_set(theme_cowplot())
root_path <- here()

#What is in this data file:
    # lm models predicting distance using valence, arousal, emo_cat, and pos and neg
    # comparison of valence versus pos/neg models
    # uses bootstrap methods to get conf intervals for R^2 values to compare model predictors

#read in data
subj_dist_long <- read.csv(here(root_path,"analysis","paper_2020","processed_data","Grid_subject_distance_item_pairs.csv"))
ratings_pairs <- read.csv(here(root_path,"analysis","paper_2020","processed_data","ratings_item_pairs.csv"))

# dimension rating correlations
ratings_pairs%>% select(dist_valence,dist_arousal,dist_pos,dist_neg) %>% cor()
ratings_pairs %>% filter(sort=="Sort1") %>% select(dist_valence,dist_arousal,dist_pos,dist_neg) %>% cor()
ratings_pairs %>% filter(sort=="Sort2") %>% select(dist_valence,dist_arousal,dist_pos,dist_neg) %>% cor()


# To open previously computed bootstrap CIs and delta R^2 values:
#drsq_v_a_ec <- read.csv(here(root_path,"analysis","paper_2020","processed_data","drsqr_v_a_ec.csv"))
#drsq_pn_a_ec <- read.csv(here(root_path,"analysis","paper_2020","processed_data","drsqr_pn_a_ec.csv"))


#select variables of interest
subj_dist_long <- subj_dist_long %>% select(participant,sort,subject,Age,Gender,age_bin,age_group,
                                            dist,items,item1,item2,image_cat_1,image_cat_2,image_tax_cat_1,
                                            image_tax_cat_2,emotion_pair_same,shared_category,category_pair)
subj_dist_long <- subj_dist_long %>%
  left_join(ratings_pairs)

# average distance item pair by group
dist_long_byGroup <- subj_dist_long %>%
  group_by(age_bin,age_group,items,sort) %>%
  summarize(N=n(),average_dist=mean(dist),ci_dist=qt(0.975, N-1)*sd(dist,na.rm=T)/sqrt(N))
dist_long_byGroup <- dist_long_byGroup %>%
  left_join(ratings_pairs)

####################################################################
# Do Valence, Arousal, and Positivity/Negativity Predict Behavior? #
####################################################################
#rating pair correlations
cor(ratings_pairs[,c("dist_valence","dist_arousal","dist_pos","dist_neg")])

dimension_data <- subj_dist_long %>% filter(sort!="Practice") %>% 
  select(subject, sort, Age, age_bin, age_group, Gender, dist, items, dist_valence, dist_arousal, dist_pos, dist_neg) %>% 
  mutate(subject= as.factor(subject), Gender=as.factor(Gender), age_group = as.factor(age_group), age_bin = as.factor(age_bin), sort=as.factor(sort)) %>% 
  mutate(sortC=ifelse(sort=="Sort1",-0.5,0.5))
# center dimensions
dimension_data <- dimension_data %>% mutate(dist_valence=dist_valence-mean(dist_valence)) %>% 
  mutate(dist_arousal=dist_arousal-mean(dist_arousal)) %>% 
  mutate(dist_pos=dist_pos-mean(dist_pos)) %>% 
  mutate(dist_neg=dist_neg-mean(dist_neg))
dimension_data <- dimension_data %>% mutate(age_groupC=ifelse(age_group=="kids",-0.5,0.5))


# Age Group Analyses: Adults versus Children

## VALENCE
m1 <- lmer(dist~dist_valence*age_groupC+(dist_valence|subject)+(1|items),data=dimension_data)
summary(m1)
confint(m1, method="Wald")
anova(m1) #yields F and p-values when lmerTest is loaded (Satterthwaite by default)

## AROUSAL
m2 <- lmer(dist~dist_arousal*age_groupC+(1+dist_arousal|subject)+(1|items),data=dimension_data)
summary(m2)
confint(m2, method="Wald")
anova(m2)

## POSITIVITY
m3 <- lmer(dist~dist_pos*age_groupC+(1+dist_pos|subject)+(1|items),data=dimension_data)
summary(m3)
confint(m3, method="Wald")
anova(m3)

## NEGATIVITY
m4 <- lmer(dist~dist_neg*age_groupC+(1+dist_neg|subject)+(1|items),data=dimension_data)
summary(m4)
confint(m4, method="Wald")
anova(m4)

# Children Only Analyses: Age is continuous
dimension_data <- dimension_data %>%mutate(AgeC=Age-mean(Age,na.rm=T))
dimension_data <- dimension_data %>% filter(age_group == "kids")

m1 <- lmer(dist~dist_valence*AgeC+(1+dist_valence|subject)+(1|items),data=dimension_data)
summary(m1)
confint(m1, method="Wald")
anova(m1)

#no longer converging on 12-9; I added in an optimzer to help with convergence
m2 <- lmer(dist~dist_arousal*AgeC+(1+dist_arousal|subject)+(1|items),data=dimension_data, 
           control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2)
confint(m2, method="Wald")
anova(m2)

m3 <- lmer(dist~dist_pos*AgeC+(1+dist_pos|subject)+(1|items),data=dimension_data)
summary(m3)
confint(m3, method="Wald")
anova(m3)

m4 <- lmer(dist~dist_neg*AgeC+(1+dist_neg|subject)+(1|items),data=dimension_data)
summary(m4)
confint(m4, method="Wald")
anova(m4)


####################################################################
# How do dimensions and emotion category compare with one another? #
####################################################################

#### Valence Models ####
#kids, 3
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 4
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 5
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 6
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m)
vif(m)
modelEffectSizes(m)

#adults
m <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m)
vif(m)
modelEffectSizes(m)

#### Positivity/Negativity Models ####

#kids, 3
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 4
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 5
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m)
vif(m)
modelEffectSizes(m)

#kids, 6
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="6 to 7"))
summary(m)
vif(m)
modelEffectSizes(m)

#adults
m <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="adults"))
summary(m)
vif(m)
modelEffectSizes(m)

############################################################
#### Comparing Valence vs. Positivity / Negativity ####
############################################################

#kids, 3
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="3 to 4"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 4
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="4 to 5"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 5
m_v <- lm(average_dist~dist_valence+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m_v)
m_pn <- lm(average_dist~dist_pos+dist_neg+dist_arousal+emotion_pair_same,data=filter(dist_long_byGroup,age_bin=="5 to 6"))
summary(m_pn)
anova(m_v,m_pn)

#kids, 6
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

#####################
# Model Comparisons #
#####################

# Delta R-squared decrement
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

bootstrap_deltarsqr_ci <- function(data, model_formula,predict_num,samples=10000){
  library(boot)
  foo <- boot(data,function(data,indices)
    modelEffectSizes(lm(model_formula,data[indices,]))$Effects[predict_num,4],R=samples)
  foo$t0
  quantile(foo$t,c(0.025,0.975))
}


#########################################################
#### Graphs: valence, arousal, emotion category      ####
#########################################################
set.seed(100)
age_bins <- as.character(unique(subj_dist_long$age_bin))
drsq_v_a_ec <- data.frame()
for (age_bin in age_bins) {
  temp_drsqr <- compute_deltaRSQ_model(dist_long_byGroup,age_bin,predictors=c("dist_valence","dist_arousal","emotion_pair_same"),dv="average_dist")
  drsq_v_a_ec <- bind_rows(drsq_v_a_ec,temp_drsqr)
}
write.csv(drsq_v_a_ec,here(root_path,"analysis","paper_2020","processed_data","drsqr_v_a_ec.csv"),row.names=F)

#clean labels for plot
age_names <- list(
  '3 to 4'="3-year-olds",
  '4 to 5'="4-year-olds",
  '5 to 6'="5-year-olds",
  '6 to 7'="6-year-olds",
  'adults'="adults")
age_labeller <- function(variable,value){
  return(age_names[value])}
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
                    name = " ", #Model Predictor
                    limits=c("dist_valence","dist_arousal","emotion_pair_same"),
                    breaks=c("dist_valence","dist_arousal","emotion_pair_same"),
                    labels=c("Valence","Arousal","Emotion Category"))+
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
  theme(#axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Delta R Squared")+
  xlab("Age Group (years)") +
  facet_wrap(~age_bin,nrow=1, labeller = age_labeller, strip.position = "bottom")+
  theme(legend.position=c(0.1,0.8))
pA
ggsave(here(root_path,"analysis","paper_2020","plots","drsqr_dimensions_valence_arousal_ec.png"),width=8,height=8)

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
ggsave(here(root_path,"analysis","paper_2020","plots","drsqr_dimensions_valence_arousal_ec_wModelRSquared.png"),width=15,height=7)


 #########################################################
####  Graphs: pos, neg, arousal, emotion category    ####
#########################################################

age_bins <- as.character(unique(subj_dist_long$age_bin))
drsq_pn_a_ec <- data.frame()
for (age_bin in age_bins) {
  temp_drsqr <- compute_deltaRSQ_model(dist_long_byGroup,age_bin,predictors=c("dist_pos","dist_neg","dist_arousal","emotion_pair_same"),dv="average_dist")
  drsq_pn_a_ec <- bind_rows(drsq_pn_a_ec,temp_drsqr)
}
write.csv(drsq_pn_a_ec,here(root_path,"analysis","paper_2020","processed_data","drsqr_pn_a_ec.csv"),row.names=F)

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
  facet_wrap(~age_bin,nrow=1,labeller = age_labeller, strip.position = "bottom")+
  theme(legend.position=c(0.1,0.8))
pA
ggsave(here(root_path,"analysis","paper_2020","plots","drsqr_dimensions_posneg_arousal_ec.png"),width=8,height=8)

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
ggsave(here(root_path,"analysis","paper_2020","plots","drsqr_dimensions_posneg_arousal_ec_wModelRSquared.png"),width=15,height=7)
