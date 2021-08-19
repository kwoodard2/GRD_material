library(tidyverse)
library(here)
library(cowplot)
library(lme4)
library(car)
library(lmerTest)
theme_set(theme_cowplot())
root_path <- here()

# This data file contains:
  #plots for emotion category
  #lmer emotion category predicts distance for adults and children
  #lmer emotion category predicts distance for kids only (age continuous)

#read in data
subj_dist_long <- read.csv(here(root_path,"analysis","paper_2020","processed_data","Grid_subject_distance_item_pairs.csv"))

##### Emotion Category: Figure 1 #####

#focus on differences in dist (same v. between categories)
avg_dist_diff_cat_by_subj <- subj_dist_long %>%
  group_by(subject, Age, Gender, age_bin,age_group,sort,category_pair) %>%
  summarize(avg_dist=mean(dist,na.rm=T))  %>%
  ungroup() %>%
  group_by(subject, Age, Gender, age_bin,age_group,sort) %>%
  pivot_wider(names_from=category_pair,values_from=avg_dist) %>%
  mutate(avg_dist_diff=between-within) %>%
  mutate(sort_name=ifelse(sort=="Sort1","Same Individual",ifelse(sort=="Sort2","Different Individuals","Practice"))) %>%
  ungroup()

#average across subjects  by age bin
avg_dist_diff_cat_across_subj <- avg_dist_diff_cat_by_subj %>%
  select(-between,-within) %>%
  group_by(age_bin,age_group,sort,sort_name) %>%
  summarize(N=n(),average_dist=mean(avg_dist_diff,na.rm=T),ci_dist=qt(0.975, N-1)*sd(avg_dist_diff,na.rm=T)/sqrt(N))

#Plot
#clean labels for plot
age_names <- list(
  '3 to 4'="3-year-olds",
  '4 to 5'="4-year-olds",
  '5 to 6'="5-year-olds",
  '6 to 7'="6-year-olds",
  'adults'="adults")
age_labeller <- function(variable,value){
  return(age_names[value])}
p <- ggplot(filter(avg_dist_diff_cat_across_subj, sort!="Practice"),aes(x=sort_name,y=average_dist,fill=age_bin,color=age_bin, linetype=sort_name))+
  geom_errorbar(aes(ymin=average_dist-ci_dist,ymax=average_dist+ci_dist),width=0,size=1)+
  geom_point(size=5)+
  facet_wrap(~age_bin,nrow=1, labeller = age_labeller, strip.position = c("bottom"))+
  geom_hline(yintercept=0)+
  theme(legend.position=c(.1,.8), legend.title = element_blank(),
        legend.margin = margin(.5,.5,.5,.5,"cm"),
        legend.background = element_rect(fill="white",size=0.6,
                                         linetype="solid", colour ="black")) +
  #theme(legend.position='top', legend.justification ='center')+
  scale_color_manual(values=c("#8c510a","#bf812d","#80cdc1","#35978f","#01665e"))+
  guides(color=FALSE) +
  guides(fill=FALSE) +
  #scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5,0.6), limits=c(0,0.5))+
  ylab("Sorting by Emotion Categories \nLow<-------------------------------------------->High")+
  xlab("Age Group")+
  labs(linetype = "Sorting Phase") +
  theme(axis.line.x = element_blank(),
    axis.text.x  = element_blank(),
        axis.ticks.x= element_blank(),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=18)) #+
p
ggsave(here(root_path,"analysis","paper_2020","plots","avg_difference_distance_emotion_within_between_byage.png"),width=10,height=8)


####  Analyses  ####

#average distances across category types  within each participant
avg_dist_cat_by_subj <- subj_dist_long %>%
  group_by(subject, Age, Gender, age_bin,age_group,sort,category_pair) %>%
  summarize(N=n(),avg_dist=mean(dist),ci_dist=qt(0.975, N-1)*sd(dist,na.rm=T)/sqrt(N))


#### Adults & children comparison ####
avg_dist_cat_by_subj <- avg_dist_cat_by_subj %>%
  ungroup() %>%
  mutate(AgeC=Age-mean(Age,na.rm=T),
         category_pairC=ifelse(category_pair=="between",-0.5,0.5),
         age_groupC=ifelse(age_group=="kids",-0.5,0.5),
         sortC=ifelse(sort=="Sort1",-0.5,0.5))

# interaction between emotion category and sort
m <- lmer(avg_dist~category_pairC*sortC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,sort!="Practice"))
summary(m)
confint(m, method="Wald")
anova(m) #Satterthwaite's method

## model removing correlation between random intercept and slope 
## to avoid singular fit yields equivalent results
m <- lmer(avg_dist~category_pairC*sortC+(1+category_pairC||subject),data=filter(avg_dist_cat_by_subj,sort!="Practice"))
summary(m)

# Emotion Category: Children versus Adults
m <- lmer(avg_dist~category_pairC*age_groupC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,sort!="Practice"))
summary(m)
confint(m, method="Wald")
anova(m)
## model removing correlation between random intercept and slope 
## to avoid singular fit yields equivalent results
m <- lmer(avg_dist~category_pairC*age_groupC+(1+category_pairC||subject),data=filter(avg_dist_cat_by_subj,sort!="Practice"))
summary(m)

#### Kids Only, Age continuous ####

m <- lmer(avg_dist~AgeC*category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_group=="kids"&sort!="Practice"))
summary(m) 
confint(m,method="Wald")
anova(m)

## model removing correlation between random intercept and slope 
## to avoid singular fit yields equivalent results
m <- lmer(avg_dist~AgeC*category_pairC+(1+category_pairC||subject),data=filter(avg_dist_cat_by_subj,age_group=="kids"&sort!="Practice"))
summary(m) 

#sub-groups
#3-4
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="3 to 4"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
anova(m)
#4-5
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="4 to 5"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
anova(m)
#5-6
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="5 to 6"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
anova(m)
#6-7
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="6 to 7"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
anova(m)
#adults
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="adults"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
anova(m)
