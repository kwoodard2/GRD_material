library(tidyverse)
library(here)
library(cowplot)
library(lme4)
library(car)
theme_set(theme_cowplot())
root_path <- here()

#### plot average distance by category labels ####
#read in data
subj_dist_long <- read.csv(here(root_path,"analysis","cogsci2020","processed_data","Grid_subject_distance_item_pairs.csv"))

##### Average Between vs. Within Categories #####
#average across everything  by category and age bin, across subjects
avg_dist_cat <- subj_dist_long %>%
  group_by(sort,age_bin,age_group,category_pair,shared_category) %>%
  summarize(N=n(),avg_dist=mean(dist),ci_dist=qt(0.975, N-1)*sd(dist,na.rm=T)/sqrt(N))

#average distances across category types  within each participant
avg_dist_cat_by_subj <- subj_dist_long %>%
  group_by(subject, Age, Gender, age_bin,age_group,sort,category_pair) %>%
  summarize(N=n(),avg_dist=mean(dist),ci_dist=qt(0.975, N-1)*sd(dist,na.rm=T)/sqrt(N))
#average across subjects  by age bin
avg_dist_cat_across_subj <- avg_dist_cat_by_subj %>%
  group_by(age_bin,age_group,sort,category_pair) %>%
  summarize(N=n(),average_dist=mean(avg_dist),ci_dist=qt(0.975, N-1)*sd(avg_dist,na.rm=T)/sqrt(N))

#plot
p1 <- ggplot(filter(avg_dist_cat_across_subj, sort!="Practice"&sort =="Sort1"),aes(category_pair,average_dist,fill=category_pair))+
  geom_bar(stat="identity",width=1)+
  geom_errorbar(aes(ymin=average_dist-ci_dist,ymax=average_dist+ci_dist),width=0,color="black")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(~age_bin,nrow=1)+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=22))+
  #scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5,0.6), limits=c(0,0.5))+
  ggtitle("Sorting Phase 1: Same Actor")+
  ylab("Average Distance between Images \n(normalized by subject)")+
  xlab("Emotion Category of Image Pair")+
  scale_x_discrete(breaks = c("between","within"), labels=c("Different","Same"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))
  
  
p2 <- ggplot(filter(avg_dist_cat_across_subj, sort!="Practice"&sort =="Sort1"),aes(category_pair,average_dist,fill=category_pair))+
  geom_bar(stat="identity",width=1)+
  geom_errorbar(aes(ymin=average_dist-ci_dist,ymax=average_dist+ci_dist),width=0,color="black")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(~age_bin,nrow=1)+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=22))+
  ylab("Average Distance between Images \n(normalized by subject)")+
  xlab("Emotion Category of Image Pair")+
  scale_x_discrete(breaks = c("between","within"), labels=c("Different","Same"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18))+
  ggtitle("Sorting Phase 2: Different Actors")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))
plot_grid(p1,p2)  
ggsave(here(root_path,"analysis","cogsci2020","plots","avg_distance_emotion_within_between.jpg"),width=14,height=8)

#focus on differences
avg_dist_diff_cat_by_subj <- subj_dist_long %>%
  group_by(subject, Age, Gender, age_bin,age_group,sort,category_pair) %>%
  summarize(avg_dist=mean(dist,na.rm=T))  %>%
  ungroup() %>%
  group_by(subject, Age, Gender, age_bin,age_group,sort) %>%
  pivot_wider(names_from=category_pair,values_from=avg_dist) %>%
  mutate(avg_dist_diff=between-within) %>%
  mutate(sort_name=ifelse(sort=="Sort1","Sorting Phase 1",ifelse(sort=="Sort2","Sorting Phase 2","Practice"))) %>%
  ungroup()
#average across subjects  by age bin
avg_dist_diff_cat_across_subj <- avg_dist_diff_cat_by_subj %>%
  select(-between,-within) %>%
  group_by(age_bin,age_group,sort,sort_name) %>%
  summarize(N=n(),average_dist=mean(avg_dist_diff,na.rm=T),ci_dist=qt(0.975, N-1)*sd(avg_dist_diff,na.rm=T)/sqrt(N))

p <- ggplot(filter(avg_dist_diff_cat_across_subj, sort!="Practice"),aes(age_bin,average_dist,fill=age_bin,color=age_bin))+
  #geom_bar(stat="identity",width=1)+
  geom_errorbar(aes(ymin=average_dist-ci_dist,ymax=average_dist+ci_dist),width=0,size=1)+
  geom_point(size=5)+
  #scale_color_brewer(palette="BrBG")+
  scale_color_manual(breaks=c("3 to 4","4 to 5","5 to 6", "6 to 7","adults"),
                     values=c("#8c510a","#bf812d","#80cdc1","#35978f","#01665e"))+
  facet_wrap(~sort_name,nrow=1)+
  geom_hline(yintercept=0)+
  theme(legend.position="none")+
  #scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5,0.6), limits=c(0,0.5))+
  ylab("Average Difference in Distance \nfor Same vs. Different Emotion Category Pairs")+
  xlab("Age Group")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))
p 
ggsave(here(root_path,"analysis","cogsci2020","plots","avg_difference_distance_emotion_within_between.jpg"),width=7,height=7)

p <- ggplot(filter(avg_dist_diff_cat_across_subj, sort!="Practice"),aes(age_bin,average_dist,fill=age_bin,color=age_bin))+
  #geom_bar(stat="identity",width=1)+
  geom_violin(data=filter(avg_dist_diff_cat_by_subj, sort!="Practice"),aes(y=avg_dist_diff),fill="white")+
  #geom_boxplot(data=filter(avg_dist_diff_cat_by_subj, sort!="Practice"),aes(y=avg_dist_diff,fill=NULL))+
  geom_errorbar(aes(ymin=average_dist-ci_dist,ymax=average_dist+ci_dist),width=0,size=1)+
  geom_point(size=5)+
  scale_color_brewer(palette="Spectral")+
  #scale_fill_brewer(palette="Set1")+
  facet_wrap(~sort_name,nrow=1)+
  geom_hline(yintercept=0)+
  theme(legend.position="none")+
  #scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5,0.6), limits=c(0,0.5))+
  ylab("Average Difference in Distance \nfor Same vs. Different Emotion Category Pairs")+
  xlab("Age Group")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=20,face="bold"),
        strip.text.x = element_text(size=18))
p 
ggsave(here(root_path,"analysis","cogsci2020","plots","avg_difference_distance_emotion_within_between_violin.jpg"),width=7,height=7)


####  Analyses  ####

####  Practice
#basic  practice  check
#paired  t-test on average distance scores
#3-4
t.test(filter(avg_dist_cat_by_subj,age_bin=="3 to 4"& sort=="Practice"&category_pair=="between")$avg_dist,
       filter(avg_dist_cat_by_subj,age_bin=="3 to 4"& sort=="Practice"&category_pair=="within")$avg_dist,
       paired=T)
#4-5
t.test(filter(avg_dist_cat_by_subj,age_bin=="4 to 5"& sort=="Practice"&category_pair=="between")$avg_dist,
       filter(avg_dist_cat_by_subj,age_bin=="4 to 5"& sort=="Practice"&category_pair=="within")$avg_dist,
       paired=T)
#5-6
t.test(filter(avg_dist_cat_by_subj,age_bin=="5 to 6"& sort=="Practice"&category_pair=="between")$avg_dist,
       filter(avg_dist_cat_by_subj,age_bin=="5 to 6"& sort=="Practice"&category_pair=="within")$avg_dist,
       paired=T)
#6-7
t.test(filter(avg_dist_cat_by_subj,age_bin=="6 to 7"& sort=="Practice"&category_pair=="between")$avg_dist,
       filter(avg_dist_cat_by_subj,age_bin=="6 to 7"& sort=="Practice"&category_pair=="within")$avg_dist,
       paired=T)
#adults
t.test(filter(avg_dist_cat_by_subj,age_bin=="adults"& sort=="Practice"&category_pair=="between")$avg_dist,
       filter(avg_dist_cat_by_subj,age_bin=="adults"& sort=="Practice"&category_pair=="within")$avg_dist,
       paired=T)

#### Adults & children comparison ####
avg_dist_cat_by_subj <- avg_dist_cat_by_subj %>%
  ungroup() %>%
  mutate(AgeC=Age-mean(Age,na.rm=T),
         category_pairC=ifelse(category_pair=="between",-0.5,0.5),
         age_groupC=ifelse(age_group=="kids",-0.5,0.5))

m <- lmer(avg_dist~category_pairC*age_groupC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,sort!="Practice"))
summary(m)
confint(m, method="Wald")
Anova(m,type="III",test="F")

#average differences
avg_dist_cat_by_age_group <- avg_dist_cat_by_subj %>%
  group_by(subject,age_group,sort) %>%
  summarize(avg_dist_between=mean(avg_dist[category_pair=="between"]),
            avg_dist_within=mean(avg_dist[category_pair=="within"]),
            avg_dist_diff=avg_dist_between-avg_dist_within) %>%
  ungroup() %>%
  group_by(age_group,sort) %>%
  summarize(N=n(),
            avg_distance_diff=mean(avg_dist_diff,na.rm=T),
            ci_distance_diff=qt(0.975, N-1)*sd(avg_dist_diff,na.rm=T)/sqrt(N),
            lower_ci=avg_distance_diff-ci_distance_diff,
            upper_ci=avg_distance_diff+ci_distance_diff)

#### Age analysis (continuous) ####
#Kids
m <- lmer(avg_dist~AgeC*category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_group=="kids"&sort!="Practice"))
summary(m) #singular fit
confint(m,method="Wald")
Anova(m,type="III",test="F")
#sub-groups
#3-4
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="3 to 4"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
Anova(m,type="III",test="F")
#4-5
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="4 to 5"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
Anova(m,type="III",test="F")
#5-6
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="5 to 6"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
Anova(m,type="III",test="F")
#6-7
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="6 to 7"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
Anova(m,type="III",test="F")
#adults
m <- lmer(avg_dist~category_pairC+(1+category_pairC|subject),data=filter(avg_dist_cat_by_subj,age_bin=="adults"&sort!="Practice"))
summary(m)
confint(m,method="Wald")
Anova(m,type="III",test="F")

