library(tidyverse)
library(here)
library(rsample)
library(tictoc)
library(cowplot)
theme_set(theme_cowplot())
root_path <- here()

set.seed(24816)
sample_num <- 1000

#functions
# sample a smaller set of between category comparisons for each subject 
# from the subject-level sorting distance dataset
sample_between <- function(dataset, sample_size=9) {
  subj_dist_long_between_sample_set <- dataset %>%
    filter(category_pair=="between"&sort!="Practice") %>%
    group_by(subject,sort) %>% 
    dplyr:::slice_sample(n=sample_size, replace=FALSE)
}

avg_dist_between_within <- function(dataset_long) {
  #average within-between difference subjects
  avg_dist_diff_cat_by_subj <- dataset_long %>% 
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
  avg_dist_diff_cat_across_subj
}

#read in data
subj_dist_long <- read.csv(here(root_path,"analysis","paper_2020","processed_data","Grid_subject_distance_item_pairs.csv"))
#create within-category-pair subset of the data
subj_dist_long_within <- subj_dist_long %>%
  filter(category_pair=="within"&sort!="Practice") 

# sample frame data
# creating equal observations for between and within category comparisons for each sample
resample_data <- data.frame()
tic()
for (i in 1:sample_num) {
  print(i)
  temp <- sample_between(subj_dist_long, sample_size=9)
  cur_data <- subj_dist_long_within %>%
    bind_rows(temp)
  cur_data$sample_index=i
  resample_data <- resample_data %>%
    bind_rows(cur_data)
}
toc()

#summarize data for each resample
summarized_resample_data <- resample_data %>%
  group_by(sample_index) %>%
  nest() %>%
  mutate(summarized_dist = purrr::map(data, avg_dist_between_within)) %>%
  select(-data) %>%
  unnest(cols=c(summarized_dist))

#store the resampled data
write.csv(summarized_resample_data, here(root_path,"analysis","paper_2020","processed_data","resampled_equal_obs_avg_category_distance.csv"), row.names=F)

#generate plot
# for facet wrap naming
age_names <- list(
  '3 to 4'="3 y/o",
  '4 to 5'="4 y/o",
  '5 to 6'="5 y/o",
  '6 to 7'="6 y/o",
  'adults'="adults")
age_labeller <- function(variable,value){
  return(age_names[value])
}

#recreate data from original emotion category plot
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

# combine with the resampled data
comparison_data <- summarized_resample_data %>%
  mutate(resample_group="resampled") %>%
  bind_rows(filter(avg_dist_diff_cat_across_subj,sort!="Practice") %>% mutate(resample_group="original"))

p1 <- ggplot(filter(comparison_data, sort_name=="Same Individual"),aes(x=resample_group,y=average_dist,fill=age_bin,color=age_bin))+
  geom_violin(data=filter(comparison_data,resample_group=="resampled"&sort_name=="Same Individual"),fill=NA)+
  geom_jitter(data=filter(comparison_data,resample_group=="resampled"&sort_name=="Same Individual"),alpha=0.01,width=0.1)+
  geom_errorbar(data=filter(comparison_data,resample_group=="original"&sort_name=="Same Individual"),aes(ymin=average_dist-ci_dist,ymax=average_dist+ci_dist),width=0,size=1)+
  geom_point(data=filter(comparison_data,resample_group=="original"&sort_name=="Same Individual"),size=4,alpha=1)+
  stat_summary(data=filter(comparison_data,resample_group=="resampled"&sort_name=="Same Individual"),fun = "mean", geom = "point",size=4)+
  facet_wrap(~age_bin,nrow=1, labeller = age_labeller, strip.position = c("top"))+
  geom_hline(yintercept=0)+
  theme(legend.position=c(.1,.8), legend.title = element_blank(),
        legend.margin = margin(.5,.5,.5,.5,"cm"),
        legend.background = element_rect(fill="white",size=0.6,
                                         linetype="solid", colour ="black")) +
  #theme(legend.position='top', legend.justification ='center')+
  scale_color_manual(values=c("#8c510a","#bf812d","#80cdc1","#35978f","#01665e"))+
  scale_fill_manual(values=c("#8c510a","#bf812d","#80cdc1","#35978f","#01665e"))+
  guides(color=FALSE) +
  guides(fill=FALSE) +
  #scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5,0.6), limits=c(0,0.5))+
  ylab("Sorting by Emotion Categories \nLow<----------------------------------->High")+
  xlab("Estimates")+
  labs(linetype = "Sorting Phase") +
  ggtitle("Same Individual Sort")+
  theme(#axis.line = element_blank(),
        #axis.text.x  = element_blank(),
        #axis.ticks.x= element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=14),
        axis.title.x = element_text(size=20,face="bold"),
        axis.text.y =  element_text(size=18),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=18),
        plot.title = element_text(hjust = 0.5,size=20))
p2 <- ggplot(filter(comparison_data, sort_name=="Different Individuals"),aes(x=resample_group,y=average_dist,fill=age_bin,color=age_bin))+
  geom_violin(data=filter(comparison_data,resample_group=="resampled"&sort_name=="Different Individuals"),fill=NA)+
  geom_jitter(data=filter(comparison_data,resample_group=="resampled"&sort_name=="Different Individuals"),alpha=0.01,width=0.1)+
  geom_errorbar(data=filter(comparison_data,resample_group=="original"&sort_name=="Different Individuals"),aes(ymin=average_dist-ci_dist,ymax=average_dist+ci_dist),width=0,size=1)+
  geom_point(data=filter(comparison_data,resample_group=="original"&sort_name=="Different Individuals"),size=4,alpha=1)+
  stat_summary(data=filter(comparison_data,resample_group=="resampled"&sort_name=="Different Individuals"),fun = "mean", geom = "point",size=4)+
  facet_wrap(~age_bin,nrow=1, labeller = age_labeller, strip.position = c("top"))+
  geom_hline(yintercept=0)+
  theme(legend.position=c(.1,.8), legend.title = element_blank(),
        legend.margin = margin(.5,.5,.5,.5,"cm"),
        legend.background = element_rect(fill="white",size=0.6,
                                         linetype="solid", colour ="black")) +
  #theme(legend.position='top', legend.justification ='center')+
  scale_color_manual(values=c("#8c510a","#bf812d","#80cdc1","#35978f","#01665e"))+
  scale_fill_manual(values=c("#8c510a","#bf812d","#80cdc1","#35978f","#01665e"))+
  guides(color=FALSE) +
  guides(fill=FALSE) +
  #scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5,0.6), limits=c(0,0.5))+
  ylab("Sorting by Emotion Categories \nLow<----------------------------------->High")+
  xlab("Estimates")+
  labs(linetype = "Sorting Phase") +
  ggtitle("Different Individuals Sort")+
  theme(#axis.line = element_blank(),
    #axis.text.x  = element_blank(),
    #axis.ticks.x= element_blank(),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=14),
    axis.title.x = element_text(size=20,face="bold"),
    axis.text.y =  element_text(size=18),
    axis.title.y= element_text(size=18,face="bold"),
    strip.text.x = element_text(size=18),
    plot.title = element_text(hjust = 0.5,size=20))
plot_grid(p1,p2,nrow=1,labels=c("A","B"),label_size=20)
ggsave(here(root_path,"analysis","paper_2020","final_plots","response_letter_fig2.png"),width=12,height=6.75)
