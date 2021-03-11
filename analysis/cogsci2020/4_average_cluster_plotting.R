library(tidyverse)
library(harrietr)
library(ggdendro)
library(ggimage)
library(dendroextras)
library(cowplot)
library(viridis)
library(cluster)
library(factoextra)
library(scales)
library(fpc)
root_path <- here()
source(here(root_path,"analysis","cogsci2020","Grid_helper_functions.R"))
theme_set(theme_cowplot())

#read in data
subj_dist_long <- read.csv(here(root_path,"analysis","cogsci2020","processed_data","Grid_subject_distance_item_pairs.csv"))

#average across all distances
avg_dist_long <- subj_dist_long %>%
  group_by(sort,age_group,item1,item2) %>%
  summarize(avg_dist=mean(dist)) %>%
  ungroup() %>%
  mutate(sort=as.character(sort),age_group=as.character(age_group),item1=as.character(item1),item2=as.character(item2))

#average distance object organized by sorting group
avg_dist <- avg_dist_long %>%
  group_by(sort,age_group) %>%
  nest() %>%
  mutate(dist_obj = map(data, long_to_dist))

#### Plot kids MDS solution: Sorting Phase 1 ####
#store current hirarchical cluster from
cur_dist <- filter(avg_dist,sort=="Sort1"&age_group=="kids")$dist_obj[[1]]
cur_cluster <- hclust(cur_dist)
#create image paths in dendrogram order (with dendroextras package)
image_paths <- paste(here("experiment","stimuli_sort1"),"/",labels(cur_cluster),".png",sep="")
#add images
cur_images <- data.frame(
  label=labels(cur_cluster),
  image=image_paths,
  x=seq(1,length(image_paths)),
  y=rep(c(-0.05,-0.1),length(image_paths)/2))
sort1_images <- cur_images
cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,cur_images)
#Plot images
p1 <- ggplot(cur_cmd,aes(X1,X2,label=label,image=image))+
  geom_image(size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Children")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))
ggsave(here(root_path,"analysis","cogsci2020","plots","cmd_kids_sort1.jpg"),width=8,height=8)
  
#### Plot adults MDS solution: Sorting Phase 1 ####
#store current hirarchical cluster from
cur_dist <- filter(avg_dist,sort=="Sort1"&age_group=="adults")$dist_obj[[1]]
cur_cluster <- hclust(cur_dist)
#create image paths in dendrogram order (with dendroextras package)
image_paths <- paste(here("experiment","stimuli_sort1"),"/",labels(cur_cluster),".png",sep="")

#add images
cur_images <- data.frame(
  label=labels(cur_cluster),
  image=image_paths,
  x=seq(1,length(image_paths)),
  y=rep(c(-0.05,-0.1),length(image_paths)/2))

sort1_images <- cur_images

cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,cur_images)


#Plot images
p2 <- ggplot(cur_cmd,aes(X1,X2,label=label,image=image))+
  geom_image(size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Adults")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))
ggsave(here(root_path,"analysis","cogsci2020","plots","cmd_adults_sort1.jpg"),width=8,height=8)

plot_grid(p1,p2,labels=c("A","B"),label_size=20)
ggsave(here(root_path,"analysis","cogsci2020","plots","cmd_kids_adults_sort1.jpg"),width=9,height=5)


#### Plot kids MDS solution: Sorting Phase 2 ####
#store current hirarchical cluster from
cur_dist <- filter(avg_dist,sort=="Sort2"&age_group=="kids")$dist_obj[[1]]
cur_cluster <- hclust(cur_dist)
#create image paths in dendrogram order (with dendroextras package)
image_paths <- paste(here("experiment","stimuli_sort2"),"/",labels(cur_cluster),".png",sep="")
#add images
cur_images <- data.frame(
  label=labels(cur_cluster),
  image=image_paths,
  x=seq(1,length(image_paths)),
  y=rep(c(-0.05,-0.1),length(image_paths)/2))
sort1_images <- cur_images
cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,cur_images)
#Plot images
p1 <- ggplot(cur_cmd,aes(X1,X2,label=label,image=image))+
  geom_image(size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Children")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))
ggsave(here(root_path,"analysis","cogsci2020","plots","cmd_kids_sort2.jpg"),width=8,height=8)

#### Plot adults MDS solution: Sorting Phase 1 ####
#store current hirarchical cluster from
cur_dist <- filter(avg_dist,sort=="Sort2"&age_group=="adults")$dist_obj[[1]]
cur_cluster <- hclust(cur_dist)
#create image paths in dendrogram order (with dendroextras package)
image_paths <- paste(here("experiment","stimuli_sort2"),"/",labels(cur_cluster),".png",sep="")

#add images
cur_images <- data.frame(
  label=labels(cur_cluster),
  image=image_paths,
  x=seq(1,length(image_paths)),
  y=rep(c(-0.05,-0.1),length(image_paths)/2))

sort1_images <- cur_images

cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,cur_images)


#Plot images
p2 <- ggplot(cur_cmd,aes(X1,X2,label=label,image=image))+
  geom_image(size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Adults")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))
ggsave(here(root_path,"analysis","cogsci2020","plots","cmd_adults_sort2.jpg"),width=8,height=8)

plot_grid(p1,p2,labels=c("A","B"),label_size=20)
ggsave(here(root_path,"analysis","cogsci2020","plots","cmd_kids_adults_sort2.jpg"),width=9,height=5)


