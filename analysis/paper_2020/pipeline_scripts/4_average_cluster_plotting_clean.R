library(tidyverse)
library(harrietr)
library(ggdendro)
library(ggimage)
library(dendroextras)
library(cowplot)
library(vegan)
library(here)
source("Grid_helper_functions.R")
theme_set(theme_cowplot())
root_path <- here()

#What's in this file:
    # MDS solution for age group (kids versus adults)
    # overlaying rating vectors on MDS plot (vegan package, envfit)

#read in data
subj_dist_long <- read.csv(here(root_path,"analysis","paper_2020","processed_data","Grid_subject_distance_item_pairs.csv"))
ratings <- read.csv(here(root_path,"analysis","paper_2020","processed_data","ratings_data.csv"))

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
  mutate(dist_obj = purrr::map(data, long_to_dist))


#### Plot MDS - Kids: Sorting Same Individual ####
cur_dist <- filter(avg_dist,sort=="Sort1"&age_group=="kids")$dist_obj[[1]]
sort1_kids_cmd <- data.frame(cmdscale(cur_dist, k=2))

# quick plot with vectors of image ratings (only show vectors with p < .05)
ratings1 <- ratings %>% filter(sort=="Sort1") %>%  dplyr::select(image, pos, neg, valence, arousal) %>% rename(positivity=pos, negativity=neg)
ordiplot(sort1_kids_cmd, type="text")
sort1k_fit <- envfit(sort1_kids_cmd,ratings1, permutations = 1000)
plot(sort1k_fit,  p.max =.05, col="red")
sort1k_fit

# prepare face images
cur_cluster <- hclust(cur_dist)
image_paths <- paste(here("experiment","stimuli_sort1"),"/",labels(cur_cluster),".png",sep="")
cur_images <- data.frame(
  label=labels(cur_cluster),
  image=image_paths,
  x=seq(1,length(image_paths)),
  y=rep(c(-0.05,-0.1),length(image_paths)/2))
sort1_images <- cur_images
cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,sort1_images)

#include correlations at correct angles
data.scores = as.data.frame(scores(sort1_kids_cmd))
en_coord_cont = as.data.frame(scores(sort1k_fit, "vectors")) * ordiArrowMul(sort1k_fit)

#Plot MDS with rating vectors
p1 <- ggplot(data=cur_cmd,aes(X1,X2))+
  geom_image(data=cur_cmd, aes(image=image), size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  geom_segment(data = en_coord_cont,aes(x = 0, y = 0, xend = X1/2, yend = X2/2, color=row.names(en_coord_cont)), 
               size =1, alpha = 0.8, arrow = arrow(length = unit(0.03, "npc"))) + #ends="both" for arrow
  geom_text(data = en_coord_cont, aes(x = X1/2, y = (X2/2) - .01), colour = "black",
            fontface = "bold", label = row.names(en_coord_cont))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Children")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))


#### Plot MDS - Adults: Sorting Same Individual ####
cur_dist <- filter(avg_dist,sort=="Sort1"&age_group=="adults")$dist_obj[[1]]
sort1_adults_cmd <- data.frame(cmdscale(cur_dist, k=2))

# quick plot with vectors of image ratings (only show vectors with p < .05)
ordiplot(sort1_adults_cmd, type="text")
sort1a_fit <- envfit(sort1_adults_cmd,ratings1, permutations=1000)
plot(sort1a_fit,  p.max =.05, col="red")
sort1a_fit

#include correlations at correct angles
data.scores = as.data.frame(scores(sort1_adults_cmd))
en_coord_cont = as.data.frame(scores(sort1a_fit, "vectors")) * ordiArrowMul(sort1a_fit)

#Plot MDS with rating vectors
cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,sort1_images)

p2 <- ggplot(data=cur_cmd,aes(X1,X2))+
  geom_image(data=cur_cmd, aes(image=image), size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  geom_segment(data = en_coord_cont,aes(x = 0, y = 0, xend = X1/2, yend = X2/2, color=row.names(en_coord_cont)), 
               size =1, alpha = 0.8, arrow = arrow(length = unit(0.03, "npc"))) + #ends="both" for arrow
  geom_text(data = en_coord_cont, aes(x = X1/2, y = (X2/2) - .01), colour = "black",
            fontface = "bold", label = row.names(en_coord_cont))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Adults")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))

#plot adults and children together
plot_grid(p1,p2,labels=c("A","B"),label_size=20)

#### Plot MDS - Kids: Sorting Different Individuals ####
cur_dist <- filter(avg_dist,sort=="Sort2"&age_group=="kids")$dist_obj[[1]]
sort2_kids_cmd <- data.frame(cmdscale(cur_dist, k=2))

# quick plot with vectors of image ratings (only show vectors with p < .05)
ordiplot(sort2_kids_cmd, type="text")
ratings2 <- ratings %>% filter(sort=="Sort2") %>%  select(image, pos, neg, valence, arousal) %>% rename(positivity=pos, negativity=neg)
sort2k_fit <- envfit(sort2_kids_cmd,ratings2, permutations=1000)
plot(sort2k_fit, p.max =.05, col="red")
sort2k_fit

#include correlations at correct angles
data.scores = as.data.frame(scores(sort2_kids_cmd))
en_coord_cont = as.data.frame(scores(sort2k_fit, "vectors")) * ordiArrowMul(sort2k_fit)

#create image paths
cur_cluster <- hclust(cur_dist)
image_paths <- paste(here("experiment","stimuli_sort2"),"/",labels(cur_cluster),".png",sep="")
cur_images <- data.frame(
  label=labels(cur_cluster),
  image=image_paths,
  x=seq(1,length(image_paths)),
  y=rep(c(-0.05,-0.1),length(image_paths)/2))
sort2_images <- cur_images
cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,sort2_images)


#Plot MDS with rating vectors
p1 <- ggplot(data=cur_cmd,aes(X1,X2))+
  geom_image(data=cur_cmd, aes(image=image), size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  geom_segment(data = en_coord_cont,aes(x = 0, y = 0, xend = X1/2, yend = X2/2, color=row.names(en_coord_cont)), 
               size =1, alpha = 0.8, arrow = arrow(length = unit(0.03, "npc"))) + #ends="both" for arrow
  geom_text(data = en_coord_cont, aes(x = X1/2, y = (X2/2) - .01), colour = "black",
            fontface = "bold", label = row.names(en_coord_cont))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Children")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))

#### Plot MDS - Adults: Sorting Different Individuals  ####
cur_dist <- filter(avg_dist,sort=="Sort2"&age_group=="adults")$dist_obj[[1]]
sort2_adults_cmd <- data.frame(cmdscale(cur_dist, k=2))

# quick plot with vectors of image ratings (only show vectors with p < .05)
ordiplot(sort2_adults_cmd, type="text")
sort2a_fit <- envfit(sort2_adults_cmd,ratings2, permutations=1000)
plot(sort2a_fit,  p.max =.05, col="red")
sort2a_fit

#include correlations at correct angles
data.scores = as.data.frame(scores(sort2_adults_cmd))
en_coord_cont = as.data.frame(scores(sort2a_fit, "vectors")) * ordiArrowMul(sort2a_fit)

# prepare images
cur_cmd <- data.frame(cmdscale(cur_dist))
cur_cmd$label <- rownames(cur_cmd)
cur_cmd <- merge(cur_cmd,sort2_images)

#Plot MDS with rating vectors
p2 <- ggplot(data=cur_cmd,aes(X1,X2))+
  geom_image(data=cur_cmd, aes(image=image), size=0.08)+
  ylab("Dimension 2")+
  xlab("Dimension 1")+
  geom_segment(data = en_coord_cont,aes(x = 0, y = 0, xend = X1/2, yend = X2/2, color=row.names(en_coord_cont)), 
               size =1, alpha = 0.8, arrow = arrow(length = unit(0.03, "npc"))) + #ends="both" for arrow
  geom_text(data = en_coord_cont, aes(x = X1/2, y = (X2/2) - .02), colour = "black",
            fontface = "bold", label = row.names(en_coord_cont))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=18),
        axis.title.x = element_text(size=18,face="bold"),
        axis.text.y =  element_text(size=16),
        axis.title.y= element_text(size=18,face="bold"),
        strip.text.x = element_text(size=16))+
  ggtitle("Adults")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=18))


plot_grid(p1,p2,labels=c("C","D"),label_size=20)