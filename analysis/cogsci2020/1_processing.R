library(tidyverse)
library(harrietr)
library(here)
root_path <- here()
source(here(root_path,"analysis","cogsci2020","Grid_helper_functions.R"))

#### ratings data ####
#read in valence & arousal data
ratings1 <- read.csv(here(root_path,"data","Grid_data","processed_data","ratings_byImage_sort1.csv"))
ratings1$sort <- "Sort1"
ratings2 <- read.csv(here(root_path,"data","Grid_data","processed_data","ratings_byImage_sort2.csv"))
ratings2$sort <- "Sort2"
#bind
ratings <- bind_rows(ratings1,ratings2)
#isolate emotion labels
ratings <- ratings %>%
  mutate(image_cat=str_replace_all(image,c("M07"="",
                                             "F01"="",
                                             "F04"="",
                                             "F07"="",
                                             "F10"="",
                                             "F13"="",
                                             "F14"="",
                                             "F15"="",
                                             "F17"="",
                                             "F22"="",
                                             "M02"="",
                                             "M04"="",
                                             "M03"="",
                                             "M05"="",
                                             "M08"="",
                                             "M12"="",
                                             "M14"="",
                                             "M15"="",
                                             "M17"="",
                                             "_o"="",
                                             "_c"="")))



#write ratings data frame
write.csv(ratings,here(root_path,"analysis","cogsci2020","processed_data","ratings_data.csv"),row.names=F)

#### sorting data ####
#read in grid data
kids <- read.csv(here(root_path,"data","Grid_data","processed_data","GridTask_allData.csv"))
adults <- read.csv(here(root_path,"data","Grid_data_adults","processed_data","GridTask_allData_adults.csv"))
#read in kid ages
subj_kids <- read.csv(here(root_path,"data","Grid_data","processed_data","GRD_ages.csv"))
subj_kids$subject <- paste("p",as.character(subj_kids$subjCode),sep="")

#clean kids subject id
kids <- kids %>%
  mutate(subject = str_replace_all(participant, c("subjCode"="","s"="","u"=""))) %>%
  mutate(subject = paste("p",subject,sep="")) %>%
  mutate(subject = case_when(
    subject == "p24" ~ "p124",
    TRUE ~ subject
  ))

#merge
kids <- kids %>%
  left_join(subj_kids)

#age group
kids <- kids %>%
  mutate(
    age_bin = case_when(
      Age < 4 ~ "3 to 4",
      4 <= Age &  Age <  5 ~ "4 to 5",
      5 <= Age & Age < 6 ~ "5 to 6",
      6 <= Age & Age < 7 ~ "6 to 7"
    ))

#clean adults subject id
adults <- adults %>%
  mutate(participant = as.character(participant),subject = paste("p",participant,sep=""))

adults$age_bin <- "adults"
kids$age_group <- "kids"
adults$age_group <- "adults"

#combine
sorting_data <- bind_rows(kids,adults)

#isolate emotion labels
sorting_data <- sorting_data %>%
  mutate(image_cat=str_replace_all(image,c("M07"="",
                                             "F01"="",
                                             "F04"="",
                                             "F07"="",
                                             "F10"="",
                                             "F13"="",
                                             "F14"="",
                                             "F15"="",
                                             "F17"="",
                                             "F22"="",
                                             "M02"="",
                                             "M04"="",
                                             "M03"="",
                                             "M05"="",
                                             "M08"="",
                                             "M12"="",
                                             "M14"="",
                                             "M15"="",
                                             "M17"="",
                                             "_o"="",
                                             "_c"=""))) %>%
  mutate(image_tax_cat=case_when(
    image_cat=="bird" ~ "animal",
    image_cat=="squirrel" ~ "animal",
    image_cat=="bus" ~"vehicle",
    image_cat=="car"  ~ "vehicle",
    image_cat=="table" ~ "furniture",
    TRUE ~ image_cat))

write.csv(sorting_data,here(root_path,"analysis","cogsci2020","processed_data","sorting_data.csv"),row.names=F)

#### summarized sorting by age group ####

# For group as a whole:
# -each child has scaled distance matrix (0 to 1)
#within each kid divide by the maximum
# -average across all children on the scaled values
#doing this by sort

#create overall data frame containing (nested) distance objects for each participant
subj_dist_byGroup <- sorting_data %>%
  group_by(subject, participant, Age, Gender, age_bin,age_group,sort) %>%
  nest() %>%
  mutate(dist_object = map(data, get_distance)) %>%
  mutate(dist_matrix = map(dist_object, as.matrix)) %>%
  mutate(dist_long= map(dist_matrix,melt_dist)) %>%
  select(-data)

#create long dataframe with pairwise distances (normalized)
subj_dist_long <-  subj_dist_byGroup %>%
  select(-dist_object,-dist_matrix) %>%
  unnest(cols = c(dist_long)) %>%
  rename(item1=iso1,item2=iso2) %>%
  mutate(items = paste(pmin(item1, item2), #alphabetically order
                       pmax(item1, item2), sep= "-")) %>%
  select(-item1,-item2) %>%
  separate(items, into=c("item1","item2"),sep="-",remove=F) %>%
  ungroup()

##### Emotion Categories #####

emotion_words=c("ang","calm","disg","exc","fear","hap","neut","sad","surp")

#isolate emotion labels
subj_dist_long <-  subj_dist_long %>%
  mutate(image_cat_1=str_replace_all(item1,c("M07"="",
                                          "F01"="",
                                          "F04"="",
                                          "F07"="",
                                          "F10"="",
                                          "F13"="",
                                          "F14"="",
                                          "F15"="",
                                          "F17"="",
                                          "F22"="",
                                          "M02"="",
                                          "M04"="",
                                          "M03"="",
                                          "M05"="",
                                          "M08"="",
                                          "M12"="",
                                          "M14"="",
                                          "M15"="",
                                          "M17"="",
                                          "_o"="",
                                          "_c"="")),
         image_cat_2=str_replace_all(item2,c("M07"="",
                                          "F01"="",
                                          "F04"="",
                                          "F07"="",
                                          "F10"="",
                                          "F13"="",
                                          "F14"="",
                                          "F15"="",
                                          "F17"="",
                                          "F22"="",
                                          "M02"="",
                                          "M04"="",
                                          "M03"="",
                                          "M05"="",
                                          "M08"="",
                                          "M12"="",
                                          "M14"="",
                                          "M15"="",
                                          "M17"="",
                                          "_o"="",
                                          "_c"=""))) %>%
  mutate(
    image_tax_cat_1=case_when(
      image_cat_1=="bird" ~ "animal",
      image_cat_1=="squirrel" ~ "animal",
      image_cat_1=="bus" ~"vehicle",
      image_cat_1=="car"  ~ "vehicle",
      image_cat_1=="table" ~ "furniture",
      TRUE ~ image_cat_1),
    image_tax_cat_2=case_when(
      image_cat_2=="bird" ~ "animal",
      image_cat_2=="squirrel" ~ "animal",
      image_cat_2=="bus" ~"vehicle",
      image_cat_2=="car"  ~ "vehicle",
      image_cat_2=="table" ~ "furniture",
      TRUE ~ image_cat_2),
    )

#assign pair categories
subj_dist_long$emotion_pair_same <- ifelse(subj_dist_long$sort=="Practice", NA,
                                                  ifelse(subj_dist_long$image_cat_1==subj_dist_long$image_cat_2,1,0))
subj_dist_long$shared_category <- ifelse(subj_dist_long$image_tax_cat_1==subj_dist_long$image_tax_cat_2,as.character(subj_dist_long$image_tax_cat_1),"between")
subj_dist_long$category_pair <- ifelse(subj_dist_long$image_tax_cat_1==subj_dist_long$image_tax_cat_2,"within","between")


write.csv(subj_dist_long, here(root_path,"analysis","cogsci2020","processed_data","Grid_subject_distance_item_pairs.csv"), row.names=F)

#get item pairs
ratings_pairs <- data.frame(items = unique(filter(subj_dist_long,sort!="Practice")$items)) %>%
  left_join(unique(select(subj_dist_long,items,emotion_pair_same,shared_category,category_pair))) %>%
  separate(items,into=c("item1","item2"),sep="-", remove=F) %>%
  left_join(ratings,by=c("item1"="image")) %>%
  rename(valence_1=valence, pos_1=pos,neg_1=neg,arousal_1=arousal,ESG_type_1=ESG_type,image_cat_1=image_cat) %>%
  left_join(ratings,by=c("item2"="image","sort")) %>%
  rename(valence_2=valence, pos_2=pos,neg_2=neg,arousal_2=arousal,ESG_type_2=ESG_type,image_cat_2=image_cat) %>%
  mutate(dist_valence=abs(valence_2-valence_1),dist_arousal=abs(arousal_2-arousal_1),dist_pos=abs(pos_2-pos_1),dist_neg=abs(neg_2-neg_1))

write.csv(ratings_pairs, here(root_path,"analysis","cogsci2020","processed_data","ratings_item_pairs.csv"), row.names=F)

  