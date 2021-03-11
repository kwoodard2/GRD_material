library(tidyverse)
#### functions ####

#### function for computing distance matrix
get_distance <- function(current_d,normalize_dist=T) {
  
  #rownames(current_d) <- current_d$image #old function in tibble, now deprecated
  current_d <- current_d %>%
    tibble::column_to_rownames(var = "image")
  
  d_dist <- current_d %>%
    dplyr::select(posX,posY) %>%
    as.matrix() %>%
    dist(diag=T,upper=T)
  
  if (normalize_dist) {
    d_dist <- d_dist/(max(d_dist))
  }
  
  d_dist
}

#### convert long data to dist object ####
long_to_dist <- function(d_long, colnames=c("item1","item2"),dist_name="avg_dist") {
  #add zero distance columns (to avoid some problems when converting to a matrix)
  zero_distance_d <- data.frame(
    col1 = unique(c(pull(d_long[colnames[1]]),pull(d_long[colnames[2]]))),
    col2 = unique(c(pull(d_long[colnames[1]]),pull(d_long[colnames[2]]))), 
    col3 = 0)
  
  colnames(zero_distance_d) <- c(colnames,dist_name)
  
  #add zero distance columns to long data frame
  d_long <- bind_rows(d_long,zero_distance_d)
  
  #convert long data to wide
  dist_wide <- d_long %>%
    select(c(colnames,all_of(dist_name))) %>%
    spread(colnames[1], dist_name) %>%
    #add rownames
    column_to_rownames("item2")
  
  #convert to matrix and then to dist object
  dist <- as.matrix(dist_wide) %>%
    as.dist()
  
  dist
}
