#====== Day 10 of 25 Days of Rtistry - Rainbow======#

#Library Load-in====
library(tidyverse) # For everything data
library(sp) # For polygon manipulation

#Function to make a rainbow==== 
rainbow_maker <- function(n){
  
  theta <- seq(0,pi, length.out = 1000)
  r <- seq(5,15,  length.out = n)
  rainbow_list <- list()
  
  for(i in seq_along(1:(n-1))){
    
  rainbow_list[[i]] <- tibble(x = c(cos(theta)*r[i],
                       r[i],r[i+1],
                       rev(cos(theta)*r[i+1]),
                       -r[i+1], -r[i]),
                 y = c(sin(theta)*r[i],
                       rep(0,2),
                       rev(sin(theta)*r[i+1]),
                       rep(0,2)),
                 group = i)
  }
  
  return(bind_rows(rainbow_list))
}

# Number of bands/colors in the rainbow====
n <- 8 

# Using the fx to make the rainbow data set====
rainbow_data <- rainbow_maker(n) 

# Pulling out the x limits====
rainbow_x_max <- max(rainbow_data$x)
rainbow_x_min <- min(rainbow_data$x)

# Pulling out the y limits====
rainbow_y_max <- max(rainbow_data$y)
rainbow_y_min <- min(rainbow_data$y)

# Filling up the designated space on the plot with points===
rainbow_points <- tibble(crossing(x = seq(rainbow_x_min, rainbow_x_max, length.out = 100),
                                  y = seq(rainbow_y_min, rainbow_y_max, length.out = 100)))

# Filtering out only points that fall in our rainbow polygons, and placing colors (hex codes) in the proper groups====
rainbow_points <- rainbow_points %>%
  rowwise() %>%
  mutate(logic = case_when(point.in.polygon(x,y,rainbow_data$x[rainbow_data$group == 1],rainbow_data$y[rainbow_data$group == 1]) == 1 ~ "#1A1333",
                           point.in.polygon(x,y,rainbow_data$x[rainbow_data$group == 2],rainbow_data$y[rainbow_data$group == 2]) == 1 ~ "#262949",
                           point.in.polygon(x,y,rainbow_data$x[rainbow_data$group == 3],rainbow_data$y[rainbow_data$group == 3]) == 1 ~ "#022C7A",
                           point.in.polygon(x,y,rainbow_data$x[rainbow_data$group == 4],rainbow_data$y[rainbow_data$group == 4]) == 1 ~ "#087353",
                           point.in.polygon(x,y,rainbow_data$x[rainbow_data$group == 5],rainbow_data$y[rainbow_data$group == 5]) == 1 ~ "#FBBF54",
                           point.in.polygon(x,y,rainbow_data$x[rainbow_data$group == 6],rainbow_data$y[rainbow_data$group == 6]) == 1 ~ "#EE6B3B",
                           point.in.polygon(x,y,rainbow_data$x[rainbow_data$group == 7],rainbow_data$y[rainbow_data$group == 7]) == 1 ~ "#EC0F47",
                           TRUE ~ NA_character_)) 

# Setting our rainbow color palette - same as above, but using this to calculate ====
color_pal <- rev(c("#EC0F47","#EE6B3B","#FBBF54","#087353","#022C7A","#262949","#1A1333"))

# Filtering out extraneous points to just leave the rainbow====
rainbow_points <- rainbow_points %>%
  filter(!is.na(logic))

# Calculating "border" colors for the points====
color_dark <- unlist(sapply(color_pal, function(x) colorRampPalette(c(x,"#000000"))(10)[3]))

# Creating a lookup table for the colors to add the darker colors to the original data frame====
color_match <- tibble(logic = color_pal, color = color_dark)

# Joining the tables====
rainbow_points <- rainbow_points %>%
  left_join(.,color_match, by = "logic")

# Creating a function to make the grass====
make_paint <- function(n,min_x,max_x,min_y,max_y,width,height){
  vertmarks <- list()
  horizmarks <- list()
  
  for(i in seq_along(1:n)){
    vertmarks[[i]] <- tibble(x=sample(seq(min_x,(max_x-width),length.out = 1000),1),
                             xend = x,
                             y = sample(seq(min_y,(max_y-height),length.out = 1000),1),
                             yend = sample(seq(min_y,(max_y-height),length.out = 1000),1))
  }
  
  for(i in seq_along(1:n)){
    horizmarks[[i]] <- tibble(x=sample(seq(min_x,(max_x-width),length.out = 1000),1),
                              xend = sample(seq(min_x,(max_x-width),length.out = 1000),1),
                              y = sample(seq(min_y,(max_y-height),length.out = 1000),1),
                              yend = y)
  }
  
  return(bind_rows(vertmarks,horizmarks))
}

# Setting an area for the "grass" to exist in====
grass_block <- tibble(x = c(-20,20,20,-20),
                      y = c(-6,-6,0,0))

# Using the function to make the data for the grass====
grass <- make_paint(6000,-20,25,-6,1,5,.5)

# Creating a palette of random colors for the grass====
grass_fill <-  tibble(fill =sample(colorRampPalette(c("#003E21","#347A2A","#21825C","#B3E55E"))(nrow(grass))))

# Final data set for the grass====
grass_new <- cbind(grass,grass_fill) %>%
  rowwise() %>%
  mutate(color = colorRampPalette(c(fill,"#000000"))(10)[2])

# Data set to make the sky/background====
  sky <- tibble(crossing(x = seq(-20,20, length.out = 100),
                         y = seq(-6,19, length.out = 100)))

# Final Image====
  ggplot()+
  theme_void()+
  geom_point(data = sky, aes(x=x,y=y), 
             color = sample(colorRampPalette(c("#28B5B5","#4B778D","#064663","#32C1CD","#ffffff"))(nrow(sky))),
             position = "jitter", 
             size = 10, 
             alpha =sample(seq(.02,.8, length.out = 60),nrow(sky), replace = TRUE))+
  geom_point(data = rainbow_points, aes(x,y, group = logic), 
             shape = 21, 
             fill = rainbow_points$logic, 
             color = rainbow_points$color, 
             position = "jitter",
             size = sample(seq(.2,3, length.out = 60),nrow(rainbow_points), replace = TRUE), 
             alpha =sample(seq(.02,.8, length.out = 20),nrow(rainbow_points), replace = TRUE))+
geom_segment(data = grass_new, aes(x=x,y=y, xend=xend,yend=yend), 
             position = "jitter", 
             size = .2, 
             alpha =sample(seq(.02,.8, length.out = 20),nrow(grass_new), replace = TRUE), 
             inherit.aes = FALSE, 
             color = grass_new$color)+
  xlim(c(-19,19))+
  ylim(c(-5,19))+
  theme(plot.background = element_rect(fill = "#4B778D" ))+
  coord_cartesian(clip = "off")