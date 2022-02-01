#====== Day 8 of 25 Days of Rtistry - geom_point()======#

# Library Load-In====
library(tidyverse) #For everything data#
library(sp) #For polygon manipulation#

# Starting the transition to a pretty wave====
##Setting up our "range" on the x axis for horizontal waves=====
wave_theta <- seq(from = -.4,
                  to = -1.5*pi, 
                  length.out = 1000) 

##Creating the "top" of our wave polygon====
curve_top <- tibble(x = wave_theta,
                    y = (sin(x)*cos(wave_theta^2))-exp(x*2)) %>%
  arrange(x)


##Grab the max X value in the wave from the top wave====
max_x_point <- curve_top[which(curve_top$x == max(curve_top$x)),]

##Create a subset of curve_top to create a "side" of the wave====
curve_side_right <- max_x_point %>%
  add_row(max_x_point - c(0,0.5))%>%
  arrange(desc(y))


##Create a copy of the curve_top dataset with the y values decreased by .5====
curve_bottom <- curve_top %>%
  mutate(y = y - .5) %>%
  arrange(desc(x))


##Grab the min X value in the wave from the top wave====
min_x_point <- curve_top[which(curve_top$x == min(curve_top$x)),]

##Create a side that will connect curve_top and curve_bottom on the left side====
curve_side_left <- min_x_point %>%
  add_row(min_x_point - c(0,.5)) %>%
  arrange(y)

##Slap all of them together
wave <- bind_rows(curve_top,curve_side_right,curve_bottom,curve_side_left)

# Creating a function for iterations====

wave_maker <- function(n, wave_df){
  
  #Creating an empty list to store our multiple dataframes(waves)#
  wave_list<- list()
  
  #Creating a for loop to iteratively make "n" amount of waves#
  for(i in seq_along(1:n)){
    
    wave_list[[i]] <- wave_df %>%
      mutate(y = y - (.5*i),
             group = i)
    
  }
  
  #returning the completed data frame to the environment#
  return(bind_rows(wave_list))
  
}

# Creating the final data frame used for plotting====
wave_layers <- wave_maker(3, wave)

#Setting some colors in a palette
colors_pal <- c("#381460","#B21F66","#FFBD69")

#Pulling max X and Y values
max_x_value <- max(wave_layers$x)
min_x_value <- min(wave_layers$x)

max_y_value <- max(wave_layers$y)
min_y_value <- min(wave_layers$y)

#Making the range for geom_points
point_range <- tibble(crossing(x = seq(min_x_value,max_x_value, length.out = 110),
                      y = seq(min_y_value,max_y_value, length.out = 110)))

#Filtering out points that only exist in our shape
point_range <- point_range %>%
  mutate(logic_group1 = point.in.polygon(x,y,wave_layers$x[which(wave_layers$group == 1)], wave_layers$y[which(wave_layers$group == 1)]),
         logic_group2 = point.in.polygon(x,y,wave_layers$x[which(wave_layers$group == 2)], wave_layers$y[which(wave_layers$group == 2)]),
         logic_group3 = point.in.polygon(x,y,wave_layers$x[which(wave_layers$group == 3)], wave_layers$y[which(wave_layers$group == 3)]),
         logic = case_when(logic_group1 == 1 ~ "group1",
                           logic_group2 == 1 ~ "group2",
                           logic_group3 == 1 ~ "group3",
                           TRUE ~ NA_character_)) %>%
  select(-c(logic_group1,logic_group2,logic_group3)) %>%
  filter(!is.na(logic))

#Pulling group counts for color distribution===
group1total <- sum(point_range$logic == "group1")
group2total <- sum(point_range$logic == "group2")
group3total <- sum(point_range$logic == "group3")

#Adding fills/colors to the set for easier mapping===
point_range <- point_range %>%
  arrange(x) %>%
  mutate(fill = c(colorRampPalette(c("#000000",colors_pal[1],colors_pal[2]))(group1total),
                   colorRampPalette(c(colors_pal[2],colors_pal[3]))(group2total),
                   colorRampPalette(c(colors_pal[3],"#000000"))(group3total)),
         color = c(colorRampPalette(c("#000000","#ffffff"))(group1total),
                   rep("#ffffff", (group2total)),
                   colorRampPalette(c("#ffffff","#000000"))(group3total)))

# Final Plotting====
point_range %>%
  ggplot(aes(x=x,y=y))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))+
  geom_point(size = sample(seq(.01,5, length.out = 200), nrow(point_range), replace = TRUE), 
             position = "jitter",
             fill = point_range$fill,
             color = point_range$color,
             alpha = sample(seq(.01,.5, length.out = 100), nrow(point_range), replace = TRUE),
             shape = 21,
             stroke = .5)
