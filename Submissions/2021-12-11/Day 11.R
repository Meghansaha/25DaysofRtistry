#====== Day 11 of 25 Days of Rtistry - geom_line()======#

# Library Load-in====
library(tidyverse)

# Creating the data for the colorwheel
data <- tibble(x = seq(0,100, length.out = 800),
               y = sample(seq(0,200, length.out = 800)))

# Setting the color palette====
color_pal <- c("#9F0243", "#F9A65E", "#FDFDBC", "#81CCA4", "#3981B7", "#5C4FA1")

# Setting up for the center point====
center_point <- tibble(x = 50,
                       y = 0)

# Final image====
data %>%
  ggplot(aes(x,y))+
  theme_void()+
  geom_line(size = 3, lineend = "square", color = colorRampPalette(color_pal)(nrow(data)))+
  geom_point(data = center_point, color = "black", size = 40)+
  theme(plot.background = element_rect(fill = "black"))+
  coord_polar()
  