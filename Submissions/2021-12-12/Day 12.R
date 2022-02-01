#====== Day 12 of 25 Days of Rtistry - Circles======#

# Library Load-in====
library(tidyverse)
library(sp)

# Setting a color palette====
color_pal <- c("#161853","#4C0070","#781D42")

# Setting a theta for the circles====
theta <- seq(0,2*pi, length = 500)

# Creating data for three main circles that'll merge to be one shape====
circles <- tibble(x = c(cos(theta)*10,
                        (cos(theta)*10)+5,
                        (cos(theta)*10)+2.5),
                  y = c(sin(theta)*10,
                        sin(theta)*10,
                        (sin(theta)*10)+5),
                  group = rep(1:3, each = 500),
                  fill = rep(color_pal, each = 500))

# Calculating polygon colors within the data set====
circles <- circles %>%
  rowwise() %>%
  mutate(color = colorRampPalette(c(fill,"#000000"))(10)[3])

# Making a custom function to fill the shape with smaller circles====
circle_maker <- function(n,min_x,max_x,min_y,max_y){
  
  theta <- seq(0,2*pi, length = 500)
  
  circles_list <- list()
  
  for(i in seq_along(1:n)){
    
    circles_list[[i]] <- tibble(x = cos(theta)*.5+sample(seq(min_x,max_x, length.out = 50),1),
                                y = sin(theta)*.5+sample(seq(min_y,max_y, length.out = 50),1),
                                group = i)
  }
  
  return(bind_rows(circles_list))
}

# Creating the data set for the smaller circles within the created shape====
interior <- circle_maker(3000,-10,15,-10,15)

# Filtering to only keep smaller circles that fall within the original shape====
interior <- interior %>%
  mutate(logic_1 = point.in.polygon(x,y,circles$x[which(circles$group == 1)],circles$y[which(circles$group == 1)]),
         logic_2 = point.in.polygon(x,y,circles$x[which(circles$group == 2)],circles$y[which(circles$group == 2)]),
         logic_3 = point.in.polygon(x,y,circles$x[which(circles$group == 3)],circles$y[which(circles$group == 3)])) %>%
  rowwise() %>%
  mutate(total = logic_1 + logic_2 +logic_3) %>%
  filter(total > 0)

# Final image====
interior  %>%
  ggplot(aes(x,y, group = group)) +
  theme_void()+
  geom_polygon(data = circles, aes(x,y,group=group), inherit.aes = FALSE, fill = circles$fill, color = circles$color, size = 5)+
  geom_polygon(alpha = seq(.02,.7, length.out = nrow(interior)),fill = sample(c(color_pal,"black","black","black"),nrow(interior),replace = TRUE), position = position_jitter(width = .05, height = .05), size = .5)+
  theme(plot.background = element_rect(fill = "#000000"))+
  coord_equal() #Thanks Ije <3