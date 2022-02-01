#====== Day 19 of 25 Days of Rtistry - No Color======#

#Library Laod-In====
library(tidyverse)

# Manual data for the road====
road <- tibble(x = c(0,100,55,45,0),
               y = c(0,0,60,60,0))

# Data for the grass/ground area====
grass <- tibble(x = c(seq(0,100, length.out = 200),
                      rep(100,200),
                      seq(100,0, length.out = 200),
                      rep(0,200)),
                y = c(rep(0,200),
                      seq(0,60, length.out = 200),
                      rep(60,200),
                      seq(60,0, length.out = 200)))

# Data for the stars====
stars <- tibble(crossing(x = seq(0,100, length.out = 20),
                y = seq(60,100, length.out = 10)))

# Theta/Angle to create the moon====
theta_moon <- seq(0,2*pi, length.out = 100)

# Data for the moon====
moon <- tibble(x = 20*cos(theta_moon)+50,
               y = 30*sin(theta_moon)+50)

# Data to add texture to the road====
road_texture <- tibble(crossing(x = seq(0,100, length.out = 600),
                       y = seq(0,60, length.out = 600))) 

# Filtering out the data to only lay in our pre-made "road" shape====
road_texture <- road_texture %>%
  mutate(logic = sp::point.in.polygon(x,y,road$x,road$y))%>%
  filter(logic == 1)

# Data for the "testure" of our ground/grass
grass_texture <- tibble(x = rep(0,200),
                        xend = rep(100,200),
                        y = seq(0,60, length.out = 200),
                        yend = y)

# Final Image====
stars %>%
  ggplot(aes(x,y))+
  theme_void()+
  geom_point(position = "jitter", size = .01, color = "white")+
  geom_polygon(data = moon, position = position_jitter(width = .3, height = .1), fill = "white", size = 1)+
  geom_polygon(data = moon, position = position_jitter(width = .3, height = .1), fill = NA, color = "black", size = 1)+
  geom_polygon(data = moon, position = position_jitter(width = .3, height = .1), fill = NA, color = "black", size = 1)+
  geom_polygon(data = moon, position = position_jitter(width = .3, height = .1), fill = NA, color = "black", size = 1)+
  geom_polygon(data = grass, position = position_jitter(width = .3, height = .1), fill = "white", color = "black", size = 1)+
  geom_polygon(data = grass, position = position_jitter(width = .3, height = .2), fill = NA, color = "black", size = 1)+
  geom_polygon(data = grass, position = position_jitter(width = .3, height = .3), fill = NA, color = "black", size = 1)+
  geom_segment(data = grass_texture, aes(x=x,y=sample(y),xend=xend,yend=sample(yend)),position = position_jitter(width = 3, height = .3), color = "black", size = .3, 
               inherit.aes = FALSE, alpha = sample(seq(.4,1, length.out = 10), nrow(grass_texture), replace = TRUE))+
  geom_point(data = road_texture, size = sample(seq(.01,1, length.out = 25),nrow(road_texture), replace = TRUE), 
             position = "jitter", color = sample(colorRampPalette(c("#000000","#666666"))(nrow(road_texture))))+
  coord_cartesian(xlim = c(0,100),
                  ylim = c(0,100), 
                  expand = FALSE)+
  theme(plot.background = element_rect(fill = "black"))