#====== Day 2 of 25 Days of Rtistry - Science======#
library(tidyverse)
library(sp)

#Setting up thetas for designs====
theta <- seq(0,2*pi, length.out = 10000)
theta_texture <- seq(0,2*pi, length.out = 5000)

#Setting the radius====
r <- 10

#Initial Circle===
circle <- tibble(x = cos(theta)*r,
                 y = sin(theta)*r) %>%
  mutate(x = x - .9)

#Second circle to act as the "background" that makes it look "3D"====
circle_back <- tibble(x = cos(theta)*r,
                      y = sin(theta)*r)

#Data to give the circle the "texture"====
circle_texture <- tibble(x = cos(theta_texture)*1:r,
                      y = sin(theta_texture)*1:r)

#Filtering out points to keep only texture that exists in the circle====
texture <- tibble(crossing(x = seq(min(circle$x), max(circle$x), length.out = nrow(circle)/20),
                           y = seq(min(circle$y), max(circle$y), length.out = nrow(circle)/20))) %>%
  mutate(logic = point.in.polygon(x,y, circle$x, circle$y)) %>%
  filter(logic == 1)

#Final image====
circle_back %>%
  ggplot(aes(x,y))+
  geom_polygon(color = "#4F3A43", size = 3, fill = "#4a0b2d")+
  geom_polygon(data = circle_texture, aes((x),(y)), inherit.aes = FALSE, color = "#4F3A43")+
  geom_polygon(data = circle, aes((x),(y)), inherit.aes = FALSE, fill = "#c42168", color = "#4F3A43",
               size = 3)+
  geom_point(data = texture, aes(x,y), inherit.aes = FALSE, color = if_else(cos(theta)*texture$y*texture$x + sin(theta)*texture$x*texture$y <= 1 , "#000000", "#b38498"),
             size = .1,
             alpha = sample(seq(.02,.9, length.out = 50),nrow(texture), replace = TRUE),
             position = position_jitter(width = .03))+
  geom_polygon(data = circle, aes((x),(y)), inherit.aes = FALSE, fill = NA, color = "#4F3A43",
               size = 1, position = position_jitter(width =.003, height = .02))+
  theme_void()+
  theme(panel.background = element_rect(fill = "#9c7c57", color = "#ffffff", size = 4))