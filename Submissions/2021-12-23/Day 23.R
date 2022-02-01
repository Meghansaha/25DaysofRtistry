#====== Day 23 of 25 Days of Rtistry - Trees======#

# This prompt was influenced by Group R Asturias
# Esto fue influenciado por el Grupo R Asturias
# https://twitter.com/grupoRasturias

# Library Load-in====
library(tidyverse)

# Data for the tree====
tree <- tibble(x = c(seq(20,80, length.out = 1000),
                     seq(80,50, length.out = 1000),
                     seq(50,20, length.out = 1000)),
               y = c(rep(15,1000),
                     seq(15,80, length.out = 1000),
                     seq(80,15, length.out = 1000)))

# Color palette for the tree====
tree_pal <- c("#444941","#32502E","#406343","#5B6D5B")

# Texture for the tree====
tree_texture <- tibble(crossing(x = c(seq(20,80, length.out = 50),
                             seq(80,50, length.out = 50),
                             seq(50,20, length.out = 50)),
                       y = c(rep(15,50),
                             seq(15,80, length.out = 50),
                             seq(80,15, length.out = 50)))) %>%
  mutate(logic = sp::point.in.polygon(x,y,tree$x,tree$y)) %>%
  filter(logic == 1)

# Data to create the ornaments====
ornaments <- tibble(crossing(x = c(seq(20,80, length.out = 6),
                                                   seq(80,50, length.out = 6),
                                                   seq(50,20, length.out = 6)),
                                             y = c(rep(15,6),
                                                   seq(15,80, length.out = 6),
                                                   seq(80,15, length.out = 6)))) %>%
  mutate(logic = sp::point.in.polygon(x,y,tree$x,tree$y)) %>%
  filter(logic == 1) 

# Applying colors to the ornaments====
ornaments <- ornaments %>%
  mutate(fill = sample(c("#BB3B0E","#F9DE79"),nrow(ornaments), replace = TRUE),
         shape = sample(c(21,23),nrow(ornaments), replace = TRUE)) %>%
  rowwise() %>%
  mutate(color = colorRampPalette(c(fill,"#000000"))(10)[3])

# Creating the texture for the tree trunk====
trunk_texture <- tibble(crossing(x = c(seq(48,52, length.out = 100),
                              rep(52,100),
                              seq(52,48, length.out = 100),
                              rep(48,100)),
                        y = c(rep(0,100),
                              seq(0,20, length.out = 100),
                              rep(20,100),
                              seq(20,0,length.out = 100))))

# Creating the color palette for the tree trunk====
trunk_pal <- c("#672F2F","#3D262A","#A35638","#AC3F21")

# Varying and applying the color palette for the tree trunk====
trunk_texture <- trunk_texture %>%
  mutate(color = sample(c("#672F2F","#3D262A","#A35638","#AC3F21"),nrow(trunk_texture), replace = TRUE))

# Creating the now cover for the ground====
snow_cover <- tibble(crossing(x = c(seq(0,100, length.out = 100),
                           100,
                           seq(100,0, length.out = 100),
                           0),
                     y = c(rep(0,100),
                           sin(100),
                           abs(sin(seq(0,100, length.out = 100)))+1,
                           0))) 

# Applying and randomizing colors for the ground====
snow_cover <- snow_cover %>%
  mutate(fill = sample(c("#F6F5FA","#D9DCED","#DEE8F2","#C7D8E8","#9BA9D0"), nrow(snow_cover), replace = TRUE)) %>%
  rowwise() %>%
  mutate(color = colorRampPalette(c(fill,"#000000"))(10)[6])
  
# Data for creating the sky/background ====
sky <- tibble(crossing(x = rep(0,50),
              y = seq(0,100, length.out = 50),
              xend = rep(100,50),
              yend = seq(0,100, length.out = 50)))

# Adding some snowfall data====
snow_fall <- tibble(crossing(x = seq(0,100, length.out = 50),
                    y = seq(0,100, length.out = 50)))

# Adding some star data====
star <- tibble(x = (5*cos(seq(0,2*pi, length.out = 200)) * 0:2) + 50,
               y = (5*sin(seq(0,2*pi, length.out = 200)) * 0:2) + 80)

# Final Image====
tree %>%
  ggplot(aes(x,y))+
  theme_void()+
  geom_segment(data = sky, aes(x = x, xend = xend, y = y, yend = yend), 
               color = sample(colorRampPalette(c("#00347A","#2A1274","#110944","#010510"))(nrow(sky))),
               size = 5, alpha = .2, position = position_jitter(width = 5, height = 2))+
  geom_text(data = trunk_texture, aes(x=x,y=y, label = sample(c("|","/","\\\\"),nrow(trunk_texture), replace = TRUE)), 
            position = position_jitter(width = .01, height = 1),
               color = trunk_texture$color, size = 10, inherit.aes = FALSE,
            alpha = sample(seq(.05,.6, length.out = nrow(trunk_texture))))+
  geom_point(data = snow_cover, color = "#ffffff", fill = snow_cover$fill, 
             size = sample(seq(.01,5, length.out = 25),nrow(snow_cover), replace =  TRUE),
             shape = 21, stroke = sample(seq(1,2,length.out = nrow(snow_cover))), alpha = sample(seq(.02,.8, length.out = nrow(snow_cover))),
             position = position_jitter(width = 1, height = 1))+
  geom_polygon(position = position_jitter(width = 1, height = 2),
               color = tree_pal[1], fill = tree_pal[1], size = 2)+
  geom_point(data = tree_texture, shape = 21, color = tree_pal[1],
             fill = sample(colorRampPalette(tree_pal)(nrow(tree_texture))),
             size = sample(seq(.1,6, length.out = 30),nrow(tree_texture), replace = TRUE),
             alpha = sample(seq(.01,.7, length.out = 20), nrow(tree_texture), replace = TRUE),
             position = position_jitter(width = 1, height = 2))+
  geom_polygon(data = star, color = "#FFCE45", fill = "#D4AC2B", size = .1)+
  geom_point(data = ornaments, shape = ornaments$shape, color = ornaments$color,
             fill = ornaments$fill,
             size = sample(seq(1,6, length.out = 30),nrow(ornaments), replace = TRUE),
             stroke = 2, position = position_jitter(width = 1, height = 2))+
  geom_point(data = snow_fall, color = "#ffffff", size = sample(seq(.01,1.4, length.out = nrow(snow_fall))),
             alpha = sample(seq(.01,.7, length.out = nrow(snow_fall))), position = position_jitter(width = 10, height = 5))+
  coord_cartesian(xlim=c(0,100),ylim=c(0,100), expand = FALSE, clip = "off")+
  theme(plot.background = element_rect(fill = "#000000"))