#====== Day 6 of 25 Days of Rtistry - Revisit an old piece======#

# Library Load-In====
library(tidyverse)

# Data for the bottom base====
base <- tibble(x = c(0,100,100,0),
               y = c(0,0,50,50))

# Data for farthest buildings===
background_buildings <- tibble(x = c(0,90,90,80,80,60,60,53,53,41,41,33,33,18,18,0),
                               y = c(50,50,89,89,50,50,72,72,50,50,93,93,50,50,74,74))

# Data for building in the middle-ground====
middle_buildings <- tibble(x = c(0,69,69,56,56,44,44,32,32,28,28,20,20,7,7,0),
                           y = c(50,50,100,100,50,50,68,68,50,50,86,86,50,50,59,59))

# Data for the buidlings in the foreground====
top_buildings <- tibble(x = c(0,99,99,82,82,76,76,63,63,51,51,42,42,36,36,22,22,16,16,2,2,0),
                           y = c(50,50,70,70,50,50,83,83,50,50,66,66,50,50,92,92,50,50,74,74,50,50))

# Function for the sky====
sky_maker <- function(size,x,xend,y,yend){
  
  iterations <- yend/size
  y <- seq(y,yend-size, length.out = iterations)
  yend <- y
  group <- 1:length(iterations)
  color <- colorRampPalette(c("#FBE267","#FC8B1A","#FA464D","#FC76AE",
                              "#F0B0E9","#55A6E7","#415362"))(iterations)

  sky <- tibble(x = x,
                xend = xend,
                y = y,
                yend = yend,
                color = color)
  
  return(sky)
}

# Data for the sky====
background <- sky_maker(size = 1,
                        x = 0,
                        xend = 100,
                        y = 49,
                        yend = 200)

# Final Image====
background %>%
  ggplot(aes(x = x, xend = xend, y =y,yend = yend))+
  theme_void()+
  geom_segment(color = background$color, size = 2)+
  geom_polygon(data = background_buildings, aes(x=x,y=y), fill = "#312c38", inherit.aes = FALSE, alpha = .6)+
  geom_polygon(data = middle_buildings, aes(x=x,y=y), fill = "#000000", inherit.aes = FALSE, alpha = .6)+
  geom_polygon(data = top_buildings, aes(x=x,y=y), fill = "#000000", inherit.aes = FALSE, alpha = .6)+
  geom_polygon(data = base, aes(x=x,y=y), fill = "#000000", size = 1, inherit.aes = FALSE)+
  geom_segment(x = 0, xend = 100, y = 50, yend = 50, color = "#ffffff", size = 1)+
  ylim(0,200)+
  xlim(0,100)+
  coord_polar("x")+
  theme(plot.background = element_rect(fill = "#000000", color = "#ffffff", size = 10))