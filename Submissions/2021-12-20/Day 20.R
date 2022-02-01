#====== Day 20 of 25 Days of Rtistry - Texture======#

# Library Load-In====
library(tidyverse)

# Setting the points and colors of the "planet"====
background <- tibble(x = seq(0,10,length.out = 10000),
                     xend = x,
                     y = 0,
                     yend = 10,
                     color = colorRampPalette(c("#E9A6A6", "#864879", "#3F3351", "#1F1D36","#3F3351","#864879","#E9A6A6"))(10000))

# Adding the "texture" data points====
circles <- tibble(crossing(x = seq(0,10, length.out = 100),
                           y = seq(0,10, length.out = 100)))

# Final Image====
background %>%
  ggplot(aes(x=x, y=y, xend = xend, yend = yend))+
  theme_void()+
  geom_segment(color = background$color,
               size = 15)+
  geom_point(data = circles, aes(x,y),
             shape = 21, 
             size = sample(1:20, nrow(circles), replace = TRUE),
             alpha = 0.005,
             fill = sample(c("#ffffff","#1F1D36"),10000, replace = TRUE),
             color = "#ffffff",
             position = "jitter",
             inherit.aes = FALSE)+
  theme(plot.background = element_rect(fill = "#000000"))+
  coord_polar()