#====== Day 16 of 25 Days of Rtistry - Air======#

#Library Load-In====
library(tidyverse)

#Setting up the background color====
background <- tibble(x = 0,
                     xend = 10,
                     y = seq(0,10,length.out = 1000),
                     yend = y,
                     color = colorRampPalette(c("#af3918", "#a21152", "#822b75", "#612884",
                                                "#154baf", "#0b82b9", "#277e9d", "#488e35",
                                                "#e3a934","#b2336a"))(1000))

#Setting up the data for the "air"/"clouds?" ====
circles <- tibble(crossing(x = seq(0,10, length.out = 100),
                           y = seq(0,10, length.out = 100)))%>%
  arrange(y)

#Calculating the maxes and minx for the axes for the border====
max_x <- max(background$xend)
min_x <- min(background$x)

max_y <- max(background$y)
min_y <- min(background$y)

#Just setting the amount of points i want within each side of the border====
border_n <- 1000

#Data set for the border
border <- tibble(x = c(seq(min_x,max_x, length.out = border_n),
                       rep(max_x,border_n),
                       seq(max_x,min_x,length.out = border_n),
                       rep(min_x,border_n)),
                 y = c(rep(min_y,border_n),
                       seq(min_y,max_y, length.out = border_n),
                       rep(max_y,border_n),
                       seq(max_y,min_y, length.out = border_n)))

#Final Image====
background %>%
  ggplot(aes(x=x, y=y, xend = xend, yend = yend))+
  theme_void()+
  geom_segment(color = background$color,
               size = 15)+
  geom_point(data = circles, aes(x,y),
             size = 5,
             alpha = 0.05,
             color = "#ffffff",
             inherit.aes = FALSE,
             position = "jitter")+
  geom_polygon(data = border, aes(x,y), 
               fill = NA, 
               color = "black", 
               position = position_jitter(height = .2, width = .2), 
              size = 20,
              inherit.aes = FALSE)+
  theme(plot.background = element_rect(fill = "#000000"))+
  coord_flip(clip = "off", expand = FALSE)

