#====== Day 17 of 25 Days of Rtistry - Bubbles======#

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

#Setting up the data for the "bubbles" ====
circles <- tibble(crossing(x = seq(0,10, length.out = 100),
                           y = seq(0,10, length.out = 100)))%>%
  arrange(y)

#Final Image====
background %>%
  ggplot(aes(x=x, y=y, xend = xend, yend = yend))+
  theme_void()+
  geom_segment(color = background$color,
               size = 15)+
  geom_point(data = circles, aes(x,y),
             shape = 21, 
             size = seq(.1,10, length.out = nrow(circles)),
             alpha = 0.05,
             fill = "#000000",
             color = "#ffffff",
             inherit.aes = FALSE,
             position = "jitter")+
  theme(plot.background = element_rect(fill = "#000000"))+
  coord_cartesian(clip = "off", expand = FALSE)