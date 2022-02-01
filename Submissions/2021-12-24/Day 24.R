#====== Day 20 of 25 Days of Rtistry - Transformations======#

# Library Load in====
library(tidyverse)

# Setting a theta====
theta <- seq(0,2*pi, length.out = 10000)

# Making a janky circle====
circle <- tibble(x = cos(theta)*1:300,
                 y = sin(theta)*1:100,
                 fill = sample(colorRampPalette(c("#33313B","#62374E","#007880","#FDC57B"))(length(theta))),
                 group = rep(1:20, each = 500 )) %>%
  rowwise() %>%
  mutate(color = colorRampPalette(c(fill,"#000000"))(10)[5])

# Final image with transformations in the aes====
circle %>%
  ggplot(aes(x^1.5,y = y*-y,fill = fill, group = group))+
  theme_void()+
  scale_fill_manual(values = circle$fill)+
  geom_point(position = position_jitterdodge(jitter.width = 10, jitter.height = 5, dodge.width = 2),
             color = "#000000", size = seq(.1,10, length.out = nrow(circle)),
             shape = 21, 
             stroke = 2, 
             alpha = sample(seq(.01,.8, length.out = 20),nrow(circle), replace = TRUE))+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#C7B198", color ="#62374E", size = 25))+
  coord_polar("y")