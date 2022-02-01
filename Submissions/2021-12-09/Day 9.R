#====== Day 9 of 25 Days of Rtistry - Iterations======#

#I was supposed to be practicing rotating shapes, but I couldn't figure out how to maintain the polygons shape -.-#

# Library Load-in====
library(tidyverse)

# Function to make transformed shapes====
shapes <- function(n,rotation,xmin,xmax,ymin,ymax,r){
  theta <- seq(0,pi, length.out = 100)
  
  midpoint_x = 0
  midpoint_y = r/2
  
  base <- tibble(x = c(cos(theta)*r,
                       seq(r,0, length.out = 100)),
                 y = c(sin(theta)*r,
                       rep(0,100)),
                 group = 1)
  
  shape_list <- list()
  
  for(i in seq_along(1:n)){
    shape_list[[i]] <-  shape %>%
      mutate(x = x + sample(seq(xmin,xmax, length.out = 10000),1)) %>%
      mutate(x = (x - (midpoint_x+sample(seq(xmin,xmax, length.out = 10000),1)))*cos(sample(seq(rotation-10,rotation,length.out = 100),1))-(y-midpoint_y)*sin(sample(seq(rotation-10,rotation,length.out = 100),1)) + (midpoint_x+i/100),
             y = (x - (midpoint_x+sample(seq(xmin,xmax, length.out = 10000),1)))*sin(sample(seq(rotation-10,rotation,length.out = 100),1))-(y-midpoint_y)*cos(sample(seq(rotation-10,rotation,length.out = 100),1)) + (midpoint_y+i/100),
             group = group + i)
  }
  
  return(bind_rows(base,shape_list))
}

# Setting the blue palette====
blues <- c("#39A2DB", "#028994", "#048A96", "#015376", "#084165", "#0F8FA5", "#0E3057")

#Maybe my cpu is a potato, but this takes a minute. could make it faster, but meh====
data <- shapes(5000,.2,0,10,0,10,1) %>%
  group_by(group) %>%
  mutate(fill = sample(colorRampPalette(blues)(200))) %>%
  rowwise() %>%
  mutate(color = colorRampPalette(c(fill,"#000000"))(10)[2])

# Final Image====
data %>%
  ggplot(aes(cos(seq(0,2*pi, length.out = nrow(data)))*x,sin(seq(0,2*pi, length.out = nrow(data)))*y, group = group))+
  theme_void()+
  geom_polygon(size = .8, color = data$color, fill = data$fill)+
  xlim(c(2,12))+
  ylim(c(0,25))+
  theme(plot.background = element_rect(fill = "#B2AB8C", color = "#8A846C", size = 15))
