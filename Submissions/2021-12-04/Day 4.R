#====== Day 4 of 25 Days of Rtistry - BYOF - "Bring Your Own Functions"======#

#Library Load-In====
library(tidyverse)

# Function to make "paint" texture (or at least some sad attempt)====
make_paint <- function(n,min_x,max_x,min_y,max_y,width,height){
  vertmarks <- list()
  horizmarks <- list()

  for(i in seq_along(1:n)){
      vertmarks[[i]] <- tibble(x=sample(seq(min_x,(max_x-width),length.out = 1000),1),
                           xend = x,
                           y = sample(seq(min_y,(max_y-height),length.out = 1000),1),
                           yend = sample(seq(min_y,(max_y-height),length.out = 1000),1))
  }
  
  for(i in seq_along(1:n)){
    horizmarks[[i]] <- tibble(x=sample(seq(min_x,(max_x-width),length.out = 1000),1),
                             xend = sample(seq(min_x,(max_x-width),length.out = 1000),1),
                             y = sample(seq(min_y,(max_y-height),length.out = 1000),1),
                             yend = y)
  }
  
  return(bind_rows(vertmarks,horizmarks))
}


# Work for the red square====
red_scratches <- make_paint(500,0,5,5,10,.1,.1) 

red_scratches  <- red_scratches  %>%
  mutate(color = sample(colorRampPalette(c("#DD4A48","#7A2928"))(nrow(red_scratches))))

red_square <- tibble(x = c(0,5,5,0),
                     y = c(5,5,10,10),
                     fill = "#DD4A48")

# Work for the tan square====
tan_scratches <- make_paint(500,5,10,5,10,.1,.1) 

tan_scratches  <- tan_scratches  %>%
  mutate(color = sample(colorRampPalette(c("#F5EEDC","#88847A"))(nrow(tan_scratches))))

tan_square <- tibble(x = c(5,10,10,5),
                     y = c(5,5,10,10),
                     fill = "#F5EEDC")

# Work for the blue square====
blue_scratches <- make_paint(500,0,5,0,5,.1,.1) 

blue_scratches  <- blue_scratches  %>%
  mutate(color = sample(colorRampPalette(c("#97BFB4","#536A64"))(nrow(blue_scratches))))

blue_square <- tibble(x = c(0,5,5,0),
                     y = c(0,0,5,5),
                     fill = "#97BFB4")

# Work for the maroon square====
maroon_scratches <- make_paint(500,5,10,0,5,.1,.1) 

maroon_scratches  <- maroon_scratches  %>%
  mutate(color = sample(colorRampPalette(c("#4F091D","#2B0510"))(nrow(maroon_scratches))))

maroon_square <- tibble(x = c(5,10,10,5),
                      y = c(0,0,5,5),
                      fill = "#4F091D")

# Data for the frame around the image====
frame <- tibble(x = c(seq(0,10,length.out = 5000),
                      rep(10,5000),
                      seq(10,0,length.out = 5000),
                      rep(0,5000)),
                y = c(rep(0,5000),
                      seq(0,10,length.out = 5000),
                      rep(10,5000),
                      seq(10,0,length.out = 5000)))

# Final Image ====
#Desired output prevents me from using any dataframe as a base here. Not sure of a cleaner way to execute this right now#
  ggplot()+
  theme_void()+
  geom_polygon(data = red_square, aes(x,y),
               fill = red_square$fill)+
    geom_segment(data = red_scratches, aes(x=x,xend=xend,y=y,yend=yend),
                 color = red_scratches$color)+
    geom_polygon(data = tan_square, aes(x,y),
                 fill = tan_square$fill)+
    geom_segment(data = tan_scratches, aes(x=x,xend=xend,y=y,yend=yend),
                 color = tan_scratches$color)+
    geom_polygon(data = blue_square, aes(x,y),
                 fill = blue_square$fill)+
    geom_segment(data = blue_scratches, aes(x=x,xend=xend,y=y,yend=yend),
                 color = blue_scratches$color)+
    geom_polygon(data = maroon_square, aes(x,y),
                 fill = maroon_square$fill)+
    geom_segment(data = maroon_scratches, aes(x=x,xend=xend,y=y,yend=yend),
                 color = maroon_scratches$color)+
    theme(panel.background = element_rect(fill = "#00A19D",color = "#f2ede4", size = 5))+
    geom_polygon(data = frame, aes(x,y), color = "#00A19D", position = position_jitter(width = .2, height = .1), 
                 fill = NA)

  
