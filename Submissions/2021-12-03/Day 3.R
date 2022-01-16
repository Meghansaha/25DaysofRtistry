#====== Day 3 of 25 Days of Rtistry - geom_segment()======#

#Library Load-In====
library(tidyverse)

#Function to make the sky background====
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

# Making sky data with function====
background <- sky_maker(size = 1,
                        x = 0,
                        xend = 10,
                        y = 0,
                        yend = 100)

# Data for water====
water_texture <- tibble(x = sample(seq(0,10,length.out = 1000)),
                        xend = sample(seq(0,10,length.out = 1000)),
                        y = sample(seq(-5,2, length.out = 1000)),
                        yend = y)

# Data for the "top" of the water====
water_top <- tibble(x = seq(0,10, length.out = 5000),
                    xend = seq(0,10, length.out = 5000),
                    y = 2,
                    yend = 2.5)

# Function to make skyscrapers====
skyscraper_maker_horiz <- function(height,width,xmin,xmax){
  building <- list()
  yindex <- seq(0,height, length.out = 100)
  
  base <- tibble(x = sample(c(xmin:(xmax-width)),1),
            xend = x + width,
            y = 0,
            yend = y)
  
  for(i in seq_along(1:length(yindex))){
    building[[i]] <- base %>%
      mutate(y = yindex[i],
             yend = y)
  }
  
  return(bind_rows(building))
}

# Using function multiple time to make buildings====

#Could have optimized this but didn't feel like it#
building <- skyscraper_maker_horiz(height = 75,
                                   width = 2.5,
                                   xmin = 0,
                                   xmax = 10)

building2 <- skyscraper_maker_horiz(height = 50,
                                   width = 1,
                                   xmin = 0,
                                   xmax = 10)

building3 <- skyscraper_maker_horiz(height = 90,
                                    width = 2,
                                    xmin = 0,
                                    xmax = 10)

building4 <- skyscraper_maker_horiz(height = 20,
                                    width = 4,
                                    xmin = 0,
                                    xmax = 10)

building5 <- skyscraper_maker_horiz(height = 40,
                                    width = 4,
                                    xmin = 0,
                                    xmax = 10)

building6 <- skyscraper_maker_horiz(height = 80,
                                    width = 1.1,
                                    xmin = 0,
                                    xmax = 10)

building7 <- skyscraper_maker_horiz(height = 60,
                                    width = 5,
                                    xmin = 0,
                                    xmax = 10)

# Final Image====
#could have optimized this, but didn't feel like it#
background %>%
  ggplot(aes(x = x, xend = xend, y = y, yend = yend))+
  geom_segment(color = background$color, size = 5, position = position_jitter(height = .1), lineend = "round")+
  geom_segment(x = -1, xend = 11, y = -1, yend = -1, size = 20)+
  geom_segment(data = building, color = "#000000", size = 3, alpha = .4)+
  geom_segment(data = building2, color = "#000000", size = 3, alpha = .3)+
  geom_segment(data = building3, color = "#000000", size = 3, alpha = .5)+
  geom_segment(data = building4, color = "#000000", size = 3, alpha = .3)+
  geom_segment(data = building5, color = "#000000", size = 3, alpha = .3)+
  geom_segment(data = building6, color = "#000000", size = 3, alpha = .3)+
  geom_segment(data = building7, color = "#000000", size = 3, alpha = .3)+
  geom_segment(data = water_texture,aes(x = x, xend = xend, y = y, yend = yend),inherit.aes = FALSE, 
               color = sample(colorRampPalette(c("#13124f","#0f1a5c"))(nrow(water_texture))),
               alpha = sample(seq(.01,.4, length.out = 100), nrow(water_texture), replace = TRUE))+
  geom_segment(data = water_top,aes(x = x, xend = xend, y = y, yend = yend),inherit.aes = FALSE, 
               color = "#13124f", position = position_jitter(height = .4))+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
  