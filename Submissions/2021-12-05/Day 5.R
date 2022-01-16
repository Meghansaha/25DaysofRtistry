#====== Day 5 of 25 Days of Rtistry - geom_text()======#

#Library Load-In====
library(showtext)
library(tidyverse)
library(ggforce)

# Getting a standard theta for the base of the clouds====
theta <- seq(0,2*pi, length.out = 100)

# Function to create dloud data====
cloud_maker <- function(n,rmin,rmax){
  
  clouds <- list()
  
  for(i in seq_along(1:n)){
    theta <- seq(0,2*pi, length.out = 90)
    r <- + sample(seq(rmin,rmax, length.out = 100),1)
    
    clouds[[i]] <- tibble(x = cos(theta)*r + sample(seq(rmin-rmin,rmax+rmax, length.out = 10),1),
                          y = sin(theta)*r + sample(seq(rmin-rmin,rmax+rmax, length.out = 10),1)) %>%
      rowwise() %>%
      mutate(x = x + sample(seq(-.5,.5, length.out = 5000),1),
             y = y +sample(seq(-.5,.5, length.out = 5000),1),
             group = i) 
  }
  return(bind_rows(clouds))
}

# Setting the cloud data====
clouds <- cloud_maker(15,0,10)

# Pulling out x and y constants (mins and maxes)
min_x <- min(clouds$x)
max_x <- max(clouds$x)
min_y <- min(clouds$y)
max_y <- max(clouds$y)

# Getting data for the background design====
background <- tibble(crossing(x = seq(min_x,max_x, length.out = 100),
                     y = seq(min_y,max_y, length.out = 100)))

# Adding in custom fonts with showtext====
font_add_google('Architects Daughter')
showtext_auto()

# Making the %>% Dreams====
text <- tibble(x = c(9,21.5),
               y = c(11,8.5),
               label = c("%>%","Dreams"),
               size = c(30,18))

# Text appearance will vary based on size of final output #
# Adjust for what's needed #

#Final Image====
clouds %>%
  ggplot(aes(x,y,group = group))+
  theme_void()+
  geom_point(data = background, aes(x=x,y=y), inherit.aes = FALSE, 
             color = sample(rep(colorRampPalette(c("#3c8a99","#88E1F2"))(nrow(background)/2), each =2)),
             size = sample(seq(.1,2, length.out = 50),nrow(background), replace = TRUE), 
             shape = sample(c(3,4), nrow(background), replace = TRUE),
             position = position_jitter(width = .5, height = .5))+
  geom_shape(radius = unit(1, 'cm'),
                      color = "#000000",
                      fill = "#fce6c0",
                      size = 5,
                      alpha = .9)+
  theme(panel.background = element_rect(fill = "#88E1F2"))+
  geom_text(data = text, aes(x,y,label = label), family = "Architects Daughter", 
            size = text$size, color = "#000000", inherit.aes = FALSE)+
  coord_cartesian(expand = FALSE)

  