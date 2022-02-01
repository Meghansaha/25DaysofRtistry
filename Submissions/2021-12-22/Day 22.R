#====== Day 22 of 25 Days of Rtistry - Suprematism======#

# Library Load-In====
library(tidyverse)
library(pracma)

# Setting up initial data and radi====
data <- tibble(x = sample(1:100000))
r <- seq(0,2*pi, length.out = nrow(data))

# Transforming it====
data <- data %>%
  mutate(y = (x^2)/x*sample(1:50)+x) %>%
  mutate(x = sin(x),
         y = cos(y))

# Adding the column of radi====
data <- cbind(data,r)

# Converting into a matrix for the pracma package====
matrix <- as.matrix(data)

# Feeble attempt at converting cartsian points to polar coordinates====
new_data <- cart2pol(matrix)

# Converting matrix back into a dataframe====
data <- as.data.frame(new_data)

# Final Image====
data %>%
  ggplot(aes((phi*sin(r)*cos(z))*z,(phi*sin(r)*sin(z))*r))+
  theme_void()+
  geom_point(size = sample(seq(.1,5,length.out = 25),nrow(data),replace = TRUE),
             alpha = sample(seq(.02,.5,length.out = 25),nrow(data),replace = TRUE),
             shape = 21,
             color =  colorRampPalette(c("#000000","#333333"))(nrow(data)),
             fill = colorRampPalette(c("#94B5C0","#350B40","#453953","#137083","#AD6C80","#1F6F8B"))(nrow(data)))+
  theme(plot.background = element_rect(fill = "#ffffff", color = "#000000", size = 10))+
  coord_polar()