#====== Day 7 of 25 Days of Rtistry - Monochrome======#

# Library Load In====
library(tidyverse)

# Making the green monochrome color palette====
color_pal <-c("#264e2c","#55761a","#9db33e", "#dfe49b", "#f6f7cd")

# Function to make the grid of squares====
grid_maker <- function(rows,cols,color_pal){
  
  base <- tibble(x = c(0,1,1,0),
                 y = c(0,0,1,1))
  row <- list()
  col <- list()
  
  for(i in seq_along(1:rows)){
    row[[i]] <- base %>%
      mutate(x = x + i)
  }
  
  row <- bind_rows(base,row)
  
  for(i in seq_along(1:cols)){
    col[[i]] <- row %>%
      mutate(y = y + i)
  }
  
  grid_squares <- (bind_rows(col))
  
  group_total <- nrow(grid_squares)/4
  
  colors_pal <- sample(color_pal,group_total, replace = TRUE)
  
  grid_squares <- grid_squares %>%
    mutate(group = rep(1:group_total, each = 4),
           fill = rep(colors_pal,each = 4))
  
  return(grid_squares)
  
}


# Function to make the internal grid of squares====
inside_grid_maker <- function(rows,cols,color_pal){
  
  base <- tibble(x = c(.3,.7,.7,.3),
                 y = c(.3,.3,.7,.7))
  row <- list()
  col <- list()
  
  for(i in seq_along(1:rows)){
    row[[i]] <- base %>%
      mutate(x = x + i)
  }
  
  row <- bind_rows(base,row)
  
  for(i in seq_along(1:cols)){
    col[[i]] <- row %>%
      mutate(y = y + i)
  }
  
  grid_squares <- (bind_rows(col))
  
  group_total <- nrow(grid_squares)/4
  
  colors_pal <- sample(color_pal,group_total, replace = TRUE)
  
  grid_squares <- grid_squares %>%
    mutate(group = rep(1:group_total, each = 4),
           fill = rep(colors_pal,each = 4)) %>%
    rowwise() %>%
    mutate(color =  colorRampPalette(c(fill,"#000000"))(10)[3])
  
  return(grid_squares)
  
}

# Using the functions to amke the data====
grid_squares <- grid_maker(10,10,color_pal) 
inside_grid <- inside_grid_maker(10,10,color_pal)

# Final Image
grid_squares %>%
  ggplot(aes(x=x,y=y, group = group))+
  theme_void()+
  geom_polygon(fill = grid_squares$fill, color = "#000000", size = 2, position = position_jitter(width = .01, height = .03))+
  geom_polygon(data = inside_grid, fill = inside_grid$fill, color = inside_grid$color, position = position_jitter(width = .01, height = .03))
