#====== Day 1 of 25 Days of Rtistry - Nature======#

#Library Load-In====
library(tidyverse)

#Work for the sky====

#Setting a color palette for the sky in the background===
sky_pal <- c("#fdacab","#c66c92","#836193","#434a73","#439895","#046c8e","#042047")

#Creating a function that will create the data for the sky in the background===
# "n" determines how many polygons will fill in the sky background #
sky_maker <- function(n){
  
  #Creating the base of a "skinny" rectangle===
  x <- c(0,10,10,0)
  
  y <- c(0,0,.01,.01)
  
  #Making an empty list to fill with lots of tibbles/data===
  list_back <- list()
  
  #For-loop to iterate "n" amount of tibbles/data frames into our empty list== 
  for(i in seq_along(1:n)){
    list_back[[i]] <- tibble(x = x,
                             y = y + i*.01,
                             group = i)
  }
  return(bind_rows(list_back)) #converts all tibbles in the list into a single data frame#
}

# Using the function to make a sky with 1000 polygons. Storing in an object called "skies"
skies <- sky_maker(1000) %>%
  mutate(fill = rep(colorRampPalette(sky_pal)(1000), each = 4))


#Work for the stars====
#Setting the initial points====
stars_grid <- tibble(crossing(x = seq(0,10, length.out = 100),
                              y = seq(0,10, length.out = 100))) 

#Taking a random sample of 200 rows==
stars <- slice_sample(stars_grid, n = 200)


#Work for the mountain ranges====

# Function that creates one set of mountains====
mountain_ranger <- function(base,height,rows){
  
  #Creating the first foundation the function will build off of==
  foundation <- tibble(x = c(0, base, seq(base,0, length.out = 20)),
                       y = c(0, 0, height, height+sample(seq(-.2,.8,length.out = 100),18, replace = TRUE), 0))
  
  #Creating an empty list to store the data for the generated mountain ranges==
  mountains <- list()
  
  #Using a for-loop to iterate through the data to create the correct number of rows/ranges we want==
  for(i in seq_along(1:rows)){
    mountains[[i]] <- foundation %>%
      mutate(y = abs(y + c(0, 0, i*(sample(seq(-.8,.8,length.out = 100), nrow(foundation) - 3)), 0)),
             group = i)
  }
  
  #Adding the "group" variable to our original foundation that was created earlier==
  foundation <- foundation %>%
    mutate(group = 0)
  
  #Binding the generated mountain data together==
  mountains <- bind_rows(mountains)
  
  #Binding the generated mountain data and the foundation data together into one data frame==
  range <- rbind(foundation,mountains)
  
  #Returning the data frame to the environment==
  return(range)
}

#Using the custom function to make two sets of ranges===
range <- mountain_ranger(6,5,3)
range2 <- mountain_ranger(10,1.8,3)

#Calculating the total amount of ROWS located within EACH group in each set of ranges==
group_n <- nrow(range)/length(unique(range$group))
group_n2 <- nrow(range2)/length(unique(range2$group))

#Calculating the TOTAL number of GROUPS within each set of ranges==
group_total <- length(unique(range$group))
group_total2 <- length(unique(range2$group))

#Creating custom color palettes for each set of ranges==

# Colors for the first range data set==
mountain_pal_range <- sort(rep(sample(c("#011d26","#02303f","#1c5a73","#4a748c"),group_total, 
                                      replace = TRUE), each = group_n))

# Colors for the second range data set==
mountain_pal_range2 <- sort(rep(sample(c("#011d26","#02303f","#1c5a73","#4a748c"),group_total2, 
                                       replace = TRUE), each = group_n2))



#Work for the trees====

#Creating data for our "Trees"===
trees <- tibble(x = c(0, 10, seq(10,0, length.out = 500)), 
                y = c(0, 0, .4, sample(seq(.4,1, length.out = 498)), 0))



#Work for the exterior frame====
frame <- tibble(x = c(0,10,10,0),
                y = c(0,0,10,10))

# Final plot creation====
skies  %>%
  ggplot(aes(x=x,y=y, group = group)) +
  theme_void()+
  theme(panel.background = element_rect(fill = "#042047"))+
  coord_cartesian(xlim = c(0,10), ylim = c(0,10), clip = "on") +
  geom_polygon(fill = colorRampPalette(sky_pal)(nrow(skies))) +
  geom_point(data = stars, aes(x=x,y=y), 
             color = "#ffffff",
             position = position_jitter(width =.05, height = .03), 
             alpha = sample(seq(.2,.9, length.out = 1000),nrow(stars), replace = TRUE),
             size = sample(seq(.02,.2, length.out = 1000),nrow(stars), replace = TRUE), 
             inherit.aes = FALSE)+ 
  geom_polygon(data = range, 
               fill = mountain_pal_range,
               color = "#011D26", 
               size =.56,
               alpha = .9, 
               position = position_jitter(width = .1)) +
  geom_polygon(data = range2, 
               fill = mountain_pal_range2,
               color = "#011D26", 
               size =.5,
               alpha = .9, 
               position = position_jitter(width = .1)) +
  geom_polygon(data = trees, aes(x=x,y=y), 
               color = "#082922", 
               size = 2, 
               fill = "#224b39",
               inherit.aes = FALSE)+
  geom_polygon(data = frame, aes(x=x,y=y), 
               color = "#000000", 
               fill = NA, 
               size = 7, 
               position = position_jitter(width = .1, height = .1),
               inherit.aes = FALSE)