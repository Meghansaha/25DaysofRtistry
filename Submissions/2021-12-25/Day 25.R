#====== Day 25 of 25 Days of Rtistry - Festive or Winter======#

# Library Load-in====
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggforce)

# Setting random values up because this started as a throwaway
# and I had no idea where I was going with this...
a = 1
b = 1
theta = seq(0,2*pi, length.out = 500)

x = a*(1-sin(theta))*cos(theta)*.5
y = b*(sin(theta))*c(50:70)

# Data for the head and other parts====

head <- tibble(x=x,
               y=y)

eyes <- tibble(x = c(-.2,.1),
               y = 0)

nose <- tibble(x = c(0,.04,-.09),
               y = c(-21,-19,-19))

mouth <- tibble(x = -.3,
                xend =.3,
                y =  -37,
                yend = -37)

brows <- tibble(x = c(-.05,.2,.2,-.05,-.3,-.3,-.05),
                y = c(6,8.3,9.5,8.1,9.5,8.3,6))


fur <- tibble(crossing(x= seq(-1,1, length.out = 100),
                       y = seq(-60,60, length.out = 100)))

fur <- fur %>%
  mutate(fur = sample(c("/","\\\\"), nrow(fur), replace = TRUE)) %>%
  mutate(logic = sp::point.in.polygon(x,y,head$x,head$y))%>%
  filter(logic == 1)

# Adding in fonts====
font_add_google("Gloria Hallelujah")
showtext_auto()

# Final Image====
head %>%
  ggplot(aes(x,y+10))+
  theme_void()+
  geom_polygon(position = position_jitter(width=.005, height = .005), size = 2, fill = "#B8BE19", color = "black")+
  geom_text(data = fur, aes(x=x,y=y+10, label = fur), 
            color = sample(colorRampPalette(c("#858a11","#B8BE19"))(40),nrow(fur), replace = TRUE),
            alpha = sample(seq(.02,.6, length.out = 25), nrow(fur), replace = TRUE))+
  geom_point(data = eyes, aes(x,y+10), shape = 21, color = "black", fill = "#FFE01A", inherit.aes = FALSE, size = 10, stroke = 2)+
  geom_point(data = eyes, aes(x,y+10), color = "black", inherit.aes = FALSE, size = 3)+
  geom_shape(data = nose, fill = "#231F20", color = "#000000", size = 2)+
  geom_curve(data = mouth, aes(x=x,y=y+10, xend = xend, yend = yend+10), inherit.aes = FALSE, curvature = .3, size = 1.5)+
  geom_polygon(data = brows, color = "black")+
  annotate(geom = "text", x = 0, y = -70, label = "Merry Christmas & Happy Holidays", family = "Gloria Hallelujah",
           color = "white", fontface = "bold", size = 8)+
  geom_polygon(position = position_jitter(width=.005, height = .005), size = 2, fill = NA, color = "black")+
  xlim(-2,2)+
  theme(plot.background = element_rect(fill = "#d61a2d", color = "white", size = 3))