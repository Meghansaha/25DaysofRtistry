#====== Day 14 of 25 Days of Rtistry - Borrow Some Tidy Tuesday Data======#

# Library Load-In====
library(tidyverse)

# Data Load In====
SG_Voices <- read_csv("Submissions/2021-12-14/SG_Voices.csv")

# Data Wrangling/Transformation=====
SG_voices_sum <- tibble(x = c(seq(20,100, by = 20),
                              seq(140,220, by = 20)),
                        y =c(sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Ginger|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Sporty|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Scary|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Baby|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Posh|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Ginger|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Sporty|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Scary|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Baby|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Posh|All"))),
                        spice = rep(c("Ginger","Sporty","Scary","Baby","Posh"),2),
                        album = c(rep("Spice",5),rep("Spiceworld",5)))

# We need Lollipops and they need a "bumpy" part====
# The foundation for our base, a theta for a circle.
theta <- seq(0,2*pi, length.out = 100)

#Setting an empty list to iterate and make each spice a lollipop for each album we recognize (Forever doesnt exist here sry)====
spice_pops <- list()

#Getting the bases of the lollipops in a list====
for(i in seq_along(1:nrow(SG_voices_sum))){
  spice_pops[[i]] <- tibble(x = 7*cos(theta)+SG_voices_sum$x[i],
                            y = 7*sin(theta)+(SG_voices_sum$y[i]),
                            group = SG_voices_sum$spice[i])
}

# Setting names for each spice's lollipop====
names(spice_pops) <- paste0(unique(SG_voices_sum$spice),c(rep("_Spice",5),rep("_SW",5)))

#Smooshing into a dataframe====
spice_pops_base <- bind_rows(spice_pops)

# Doing the same process for the bumpy part of the lollipop. We can use geom_segment to make the illusion of a "bump" in the lollipop====
bumpy_part <- list()

for(i in seq_along(1:length(spice_pops))){
  bumpy_part[[i]] <- tibble(x = min(spice_pops[[names(spice_pops)[i]]][["x"]]),
                            xend = max(spice_pops[[names(spice_pops)[i]]][["x"]]),
                            y = median(spice_pops[[names(spice_pops)[i]]][["y"]]),
                            yend = median(spice_pops[[names(spice_pops)[i]]][["y"]]),
                            group = SG_voices_sum$spice[i])
}

spice_pops_bump <- bind_rows(bumpy_part)

# Setting up the color palettes====
color_pal <- rep(c("#C04000","#90ee90","#DAA520","#f4c2c2","#2c003f"),2)
border_pal <- sapply(color_pal, function(x) colorRampPalette(c(x,"#000000"))(10)[3], USE.NAMES = FALSE)


# Creating the block backgrounds====
background_left <- tibble(x = c(-Inf,120,120,-Inf),
                          y = c(-Inf,-Inf,Inf,Inf))

background_right <- tibble(x = c(120,Inf,Inf,120),
                           y = c(-Inf,-Inf,Inf,Inf))

# Making the final plot====
spice_pops_base %>%
  ggplot(aes(x,y, group = group))+
  theme_void()+
  geom_polygon(data = background_left, aes(x,y), fill = "#A1C724", inherit.aes = FALSE)+
  geom_polygon(data = background_right, aes(x,y), fill = "#E52B97", inherit.aes = FALSE)+
  geom_segment(data = SG_voices_sum, aes(x =x, xend = x, y = -9, yend = y), color = "black", size = 3, inherit.aes = FALSE)+
  geom_segment(data = SG_voices_sum, aes(x =x, xend = x, y = -9, yend = y), color = "white", size = 2, inherit.aes = FALSE)+
  geom_polygon(fill = rep(color_pal, each = 100))+
  geom_segment(data = spice_pops_bump, aes(x = x, xend = xend, y=y, yend=yend, group = group), inherit.aes = FALSE, color = border_pal, size = 3.5, lineend = "round")+
  geom_segment(data = spice_pops_bump, aes(x = x, xend = xend, y=y, yend=yend, group = group), inherit.aes = FALSE, color = color_pal, size = 2.5, lineend = "round")+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"))+
  coord_polar()