#===============================================================================
# Prepare the working environment
#===============================================================================
dir <- "your directory path"
setwd(dir)

library(readxl)
library(tidyverse)
library(ggmap)

#===============================================================================
# Read the data from OECD and map data map package
#===============================================================================

data <- read_xlsx("OECD Families.xlsx", sheet = 2)

world <- map_data("world")

# Map data has ancronyms for USA and UK
data$cname <- ifelse(data$cname == 'United States', 'USA', data$cname)
data$cname <- ifelse(data$cname == 'United Kingdom', 'UK', data$cname)

#join the data
world<- left_join(world, data, by = c('region' = 'cname'))

#fill the fertility rate columns with 0 if a country is not in OECD
world$tot.fert <- ifelse(is.na(world$tot.fert), 0, world$tot.fert)

#===============================================================================
# Map
#===============================================================================

#Coordinates for the israel arrow
arrow <- 
  tibble(
    x1 = c(45),
    x2 = c(37),
    y1 = c(23), 
    y2 = c(31)
  )

#only israel data
israel <- world %>% filter(region == "Israel")

#only israel map
israel_map <- ggplot() +
  geom_polygon(data = israel, aes(x = long, y = lat), fill = "brown1") +
  labs(title = 'Israel') +
  theme(text = element_text(family = "serif", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )

#world map
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group, fill = tot.fert)) +
  scale_fill_gradient("Fertility Rate", low = "bisque", high = "brown1") +
  labs(title = 'OECD Fertility Rates', subtitle = "The Organization for Economic Co-operation and Development \n Author: Nikola Baci") +
  theme(text = element_text(family = "serif", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30, hjust = 0.5)
        ,plot.subtitle = element_text(size = 10, hjust = 0.5)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.background = element_rect(fill = "#444444"),
        legend.position="bottom"
  ) +
  guides(fill=guide_colorbar(ticks.colour = NA)) +
  #embed Israel map
  annotation_custom(grob = ggplotGrob(israel_map), xmin = 5, xmax = 45, ymin = -15, ymax = 23) +
  #embed the arrow
  geom_curve(
    data = arrow, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 1,
    color = "black", curvature = 1)


