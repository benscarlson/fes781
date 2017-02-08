#script history:
# copied from scripts/figures/map_of_tracks_lines.r
# 2017-01-27 copied from /Users/benc/Documents/Yale/fellowships_grants/nessf17/src/map_of_tracks.r

library(dplyr)
library(ggplot2)
library(readr)
library(rgdal)

setwd('/Users/benc/Documents/Yale/classes/2017 spring/fes781/r')
source('src/clipForPlot.r') #used to clip range

tracks <- read_csv('data/swg13_storks.csv')

#################
# map of tracks #
#################

dat <- tracks

## load range map
range <- readOGR(
  dsn=path.expand('data/gis/Ciconia_ciconia_range/'),
  layer="cartodb-query",verbose=TRUE)
range <- subset(range,range$provider=='jetz')

# load background world map
wmap <- readOGR(
  dsn=path.expand('data/gis/ne_110m_land/'), 
  layer="ne_110m_land")

lonPad <- 3;latPad <- 3

wmap_df_clip <- clipForPlot(wmap,dat,lonPad,latPad)

ggplot() + 
  geom_polygon(data=wmap_df_clip, fill='white',color='black',
               aes(long,lat, group=group)) +
  geom_polygon(data=fortify(range),
               aes(long,lat,group=group), alpha=0.1) + #, color='white'
  coord_map(xlim = c(min(dat$lon)-lonPad, max(dat$lon)+lonPad),
            ylim=c(min(dat$lat)-latPad, max(dat$lat)+latPad)) +
  # geom_path(aes(x=lon, y=lat, color=individual_id), #color=individual_id,
  #           data=dat, alpha=1, size=1.5) +
  geom_point(aes(x=lon,y=lat,color=short_name),data=dat,size=0.5) +
  scale_color_discrete('Individual') +
  theme(
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_blank(),
    axis.title=element_blank(),
    axis.line = element_blank(),
    legend.key = element_blank()
    ) +
  guides(colour = guide_legend(override.aes = list(size=3)))

ggsave(filename='figures/swg13_point_map.pdf', height=6, width=6, units='in')
