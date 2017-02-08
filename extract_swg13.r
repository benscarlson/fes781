library(dplyr)
library(readr)

setwd('/Users/benc/Documents/Yale/classes/2017 spring/fes781/r')

tracks <- read_csv('data/tracks.csv')

nrow(tracks)
tracks %>%
  group_by(study_name) %>%
  summarize(num_points=n())

tracks %>%
  group_by(study_name,individual_id) %>%
  summarize(num_points=n()) %>%
  group_by(study_name) %>%
  summarize(num_indivs=n(),sum(num_points))

alltracks <- read_csv('~/projects/whitestork/data/derived/white_storks_filtered.csv')
short_name <- read_csv('~/projects/whitestork/whitestork_repo/db/short_name.csv')

alltracks %>%
  group_by(study_name,individual_id) %>%
  summarize(num_points=n()) %>%
  group_by(study_name) %>%
  summarize(num_indivs=n(),sum(num_points)) %>% View()

swg13_storks <- alltracks %>% filter(study_name=='LifeTrack White Stork SWGermany 2013') %>%
  left_join(short_name, by=c('individual_id','study_name'))

#nrow(swg13_storks) 
#writeLines(sort(unique(swg13_storks$individual_id)))

write_csv(swg13_storks,'data/swg13_storks.csv')
