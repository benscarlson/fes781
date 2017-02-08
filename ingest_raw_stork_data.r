library(readr)
library(dplyr)

setwd('/Users/benc/Documents/Yale/classes/2017 spring/fes781/r')

tracks <- read_csv('data/storks.csv') #I called the csv file 'storks.csv'
colnames(tracks) <- gsub('-','_',colnames(tracks))

#event-id: unique row number
#visible: not sure, i think it has to do with data permissions. it's always true so we don't need it
#timestamp: the time/date the point was taken
#location_long: longitude
#location_lat: latitude
#study_specific_measurement: name of study
#sensor_type: can be different sensors, but it's always gps here
#individual_taxon_cannonical_name: species name. always Ciconia ciconia
#tag_local_identifier: I think this is the id of the sensor. storks can have multiple sensors over their lifetimes
#study_name: this is actually the dataset name. 

tracks <- tracks %>%
  select(-c(visible,sensor_type,individual_taxon_canonical_name,
    tag_local_identifier,study_name)) %>%
  rename(lon=location_long,lat=location_lat,study_name=study_specific_measurement,
    individual_id=individual_local_identifier) %>%
  filter(complete.cases(.))

write_csv(tracks,'data/tracks.csv')

