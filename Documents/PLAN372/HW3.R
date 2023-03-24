library(tidyverse)
library(sf)

#load in the block groups and project
block_groups <- read_sf("hw3-data/orange_durham_wake_block_groups.shp")

ggplot() + 
  geom_sf(data=block_groups)

#load in the supermarkets and project
supermarkets <- read_sf("hw3-data/triangle_supermarkets_osm.shp")

ggplot()+
  geom_sf(data=block_groups) +
  geom_sf(data=supermarkets, color = "blue")

#transform the data so it is in NC projection 
block_groups <- st_transform(block_groups,32119)
supermarkets <- st_transform(supermarkets, 32119)

#make the buffer for the supermarkets and map it again with buffer
sm_buffer <- st_buffer(supermarkets, 1609.34)

ggplot()+
  geom_sf(data=block_groups) + 
  geom_sf(data= sm_buffer, color = "blue") +
  geom_sf(data=supermarkets, color = "pink") 

#Find the census block groups near the supermarkets
intersects <- st_intersects(block_groups, sm_buffer)

block_groups$supermarketzone <- apply(intersects, 1, any)

wake_county <- subset(block_groups, COUNTYFP== "183")
  
ggplot() + 
  geom_sf(data=wake_county, aes(fill = supermarketzone))+
  geom_sf(data=supermarkets, color = "pink")

write_sf(wake_county, "wake_county.shp")
write_sf(supermarkets, "supermarkets.shp")
write_sf(sm_buffer, "supermarketbuffer.shp")

#Find out what percentage of the wake county population resides in a food desert 
census = read_csv( "hw3-data/triangle_census.csv",
  col_types=c(GEOID="character"))

wake_county <- left_join(wake_county, census, by = "GEOID")

wake_county_sm <- group_by(wake_county, supermarketzone) %>% summarize(total_population = sum(total_population))



wake_county_sm[1,2]/1069079 #9.9% of wake county is in a food desert
wake_county_sm[2,2]/1069079 #90.1% of wake county is not in a food desert 

#Find the percentage of people that do not own a vehicle in food deserts in wake county 

wake_county_fd <- subset(wake_county, supermarketzone == FALSE)

fd_no_vehicle <-sum(wake_county_fd$zero_vehicle_households)/sum(wake_county_fd$total_households)
#3.8% of people in wake county food deserts do not own cars

#Find the percentage of people that are low income in food deserts in wake county 

fd_low_income <- sum(wake_county_fd$households_income_less_than_35k) / sum(wake_county_fd$total_households)
#21.4% of households in wake county food deserts are considered low income

#Find the percentage of people that do not own a vehicle in wake county 
wake_no_vehicle <- sum(wake_county$zero_vehicle_households) / sum(wake_county$total_households)
#3.9% of households in wake county do not own a vehicle

#Find the percentage of people that are low income in wake county 
wake_low_income <- sum(wake_county$households_income_less_than_35k)/sum(wake_county$total_households)
#18.8% of people in wake county are low income

#Make a two mile buffer for supermarkers and a new block groups map 
sm_buffer2 <- st_buffer(supermarkets, 3218.68)
      
write_sf(sm_buffer2, "2milesupermarketbuffer.shp")

intersects2 <- st_intersects(block_groups, sm_buffer2)

block_groups$supermarketzone2 <- apply(intersects2, 1, any)

wake_county2 <- subset(block_groups, COUNTYFP== "183")

ggplot() + 
  geom_sf(data=wake_county2, aes(fill = supermarketzone2))+
  geom_sf(data=supermarkets, color = "pink")

write_sf(wake_county2, "wakecounty2.shp")
