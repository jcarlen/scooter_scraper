#install.packages("rjson")
#install.packages("jsonlite") #need that version of fromJSON for flatten? or could do with rjson's fromJSON?
library(rjson)
library(httr)
# list of stream sources
# https://github.com/black-tea/swarm-of-scooters/blob/master/data/systems.csv

# need to care about diff bt last update time and sys time?

# LA ----

tmp = fromJSON(file = "https://lime.bike/api/partners/v1/gbfs/los_angeles/free_bike_status.json")
#time = tmp[[1]] #1555631848 is 4:57 pm on thursday pst

#credit to https://github.com/black-tea/swarm-of-scooters/blob/master/server.R for how to scrape multi-page:
lastpg <- tmp$max_page
scoot_data <- vector(mode = 'list', length = lastpg + 1)
for (i in seq(1, lastpg)){
  paginatedurl <- paste0("https://lime.bike/api/partners/v1/gbfs/los_angeles/free_bike_status.json", "?page=", i)
  page <- GET(paginatedurl)
  df <- jsonlite::fromJSON(content(page, as='text'), flatten=TRUE)
  scoot_data[[i]] <- df$data$bikes
}

scoot_data = do.call("rbind", scoot_data)
pull_time = gsub(as.character(Sys.time()), pattern = " |:", replacement = "_")
write.csv(scoot_data, paste(Sys.getenv("HOME"),"/Documents/scooters/LA_lime_data/LA_lime_", 
                            pull_time, ".csv", sep = ""), row.names = F)


tmp = fromJSON(file = "https://mds.bird.co/gbfs/los-angeles/free_bikes")
#time = tmp[[1]] #1555631848 is 4:57 pm on thursday pst
scoot_data = tmp[[3]]
scoot_data = do.call("rbind", scoot_data[[1]])
pull_time = gsub(as.character(Sys.time()), pattern = " |:", replacement = "_")
write.csv(scoot_data, paste(Sys.getenv("HOME"), "/Documents/scooters/LA_bird_data/LA_bird_", 
                            pull_time, ".csv", sep = ""), row.names = F)

tmp = fromJSON(file = "https://mds.bird.co/gbfs/santamonica/free_bikes")
#time = tmp[[1]] #1555631848 is 4:57 pm on thursday pst
scoot_data = tmp[[3]]
scoot_data = do.call("rbind", scoot_data[[1]])
pull_time = gsub(as.character(Sys.time()), pattern = " |:", replacement = "_")
write.csv(scoot_data, paste(Sys.getenv("HOME"), "/Documents/scooters/SM_bird_data/SM_bird_", 
                            pull_time, ".csv", sep = ""), row.names = F)

