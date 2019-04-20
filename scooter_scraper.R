#install.packages("rjson")
library(rjson)
# list of stream sources
# https://github.com/black-tea/swarm-of-scooters/blob/master/data/systems.csv
# LA ----

tmp = fromJSON(file = "https://lime.bike/api/partners/v1/gbfs/los_angeles/free_bike_status.json")
#time = tmp[[1]] #1555631848 is 4:57 pm on thursday pst
data = tmp[[4]]
data = do.call("rbind", data[[1]])
time = gsub(as.character(Sys.time()), pattern = " |:", replacement = "_")
write.csv(data, paste("/Users/janecarlen/Documents/scooters/LA_lime_data/LA_lime_", time, ".csv", sep = ""), row.names = F)


tmp = fromJSON(file = "https://mds.bird.co/gbfs/los-angeles/free_bikes")
#time = tmp[[1]] #1555631848 is 4:57 pm on thursday pst
data = tmp[[3]]
data = do.call("rbind", data[[1]])
time = gsub(as.character(Sys.time()), pattern = " |:", replacement = "_")
write.csv(data, paste("/Users/janecarlen/Documents/scooters/LA_bird_data/LA_bird_", time, ".csv", sep = ""), row.names = F)

tmp = fromJSON(file = "https://mds.bird.co/gbfs/santamonica/free_bikes")
#time = tmp[[1]] #1555631848 is 4:57 pm on thursday pst
data = tmp[[3]]
data = do.call("rbind", data[[1]])
time = gsub(as.character(Sys.time()), pattern = " |:", replacement = "_")
write.csv(data, paste("/Users/janecarlen/Documents/scooters/SM_bird_data/SM_bird_", time, ".csv", sep = ""), row.names = F)
