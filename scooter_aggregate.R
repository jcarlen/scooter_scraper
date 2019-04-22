# EDA for scooter data
# Jane Carlen, April 2019
# lime - la
# bird - la
# bird -sm
# Approximations:
#  ROUND lat/lon to 3 digits to know if there's been a move, detects rughly anything as large as a block - wouldn't detect user bring it right back where they started (underestimate)
#  Looking every 15 min, so don't know if there were two 5 min trips within a 15 min window, for example (underestimate)

# 1. setup ----
library(stringr)
library(ggplot2)
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(dplyr)
library(cowplot)



# 2. map ----
register_google(key = "AIzaSyAnUkysR7a3N20-dLLPZ3PPJX2j6BPhGt4")
has_google_key()
tryCatch({la_scoot_map = get_map(location = c(lon = median(scooter_data$LA_lime_data$lon),
                                    lat = median(scooter_data$LA_lime_data$lat)), maptype = "roadmap")}, 
         error = function(e) "Couldn't get map. Check API key and internet connection")

# 3. data ----
scooter_dirs = list.dirs(paste0(Sys.getenv("HOME"),"/Documents/scooters"), recursive = F)
scooter_dirs = scooter_dirs[!grepl(scooter_dirs, pattern = "/\\.")]

scooter_data = lapply(scooter_dirs, function(folder) {
  files = list.files(folder, full.names = T, include.dirs = FALSE, pattern = "csv")
  datas = lapply(files, function(x) {
    data = read.csv(x)
    time = str_extract(x, pattern = "[0-9]{4}.*[0-9]")
    time = str_split(time, "_")[[1]]
    data$date = as.Date(time[1], "%Y-%m-%d")
    data$hour = as.numeric(time[2])
    data$minute = as.numeric(time[3])
    data$system = dplyr::last(str_split(folder, "/")[[1]])
    data
    
  })
  do.call("rbind", datas)
})

# systems have different numbers of columns bc lime has a vehicle type
names(scooter_data) = sapply(str_split(scooter_dirs, "/"), last)

# only scooters for lime
lime_data = filter(scooter_data$LA_lime_data, vehicle_type == "scooter") %>%
              mutate(period = cut(hour, breaks = c(0, 6,12, 18, 24), include.lowest = T))
# combine bird LA and SM
bird_data = rbind(scooter_data$LA_bird_data, scooter_data$SM_bird_data) %>%
               mutate(period = cut(hour, breaks = c(0, 6,12, 18, 24), include.lowest = T))

write.csv(lime_data, "~/Documents/scooters/sofar_lime_data.csv", row.names = F)
write.csv(bird_data, "~/Documents/scooters/sofar_bird_data.csv", row.names = F)

# 4. plots ----

  # hex, map background ----
lime_plot = ggmap(la_scoot_map, base_layer = ggplot(aes(x = lon, y = lat), data = lime_data)) +
  coord_equal()  +
  scale_fill_gradient(low = "black", high = "limegreen") +
  geom_hex(alpha = .7, bins = 50) +
  theme_bw() +
  xlim(-118.6, -118.1) +
  ggtitle("Lime: Overall trip distribution")

bird_plot = ggmap(la_scoot_map, base_layer = ggplot(aes(x = lon, y = lat), data = bird_data)) +
  coord_equal()  +
  scale_fill_gradient(low = "black", high = "skyblue") +
  geom_hex(alpha = .7, bins = 50) +
  theme_bw() + 
  xlim(-118.6, -118.1) +
  ggtitle("Bird: Overall trip distribution")

ggsave('~/Documents/scooters/lime_bird_hex.png', cowplot::plot_grid(lime_plot, bird_plot))

  # hex, no map background ----
lime_plot_period = 
  ggplot(data = lime_data) + geom_hex(aes(y = lat, x = lon)) +
  facet_wrap(~period) +
  theme_bw() +
  ggtitle("Lime: Trip distribution by period")

bird_plot_period = 
  ggplot(data = bird_data) + geom_hex(aes(y = lat, x = lon)) +
  facet_wrap(~period) +
  theme_bw() +
  ggtitle("Bird: Trip distribution by period")

ggsave('~/Documents/scooters/lime_bird_period.png', 
       cowplot::plot_grid(lime_plot_period, bird_plot_period),
       height =4)

# 5. individual scooter info ---- 
#  means you have to go at least a couple blocks for it to be considered a move
bird_data_id = bird_data  %>% group_by(bike_id) %>% 
  mutate(location_id = interaction(round(lat,3),round(lon,3))) %>%
  summarize(entries = n(),
            location_changes = n_distinct(location_id),
            location_changes_per_hour = 4 * location_changes/entries, #each entry is 15 min
            med_lat = median(lat),
            med_lon = median(lon))

lime_data_id = lime_data %>% group_by(bike_id) %>% 
  mutate(location_id = interaction(round(lat,3),round(lon,3))) %>%
  summarize(entries = n(),
            location_changes = n_distinct(location_id),
            location_changes_per_hour = 4 * location_changes/entries, #each 
            med_lat = median(lat),
            med_lon = median(lon)) 

bird_slices = nrow(unique(bird_data[,c("date", "hour", "minute")]))
lime_slices = nrow(unique(lime_data[,c("date", "hour", "minute")]))

  # paths ----
ggplot(bird_data %>% arrange(date, hour, minute), # %>% filter(bike_id == "05ec7dad-989b-4ef6-827e-754a46cbc0d3"),
       aes(x = lon, y = lat, color = bike_id)) + 
       geom_line(size = .2) +
       theme_bw() +
       #xlim(-118.5, -118.4) + ylim(33.98, 34.1) + #santa monica and ucla
       guides(color = "none")

ggplot(lime_data %>% arrange(date, hour, minute),
       aes(x = lon, y = lat, color = bike_id)) + 
       theme_bw() +
       geom_line(size = .2) + 
       #xlim(-118.5, -118.4) + ylim(33.98, 34.1) + #santa monica and ucla
       guides(color = "none")

  # location changes  ----


    # distribution of number of location changes by scooter ----

lime_bird_loc_change_dist = plot_grid(nrow = 3,
  
  ggplot(bird_data_id) + geom_histogram(aes(x = location_changes), fill = "skyblue", color = "black") + 
    theme_bw() +
    ggtitle(paste0("bird: location changes per scooter\nintervals observed = ", 
                   bird_slices, "; ", "unique ids = ", nrow(bird_data_id))),
  
  ggplot(lime_data_id) + geom_histogram(aes(x = location_changes), fill = "limegreen", color = "black") +
    theme_bw() +
    ggtitle(paste0("lime: location changes per scooter\nintervals observed = ", 
                   lime_slices, "; ", "unique ids = ", nrow(lime_data_id))), 
  
  ggplot(bird_data_id) + geom_histogram(aes(x = location_changes_per_hour), fill = "skyblue", color = "black") + 
    theme_bw() +
    ggtitle("bird avg. location changes per scooter per hour"),
  
  ggplot(lime_data_id) + geom_histogram(aes(x = location_changes_per_hour), fill = "limegreen", color = "black") +
    theme_bw() +
    ggtitle("lime avg. location changes per scooter per hour"),
  
    # why are there so many bird ids that appear only once? 
  
  ggplot(bird_data_id) + geom_histogram(aes(x = entries), fill = "skyblue", color = "black") + 
    theme_bw() +
    ggtitle(paste0("bird: appearances of each id\nintervals observed = ", 
                   bird_slices, "; ", "unique ids = ", nrow(bird_data_id))),
  
  ggplot(lime_data_id) + geom_histogram(aes(x = entries), fill = "limegreen", color = "black") +
    theme_bw() +
    ggtitle(paste0("lime: appearances of each id\nintervals observed = ", 
                   lime_slices, "; ", "unique ids = ", nrow(lime_data_id)))
)

ggsave('~/Documents/scooters/lime_bird_loc_change_dist.png', lime_bird_loc_change_dist)

   

    # highlight locations with frequent movers ----

lime_bird_loc_change_map = plot_grid(
  
  ggmap(la_scoot_map, base_layer = ggplot(bird_data_id)) +
    geom_point(aes(y= med_lat, x = med_lon, size = location_changes_per_hour,
                   alpha = location_changes/max(location_changes)), 
               shape = 21, fill = "skyblue", color = "black", stroke = .2) +
    scale_size_continuous(range = c(.1,2), breaks = c(.5,1,2,3,4)) +
    theme_bw() +
    xlim(range(lime_data$lon)) + # Use lime range for both bc more concentrated
    ylim(range(lime_data$lat)) +
    guides(alpha =  "none") + ggtitle(paste0("bird", " (unique IDs= ", n_distinct(bird_data$bike_id), ")")),
  
  ggmap(la_scoot_map, base_layer = ggplot(lime_data_id)) +
    geom_point(aes(y= med_lat, x = med_lon, size = location_changes_per_hour,
                   alpha = location_changes/max(location_changes)),
               shape = 21, fill = "limegreen", color = "black", stroke = .2) +
    scale_size_continuous(range = c(.1,2), breaks = c(.5,1,2,3,4)) +
    theme_bw() +
    xlim(range(lime_data$lon)) +
    ylim(range(lime_data$lat)) +
    guides(alpha =  "none") + ggtitle(paste0("lime", " (unique IDs = ", n_distinct(lime_data$bike_id), ")"))
  
)

# tried in plotly , too slow
ggsave('~/Documents/scooters/lime_bird_loc_change_map.png', lime_bird_loc_change_map, width = 14)

  # disabled scooters - none? ----
  # reserved scooters ----

ggplot(data = lime_data %>% filter(is_reserved==1)) + 
  geom_hex(aes(x = lon, y = lat)) +
  geom_point(aes(x = lon, y = lat, color = hour), alpha = .8) +
  scale_fill_gradient(low = "white", high = "red", name = "total_reserved") +
  scale_color_gradient(high = "#132B43", low = "#56B1F7", name = "reserved_at_hour")
  

# none 
#ggplot(data = bird_data %>% filter(is_reserved==1)) + 
#  geom_point(aes(x = lon, y = lat), color = "red")




