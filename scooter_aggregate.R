# EDA for scooter data
# Jane Carlen, April 2019
# lime - la
# bird - la
# bird -sm

# setup ----
library(stringr)
library(ggplot2)
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(dplyr)
library(cowplot)



# map ----
register_google(key = "AIzaSyAnUkysR7a3N20-dLLPZ3PPJX2j6BPhGt4")
has_google_key()
la_scoot_map = get_map(location = c(lon = median(scooter_data$LA_lime_data$lon),
                                    lat = median(scooter_data$LA_lime_data$lat)), maptype = "roadmap")

# data ----
scooter_data = lapply(list.dirs("~/Documents/scooters", recursive = F), function(folder) {
  files = list.files(folder, full.names = T)
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
names(scooter_data) = list.dirs("~/Documents/scooters", recursive = F, full.names = F)
  
# plots ----

# only scooters for lime
lime_data = filter(scooter_data$LA_lime_data, vehicle_type == "scooter") %>%
              mutate(period = cut(hour, breaks = c(0, 6,12,18), include.lowest = T))
# combine bird LA and SM
bird_data = rbind(scooter_data$LA_bird_data, scooter_data$SM_bird_data) %>%
               mutate(period = cut(hour, breaks = c(0, 6,12,18), include.lowest = T))

write.csv(lime_data, "~/Documents/scooters/sofar_lime_data.csv", row.names = F)
write.csv(bird_data, "~/Documents/scooters/sofar_bird_data.csv", row.names = F)

#map background
lime_plot = ggmap(la_scoot_map, base_layer = ggplot(aes(x = lon, y = lat), data = lime_data)) +
  coord_equal()  +
  scale_fill_gradient(low = "black", high = "limegreen") +
  geom_hex(alpha = .7, bins = 50) +
  theme_bw() +
  xlim(-118.6, -118.1) +
  ggtitle("Lime Scooters")

bird_plot = ggmap(la_scoot_map, base_layer = ggplot(aes(x = lon, y = lat), data = bird_data)) +
  coord_equal()  +
  scale_fill_gradient(low = "black", high = "skyblue") +
  geom_hex(alpha = .7, bins = 50) +
  theme_bw() + 
  xlim(-118.6, -118.1) +
  ggtitle("Bird Scooters")

ggsave('~/Documents/scooters/lime_bird_hex.png', cowplot::plot_grid(lime_plot, bird_plot))

#map no background
lime_plot_period = 
  ggplot(data = lime_data) + geom_hex(aes(y = lat, x = lon)) +
  facet_wrap(~period) +
  theme_bw() +
  ggtitle("lime by period")

bird_plot_period = 
  ggplot(data = bird_data) + geom_hex(aes(y = lat, x = lon)) +
  facet_wrap(~period) +
  theme_bw() +
  ggtitle("bird by period")

ggsave('~/Documents/scooters/lime_bird_period.png', 
       cowplot::plot_grid(lime_plot_period, bird_plot_period),
       height =4)

  # individual scooter info ---- 

bird_data_id = bird_data %>% group_by(bike_id) %>% 
  mutate(location_id = interaction(lat,lon)) %>%
  summarize(location_changes = n() - n_distinct(location_id),
            med_lat = median(lat),
            med_lon = median(lon))

lime_data_id = lime_data %>% group_by(bike_id) %>% 
  mutate(location_id = interaction(lat,lon)) %>%
  summarize(location_changes = n() - n_distinct(location_id),
            med_lat = median(lat),
            med_lon = median(lon)) 

  # paths ----
ggplot(bird_data %>% arrange(date, hour, minute) %>%
       aes(x = lon, y = lat, color = bike_id)) + 
       geom_line(size = .2) +
       theme_bw() +
       #xlim(-118.5, -118.4) + ylim(33.98, 34.1) + #santa monica and ucla
       guides(color = "none")

ggplot(lime_data %>% arrange(date, hour, minute) %>%
       aes(x = lon, y = lat, color = bike_id)) + 
  theme_bw() +
  geom_line(size = .2) + 
  #xlim(-118.5, -118.4) + ylim(33.98, 34.1) + #santa monica and ucla
  guides(color = "none")

  # location changes  ----


# highlight locations with frequent movers 

lime_bird_loc_change_dist = plot_grid(
  
  ggplot(bird_data_id) + 
    geom_point(aes(y= med_lat, x = med_lon, size = location_changes,
                   alpha = location_changes/max(location_changes)), 
               shape = 22, fill = "skyblue", color = "black") +
    scale_size_continuous(range = c(.2,4)) +
    guides(alpha =  "none") + ggtitle("bird"),
  
  ggplot(lime_data_id) + 
    geom_point(aes(y= med_lat, x = med_lon, size = location_changes,
                   alpha = location_changes/max(location_changes)),
               shape = 22, fill = "limegreen", color = "black") +
    scale_size_continuous(range = c(.2,4)) +
    guides(alpha =  "none") + ggtitle("lime")
  
)

ggsave('~/Documents/scooters/lime_bird_loc_change_dist.png', lime_bird_loc_change_dist)

lime_bird_loc_change_map = plot_grid(
  

  ggplot(bird_data_id) + geom_histogram(aes(x = location_changes),
                                        fill = "skyblue", color = "black") + 
    ggtitle("bird location changes per scooter"),
  
  ggplot(lime_data_id) + geom_histogram(aes(x = location_changes),
                                        fill = "limegreen", color = "black") +
    ggtitle("lime location changes per scooter")

)

ggsave('~/Documents/scooters/lime_bird_loc_change_map.png', lime_bird_loc_change_map)


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




