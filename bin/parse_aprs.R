
library(dplyr)
library(ggplot2)
library("leaflet")

# Read data
d_raw <- readLines("data/APRS data from Pembina Gorge 2020-09-18.txt") %>% 
            as.data.frame() %>% 
            setNames("packet")

# Grab lines with location
d <- d_raw %>% 
        # Keep only rows with locations
        filter(grepl(".*:=(.*)>(.*)", packet)) %>% 
        # Pull out raw location parts
        mutate(location_ns     = gsub(".*:=(.*)[NS]/(.*)", "\\1", packet) %>% as.numeric(),
               location_ns_dir = gsub(".*[0-9]([NS])/(.*)", "\\1", packet),
               location_ew     = gsub(".*/(.*)[EW]>(.*)", "\\1", packet) %>% as.numeric(),
               location_ew_dir = gsub(".*[0-9]([EW])>(.*)", "\\1", packet)) %>% 
        # Set directions and fix to long/lat
        mutate(lat = if_else(location_ns_dir == "N",
                             location_ns / 100,
                             -location_ns / 100),
               lon = if_else(location_ew_dir == "E",
                             location_ew / 100,
                             -location_ew / 100))

# Simple map
ggplot() +
    geom_point(data = d,
               aes(x = lon,
                   y = lat))

# Interactive map
# https://towardsdatascience.com/making-interactive-maps-in-r-with-less-than-15-lines-of-code-bfd81f587e12
d %>% 
    leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
    addMarkers(label = NA, 
               popup = d$packet) # Val False
    


