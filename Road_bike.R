library(leaflet)
library(sp)   
library(rgdal) # For reading GPX files

routes <- list()
gpx_files <- list.files(path = "Bike_routes", pattern = "\\.gpx$", full.names = TRUE)

for (i in seq_along(gpx_files)) {
  routes[[i]] <- rgdal::readOGR(gpx_files[i], layer = "tracks", verbose = FALSE)
}


map <- leaflet() %>%
  addTiles()

for (route in routes) {
  map <- map %>% addPolylines(data = route, color = "blue", weight = 3, opacity = 0.7)
}

map
