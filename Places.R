library("tidyverse")
library("leaflet")

Places <- readxl::read_excel("Places.xlsx") %>% 
  mutate("Country" = factor(Country) )


quakeIcons <- iconList(Switzerland = makeIcon("icons/Swiss.jfif", iconWidth = 24, iconHeight = 24),
                       Netherlands = makeIcon("icons/Neth.jfif", iconWidth = 24, iconHeight = 24),
                       Belgium = makeIcon("icons/Be.jfif", iconWidth = 24, iconHeight = 24),
                       Bulgaria = makeIcon("icons/Bu.jfif", iconWidth = 24, iconHeight = 24),
                       Italy = makeIcon("icons/It.jfif", iconWidth = 24, iconHeight = 24),
                       Scotland = makeIcon("icons/Sc.jfif", iconWidth = 24, iconHeight = 24),
                       Kosovo = makeIcon("icons/Ko.jfif", iconWidth = 24, iconHeight = 24))



  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(data = Places,
             lat = ~Latitude,
             lng = ~Longitude,
             label = Places$Place, 
             clusterOptions = markerClusterOptions(),
             popup = Places$People,
             icon = ~quakeIcons[Country],
             options = markerOptions(opacity = .6)) 
