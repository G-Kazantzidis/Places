library("tidyverse")
library("leaflet")

Places <- readxl::read_excel("Places.xlsx", col_types = c("text","text", "text",
                                                          "numeric", "numeric", 
                                                          "numeric", "date", "text")) %>% 
  mutate("Country" = factor(Country) ) %>% 
  mutate("Info" = paste("Place:", Place, "<br/>", 
                        "Date:", Date, "<br/>", 
                        "Year:", Year, "<br/>",
                        "People:", People, "<br/>",
                        "Memory:", Memory))



train_map <- read.csv("trains/Interrail.csv") 
train_map <- train_map %>% 
  as_tibble() %>% 
  rename("lat" = colnames(train_map)[1], "lon" = colnames(train_map)[2] )



quakeIcons <- iconList(Switzerland = makeIcon("icons/Swiss.jfif", iconWidth = 24, iconHeight = 24),
                       Netherlands = makeIcon("icons/Neth.jfif", iconWidth = 24, iconHeight = 24),
                       Belgium = makeIcon("icons/Be.jfif", iconWidth = 24, iconHeight = 24),
                       Bulgaria = makeIcon("icons/Bu.jfif", iconWidth = 24, iconHeight = 24),
                       Italy = makeIcon("icons/It.jfif", iconWidth = 24, iconHeight = 24),
                       Scotland = makeIcon("icons/Sc.jfif", iconWidth = 24, iconHeight = 24),
                       Kosovo = makeIcon("icons/Ko.jfif", iconWidth = 24, iconHeight = 24),
                       Greece = makeIcon("icons/gr.jfif", iconWidth = 24, iconHeight = 24),
                       France = makeIcon("icons/Fra.jfif", iconWidth = 24, iconHeight = 24),
                       Irland = makeIcon("icons/Irl.jfif", iconWidth = 24, iconHeight = 24),
                       Spain = makeIcon("icons/Sp.jfif", iconWidth = 24, iconHeight = 24),
                       Norway = makeIcon("icons/No.jfif", iconWidth = 24, iconHeight = 24),
                       Denmark = makeIcon("icons/Den.jfif", iconWidth = 24, iconHeight = 24),
                       Germany = makeIcon("icons/Ge.jfif", iconWidth = 24, iconHeight = 24),
                       Cyprus = makeIcon("icons/cy.jfif", iconWidth = 24, iconHeight = 24))



  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(data = Places,
             lat = ~Latitude,
             lng = ~Longitude,
             label = Places$Place, 
             clusterOptions = markerClusterOptions(spiderLegPolylineOptions = list(weight = .5),
                                                   freezeAtZoom = "maxKeepSpiderify"),
             popup = Places$Info,
             icon = ~quakeIcons[Country],
             options = markerOptions(opacity = .6)) 
#### People count ####
  People_count <- Places %>% 
    filter(Year > 2015) %>% 
    select(People) %>% 
    separate_rows(People, sep = ", ") %>%
    drop_na() %>% 
    mutate(People = factor(People)) %>%
    count(People) %>% 
    arrange(n) 
  
  People_count %>% 
    filter(n>4) %>% 
    ggplot(aes(y = n, x = reorder(People, -n) ))+
    geom_bar(position="dodge", stat="identity", col = "lightgreen", fill = "lightblue", alpha = .8)+
    ylab(label = "Number of places")+
    xlab(label = "Peple")+
    theme_minimal()
  
  Places %>% 
    filter(Year > 2015) %>% 
    separate_rows(People, sep = ", ") %>% 
    filter(People == "Sofia") %>% 
    select(Place, Date, Memory)
  
#### 2023 places ####

 Places %>% 
    filter(Year == "2023") %>% view()
  
  
#### Sofia and me map ####
  
  Sofia_Places <- Places %>% 
    filter(str_detect(People, "Maria"))
    filter(Year == "2023")
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addMarkers(data = Sofia_Places,
               lat = ~Latitude,
               lng = ~Longitude,
               label = Sofia_Places$Place, 
               clusterOptions = markerClusterOptions(spiderLegPolylineOptions = list(weight = .5),
                                                     freezeAtZoom = "maxKeepSpiderify"),
               popup = Sofia_Places$Info,
               icon = ~quakeIcons[Country],
               options = markerOptions(opacity = .6)) 
  