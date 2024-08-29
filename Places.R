library("tidyverse")
library("leaflet")


  
  Places <- readxl::read_excel("Places_V2.xlsx", col_types = c("text","text", "text", "text", "text",
                                                            "numeric", "numeric", 
                                                            "numeric", "date", "text"))
  
  dup_pl <- Places %>% 
    filter(duplicated(Place)) %>% 
    distinct(Place)
  

  
  Places <-  Places %>% 
    mutate("Country" = factor(Country) ) %>% 
    mutate(Place_dup_FL = ifelse(duplicated(Place), "Y", "N"),
           date_der = as.character(Date),
           year_der = as.character(Year)) %>% 
    group_by(Place) %>% 
    mutate(Memory_all = if_else(Place %in% dup_pl$Place, paste(Memory, collapse = " <br/> <br/>"), Memory),
           Date_all = if_else(Place %in% dup_pl$Place, paste(date_der, collapse = "<br/> "), date_der),
           Year_all = if_else(Place %in% dup_pl$Place, paste(year_der, collapse = "<br/> "), year_der),
           People_by_trip = if_else(Place %in% dup_pl$Place, paste(People, collapse = " <br/>"), People),
           Trip_name_der = if_else(Place %in% dup_pl$Place, paste(Trip_name, collapse = "<br/>"), Trip_name)) %>%
    mutate("Info" = paste("Place:", Place, "<br/>", 
                          "Date: <br/>", Date_all, "<br/>", 
                          "<br/> Trip Name: <br/>", Trip_name_der, "<br/>",
                          "<br/> People: <br/>", People_by_trip, "<br/>",
                          "<br/> Memory: <br/>", Memory_all)) %>% 
    filter(Place_dup_FL == "N")
  
  
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
                         Cyprus = makeIcon("icons/cy.jfif", iconWidth = 24, iconHeight = 24),
                         Canada = makeIcon("icons/Ca.jfif", iconWidth = 24, iconHeight = 24))
  
  
  # All places with zoom grouping 
  
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
               options = markerOptions(opacity = .6),
               group = Places$Trip_name)  
  
  
  # Places per trip 
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addMarkers(data = Places,
               lat = ~Latitude,
               lng = ~Longitude,
               label = Places$Place, 
               popup = Places$Info,
               icon = ~quakeIcons[Country],
               options = markerOptions(opacity = .6),
               group = Places$Trip_name)   %>%
    addLayersControl(
      overlayGroups = unique(Places$Trip_name),
      options = layersControlOptions(collapsed = FALSE)
    ) 
  
  
  #### People count ####
  People_count <- Places %>% 
    filter(Year %in% c( 2024)) %>%
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
    select(Place, Date, Memory) %>% view()
  
  #### 2023 places ####
  
  Places %>% 
    filter(Year == "2023") %>% view()
  
  
  #### Sofia and me map ####
  
  Sofia_Places <- Places %>% 
    filter(Year == "2024")
  filter(Country == "Switzerland")
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addMarkers(data = Sofia_Places,
               lat = ~Latitude,
               lng = ~Longitude,
               label = Sofia_Places$Place, 
               # clusterOptions = markerClusterOptions(spiderLegPolylineOptions = list(weight = .5),
               #                                       freezeAtZoom = "maxKeepSpiderify"),
               popup = Sofia_Places$Info,
               icon = ~quakeIcons[Country],
               options = markerOptions(opacity = .6)) 
  
  
  ########################
  
  