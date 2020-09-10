# Peter Boyd
# Animation Final


# Libraries ---------------------------------------------------------------


library(tidyverse) 
library(leaflet)
library(geojsonio)
# below libraries shouldn't be needed
# library(maps)
# library(rgdal)
# library(sf)
# library(sp)

# Data --------------------------------------------------------------------

# need to load in this data set
shark <- read_csv("~/OSU/Year2/Spring/ST_537/Data/attacks.csv")
shark <- shark[1:6302,] 
shark <- shark %>% rename(Fatal = `Fatal (Y/N)`)
shark$Beach <- gsub(",.*","",shark$Location)

surf <- shark %>% filter(Activity == "Surfing") 
surf <- surf %>% filter(Year >= 1960)

usa_surf <- surf %>% filter(Country == "USA") %>%
  filter(!is.na(Area)) %>% 
  rename(name = Area) %>% 
  mutate(Ocean = ifelse(name %in% c("California",
                                    "Hawaii", 
                                    "Oregon", 
                                    "Washington"), 
                        "Pacific", "Atlantic")) %>% 
  group_by(name, Ocean) %>% 
  summarise(n = n(), prop_fatal = sum(Fatal == "Y", na.rm = TRUE)/ n)


# Map Stuff ---------------------------------------------------------------

states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
  )

class(states)
states$attacks <- c(rep(0,4), 75, rep(0,4), 369, 1, 50, 
                    rep(0,18), 1, 0, 1, 20, rep(0,3), 23, 0, 0,
                    9, 0, 0, 7, 0, 0, 1, 1, 0, 0, 0, 1)
# tried to use left_join to add the attacks
# but had trouble since states is of a weird class

bins <- c(0, 1, 10, 20, 50, 100, 370)
pal <- colorBin("Blues", domain = states$attacks, bins = bins)


labels <- sprintf(
  "<strong>%s</strong><br/>%g shark attacks ",
  states$name, states$attacks
) %>% lapply(htmltools::HTML)


leaflet(states) %>%
  setView(-96, 37.8, 3.5) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(attacks),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~attacks, opacity = 0.7, 
title = "Shark Attacks",
            position = "bottomright") %>% 
  addControl("Surfing Related Shark Attacks Since 1960",
             position = "bottomleft")

