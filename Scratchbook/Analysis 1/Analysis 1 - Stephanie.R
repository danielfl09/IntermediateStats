library(tidyverse)
library(mosaic)
library(leaflet)
library(USAboundaries)

Rent <- read_csv("./Data/Rent.csv") %>% 
  na.omit()


### Distance ~ Price ~ Gender
Rent %>% 
  ggplot(aes(MilesToCampus, Price, color = Gender, group = Gender)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Gender ~ .) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Miles to Campus",
    y = "Price",
    color = "Gender"
  )





### Leaflet Map
getColor <- function(Rent) {
  sapply(Rent$MilesToCampus, function(MilesToCampus) {
    if(MilesToCampus <= 0.25) {
      "green"
    } else if(MilesToCampus <= 0.5) {
      "yellow"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(Rent)
)

leaflet(options = leafletOptions()) %>% 
  addTiles() %>% 
  addAwesomeMarkers(data = Rent,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    icon = icons,
                    popup = ~paste0("Name: ", Apartment, "<br/>",
                                    "Gender: ", Gender, "<br/>",
                                    "Address: ", Address, "<br/>",
                                    "Phone: ", Phone, "<br/>",
                                    "Shared Room: $", Price, "<br/>",
                                    "Private Room: $", PrivateRoomPrice, "<br/>",
                                    "Deposit: $", Deposit, "<br/>"
                                    ))
                    