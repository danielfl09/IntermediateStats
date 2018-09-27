library(mosaic)
library(USAboundaries)
library(scales)
library(tidyverse)
library(leaflet)
library(ggrepel)
library(plotly)

Rent <- read_csv("./Data/Rent.csv") %>% 
  na.omit() %>% 
  mutate(
    Size = case_when(
      Capacity < 50 ~ "Small",
      Capacity < 100 ~ "Medium",
      TRUE ~ "Large"
    )
  )


### Distance ~ Price ~ Gender
CheapNClose <- 
  Rent %>% 
  filter(Gender == "F") %>%
  filter(Price < quantile(Rent$Price, 0.30)) %>% 
  filter(MilesToCampus < quantile(Rent$MilesToCampus, 0.30))


Rent %>% 
  filter(Gender == "F") %>% 
  ggplot(aes(MilesToCampus, Price, color = Gender, group = Gender),
         color = ) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0,0.8,by = 0.1)) +
  scale_y_continuous(labels = dollar_format()) +
  facet_grid(Size ~ .) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  labs(
    x = "Miles to Campus",
    y = "Price",
    color = "Gender"
  )

Rent %>% plot_ly(x = ~MilesToCampus,
                 y = ~Price,
                 size = ~Capacity,
                 color = ~Price,
                 text = ~paste("Apartment: ", Apartment,
                               "<br>Price: ", Price,
                               "<br>Capacity: ", Capacity))



### Leaflet Map
getColor <- function(Rent) {
  sapply(Rent$Price, function(Price) {
    if(Price <= 1000) {
      "green"
    } else if(Price <= 1200) {
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

# leafIcons <- icons(
#   iconUrl = ifelse(Rent$MilesToCampus > 0.25,
#                    "https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
#                    "https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png")
# )

leaflet(options = leafletOptions()) %>% 
  addTiles() %>% 
  addLabelOnlyMarkers(data = CheapNClose,
             label = ~Apartment,
             labelOptions = labelOptions(noHide = T)) %>% 
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
                    