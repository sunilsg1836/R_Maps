library(tidyverse)
library(janitor)
library(sf)
library(leaflet)

dir(path = "India Shape")

india <- st_read("India Shape/india_ds.shp")

india %>% 
  mutate(random_no = sample(c(1:100), 482, replace = T)) ->india

india %>% view()


#India Map

ggplot(data = india) +
  geom_sf(fill = "lightblue", color = "black")+
  ggtitle("Map of India")+
  theme_minimal()

ggplot(data = india) +
  geom_sf(aes(fill = random_no), color = "black")+
  ggtitle("Map of India")+
  theme_minimal()

ggplot(data = india) +
  geom_sf(aes(fill = random_no), color = "black")+
  ggtitle("Map of India")+
  scale_fill_gradient(low = "red", high = "blue")+
  theme_minimal()


india %>% 
  filter(STATE == "KARNATAKA")-> ka_map

ggplot(data = ka_map) +
  geom_sf(fill = "lightblue", color = "black")+
  ggtitle("Map of Karnataka")+
  theme_minimal()

ggplot(data = ka_map) +
  geom_sf(aes(fill = random_no), color = "black")+
  ggtitle("Map of Karnataka")+
  theme_minimal()

ggplot(data = ka_map) +
  geom_sf(aes(fill = random_no), color = "black")+
  ggtitle("Map of Karnataka")+
  scale_fill_gradient(low = "red", high = "blue")+
  theme_minimal()

india %>% 
  group_by(STATE) %>% 
  summarise(geometry= st_union(geometry)) -> india_state_b

india %>% 
  group_by(DISTRICT) %>% 
  summarise(geometry= st_union(geometry)) -> india_state_bc

ggplot(data = india_state_bc)+
  geom_sf(fill = "grey", color = "black")+
  ggtitle("Map of India")+
  theme_minimal()
  
leaflet() %>% 
  addTiles()  


#create a leaflet map
leaflet(india) %>% 
  addTiles() %>% #base map
  addPolygons(
    fillColor = "lightblue",
    weight = 1,
    color = "black",
    fillOpacity = 0.5,
    popup = ~as.character(random_no)
  )

#create a leaflet map
leaflet(india) %>% 
  addProviderTiles("Stadia.AlidadeSmoothDark") %>% #base map
  addPolygons(
    fillColor = "green",
    weight = 1,
    color = "black",
    fillOpacity = 0.5,
    popup = ~as.character(random_no)
  )





#create color palette

pal <- colorNumeric(palette = "red", domain = india$random_no)







  

