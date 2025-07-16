library(tidyverse)
library(janitor)
library(sf)
library(leaflet)

dir(path = "India Shape")

india <- st_read("India Shape/india_ds.shp") %>%
  clean_names() %>%
  mutate(
    state_clean = state %>%
      toupper() %>%
      str_replace_all("[^A-Z]", " ") %>%
      str_squish()
  )

gsdp_data_cleaned <- read_csv("final_statewise_gsdp.csv") %>%
  clean_names()


gsdp_total_by_state <- gsdp_data_cleaned %>%
  group_by(state) %>%
  summarise(GSDP_Total_Lakh_Crore = sum(gsdp / 10000, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    state = case_when(
      state == "Odisha" ~ "Orissa",
      TRUE ~ state),
    state_clean = state %>%
      toupper() %>%
      str_replace_all("[^A-Z]", " ") %>%
      str_squish()
  )

random_population <- tibble(
  state_clean = gsdp_total_by_state$state_clean,
  population = sample(1e7:1.2e8, size = nrow(gsdp_total_by_state), replace = T)
)


gsdp_percapita <- gsdp_total_by_state %>%
  left_join(random_population, by = "state_clean") %>%
  mutate(
    GSDP_Per_Capita = round((GSDP_Total_Lakh_Crore * 1e5) / population)
  )

india_gsdp <- india %>%
  left_join(gsdp_percapita, by = "state_clean")
pal_total <- colorNumeric(
  palette = "YlOrRd",
  domain = india_gsdp$GSDP_Total_Lakh_Crore,
  na.color = "lightgrey"
)


#Map of overall gsdp

leaflet(india_gsdp) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_total(GSDP_Total_Lakh_Crore),
    weight = 1,
    color = "black",
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>State:</b> ", state_clean, "<br/>",
      "<b>Total GSDP:</b> ", format(GSDP_Total_Lakh_Crore, big.mark = ","), " Lakh Crore"
    )
  ) %>%
  addLegend(
    pal = pal_total,
    values = ~GSDP_Total_Lakh_Crore,
    title = "Total GSDP (Lakh Crore)",
    position = "bottomright"
  ) %>%
  addControl(
    html = "Indian States by Total GSDP (All Years)",
    position = "topright"
  )
pal_percapita <- colorNumeric(
  palette = "YlGnBu",
  domain = india_gsdp$GSDP_Per_Capita,
  na.color = "lightgrey"
)

#map of Percapita by assuming random number 

leaflet(india_gsdp) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_percapita(GSDP_Per_Capita),
    weight = 1,
    color = "black",
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>State:</b> ", state_clean, "<br/>",
      "<b>Total GSDP:</b> ", format(GSDP_Total_Lakh_Crore, big.mark = ","), " Lakh Crore<br/>",
      "<b>Per Capita GSDP:</b> ₹", format(GSDP_Per_Capita, big.mark = ",")
    )
  ) %>%
  addLegend(
    pal = pal_percapita,
    values = ~GSDP_Per_Capita,
    title = "GSDP Per Capita (INR)",
    position = "bottomright"
  ) %>%
  addControl(
    html = "Indian States by GSDP Per Capita",
    position = "topright"
  )
