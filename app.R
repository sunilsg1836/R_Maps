library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(janitor)
library(DT)
library(plotly)
library(htmlwidgets)
library(webshot2)
library(shinyWidgets)

state_lookup <- tibble(
  state_ut = c(
    "ANDHRA PRADESH", "ARUNACHAL PRADESH", "ASSAM", "BIHAR",
    "CHANDIGARH", "CHHATTISGARH", "GOA", "GUJARAT", "HARYANA",
    "HIMACHAL PRADESH", "JAMMU & KASHMIR", "JHARKHAND", "KARNATAKA",
    "KERALA", "MADHYA PRADESH", "MAHARASHTRA", "MANIPUR", "MEGHALAYA",
    "MIZORAM", "NAGALAND", "ORISSA", "PUNJAB", "RAJASTHAN", "SIKKIM",
    "TAMIL NADU", "TRIPURA", "UTTAR PRADESH", "UTTARANCHAL", "WEST BENGAL",
    "A & N ISLANDS", "D & N HAVELI", "DAMAN & DIU", "DELHI UT",
    "LAKSHADWEEP", "PUDUCHERRY"
  ),
  STATE = c(
    "ANDHRA PRADESH", "ARUNACHAL PRADESH", "ASSAM", "BIHAR",
    "CHANDIGARH", "CHHATTISGARH", "GOA", "GUJARAT", "HARYANA",
    "HIMACHAL PRADESH", "JAMMU AND KASHMIR", "JHARKHAND", "KARNATAKA",
    "KERALA", "MADHYA PRADESH", "MAHARASHTRA", "MANIPUR", "MEGHALAYA",
    "MIZORAM", "NAGALAND", "ODISHA", "PUNJAB", "RAJASTHAN", "SIKKIM",
    "TAMIL NADU", "TRIPURA", "UTTAR PRADESH", "UTTARAKHAND", "WEST BENGAL",
    "ANDAMAN & NICOBAR", "DADRA & NAGAR HAVELI", "DAMAN & DIU", "DELHI",
    "LAKSHADWEEP", "PUDUCHERRY"
  )
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar-panel {
        background-color: #f8f8f8;
        border-right: 2px solid #e74c3c;
        height: 100vh;
        padding: 20px;
      }
      .sidebar-panel h3 {
        color: #e74c3c;
        font-weight: bold;
        margin-top: 0;
      }
      .red-button {
        background-color: #e74c3c !important;
        color: white !important;
        width: 100%;
        margin-bottom: 10px;
      }
    "))
  ),
  fluidRow(
    column(
      width = 3,
      div(
        class = "sidebar-panel",
        h3("Crime Explorer"),
        pickerInput("year", "Select Year", choices = NULL,
                    options = list(style = "btn-danger")),
        pickerInput("crime_type", "Select Crime Type", choices = NULL,
                    options = list(style = "btn-danger")),
        downloadButton("download_leaflet_map", "Export Leaflet Map (PNG)", class = "red-button"), # Renamed this button
        downloadButton("download_ggplot_map", "Export ggplot Map (PNG)", class = "red-button") # New button for ggplot map
      )
    ),
    column(
      width = 9,
      leafletOutput("crime_map", height = 500),
      br(),
      tabsetPanel(
        tabPanel("Plots",
                 plotlyOutput("bar_plot", height = 300),
                 plotlyOutput("line_plot", height = 300),
                 plotlyOutput("pie_plot", height = 300)
        ),
        tabPanel("Data Table",
                 DTOutput("crime_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Ensure these files are in your app's directory or provide full paths
  india_sf <- st_read("India Shape/india_st.shp", quiet = TRUE)
  
  crime_data_wide <- read_csv("17_Crime_by_place_of_occurrence_2001_2012.csv", show_col_types = FALSE) %>%
    clean_names()
  
  # pivot longer
  crime_data_long <- crime_data_wide %>%
    pivot_longer(
      cols = -c(state_ut, year),
      names_to = "crime_type",
      values_to = "number_of_cases"
    ) %>%
    mutate(crime_type = str_replace_all(crime_type, "_", " ") %>% toupper())
  
  observe({
    updatePickerInput(session, "year",
                      choices = sort(unique(crime_data_long$year)),
                      selected = max(crime_data_long$year, na.rm = TRUE))
    
    updatePickerInput(session, "crime_type",
                      choices = sort(unique(crime_data_long$crime_type)),
                      selected = unique(crime_data_long$crime_type)[1])
  })
  
  crime_data_mapped <- reactive({
    crime_data_long %>%
      left_join(state_lookup, by = "state_ut") %>%
      filter(!is.na(STATE))
  })
  
  filtered_crime <- reactive({
    req(input$year, input$crime_type)
    crime_data_mapped() %>%
      filter(year == input$year,
             crime_type == input$crime_type) %>%
      group_by(STATE) %>%
      summarise(total_crimes = sum(number_of_cases, na.rm = TRUE))
  })
  
  joined_data <- reactive({
    india_sf %>%
      left_join(filtered_crime(), by = "STATE") %>%
      mutate(total_crimes = replace_na(total_crimes, 0))
  })
  
  # Leaflet Map Rendering
  map_widget <- reactive({
    data <- joined_data()
    pal <- colorNumeric(palette = "YlOrRd", domain = data$total_crimes)
    
    main_title <- "Crime Explorer Map"
    sub_title <- paste0(input$crime_type, " — ", input$year)
    
    title_html <- sprintf(
      "<div style='
        background-color: white;
        border: 1px solid #e74c3c;
        border-radius: 4px;
        padding: 8px;
        font-weight: bold;
        color: #e74c3c;
        font-size: 16px;
        text-align: center;
        width: 100%%;
    '>%s<br/><span style='font-size: 14px; color: black;'>%s</span></div>",
      main_title,
      sub_title
    )
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(total_crimes),
        fillOpacity = 0.7,
        color = "#BDBDC3",
        weight = 1,
        label = ~paste0(STATE, ": ", total_crimes, " cases")
      ) %>%
      addLegend(
        pal = pal,
        values = data$total_crimes,
        title = "Number of Crimes"
      ) %>%
      addControl(
        html = title_html,
        position = "topleft"
      )
  })
  
  
  output$crime_map <- renderLeaflet({
    map_widget()
  })
  
  # Download Handler for Leaflet Map
  output$download_leaflet_map <- downloadHandler(
    filename = function() {
      paste0("leaflet_crime_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      saveWidget(map_widget(), "temp_leaflet_map.html", selfcontained = TRUE)
      webshot("temp_leaflet_map.html", file = file, cliprect = "viewport", zoom = 2) # Added zoom for better quality
      unlink("temp_leaflet_map.html")
    }
  )
  
  # ggplot Map Rendering (for download)
  ggplot_map_reactive <- reactive({
    data <- joined_data()
    ggplot(data) +
      geom_sf(aes(fill = total_crimes, text = paste0(STATE, ": ", total_crimes, " cases")), color = "#BDBDC3", size = 0.5) +
      scale_fill_viridis_c(option = "YlOrRd", direction = -1, name = "Number of Crimes") + # Using viridis for color scale
      labs(title = paste0("Crime Rate in India: ", input$crime_type, " (", input$year, ")"),
           subtitle = "Data by State/UT") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      )
  })
  
  # Download Handler for ggplot Map
  output$download_ggplot_map <- downloadHandler(
    filename = function() {
      paste0("ggplot_crime_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = ggplot_map_reactive(), device = "png", width = 10, height = 8, units = "in", dpi = 300)
    }
  )
  
  output$bar_plot <- renderPlotly({
    plot_data <- filtered_crime() %>%
      arrange(desc(total_crimes)) %>%
      slice_head(n = 10)
    p <- ggplot(plot_data, aes(x = reorder(STATE, total_crimes), y = total_crimes)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() +
      labs(title = "Top 10 States by Crime Count",
           x = "State/UT",
           y = "Number of Crimes")
    ggplotly(p)
  })
  
  output$line_plot <- renderPlotly({
    line_data <- crime_data_mapped() %>%
      filter(crime_type == input$crime_type) %>%
      group_by(year) %>%
      summarise(total_crimes = sum(number_of_cases, na.rm = TRUE))
    p <- ggplot(line_data, aes(x = year, y = total_crimes)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(size = 2, color = "green") +
      scale_x_continuous(breaks = unique(line_data$year)) +
      labs(title = paste("Trend of", input$crime_type, "in India"),
           x = "Year", y = "Number of Crimes")
    ggplotly(p)
  })
  
  output$pie_plot <- renderPlotly({
    pie_data <- filtered_crime() %>%
      arrange(desc(total_crimes)) %>%
      slice_head(n = 5) %>%
      mutate(percent = total_crimes / sum(total_crimes) * 100)
    plot_ly(
      pie_data,
      labels = ~STATE,
      values = ~total_crimes,
      type = 'pie',
      textinfo = 'label+percent'
    ) %>%
      layout(title = "Share of Crime among Top 5 States")
  })
  
  output$crime_table <- renderDT({
    datatable(
      filtered_crime(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)
