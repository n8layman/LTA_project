# Shiny Application (app.R) to Display GeoJSON Polygons on a Leaflet Map
#
# Updated for new tick data schema and helpers.R
library(shiny)
library(munsell)
library(leaflet)
library(DT)
library(leaflet.providers)
library(tidyr)
library(dplyr)
library(glue)

# Source helper functions
source("helpers.R")

# --- Configuration & Data Loading ---

# Paths to GeoJSON files
POLYGONS_FILE_PATH <- "mianus_polygons.geojson"
EXCLOSURES_FILE_PATH <- "mianus_exclosures.geojson"
TRANSECTS_FILE_PATH <- "mianus_transects.geojson"
TRAILS_FILE_PATH <- "mianus_trails.geojson"

# Standardized Site Coordinates
SITE_COORDS <- list(
  "Mohonk Preserve" = c(41.776, -74.135),
  "Mianus River Gorge" = c(41.171636, -73.620278) 
)

# Load GeoJSON data
polygons_data <- load_geojson(POLYGONS_FILE_PATH)
exclosures_data <- load_geojson(EXCLOSURES_FILE_PATH)
transects_data <- load_geojson(TRANSECTS_FILE_PATH)
trails_data <- load_geojson(TRAILS_FILE_PATH)

# --- Load Tick Data ---
tick_data <- load_tick_data("tick_data_new.csv") %>%
  arrange(Date, SiteName, Transect)

# Mohonk exclosures data
mohonk_exclosures <- read.csv("mohonk_exclosures.csv", stringsAsFactors = FALSE)

# Date range
date_range <- get_date_range(tick_data)
min_date <- date_range[1]
max_date <- date_range[2]

# --- User Interface (UI) ---
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
    ),
    includeCSS("styles.css")
  ),
  titlePanel(
    div(h1("Regional Tick Surveillance Data Explorer", class = "text-2xl font-bold text-gray-800")),    
    windowTitle = "Tick Study App"
  ),
  tabsetPanel(
    id = "site_tabs",
    # Mohonk Tab
    tabPanel(title = "Mohonk Preserve, NY", value = "Mohonk",
      h3("Mohonk Preserve, NY - Location and Data Overview", class = "text-xl font-semibold mb-4 text-gray-700"),
      fluidRow(
        column(8, leafletOutput("mohonk_map", height = "500px")),
        column(4, div(class = "site-summary-panel", 
                     h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), 
                     uiOutput("site_summary_ui"), br(), 
                     plotOutput("mohonk_species_plot", height = "250px")))
      ),
      div(class = "data-table-styled-container", 
          h4("Mohonk Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"), 
          DTOutput("mohonk_data_table"))
    ),
    # Mianus Tab
    tabPanel(title = "Mianus River Gorge Preserve, CT/NY", value = "Mianus River Gorge",
      h3("Mianus River Gorge Preserve - Location and Data Overview", class = "text-xl font-semibold mb-4 text-gray-700"),
      fluidRow(
        column(8, leafletOutput("mianus_map", height = "500px")),
        column(4, div(class = "site-summary-panel", 
                     h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), 
                     uiOutput("site_summary_ui_mianus"), br(), 
                     plotOutput("mianus_species_plot", height = "250px")))
      ),
      div(class = "data-table-styled-container", 
          h4("Mianus River Gorge Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"), 
          DTOutput("mianus_data_table"))
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  filtered_data <- reactive({ tick_data })
  
  # --- Site Summary UI ---
  output$site_summary_ui <- renderUI({
    generate_site_summary(filtered_data(), "Mohonk Preserve", min_date, max_date)
  })
  
  output$site_summary_ui_mianus <- renderUI({
    generate_site_summary(filtered_data(), "Mianus River Gorge", min_date, max_date)
  })
  
  # --- Species Plots ---
  output$mohonk_species_plot <- renderPlot({
    generate_species_plot(filtered_data(), "Mohonk Preserve")
  })
  
  output$mianus_species_plot <- renderPlot({
    generate_species_plot(filtered_data(), "Mianus River Gorge")
  })
  
  # --- Data Tables ---
  output$mohonk_data_table <- renderDT({
    datatable(
      filtered_data() %>% filter(SiteName == "Mohonk Preserve") %>% select(-SiteName),
      options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'),
      rownames = FALSE
    )
  })
  
  output$mianus_data_table <- renderDT({
    datatable(
      filtered_data() %>% filter(SiteName == "Mianus River Gorge") %>% select(-SiteName),
      options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'),
      rownames = FALSE
    )
  })
  
  # --- Mohonk Map ---
  output$mohonk_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mohonk Preserve"]][1]
    lng <- SITE_COORDS[["Mohonk Preserve"]][2]
    
    mohonk_summary <- summarize_by_transect(filtered_data(), "Mohonk Preserve")
    
    exclosures_with_labels <- mohonk_exclosures %>%
      rowwise() %>%
      mutate(
        adult_count = get_count(mohonk_summary %>% filter(Transect == Exclosure_Name), "Adult"),
        nymph_count = get_count(mohonk_summary %>% filter(Transect == Exclosure_Name), "Nymph"),
        total_count = adult_count + nymph_count,
        marker_color = if(total_count == 0) "gray" else "red",
        label_text = create_tick_tooltip(
          title = Exclosure_Name,
          create_table_row("Transect", adult_count, nymph_count, total_count)
        )
      ) %>%
      ungroup()
    
    icons_list <- lapply(exclosures_with_labels$marker_color, function(color) {
      awesomeIcons(icon = 'map-pin', iconColor = 'white', library = 'fa', markerColor = color)
    })
    
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles(group = "Default Map") %>%
      setView(lng = lng, lat = lat, zoom = 12.5) %>%
      addAwesomeMarkers(
        data = exclosures_with_labels,
        lat = ~Latitude,
        lng = ~Longitude,
        icon = icons_list,
        label = ~lapply(label_text, HTML),
        labelOptions = labelOptions(permanent = FALSE, direction = "auto"),
        group = "Exclosures"
      ) %>%
      addCircles(lng = lng, lat = lat, radius = 5000, color = "#FF0000", fillOpacity = 0.2, group = "Model Layer") %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"),
        overlayGroups = c("Exclosures", "Model Layer"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Model Layer")
  })
  
  # --- Mianus Map ---
  output$mianus_map <- renderLeaflet({
    center_lat <- SITE_COORDS[["Mianus River Gorge"]][1]
    center_lng <- SITE_COORDS[["Mianus River Gorge"]][2]
    
    map <- init_leaflet_map(center_lng, center_lat, zoom = 17, max_zoom = 20)
    map <- add_preserve_polygons(map, polygons_data)
    map <- add_exclosure_polygons(map, exclosures_data)
    
    if (!is.null(transects_data)) {
      transects_properties_tibble <- lapply(seq_along(transects_data$features), function(i) {
        f <- transects_data$features[[i]]
        props <- f$properties
        data.frame(
          SegmCode = trimws(props$SegmCode),
          Treatm = props$Treatm,
          .original_index = i,
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()
      
      tick_segment_totals <- get_segment_tick_totals(filtered_data())
      
      transects_joined_data <- transects_properties_tibble %>%
        left_join(tick_segment_totals, by = "SegmCode") %>%
        mutate(
          Total_Ticks = replace_na(Total_Ticks, 0),
          Adult_Count = replace_na(Adult_Count, 0),
          Nymph_Count = replace_na(Nymph_Count, 0),
          line_color = case_when(
            Total_Ticks == 0 ~ "#9CA3AF",
            Treatm == "Unf" ~ "#EF4444",
            TRUE ~ "#7C3AED"
          )
        )
      
      summaries <- summarize_mianus_hierarchical(filtered_data())
      transect_summary <- summaries$transect
      line_summary <- summaries$line
      
      for (i in seq_along(transects_data$features)) {
        feature <- transects_data$features[[i]]
        coords <- feature$geometry$coordinates
        segment_data <- transects_joined_data %>% filter(.original_index == i) %>% head(1)
        segm_code <- segment_data$SegmCode
        line_color <- segment_data$line_color
        
        # Extract Transect/Line/Segment info
        transect_id <- sub("MRG-(\\d+)-.*", "\\1", segm_code)
        line_code <- sub("-[A-Z]$", "", segm_code)
        type_id <- sub("MRG-\\d+-([A-Za-z]+)-.*", "\\1", segm_code)
        line_num <- sub("MRG-\\d+-[A-Za-z]+-(\\d+)-[A-Z]$", "\\1", segm_code)
        
        t_adult <- get_count(transect_summary %>% filter(Transect == transect_id), "Adult")
        t_nymph <- get_count(transect_summary %>% filter(Transect == transect_id), "Nymph")
        t_total <- t_adult + t_nymph
        
        l_adult <- get_count(line_summary %>% filter(Line_Code == line_code), "Adult")
        l_nymph <- get_count(line_summary %>% filter(Line_Code == line_code), "Nymph")
        l_total <- l_adult + l_nymph
        
        s_adult <- segment_data$Adult_Count
        s_nymph <- segment_data$Nymph_Count
        s_total <- segment_data$Total_Ticks
        
        tooltip_content <- create_tick_tooltip(
          title = segm_code,
          create_table_row(paste("Transect", transect_id), t_adult, t_nymph, t_total),
          create_table_row(paste("Line", line_num, " (", type_id, ")", sep=""), l_adult, l_nymph, l_total, bgcolor = "#f9fafb"),
          create_table_row("Segment", s_adult, s_nymph, s_total)
        )
        
        map <- map %>%
          addPolylines(
            lng = sapply(coords, function(x) x[[1]]),
            lat = sapply(coords, function(x) x[[2]]),
            color = line_color,
            weight = 7,
            opacity = 0.9,
            group = "Transects",
            label = lapply(tooltip_content, HTML),
            highlightOptions = highlightOptions(
              weight = 9,
              color = "#FBBF24",
              opacity = 1,
              bringToFront = TRUE
            )
          )
      }
    }
    
    map <- add_trail_polylines(map, trails_data)
    
    map %>%
      addCircles(lng = center_lng, lat = center_lat, radius = 1000, color = "#0000FF", fillOpacity = 0.2, group = "Model Layer") %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"),
        overlayGroups = c("Preserve Boundary", "Exclosures", "Transects", "Trails", "Model Layer"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Trails") %>%
      hideGroup("Preserve Boundary") %>%
      hideGroup("Exclosures") %>%
      hideGroup("Model Layer")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
