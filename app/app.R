# Shiny Application (app.R) to Display GeoJSON Polygons on a Leaflet Map
#
# This file is set up for shinylive deployment.
library(shiny)
library(munsell)
library(leaflet)
library(DT)
library(leaflet.providers)

# Source helper functions
source("helpers.R")

# --- Configuration & Data Loading ---

# Paths to the GeoJSON files created by the conversion script
POLYGONS_FILE_PATH <- "mianus_polygons.geojson"
EXCLOSURES_FILE_PATH <- "mianus_exclosures.geojson"
TRANSECTS_FILE_PATH <- "mianus_transects.geojson"
TRAILS_FILE_PATH <- "mianus_trails.geojson"

# --- 1. DATA SETUP: Standardized Site Coordinates ---
SITE_COORDS <- list(
  "Mohonk Preserve" = c(41.776, -74.135),
  "Mianus River Gorge" = c(41.171636, -73.620278) 
)

# Load the data once at app startup
polygons_data <- load_geojson(POLYGONS_FILE_PATH)
exclosures_data <- load_geojson(EXCLOSURES_FILE_PATH)
transects_data <- load_geojson(TRANSECTS_FILE_PATH)
trails_data <- load_geojson(TRAILS_FILE_PATH)


# --- 2. Load Tick Data from CSV ---

# Read tick data from CSV file
tick_data <- load_tick_data("tick_data.csv") %>%
  arrange(Date, Site, Transect)

# Mohonk exclosures data (for markers on Mohonk map)
mohonk_exclosures <- read.csv("mohonk_exclosures.csv", stringsAsFactors = FALSE)

# Get date range
date_range <- get_date_range(tick_data)
min_date <- date_range[1]
max_date <- date_range[2]

# --- 3. User Interface (UI) ---

ui <- fluidPage(
  tags$head(
  # Load Font Awesome 6 for icons
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
  ),
  
  # Include custom fonts and styles
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;500;700&display=swap');
    
    body {
      font-family: 'Inter', sans-serif;
      background-color: #f7f9fc;
    }

    /* --- Base UI Styling --- */
    .tab-content {
      background-color: #ffffff;
      border: 1px solid #e5e7eb;
      border-radius: 0 0.5rem 0.5rem 0.5rem;
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1),
                  0 2px 4px -2px rgba(0, 0, 0, 0.1);
      padding: 1.5rem;
      margin-bottom: 1rem;
    }

    .leaflet-container {
      border-radius: 0.5rem;
      box-shadow: none;
      border: 1px solid #e5e7eb;
      height: 500px;
    }

    .site-summary-panel {
      height: 500px;
      padding: 1.5rem;
      background-color: #f9f9f9;
      border: 1px solid #e5e7eb;
      border-radius: 0.5rem;
      margin-left: 1rem;
    }

    .data-table-styled-container {
      background-color: #f9f9f9;
      border: 1px solid #e5e7eb;
      border-radius: 0.5rem;
      padding: 1.5rem;
      margin-top: 1.5rem;
    }

    /* --- Enable Font Awesome inside Leaflet EasyButtons --- */
    .easy-button-button i.fa,
    .easy-button-button i.fas,
    .easy-button-button i.far,
    .easy-button-button i.fab {
      font-family: 'Font Awesome 6 Free' !important;
      font-weight: 900 !important;
      font-size: 18px !important;      /* Icon size */
      line-height: 36px !important;
      vertical-align: middle;
    }

    /* --- Resize and tighten EasyButton --- */
    .easy-button-button {
      width: 36px !important;
      height: 36px !important;
      line-height: 36px !important;
      padding: 0 !important;
      border-radius: 6px !important;
      font-size: 18px !important;
      color: #333;
      transition: all 0.2s ease;
      background-color: white !important;
      border: 1px solid #ddd !important;
    }

    .easy-button-button:hover {
      color: #007bff !important;
      background-color: #f0f0f0 !important;
    }

    .easy-button-button.active {
      color: #fff !important;
      background-color: #007bff !important;
      border-color: #007bff !important;
    }
  "))
),

  
  titlePanel(
    div(h1("Regional Tick Surveillance Data Explorer", class = "text-2xl font-bold text-gray-800")),    
    windowTitle = "Tick Study App"
  ),
  
  # --- Central Tabset Panel ---
  tabsetPanel(
    id = "site_tabs",
    
    # Tab 1: Mohonk Site View
    tabPanel(title = "Mohonk Preserve, NY", value = "Mohonk",
      h3("Mohonk Preserve, NY - Location and Data Overview", class = "text-xl font-semibold mb-4 text-gray-700"),
      fluidRow(
        column(8, leafletOutput("mohonk_map", height = "500px")),
        column(4, div(class = "site-summary-panel", h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), uiOutput("site_summary_ui"), br(), plotOutput("mohonk_species_plot", height = "250px")))
      ),
      div(class = "data-table-styled-container", h4("Mohonk Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"), DTOutput("mohonk_data_table"))
    ),
    
    # Tab 2: Mianus River Gorge Preserve Site View
    tabPanel(title = "Mianus River Gorge Preserve, CT/NY",
      value = "Mianus River Gorge",
      h3("Mianus River Gorge Preserve - Location and Data Overview", class = "text-xl font-semibold mb-4 text-gray-700"),
      fluidRow(
        column(8, leafletOutput("mianus_map", height = "500px")),
        column(4, div(class = "site-summary-panel", h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), uiOutput("site_summary_ui_mianus"), br(), plotOutput("mianus_species_plot", height = "250px")))
      ),
      div(class = "data-table-styled-container", h4("Mianus River Gorge Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"), DTOutput("mianus_data_table"))
    )
  )
)

# --- 4. Server Logic ---

server <- function(input, output, session) {

  filtered_data <- reactive({ tick_data })

  # --- Data and Summary Generation ---

  output$site_summary_ui <- renderUI({
    generate_site_summary(filtered_data(), "Mohonk Preserve", min_date, max_date)
  })
  output$site_summary_ui_mianus <- renderUI({
    generate_site_summary(filtered_data(), "Mianus River Gorge", min_date, max_date)
  })

  output$mohonk_species_plot <- renderPlot({
    generate_species_plot(filtered_data(), "Mohonk Preserve")
  })
  output$mianus_species_plot <- renderPlot({
    generate_species_plot(filtered_data(), "Mianus River Gorge")
  })

  output$mohonk_data_table <- renderDT({
    datatable(filtered_data() %>% filter(Site == "Mohonk Preserve") %>% select(-Site),
              options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'),
              rownames = FALSE)
  })
  output$mianus_data_table <- renderDT({
    datatable(filtered_data() %>% filter(Site == "Mianus River Gorge") %>% select(-Site),
              options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'),
              rownames = FALSE)
  })

  # 1. Mohonk Map
  output$mohonk_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mohonk Preserve"]][1]
    lng <- SITE_COORDS[["Mohonk Preserve"]][2]

    # Calculate life stage counts for each exclosure
    mohonk_summary <- summarize_by_transect(filtered_data(), "Mohonk Preserve")

    # Create labels with life stage breakdown in table format and colored markers
    exclosures_with_labels <- mohonk_exclosures %>%
      rowwise() %>%
      mutate(
        adult_count = get_count(mohonk_summary %>% filter(Transect == Exclosure_Name, Life_Stage == "Adult")),
        nymph_count = get_count(mohonk_summary %>% filter(Transect == Exclosure_Name, Life_Stage == "Nymph")),
        total_count = adult_count + nymph_count,
        marker_color = if(total_count == 0) "gray" else "red",
        label_text = create_tick_tooltip(
          title = Exclosure_Name,
          create_table_row("Transect", adult_count, nymph_count, total_count)
        )
      ) %>%
      ungroup()

    # Create custom icons for each marker based on color
    icons_list <- lapply(exclosures_with_labels$marker_color, function(color) {
      awesomeIcons(
        icon = 'map-pin',
        iconColor = 'white',
        library = 'fa',
        markerColor = color
      )
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
        labelOptions = labelOptions(
          permanent = FALSE,
          direction = "auto",
          textOnly = FALSE,
          style = list("background-color" = "white", "border" = "1px solid #ccc", "padding" = "8px")
        ),
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
  
  # 2. Mianus River Map - With Preserve Boundary, Exclosures, and Transects
  output$mianus_map <- renderLeaflet({
    # Use hard-coded Mianus River Gorge coordinates
    center_lat <- SITE_COORDS[["Mianus River Gorge"]][1]
    center_lng <- SITE_COORDS[["Mianus River Gorge"]][2]

    # Initialize map with base tiles
    map <- init_leaflet_map(center_lng, center_lat, zoom = 17, max_zoom = 20)

    # Add layers in order (bottom to top)
    map <- add_preserve_polygons(map, polygons_data)
    map <- add_exclosure_polygons(map, exclosures_data)

    # --- Add Transects ---
    if (!is.null(transects_data)) {
      # Calculate hierarchical counts (transect, line, segment levels)
      summaries <- summarize_mianus_hierarchical(filtered_data())
      transect_summary <- summaries$transect
      line_summary <- summaries$line
      segment_summary <- summaries$segment

      for (i in seq_along(transects_data$features)) {
        feature <- transects_data$features[[i]]
        coords <- feature$geometry$coordinates

        # Get properties
        segm_code <- feature$properties$SegmCode
        treatm <- feature$properties$Treatm

        # Parse hierarchy from segment code (e.g., MRG-1-Fen-4-A)
        transect_num <- sub("^(MRG-\\d+).*", "\\1", segm_code)  # MRG-1
        line_code <- sub("-[A-Z]$", "", segm_code)             # MRG-1-Fen-4

        # Get TRANSECT level counts (e.g., all of MRG-1)
        t_adult <- get_count(transect_summary %>% filter(Transect_Num == transect_num, Life_Stage == "Adult"))
        t_nymph <- get_count(transect_summary %>% filter(Transect_Num == transect_num, Life_Stage == "Nymph"))
        t_total <- t_adult + t_nymph

        # Get LINE level counts (e.g., all segments in MRG-1-Fen-4)
        l_adult <- get_count(line_summary %>% filter(Line == line_code, Life_Stage == "Adult"))
        l_nymph <- get_count(line_summary %>% filter(Line == line_code, Life_Stage == "Nymph"))
        l_total <- l_adult + l_nymph

        # Get SEGMENT level counts (e.g., just MRG-1-Fen-4-A)
        s_adult <- get_count(segment_summary %>% filter(Segment == segm_code, Life_Stage == "Adult"))
        s_nymph <- get_count(segment_summary %>% filter(Segment == segm_code, Life_Stage == "Nymph"))
        s_total <- s_adult + s_nymph

        # Color: grey if no ticks on THIS SEGMENT, red/purple if has ticks
        if (s_total == 0) {
          line_color <- "#9CA3AF"  # Grey
        } else {
          line_color <- if (!is.null(treatm) && treatm == "Unf") "#EF4444" else "#7C3AED"  # Red or Purple
        }

        # Create table-style tooltip content using unified template
        tooltip_content <- create_tick_tooltip(
          title = segm_code,
          create_table_row("Transect", t_adult, t_nymph, t_total),
          create_table_row("Line", l_adult, l_nymph, l_total, bgcolor = "#f9fafb"),
          create_table_row("Segment", s_adult, s_nymph, s_total)
        )

        # Add polyline
        map <- map %>%
          addPolylines(
            lng = sapply(coords, function(x) x[[1]]),
            lat = sapply(coords, function(x) x[[2]]),
            color = line_color,
            weight = 7,
            opacity = 0.9,
            group = "Transects",
            label = lapply(tooltip_content, HTML),
            labelOptions = labelOptions(
              permanent = FALSE,
              direction = "auto",
              textOnly = FALSE,
              style = list("background-color" = "white", "border" = "1px solid #ccc", "padding" = "8px")
            ),
            highlightOptions = highlightOptions(
              weight = 9,
              color = "#FBBF24",
              opacity = 1,
              bringToFront = TRUE
            )
          )
      }
    }

    # Add trails
    map <- add_trail_polylines(map, trails_data)

    # Add Model Layer circle and Layers Control
    map %>%
      addCircles(lng = center_lng, lat = center_lat, radius = 1000, color = "#0000FF", fillOpacity = 0.2, group = "Model Layer") %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"),
        overlayGroups = c("Preserve Boundary", "Exclosures", "Transects", "Trails", "Model Layer"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Trails") %>%  # Hide trails by default
      hideGroup("Preserve Boundary") %>%  # Hide preserve boundary by default
      hideGroup("Exclosures") %>%  # Hide exclosures by default
      hideGroup("Model Layer")  # Hide model layer by default
  })
}

# Run the application
shinyApp(ui = ui, server = server)
