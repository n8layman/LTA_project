# Shiny Application (app.R) to Display GeoJSON Polygons on a Leaflet Map
#
# This file is set up for shinylive deployment.
library(shiny)
library(leaflet)
library(jsonlite)
library(dplyr)
library(DT)
library(lubridate)
library(leaflet.providers)
library(leaflet.extras)
library(readxl) 
library(htmltools) 

# --- Configuration & Data Loading ---

# Path to the GeoJSON file created by the conversion script
GEOJSON_FILE_PATH <- "mianus_polygons.geojson"

# --- 1. DATA SETUP: Standardized Site Coordinates ---
SITE_COORDS <- list(
  "Mohonk Preserve" = c(41.776, -74.135),
  "Mianus River Gorge" = c(41.1767, -73.620) 
)

# Function to safely load and parse GeoJSON data (Made robust)
load_geojson <- function(file_path) {
  # Fallback coordinates
  fallback_lon <- SITE_COORDS[["Mianus River Gorge"]][2] 
  fallback_lat <- SITE_COORDS[["Mianus River Gorge"]][1]
  
  if (!file.exists(file_path)) {
    warning(paste("GeoJSON file not found at:", file_path))
    return(list(geojson = NULL, center = c(fallback_lon, fallback_lat)))
  }
  
  geojson_string <- readLines(file_path, warn = FALSE) %>% 
    paste(collapse = "\n")
  
  # Safely parse the GeoJSON string
  data <- tryCatch({
    jsonlite::fromJSON(geojson_string, simplifyVector = FALSE)
  }, error = function(e) {
    warning(paste("Error parsing GeoJSON:", e$message))
    return(NULL)
  })
  
  if (is.null(data)) {
    warning("Parsed GeoJSON data is NULL. Using site coordinates for center.")
    return(list(geojson = NULL, center = c(fallback_lon, fallback_lat)))
  }
  
  # Find the center of the bounding box for map focus
  if (!is.null(data$bbox) && length(data$bbox) >= 4) {
    bbox <- data$bbox
    center_lon <- (bbox[1] + bbox[3]) / 2
    center_lat <- (bbox[2] + bbox[4]) / 2
  } else {
    # Fallback to the Mianus site coordinates
    center_lon <- fallback_lon
    center_lat <- fallback_lat
  }
  
  return(list(
    geojson = data,
    center = c(center_lon, center_lat)
  ))
}

# Load the data once at app startup
geojson_data_list <- load_geojson(GEOJSON_FILE_PATH)

if (is.null(geojson_data_list$geojson)) {
  warning("GeoJSON data could not be loaded. Please ensure polygons.geojson is in app/GIS_MRG.")
}


# --- 2. Data Generation and Setup ---

set.seed(42)
start_date <- ymd("2023-04-01")
end_date <- ymd("2024-09-30")
dates <- seq(start_date, end_date, by = "day")
n <- 500

mock_data <- data.frame(
  Date = sample(dates, n, replace = TRUE),
  # Use standardized Mianus name
  Site = sample(
    c("Mohonk Preserve", "Mianus River Gorge"), 
    n, replace = TRUE, prob = c(0.6, 0.4)
  ),
  Species = sample(
    c("Ixodes scapularis (Blacklegged)", "Amblyomma americanum (Lone Star)", "Dermacentmaus variabilis (Dog Tick)"),
    n, replace = TRUE, prob = c(0.5, 0.3, 0.2)
  ),
  Count = round(rlnorm(n, meanlog = 1.5, sdlog = 0.8)) + 1
) %>%
  arrange(Date, Site)
exclosures_data <- data.frame(
  Exclosure_Name = c("Cedar Drive Loop", "Canaan Rd", "Glory Hill", "Undercliff Rd"),
  Latitude = c(41.79766, 41.78495, 41.74764, 41.75430),
  Longitude = c(-74.11761, -74.10986, -74.14738, -74.16813)
)
min_date <- min(mock_data$Date)
max_date <- max(mock_data$Date)

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
        column(4, div(class = "site-summary-panel", h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), uiOutput("site_summary_ui")))
      ),
      div(class = "data-table-styled-container", h4("Mohonk Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"), DTOutput("mohonk_data_table"))
    ),
    
    # Tab 2: Mianus River Gorge Preserve Site View
    tabPanel(title = "Mianus River Gorge Preserve, CT/NY", 
      value = "Mianus River Gorge",
      h3("Mianus River Gorge Preserve - Location and Data Overview", class = "text-xl font-semibold mb-4 text-gray-700"),
      fluidRow(
        column(8, leafletOutput("mianus_map", height = "500px")),
        column(4, div(class = "site-summary-panel", h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), uiOutput("site_summary_ui_mianus")))
      ),
      div(class = "data-table-styled-container", h4("Raw Data: Mianus River Gorge Preserve", class = "text-lg font-semibold mb-3 text-gray-800"), DTOutput("mianus_data_table"))
    )
  )
)

# --- 4. Server Logic ---

server <- function(input, output, session) {

  filtered_data <- reactive({ mock_data })
  
  # --- Toggle State Reactive Values ---
  mohonk_model_state <- reactiveVal(FALSE) 
  mianus_model_state <- reactiveVal(FALSE)
  
  # ------------------------------------------------------------------
  # 4a. GEOJSON STYLING AND INTERACTION FUNCTIONS (Mianus Map)
  # ------------------------------------------------------------------
  
  # 1. JavaScript function for INITIAL style (passed to 'style' argument)
  # This uses pure JS to avoid R serialization issues.
  polygon_style_js <- JS("function(feature) {
    // Blue (#00A0B0) for Done (1), Red (#E54028) for Not Done (0)
    var color = feature.properties.Done == 1 ? '#00A0B0' : '#E54028';
    
    return {
      fillColor: color,
      weight: 2,
      opacity: 1,
      color: 'white',
      fillOpacity: 0.5
    };
  }")
  
  # 2. JavaScript function for HOVER, POPUP, and CLICK interactions (passed to 'onEachFeature' argument)
  on_each_feature_interactions <- JS(
    "function(feature, layer) {
      
      // 1. Define highlight style for hover
      var highlightStyle = {
        weight: 5,
        color: '#F9D030', // Bright yellow outline for hover
        dashArray: '',
        fillOpacity: 0.7
      };
      
      // 2. Define original style (references the styling function passed to 'style')
      var originalStyle = layer.options.style(feature); // Retrieve base style from the main style function

      // 3. Event Handlers: Mouseover, Mouseout, Click
      layer.on({
        mouseover: function(e) {
          layer.setStyle(highlightStyle); // Apply highlight style
          // Bring the highlighted layer to the front for visibility
          if (!L.Browser.ie && !L.Browser.opera && !L.Browser.edge) {
            layer.bringToFront();
          }
        },
        mouseout: function(e) {
          layer.setStyle(originalStyle); // Reset to original style
        },
        click: function(e) {
          // Zoom to the feature on click
          e.target._map.fitBounds(e.target.getBounds());
        }
      });

      // 4. Popup and Tooltip Binding
      if (feature.properties) {
        var status = feature.properties.Done == 1 ? 'Yes' : 'No';
        var colorClass = feature.properties.Done == 1 ? 'text-green-600 font-bold' : 'text-red-600 font-bold';

        var label = '<div class=\"font-sans text-sm\">' +
                    '<strong>Area ID: ' + feature.properties.Area + '</strong>' +
                    '<br>Year Planned: ' + feature.properties.YearPlanne + 
                    '<br>Done: <span class=\"' + colorClass + '\">' + status + '</span>' +
                    '</div>';
                    
        layer.bindPopup(label);
        // Sticky tooltip showing the Area ID
        layer.bindTooltip('Area ' + feature.properties.Area, { sticky: true });
      }
    }"
  )
  
  # ------------------------------------------------------------------
  # 4b. MODEL AND DATA LOGIC (UNCHANGED)
  # ------------------------------------------------------------------
  
  # --- Mohonk Model Layer Toggle Logic ---
  observeEvent(input$mohonk_model_toggle, {
    new_state <- !mohonk_model_state()
    mohonk_model_state(new_state) 
    session$sendCustomMessage("set-attribute", list(id = "easy_button_mohonk"))
  })
  
  observeEvent(mohonk_model_state(), {
    proxy <- leafletProxy("mohonk_map", session)
    if (mohonk_model_state()) {
      proxy %>% addCircles(lng = SITE_COORDS[["Mohonk Preserve"]][2], lat = SITE_COORDS[["Mohonk Preserve"]][1], radius = 5000, color = "#FF0000", fillOpacity = 0.2, group = "Model Layer", layerId = "Model Layer")
    } else {
      proxy %>% removeShape(layerId = "Model Layer") 
    }
  })
  
  # --- Mianus Model Layer Toggle Logic ---
  observeEvent(input$mianus_model_toggle, {
    new_state <- !mianus_model_state()
    mianus_model_state(new_state) 
    session$sendCustomMessage("set-attribute", list(id = "easy_button_mianus"))
  })
  
  observeEvent(mianus_model_state(), {
    proxy <- leafletProxy("mianus_map", session)
    if (mianus_model_state()) {
      proxy %>% addCircles(lng = SITE_COORDS[["Mianus River Gorge"]][2], lat = SITE_COORDS[["Mianus River Gorge"]][1], radius = 1000, color = "#0000FF", fillOpacity = 0.2, group = "Mianus Model Layer", layerId = "Mianus Model Layer")
    } else {
      proxy %>% removeShape(layerId = "Mianus Model Layer") 
    }
  })

  # --- CUSTOM JAVASCRIPT FOR TOGGLING EASYBUTTON ACTIVE CLASS ---
  session$sendCustomMessage("set-attribute", message = list(code = 
    "Shiny.setInputValue('trigger', Math.random());
     Shiny.addCustomMessageHandler('set-attribute', function(message) {
       // Toggle the 'active' class on the EasyButton element itself
       $('#' + message.id).toggleClass('active');
     });"
  ))
  
  # --- Data and Summary Generation ---
  
  generate_site_summary <- function(site_name) {
    data <- filtered_data() %>% filter(Site == site_name) 
    total_ticks <- sum(data$Count)
    unique_species <- length(unique(data$Species))
    
    tagList(
      p(HTML(paste0("<strong>Site:</strong> ", site_name)), class = "text-gray-600"),
      p(HTML(paste0("<strong>Total Ticks Counted:</strong> ", format(total_ticks, big.mark = ","))), class = "text-gray-600"),
      p(HTML(paste0("<strong>Unique Species Found:</strong> ", unique_species)), class = "text-gray-600"),
      p(paste0("The summary above reflects all data collected between ", min_date, " and ", max_date, " at this site."), class = "text-sm text-gray-500 mt-2")
    )
  }
  
  output$site_summary_ui <- renderUI({ generate_site_summary("Mohonk Preserve") })
  output$site_summary_ui_mianus <- renderUI({ generate_site_summary("Mianus River Gorge") })
  
  output$mohonk_data_table <- renderDT({ datatable(filtered_data() %>% filter(Site == "Mohonk Preserve") %>% select(-Site), options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'), rownames = FALSE) })
  output$mianus_data_table <- renderDT({ datatable(filtered_data() %>% filter(Site == "Mianus River Gorge") %>% select(-Site), options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'), rownames = FALSE, caption = 'All data points for Mianus River Gorge Preserve.') })
  
  # 1. Mohonk Map
  output$mohonk_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mohonk Preserve"]][1]
    lng <- SITE_COORDS[["Mohonk Preserve"]][2]
    
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles(group = "Default Map") %>%
      setView(lng = lng, lat = lat, zoom = 12.5) %>%
      addMarkers(data = exclosures_data, lat = ~Latitude, lng = ~Longitude, popup = ~paste("Exclosure:", Exclosure_Name), label = ~Exclosure_Name) %>%
      
      addLayersControl(baseGroups = c("Default Map", "Topographic", "Satellite"), options = layersControlOptions(collapsed = TRUE)) %>%
      
      addEasyButton(
        easyButton(
          id = "easy_button_mohonk", 
          icon = "fas fa-chart-column",
          title = "Show Predictive Modeling Results (TBD)",
          position = "bottomleft",
          onClick = JS("function(btn, map) { Shiny.setInputValue('mohonk_model_toggle', Math.random(), {priority: 'event'}); }")
        )
      )
  })
  
  # 2. Mianus River Map - Using JS for Style and Interactions to fix serialization error
  output$mianus_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mianus River Gorge"]][1]
    lng <- SITE_COORDS[["Mianus River Gorge"]][2]
    
    # Retrieve the pre-loaded GeoJSON data
    geojson_data <- geojson_data_list$geojson
    
    map <- leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      addTiles(group = "Default Map") %>%
      setView(lng = lng, lat = lat, zoom = 14.5) 

    # Conditionally add GeoJSON if it was loaded successfully
    if (!is.null(geojson_data)) {

      # Extract features and convert to spatial data
      for (i in seq_along(geojson_data$features)) {
        feature <- geojson_data$features[[i]]
        coords <- feature$geometry$coordinates

        # Get properties
        area <- feature$properties$Area
        done <- feature$properties$Done
        year_planned <- feature$properties$YearPlanne

        # Set color based on Done status
        fill_color <- if (done == 1) "#00A0B0" else "#E54028"
        status_text <- if (done == 1) "Yes" else "No"

        # Create popup content
        popup_content <- sprintf(
          '<div style="font-family: sans-serif; font-size: 12px;"><strong>Area ID: %s</strong><br>Year Planned: %s<br>Done: <span style="color: %s; font-weight: bold;">%s</span></div>',
          area, year_planned,
          if (done == 1) "#059669" else "#DC2626",
          status_text
        )

        # Add polygon
        map <- map %>%
          addPolygons(
            lng = sapply(coords[[1]], function(x) x[[1]]),
            lat = sapply(coords[[1]], function(x) x[[2]]),
            fillColor = fill_color,
            fillOpacity = 0.5,
            color = "white",
            weight = 2,
            opacity = 1,
            group = "Boundary Polygons",
            popup = popup_content,
            label = paste("Area", area),
            highlightOptions = highlightOptions(
              weight = 5,
              color = "#F9D030",
              fillOpacity = 0.7,
              bringToFront = TRUE
            )
          )
      }
    }
    
    # Add Layers Control and EasyButton
    map %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"), 
        overlayGroups = c("Boundary Polygons"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addEasyButton(
        easyButton(
          id = "easy_button_mianus", 
          icon = "fas fa-chart-column",
          title = "Show Predictive Modeling Results (TBD)",
          position = "bottomleft",
          onClick = JS("function(btn, map) { Shiny.setInputValue('mianus_model_toggle', Math.random(), {priority: 'event'}); }")
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
