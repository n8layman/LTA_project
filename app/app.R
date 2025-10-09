# Shiny Application (app.R) to Display GeoJSON Polygons on a Leaflet Map
#
# This file is set up for shinylive deployment.
library(shiny)
library(munsell)
library(leaflet)
library(jsonlite)
library(dplyr)
library(DT)
library(lubridate)
library(leaflet.providers)
library(leaflet.extras)
library(readxl)
library(htmltools)
library(ggplot2)

# --- Configuration & Data Loading ---

# Paths to the GeoJSON files created by the conversion script
POLYGONS_FILE_PATH <- "mianus_polygons.geojson"
EXCLOSURES_FILE_PATH <- "mianus_exclosures.geojson"
TRANSECTS_FILE_PATH <- "mianus_transects.geojson"
TRAILS_FILE_PATH <- "mianus_trails.geojson"

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
polygons_data_list <- load_geojson(POLYGONS_FILE_PATH)
exclosures_data_list <- load_geojson(EXCLOSURES_FILE_PATH)
transects_data_list <- load_geojson(TRANSECTS_FILE_PATH)
trails_data_list <- load_geojson(TRAILS_FILE_PATH)

if (is.null(polygons_data_list$geojson)) {
  warning("Polygons GeoJSON data could not be loaded. Please ensure mianus_polygons.geojson is in app directory.")
}
if (is.null(exclosures_data_list$geojson)) {
  warning("Exclosures GeoJSON data could not be loaded. Please ensure mianus_exclosures.geojson is in app directory.")
}
if (is.null(transects_data_list$geojson)) {
  warning("Transects GeoJSON data could not be loaded. Please ensure mianus_transects.geojson is in app directory.")
}
if (is.null(trails_data_list$geojson)) {
  warning("Trails GeoJSON data could not be loaded. Please ensure mianus_trails.geojson is in app directory.")
}


# --- 2. Load Tick Data from CSV ---

# Read tick data from CSV file
tick_data <- read.csv("tick_data.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = ymd(Date)) %>%
  arrange(Date, Site, Transect)
exclosures_data <- data.frame(
  Exclosure_Name = c("Cedar Drive Loop", "Canaan Rd", "Glory Hill", "Undercliff Rd"),
  Latitude = c(41.79766, 41.78495, 41.74764, 41.75430),
  Longitude = c(-74.11761, -74.10986, -74.14738, -74.16813)
)
min_date <- min(tick_data$Date)
max_date <- max(tick_data$Date)

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

  # Generate species distribution plots by life stage
  generate_species_plot <- function(site_name) {
    data <- filtered_data() %>%
      filter(Site == site_name) %>%
      group_by(Species, Life_Stage) %>%
      summarize(Total = sum(Count), .groups = "drop") %>%
      mutate(
        Species_Abbrev = case_when(
          Species == "Ixodes scapularis (Blacklegged)" ~ "I. scap",
          Species == "Amblyomma americanum (Lone Star)" ~ "A. amer",
          Species == "Dermacentmaus variabilis (Dog Tick)" ~ "D. var",
          TRUE ~ Species
        )
      )

    ggplot(data, aes(x = Species_Abbrev, y = Total, fill = Life_Stage)) +
      geom_bar(stat = "identity", position = "stack", width = 0.6) +
      scale_fill_manual(
        values = c("Adult" = "#3B82F6", "Nymph" = "#10B981"),
        labels = c("Adult", "Nymph")
      ) +
      labs(x = NULL, y = "Count", fill = NULL) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(5, 5, 5, 5)
      )
  }

  output$mohonk_species_plot <- renderPlot({ generate_species_plot("Mohonk Preserve") })
  output$mianus_species_plot <- renderPlot({ generate_species_plot("Mianus River Gorge") })

  output$mohonk_data_table <- renderDT({ datatable(filtered_data() %>% filter(Site == "Mohonk Preserve") %>% select(-Site), options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'), rownames = FALSE) })
  output$mianus_data_table <- renderDT({ datatable(filtered_data() %>% filter(Site == "Mianus River Gorge") %>% select(-Site), options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'), rownames = FALSE) })
  
  # 1. Mohonk Map
  output$mohonk_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mohonk Preserve"]][1]
    lng <- SITE_COORDS[["Mohonk Preserve"]][2]

    # Calculate life stage counts for each exclosure
    mohonk_summary <- filtered_data() %>%
      filter(Site == "Mohonk Preserve") %>%
      group_by(Transect, Life_Stage) %>%
      summarize(Total = sum(Count), .groups = "drop")

    # Create labels with life stage breakdown in table format and colored markers
    exclosures_with_labels <- exclosures_data %>%
      rowwise() %>%
      mutate(
        adult_count = mohonk_summary %>% filter(Transect == Exclosure_Name, Life_Stage == "Adult") %>% pull(Total) %>% {if(length(.) > 0) sum(.) else 0},
        nymph_count = mohonk_summary %>% filter(Transect == Exclosure_Name, Life_Stage == "Nymph") %>% pull(Total) %>% {if(length(.) > 0) sum(.) else 0},
        total_count = adult_count + nymph_count,
        marker_color = if(total_count == 0) "gray" else "red",
        label_text = sprintf(
          '<div style="font-family: sans-serif; font-size: 11px;">
          <strong>%s</strong><br>
          <table style="margin-top: 6px; border-collapse: collapse;">
            <tr style="background-color: #f3f4f6;">
              <th style="padding: 3px 6px; text-align: left; border: 1px solid #d1d5db; font-size: 10px;">Level</th>
              <th style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">Adult</th>
              <th style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">Nymph</th>
              <th style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">Total</th>
            </tr>
            <tr>
              <td style="padding: 3px 6px; border: 1px solid #d1d5db; font-size: 10px;">Transect</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
            </tr>
          </table>
          </div>',
          Exclosure_Name, adult_count, nymph_count, total_count
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
    # Retrieve the pre-loaded GeoJSON data
    polygons_data <- polygons_data_list$geojson
    exclosures_data <- exclosures_data_list$geojson
    transects_data <- transects_data_list$geojson
    trails_data <- trails_data_list$geojson

    # Calculate center of all transects
    if (!is.null(transects_data) && length(transects_data$features) > 0) {
      all_coords <- list()
      for (i in seq_along(transects_data$features)) {
        coords <- transects_data$features[[i]]$geometry$coordinates
        for (coord in coords) {
          all_coords[[length(all_coords) + 1]] <- coord
        }
      }
      lngs <- sapply(all_coords, function(x) x[[1]])
      lats <- sapply(all_coords, function(x) x[[2]])
      center_lng <- mean(lngs)
      center_lat <- mean(lats)
    } else {
      # Fallback to site coordinates
      center_lat <- SITE_COORDS[["Mianus River Gorge"]][1]
      center_lng <- SITE_COORDS[["Mianus River Gorge"]][2]
    }

    map <- leaflet(options = leafletOptions(maxZoom = 20)) %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic", options = providerTileOptions(maxZoom = 20)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite", options = providerTileOptions(maxZoom = 20)) %>%
      addTiles(group = "Default Map", options = tileOptions(maxZoom = 20)) %>%
      setView(lng = center_lng, lat = center_lat, zoom = 17)

    # --- Add Preserve Boundary Polygons (FIRST - so they're on bottom) ---
    if (!is.null(polygons_data)) {
      for (i in seq_along(polygons_data$features)) {
        feature <- polygons_data$features[[i]]
        coords <- feature$geometry$coordinates

        # Get properties
        name <- feature$properties$Name
        acres <- feature$properties$Acres
        nhp_name <- feature$properties$NHP_Name

        # Create popup content
        popup_content <- sprintf(
          '<div style="font-family: sans-serif; font-size: 12px;"><strong>%s</strong><br>Area: %.2f acres<br>NHP: %s</div>',
          ifelse(!is.null(name) && name != "", name, "Unnamed Parcel"),
          ifelse(!is.null(acres), acres, 0),
          ifelse(!is.null(nhp_name), nhp_name, "N/A")
        )

        # Add polygon - green for preserve boundaries
        map <- map %>%
          addPolygons(
            lng = sapply(coords[[1]], function(x) x[[1]]),
            lat = sapply(coords[[1]], function(x) x[[2]]),
            fillColor = "#10B981",
            fillOpacity = 0.2,
            color = "#059669",
            weight = 2,
            opacity = 0.8,
            group = "Preserve Boundary",
            popup = popup_content,
            label = ifelse(!is.null(name) && name != "", name, "Parcel"),
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#34D399",
              fillOpacity = 0.4,
              bringToFront = FALSE  # Keep preserve boundary on bottom
            )
          )
      }
    }

    # --- Add Exclosures ---
    if (!is.null(exclosures_data)) {
      for (i in seq_along(exclosures_data$features)) {
        feature <- exclosures_data$features[[i]]
        coords <- feature$geometry$coordinates

        # Get properties
        area <- feature$properties$Area
        done <- feature$properties$Done
        year_planned <- feature$properties$YearPlanne

        # Set color - purple for exclosures
        fill_color <- if (!is.null(done) && done == 1) "#9333EA" else "#C084FC"
        status_text <- if (!is.null(done) && done == 1) "Yes" else "No"

        # Create unique label using sequential ID
        unique_label <- paste0("Exclosure #", i)

        # Create popup content with area and year details
        popup_content <- sprintf(
          '<div style="font-family: sans-serif; font-size: 12px;"><strong>Exclosure #%s</strong><br>Area: %s<br>Year Planned: %s<br>Done: <span style="color: %s; font-weight: bold;">%s</span></div>',
          i, area, year_planned,
          if (!is.null(done) && done == 1) "#059669" else "#DC2626",
          status_text
        )

        # Add polygon
        map <- map %>%
          addPolygons(
            lng = sapply(coords[[1]], function(x) x[[1]]),
            lat = sapply(coords[[1]], function(x) x[[2]]),
            fillColor = fill_color,
            fillOpacity = 0.15,
            color = "#7C3AED",
            weight = 2,
            opacity = 0.7,
            group = "Exclosures"
          )
      }
    }

    # --- Add Transects ---
    if (!is.null(transects_data)) {
      # Calculate counts at different hierarchical levels
      mianus_data <- filtered_data() %>%
        filter(Site == "Mianus River Gorge")

      # Extract hierarchy: MRG-1-Fen-4-A -> Transect: MRG-1, Line: MRG-1-Fen-4, Segment: MRG-1-Fen-4-A
      # For data, we need to create hierarchy columns
      mianus_data_with_hierarchy <- mianus_data %>%
        mutate(
          # Extract transect number (e.g., "MRG-1" from "MRG-1-Fen-4")
          Transect_Num = sub("^(MRG-\\d+).*", "\\1", Transect),
          # Line is the Transect column (e.g., "MRG-1-Fen-4")
          Line = Transect
        )

      # Level 1: Transect totals (e.g., all of MRG-1)
      transect_summary <- mianus_data_with_hierarchy %>%
        group_by(Transect_Num, Life_Stage) %>%
        summarize(Total = sum(Count), .groups = "drop")

      # Level 2: Line totals (e.g., all segments in MRG-1-Fen-4)
      line_summary <- mianus_data_with_hierarchy %>%
        group_by(Line, Life_Stage) %>%
        summarize(Total = sum(Count), .groups = "drop")

      # Level 3: Segment totals (e.g., just MRG-1-Fen-4-A)
      segment_summary <- mianus_data %>%
        group_by(Segment, Life_Stage) %>%
        summarize(Total = sum(Count), .groups = "drop")

      for (i in seq_along(transects_data$features)) {
        feature <- transects_data$features[[i]]
        coords <- feature$geometry$coordinates

        # Get properties
        segm_code <- feature$properties$SegmCode
        site <- feature$properties$Site
        pair <- feature$properties$Pair
        treatm <- feature$properties$Treatm
        transect <- feature$properties$Transect
        segment <- feature$properties$Segment

        # Parse hierarchy from segment code (e.g., MRG-1-Fen-4-A)
        transect_num <- sub("^(MRG-\\d+).*", "\\1", segm_code)  # MRG-1
        line_code <- sub("-[A-Z]$", "", segm_code)             # MRG-1-Fen-4

        # Get TRANSECT level counts (e.g., all of MRG-1)
        t_adult <- transect_summary %>%
          filter(Transect_Num == transect_num, Life_Stage == "Adult") %>%
          pull(Total) %>%
          {if(length(.) > 0) sum(.) else 0}

        t_nymph <- transect_summary %>%
          filter(Transect_Num == transect_num, Life_Stage == "Nymph") %>%
          pull(Total) %>%
          {if(length(.) > 0) sum(.) else 0}

        t_total <- t_adult + t_nymph

        # Get LINE level counts (e.g., all segments in MRG-1-Fen-4)
        l_adult <- line_summary %>%
          filter(Line == line_code, Life_Stage == "Adult") %>%
          pull(Total) %>%
          {if(length(.) > 0) sum(.) else 0}

        l_nymph <- line_summary %>%
          filter(Line == line_code, Life_Stage == "Nymph") %>%
          pull(Total) %>%
          {if(length(.) > 0) sum(.) else 0}

        l_total <- l_adult + l_nymph

        # Get SEGMENT level counts (e.g., just MRG-1-Fen-4-A)
        s_adult <- segment_summary %>%
          filter(Segment == segm_code, Life_Stage == "Adult") %>%
          pull(Total) %>%
          {if(length(.) > 0) sum(.) else 0}

        s_nymph <- segment_summary %>%
          filter(Segment == segm_code, Life_Stage == "Nymph") %>%
          pull(Total) %>%
          {if(length(.) > 0) sum(.) else 0}

        s_total <- s_adult + s_nymph

        # Color: grey if no ticks on THIS SEGMENT, red/purple if has ticks
        if (s_total == 0) {
          line_color <- "#9CA3AF"  # Grey
        } else {
          line_color <- if (!is.null(treatm) && treatm == "Unf") "#EF4444" else "#7C3AED"  # Red or Purple
        }

        # Create table-style tooltip content
        tooltip_content <- sprintf(
          '<div style="font-family: sans-serif; font-size: 11px;">
          <strong>%s</strong><br>
          <table style="margin-top: 6px; border-collapse: collapse;">
            <tr style="background-color: #f3f4f6;">
              <th style="padding: 3px 6px; text-align: left; border: 1px solid #d1d5db; font-size: 10px;">Level</th>
              <th style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">Adult</th>
              <th style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">Nymph</th>
              <th style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">Total</th>
            </tr>
            <tr>
              <td style="padding: 3px 6px; border: 1px solid #d1d5db; font-size: 10px;">Transect</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
            </tr>
            <tr style="background-color: #f9fafb;">
              <td style="padding: 3px 6px; border: 1px solid #d1d5db; font-size: 10px;">Line</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
            </tr>
            <tr>
              <td style="padding: 3px 6px; border: 1px solid #d1d5db; font-size: 10px;">Segment</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
              <td style="padding: 3px 6px; text-align: right; border: 1px solid #d1d5db; font-size: 10px;">%d</td>
            </tr>
          </table>
          </div>',
          segm_code,
          t_adult, t_nymph, t_total,
          l_adult, l_nymph, l_total,
          s_adult, s_nymph, s_total
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

    # --- Add Trails ---
    if (!is.null(trails_data)) {
      for (i in seq_along(trails_data$features)) {
        feature <- trails_data$features[[i]]
        coords <- feature$geometry$coordinates

        # Get properties
        track_name <- feature$properties$TRACK
        color_name <- feature$properties$Color
        geotrans <- feature$properties$GEOTRANS

        # Create popup content
        popup_content <- sprintf(
          '<div style="font-family: sans-serif; font-size: 12px;"><strong>Trail: %s</strong><br>Color: %s</div>',
          ifelse(!is.null(track_name), track_name, "Unnamed Trail"),
          ifelse(!is.null(color_name), color_name, "N/A")
        )

        # Add polyline in dark grey
        map <- map %>%
          addPolylines(
            lng = sapply(coords, function(x) x[[1]]),
            lat = sapply(coords, function(x) x[[2]]),
            color = "#4B5563",  # Dark grey
            weight = 2,
            opacity = 0.7,
            group = "Trails",
            popup = popup_content,
            label = ifelse(!is.null(track_name), track_name, "Trail"),
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#1F2937",
              opacity = 1,
              bringToFront = TRUE
            )
          )
      }
    }

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
