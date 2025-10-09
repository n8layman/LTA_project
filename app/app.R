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
MOHONK_TRANSECTS_FILE_PATH <- "mohonk_transects.geojson"  # For future use

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
mohonk_transects_data <- load_geojson(MOHONK_TRANSECTS_FILE_PATH) 

# --- Load Tick Data ---
tick_data <- load_tick_data("tick_data_processed.csv") %>%
  arrange(SiteName, Transect, Segment)

# Mohonk exclosures data
mohonk_exclosures <- read.csv("mohonk_exclosures.csv", stringsAsFactors = FALSE)

# Date range (optional - will be NA if no dates in data)
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
      filtered_data() %>%
        filter(SiteName == "Mohonk Preserve"),
      extensions = 'Buttons',   # Enable Buttons extension
      options = list(
        dom = 'Btip',            # B = buttons, t = table, i = info, p = pagination
        buttons = list(
          list(
            extend = "excel",
            text = "Download"     # Button label
          )
        ),
        pageLength = 10,
        scrollY = '300px',
        autoWidth = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(2,3)) 
        )
      ),
      filter = "top",
      rownames = FALSE,
      selection = list(mode = "single", target = "row")
    )
  })

  output$mianus_data_table <- renderDT({
    datatable(
      filtered_data() %>%
        filter(SiteName == "Mianus River Gorge"),
        extensions = 'Buttons',   # Enable Buttons extension
        options = list(
          dom = 'Btip',            # B = buttons, t = table, i = info, p = pagination
          buttons = list(
            list(
              extend = "excel",
              text = "Download"     # Button label
            )
          ),
          pageLength = 10,
          scrollY = '300px',
          autoWidth = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = c(2,3)) 
          )
        ),
        filter = "top",
        rownames = FALSE,
        selection = list(mode = "single", target = "row")
      )
    })

  # --- Row Selection Observers ---

  # Mohonk table selection - highlight marker
  observeEvent(input$mohonk_data_table_rows_selected, {
    selected_row <- input$mohonk_data_table_rows_selected
    if (!is.null(selected_row)) {
      mohonk_data <- filtered_data() %>% filter(SiteName == "Mohonk Preserve")
      selected_site_location <- mohonk_data[selected_row, "Site_location"]

      # Find the exclosure matching this site location using Site_Match column
      matching_exclosure <- mohonk_exclosures %>%
        filter(Site_Match == selected_site_location)

      if (nrow(matching_exclosure) > 0) {
        leafletProxy("mohonk_map") %>%
          setView(
            lng = matching_exclosure$Longitude[1],
            lat = matching_exclosure$Latitude[1],
            zoom = 15
          )
      }
    }
  })

  # Mianus table selection - highlight segment
  observeEvent(input$mianus_data_table_rows_selected, {
    selected_row <- input$mianus_data_table_rows_selected
    if (!is.null(selected_row)) {
      mianus_data <- filtered_data() %>% filter(SiteName == "Mianus River Gorge")
      selected_segm_code <- mianus_data[selected_row, "SegmCode"]

      # Find the segment in the transects GeoJSON
      if (!is.null(transects_data)) {
        for (i in seq_along(transects_data$features)) {
          feature <- transects_data$features[[i]]
          props <- feature$properties
          if (!is.null(props$SegmCode) && trimws(props$SegmCode) == selected_segm_code) {
            coords <- feature$geometry$coordinates
            # Calculate center of the line
            lngs <- sapply(coords, function(x) x[[1]])
            lats <- sapply(coords, function(x) x[[2]])
            center_lng <- mean(lngs)
            center_lat <- mean(lats)

            leafletProxy("mianus_map") %>%
              setView(lng = center_lng, lat = center_lat, zoom = 18)
            break
          }
        }
      }
    }
  })

  # --- Mohonk Map ---
  output$mohonk_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mohonk Preserve"]][1]
    lng <- SITE_COORDS[["Mohonk Preserve"]][2]

    # Summarize Mohonk data by Site_location (which maps to exclosure areas)
    mohonk_summary <- filtered_data() %>%
      filter(SiteName == "Mohonk Preserve") %>%
      group_by(Site_location) %>%
      summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nymphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      )

    # Create labels with detailed transect breakdown for each exclosure
    # Use Site_Match column to join with data (some locations may not have data yet)
    exclosures_with_labels <- mohonk_exclosures %>%
      left_join(mohonk_summary, by = c("Site_Match" = "Site_location")) %>%
      mutate(
        Adult = replace_na(Adult, 0),
        Nymph = replace_na(Nymph, 0),
        Total = replace_na(Total, 0),
        marker_color = if_else(Total == 0, "#9CA3AF", "#EF4444"),  # Gray or Red
        marker_fill_opacity = 0.8
      ) %>%
      rowwise() %>%
      mutate(
        # Only create detailed tooltip if there's data (Site_Match is not empty)
        label_text = if (!is.na(Site_Match) && Site_Match != "") {
          create_mohonk_location_tooltip(filtered_data(), Site_Match, Exclosure_Name)
        } else {
          create_tick_tooltip(
            title = Exclosure_Name,
            create_table_row("Total", 0, 0, 0)
          )
        }
      ) %>%
      ungroup()

    map <- leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles(group = "Default Map") %>%
      setView(lng = lng, lat = lat, zoom = 12.5) %>%
      addCircleMarkers(
        data = exclosures_with_labels,
        lat = ~Latitude,
        lng = ~Longitude,
        radius = 10,
        color = ~marker_color,
        fillColor = ~marker_color,
        fillOpacity = ~marker_fill_opacity,
        weight = 2,
        opacity = 1,
        label = ~lapply(label_text, HTML),
        labelOptions = labelOptions(permanent = FALSE, direction = "auto"),
        group = "Exclosures"
      ) %>%
      addCircles(lng = lng, lat = lat, radius = 5000, color = "#FF0000", fillOpacity = 0.2, group = "Model Layer")

    # Add transect lines if polygon data is available
    if (!is.null(mohonk_transects_data)) {
      map <- add_mohonk_transects(map, mohonk_transects_data, filtered_data())
      map <- map %>%
        addLayersControl(
          baseGroups = c("Default Map", "Topographic", "Satellite"),
          overlayGroups = c("Exclosures", "Mohonk Transects", "Model Layer"),
          options = layersControlOptions(collapsed = TRUE)
        )
    } else {
      map <- map %>%
        addLayersControl(
          baseGroups = c("Default Map", "Topographic", "Satellite"),
          overlayGroups = c("Exclosures", "Model Layer"),
          options = layersControlOptions(collapsed = TRUE)
        )
    }

    map %>% hideGroup("Model Layer")
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
        
        # Extract hierarchy info from SegmCode
        # SegmCode format: MRG-1-Unf-2-B
        # where: 1=Pair, Unf=Treatment, 2=Line number, B=Segment
        # New hierarchy: Transect = Pair + Treatment, Line = Number, Segment = Letter
        pair_num <- sub("MRG-(\\d+)-.*", "\\1", segm_code)  # 1
        treatm_type <- sub("MRG-\\d+-([A-Za-z]+)-.*", "\\1", segm_code)  # Unf or Fen
        line_num <- sub("MRG-\\d+-[A-Za-z]+-(\\d+)-[A-Z]$", "\\1", segm_code)  # 2

        # Build codes for lookups
        transect_code <- paste(pair_num, treatm_type, sep = "-")  # "1-Unf"
        line_code <- paste(pair_num, treatm_type, line_num, sep = "-")  # "1-Unf-2"

        # Get counts for each level
        t_adult <- get_count(transect_summary %>% filter(Transect_Code == transect_code), "Adult")
        t_nymph <- get_count(transect_summary %>% filter(Transect_Code == transect_code), "Nymph")
        t_total <- t_adult + t_nymph

        l_adult <- get_count(line_summary %>% filter(Line_Code == line_code), "Adult")
        l_nymph <- get_count(line_summary %>% filter(Line_Code == line_code), "Nymph")
        l_total <- l_adult + l_nymph

        s_adult <- segment_data$Adult_Count
        s_nymph <- segment_data$Nymph_Count
        s_total <- segment_data$Total_Ticks

        tooltip_content <- create_tick_tooltip(
          title = segm_code,
          create_table_row(paste("Transect (Pair", pair_num, treatm_type, ")", sep = " "), t_adult, t_nymph, t_total),
          create_table_row(paste("Line", line_num), l_adult, l_nymph, l_total, bgcolor = "#f9fafb"),
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
    
    # Add trails with tick data highlighting
    if (!is.null(trails_data)) {
      # Get trailside tick summary (all trails combined since we don't have trail-specific mapping)
      trailside_summary <- summarize_trailside_data(filtered_data())

      has_ticks <- if (nrow(trailside_summary) > 0 && trailside_summary$Total[1] > 0) TRUE else FALSE
      trail_color <- if (has_ticks) "#EF4444" else "#4B5563"  # Red if ticks, gray if none

      # Create tooltip
      if (nrow(trailside_summary) > 0) {
        tooltip_content <- create_tick_tooltip(
          title = "Trail Network (All Trailside Surveys)",
          create_table_row("Total", trailside_summary$Adult[1], trailside_summary$Nymph[1], trailside_summary$Total[1])
        )
      } else {
        tooltip_content <- create_tick_tooltip(
          title = "Trail Network",
          create_table_row("Total", 0, 0, 0)
        )
      }

      # Add all trail segments with same color/tooltip
      for (i in seq_along(trails_data$features)) {
        feature <- trails_data$features[[i]]
        coords <- feature$geometry$coordinates
        trail_name <- feature$properties$TRACK

        map <- map %>%
          addPolylines(
            lng = sapply(coords, function(x) x[[1]]),
            lat = sapply(coords, function(x) x[[2]]),
            color = trail_color,
            weight = 3,
            opacity = 0.7,
            group = "Trails",
            label = lapply(tooltip_content, HTML),
            highlightOptions = highlightOptions(
              weight = 5,
              color = "#FBBF24",
              opacity = 1,
              bringToFront = TRUE
            )
          )
      }
    }

    map %>%
      addCircles(lng = center_lng, lat = center_lat, radius = 1000, color = "#0000FF", fillOpacity = 0.2, group = "Model Layer") %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"),
        overlayGroups = c("Preserve Boundary", "Exclosures", "Transects", "Trails", "Model Layer"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Preserve Boundary") %>%
      hideGroup("Exclosures") %>%
      hideGroup("Model Layer")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
