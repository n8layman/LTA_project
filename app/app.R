# Shiny Application (app.R) to Display GeoJSON Polygons on a Leaflet Map
#
# Updated for new tick data schema and helpers.R

# Load core packages
library(dplyr)   # For %>% pipe operator and data manipulation
library(leaflet) # For mapping - too many functions to use explicit namespacing
library(leaflet.extras) # For easyButton functionality
library(munsell) # Necessary to specify as dependency to get ggplot2 to work with shinylive

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
  dplyr::arrange(SiteName, Transect, Segment)

# Mohonk exclosures data
mohonk_exclosures <- read.csv("mohonk_exclosures.csv", stringsAsFactors = FALSE)

# Date range (optional - will be NA if no dates in data)
date_range <- get_date_range(tick_data)
min_date <- date_range[1]
max_date <- date_range[2]

# --- User Interface (UI) ---
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
    ),
    shiny::includeCSS("styles.css")
  ),
  shiny::titlePanel(
    shiny::div(shiny::h1("Regional Tick Surveillance Data Explorer", class = "text-2xl font-bold text-gray-800")),
    windowTitle = "Tick Study App"
  ),
  shiny::tabsetPanel(
    id = "site_tabs",
    # Mohonk Tab
    shiny::tabPanel(title = "Mohonk Preserve", value = "Mohonk",
      shiny::h3("Mohonk Preserve, NY", class = "text-xl font-semibold mb-4 text-gray-700"),
      shiny::fluidRow(
        shiny::column(8, leafletOutput("mohonk_map", height = "500px")),
        shiny::column(4, shiny::div(class = "site-summary-panel",
                     shiny::h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"),
                     shiny::uiOutput("site_summary_ui"), shiny::br(),
                     shiny::plotOutput("mohonk_species_plot", height = "250px")))
      ),
      shiny::div(class = "data-table-styled-container",
          shiny::h4("Mohonk Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"),
          DT::DTOutput("mohonk_data_table"))
    ),

    # Mianus Tab
    shiny::tabPanel(title = "Mianus River Gorge Preserve", value = "Mianus River Gorge",
      shiny::h3("Mianus River Gorge Preserve, CT/NY", class = "text-xl font-semibold mb-4 text-gray-700"),
      shiny::fluidRow(
        shiny::column(8, leafletOutput("mianus_map", height = "500px")),
        shiny::column(4, shiny::div(class = "site-summary-panel",
                     shiny::h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"),
                     shiny::uiOutput("site_summary_ui_mianus"), shiny::br(),
                     shiny::plotOutput("mianus_species_plot", height = "250px")))
      ),
      shiny::div(class = "data-table-styled-container",
          shiny::h4("Mianus River Gorge Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"),
          DT::DTOutput("mianus_data_table"))
    ),

    # Project Summary Tab
    shiny::tabPanel(title = "Project Summary", value = "Summary",
      shiny::div(class = "container", style = "max-width: 900px; margin: 20px auto; padding: 20px;",
        shiny::h3("Regional Tick Surveillance Project", class = "text-xl font-semibold mb-4 text-gray-700"),
        shiny::p("This interactive application provides visualization and analysis tools for tick surveillance data collected across multiple field sites in the northeastern United States. Our research focuses on understanding tick population dynamics, species distribution, and their relationship with habitat management practices."),
        shiny::h4("Study Sites", class = "text-lg font-semibold mt-4 mb-2 text-gray-700"),
        shiny::p("Data has been collected from two primary locations:"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("Mohonk Preserve, NY:"), " A 8,000-acre nature preserve in the Shawangunk Ridge featuring diverse forest habitats and experimental deer exclosures."),
          shiny::tags$li(shiny::strong("Mianus River Gorge Preserve, CT/NY:"), " A 1,100-acre old-growth forest preserve with paired experimental and control transects along trail systems.")
        ),
        shiny::h4("Methodology", class = "text-lg font-semibold mt-4 mb-2 text-gray-700"),
        shiny::p("Tick surveillance is conducted using standardized drag sampling techniques along established transects. Collected specimens are identified to species and life stage (adult, nymph, or larva). Data collection follows consistent protocols across all sites to enable comparative analysis of tick abundance and distribution patterns."),
        shiny::h4("Key Species", class = "text-lg font-semibold mt-4 mb-2 text-gray-700"),
        shiny::p("Our surveillance efforts focus on three primary tick species of medical importance:"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("Blacklegged Tick"), " (", shiny::em("Ixodes scapularis"), "): Primary vector of Lyme disease"),
          shiny::tags$li(shiny::strong("Lone Star Tick"), " (", shiny::em("Amblyomma americanum"), "): Associated with alpha-gal syndrome and ehrlichiosis"),
          shiny::tags$li(shiny::strong("Dog Tick"), " (", shiny::em("Dermacentor variabilis"), "): Vector of Rocky Mountain spotted fever")
        ),
        shiny::h4("Using This Application", class = "text-lg font-semibold mt-4 mb-2 text-gray-700"),
        shiny::p("Navigate between site tabs to explore interactive maps, data tables, and visualizations. Maps display tick abundance by location with color-coded markers. Click on table rows to highlight specific locations on the map. Download buttons allow you to export filtered data for further analysis."),
        shiny::h4("Contact", class = "text-lg font-semibold mt-4 mb-2 text-gray-700"),
        shiny::p(shiny::HTML('For more information about this project, please contact: <a href="mailto:nichargregory1@gmail.com" style="color: #3B82F6;">nichargregory1@gmail.com</a>'))
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {

  filtered_data <- shiny::reactive({ tick_data })

  # --- Site Summary UI ---
  output$site_summary_ui <- shiny::renderUI({
    generate_site_summary(filtered_data(), "Mohonk Preserve", min_date, max_date)
  })

  output$site_summary_ui_mianus <- shiny::renderUI({
    generate_site_summary(filtered_data(), "Mianus River Gorge", min_date, max_date)
  })

  # --- Species Plots ---
  output$mohonk_species_plot <- shiny::renderPlot({
    generate_species_plot(filtered_data(), "Mohonk Preserve")
  })

  output$mianus_species_plot <- shiny::renderPlot({
    generate_species_plot(filtered_data(), "Mianus River Gorge")
  })

    # --- Data Tables ---
  output$mohonk_data_table <- DT::renderDT({
    DT::datatable(
      filtered_data() %>%
        dplyr::filter(SiteName == "Mohonk Preserve"),
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

  output$mianus_data_table <- DT::renderDT({
    DT::datatable(
      filtered_data() %>%
        dplyr::filter(SiteName == "Mianus River Gorge"),
        extensions = 'Buttons',   # Enable Buttons extension
        options = list(
          dom = 'Btip',            # B = buttons, t = table, i = info, p = pagination
          buttons = list(
            list(
              extend = "excel",
              text = "Download",    # Button label
              filename = "LTA_tick_data"
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
  shiny::observeEvent(input$mohonk_data_table_rows_selected, {
    selected_row <- input$mohonk_data_table_rows_selected
    if (!is.null(selected_row)) {
      mohonk_data <- filtered_data() %>% dplyr::filter(SiteName == "Mohonk Preserve")
      selected_site_location <- mohonk_data[selected_row, "Site_location"]

      # Find the exclosure matching this site location using Site_Match column
      matching_exclosure <- mohonk_exclosures %>%
        dplyr::filter(Site_Match == selected_site_location)

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
  shiny::observeEvent(input$mianus_data_table_rows_selected, {
    selected_row <- input$mianus_data_table_rows_selected
    if (!is.null(selected_row)) {
      mianus_data <- filtered_data() %>% dplyr::filter(SiteName == "Mianus River Gorge")
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
      dplyr::filter(SiteName == "Mohonk Preserve") %>%
      dplyr::group_by(Site_location) %>%
      dplyr::summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nymphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      )

    # Create labels with detailed transect breakdown for each exclosure
    # Use Site_Match column to join with data (some locations may not have data yet)
    exclosures_with_labels <- mohonk_exclosures %>%
      dplyr::left_join(mohonk_summary, by = c("Site_Match" = "Site_location")) %>%
      dplyr::mutate(
        Adult = tidyr::replace_na(Adult, 0),
        Nymph = tidyr::replace_na(Nymph, 0),
        Total = tidyr::replace_na(Total, 0),
        marker_color = dplyr::if_else(Total == 0, "#9CA3AF", "#EF4444"),  # Gray or Red
        marker_fill_opacity = 0.8
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
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
      dplyr::ungroup()

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
        label = ~lapply(label_text, shiny::HTML),
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

    map %>%
      hideGroup("Model Layer") %>%
      addEasyButton(
        easyButton(
          icon = "fa-location-crosshairs",
          title = "Reset View",
          position = "bottomleft",
          onClick = JS(sprintf("function(btn, map){ map.setView([%f, %f], 12.5); }", lat, lng))
        )
      )
  })

  # --- Mianus Map ---
  output$mianus_map <- renderLeaflet({
    center_lat <- SITE_COORDS[["Mianus River Gorge"]][1]
    center_lng <- SITE_COORDS[["Mianus River Gorge"]][2]

    # Initialize map
    map <- init_leaflet_map(center_lng, center_lat, zoom = 17, max_zoom = 20)

    # Add layers in order from bottom to top (preserve -> exclosures -> model -> trails -> transects)
    # This ensures transects are always on top and hoverable
    map <- add_preserve_polygons(map, polygons_data)
    map <- add_exclosure_polygons(map, exclosures_data)

    # Add model layer
    map <- map %>%
      addCircles(lng = center_lng, lat = center_lat, radius = 1000, color = "#0000FF", fillOpacity = 0.2, group = "Model Layer", options = pathOptions(interactive = FALSE))

    # Add trails (simple gray lines, non-interactive)
    if (!is.null(trails_data)) {
      for (i in seq_along(trails_data$features)) {
        feature <- trails_data$features[[i]]
        coords <- feature$geometry$coordinates

        map <- map %>%
          addPolylines(
            lng = sapply(coords, function(x) x[[1]]),
            lat = sapply(coords, function(x) x[[2]]),
            color = "#4B5563",
            weight = 3,
            opacity = 0.7,
            group = "Trails",
            options = pathOptions(interactive = FALSE)
          )
      }
    }

    # Add transects LAST so they're always on top and hoverable
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
      }) %>% dplyr::bind_rows()

      tick_segment_totals <- get_segment_tick_totals(filtered_data())

      transects_joined_data <- transects_properties_tibble %>%
        dplyr::left_join(tick_segment_totals, by = "SegmCode") %>%
        dplyr::mutate(
          Total_Ticks = tidyr::replace_na(Total_Ticks, 0),
          Adult_Count = tidyr::replace_na(Adult_Count, 0),
          Nymph_Count = tidyr::replace_na(Nymph_Count, 0),
          line_color = dplyr::case_when(
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
        segment_data <- transects_joined_data %>% dplyr::filter(.original_index == i) %>% head(1)
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
        t_adult <- get_count(transect_summary %>% dplyr::filter(Transect_Code == transect_code), "Adult")
        t_nymph <- get_count(transect_summary %>% dplyr::filter(Transect_Code == transect_code), "Nymph")
        t_total <- t_adult + t_nymph

        l_adult <- get_count(line_summary %>% dplyr::filter(Line_Code == line_code), "Adult")
        l_nymph <- get_count(line_summary %>% dplyr::filter(Line_Code == line_code), "Nymph")
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
            layerId = segm_code,
            label = lapply(tooltip_content, shiny::HTML),
            highlightOptions = highlightOptions(
              weight = 9,
              color = "#FBBF24",
              opacity = 1,
              bringToFront = TRUE
            )
          )
      }
    }

    map %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"),
        overlayGroups = c("Preserve Boundary", "Exclosures", "Transects", "Trails", "Model Layer"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Preserve Boundary") %>%
      hideGroup("Exclosures") %>%
      hideGroup("Trails") %>%
      hideGroup("Model Layer") %>%
      addEasyButton(
        easyButton(
          icon = "fa-location-crosshairs",
          title = "Reset View",
          position = "bottomleft",
          onClick = JS(sprintf("function(btn, map){ map.setView([%f, %f], 17); }", center_lat, center_lng))
        )
      )
  })

  # --- Click handler for transect segments (Mianus map) ---
  shiny::observeEvent(input$mianus_map_shape_click, {
    click <- input$mianus_map_shape_click

    if (!is.null(click$id)) {
      segm_code <- click$id

      # Get data for this segment
      segment_data <- filtered_data() %>%
        dplyr::filter(SiteName == "Mianus River Gorge", SegmCode == segm_code)

      if (nrow(segment_data) > 0) {
        # Extract hierarchy info from SegmCode
        pair_num <- sub("MRG-(\\d+)-.*", "\\1", segm_code)
        treatm_type <- sub("MRG-\\d+-([A-Za-z]+)-.*", "\\1", segm_code)
        line_num <- sub("MRG-\\d+-[A-Za-z]+-(\\d+)-[A-Z]$", "\\1", segm_code)

        # Get summaries
        summaries <- summarize_mianus_hierarchical(filtered_data())
        transect_summary <- summaries$transect
        line_summary <- summaries$line

        transect_code <- paste(pair_num, treatm_type, sep = "-")
        line_code <- paste(pair_num, treatm_type, line_num, sep = "-")

        # Get counts for each level
        t_adult <- get_count(transect_summary %>% dplyr::filter(Transect_Code == transect_code), "Adult")
        t_nymph <- get_count(transect_summary %>% dplyr::filter(Transect_Code == transect_code), "Nymph")
        t_total <- t_adult + t_nymph

        l_adult <- get_count(line_summary %>% dplyr::filter(Line_Code == line_code), "Adult")
        l_nymph <- get_count(line_summary %>% dplyr::filter(Line_Code == line_code), "Nymph")
        l_total <- l_adult + l_nymph

        s_adult <- sum(segment_data$Adults, na.rm = TRUE)
        s_nymph <- sum(segment_data$Nymphs, na.rm = TRUE)
        s_total <- sum(segment_data$Total, na.rm = TRUE)

        # Create copyable text content
        modal_content <- shiny::tagList(
          shiny::h4(segm_code, style = "margin-top: 0; margin-bottom: 12px; font-size: 16px;"),
          shiny::tags$table(
            style = "width: 100%; border-collapse: collapse; font-family: sans-serif; font-size: 13px;",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Level", style = "text-align: left; padding: 4px 6px; border-bottom: 1px solid #ccc; font-weight: 600;"),
                shiny::tags$th("Adults", style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #ccc; font-weight: 600;"),
                shiny::tags$th("Nymphs", style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #ccc; font-weight: 600;"),
                shiny::tags$th("Total", style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #ccc; font-weight: 600;")
              )
            ),
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(paste("Transect (Pair", pair_num, treatm_type, ")"), style = "padding: 4px 6px; border-bottom: 1px solid #eee;"),
                shiny::tags$td(t_adult, style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #eee;"),
                shiny::tags$td(t_nymph, style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #eee;"),
                shiny::tags$td(t_total, style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #eee;")
              ),
              shiny::tags$tr(
                shiny::tags$td(paste("Line", line_num), style = "padding: 4px 6px; border-bottom: 1px solid #eee; background-color: #f9fafb;"),
                shiny::tags$td(l_adult, style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #eee; background-color: #f9fafb;"),
                shiny::tags$td(l_nymph, style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #eee; background-color: #f9fafb;"),
                shiny::tags$td(l_total, style = "text-align: right; padding: 4px 6px; border-bottom: 1px solid #eee; background-color: #f9fafb;")
              ),
              shiny::tags$tr(
                shiny::tags$td("Segment", style = "padding: 4px 6px;"),
                shiny::tags$td(s_adult, style = "text-align: right; padding: 4px 6px;"),
                shiny::tags$td(s_nymph, style = "text-align: right; padding: 4px 6px;"),
                shiny::tags$td(s_total, style = "text-align: right; padding: 4px 6px;")
              )
            )
          )
        )

        shiny::showModal(
          shiny::modalDialog(
            modal_content,
            title = "Transect Segment Data",
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
      }
    }
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
