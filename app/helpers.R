# ---- helpers.R ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(htmltools)
library(jsonlite)
library(glue)

# Load HTML templates for tooltips (hover labels)
TICK_COUNT_TOOLTIP_TEMPLATE <- paste(readLines("tick_count_tooltip.html"), collapse = "\n")
TABLE_ROW_TEMPLATE <- paste(readLines("table_row.html"), collapse = "\n")

# Helper function to create table rows
create_table_row <- function(level, adult, nymph, total, bgcolor = "") {
  glue(TABLE_ROW_TEMPLATE,
    bgcolor = if (bgcolor != "") glue(' style="background-color: {bgcolor};"') else "",
    level = level,
    adult = adult,
    nymph = nymph,
    total = total
  )
}

# Create tick count tooltip with variable number of rows
create_tick_tooltip <- function(title, ...) {
  rows_list <- list(...)
  rows <- paste(rows_list, collapse = "\n")
  glue(TICK_COUNT_TOOLTIP_TEMPLATE,
    title = title,
    rows = rows
  )
}

# 1. Load tick data safely
load_tick_data <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("Tick data file not found at:", file_path))
    return(data.frame(
      Site = character(),
      Transect = character(),
      Species = character(),
      Life_Stage = character(),
      Count = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  tick_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  if ("Date" %in% names(tick_data)) {
    tick_data$Date <- lubridate::ymd(tick_data$Date)
  }
  
  return(tick_data)
}

# Load and parse GeoJSON data safely
load_geojson <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("GeoJSON file not found at:", file_path))
    return(NULL)
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

  return(data)
}

# Helper function to safely get count from summary data
get_count <- function(df) {
  result <- df %>% pull(Total)
  if (length(result) > 0) sum(result) else 0
}

# Summarize tick data by transect and life stage
summarize_by_transect <- function(data, site_name) {
  data %>%
    filter(Site == site_name) %>%
    group_by(Transect, Life_Stage) %>%
    summarize(Total = sum(Count), .groups = "drop")
}

# Create hierarchical data structure for Mianus transects
create_mianus_hierarchy <- function(data) {
  data %>%
    filter(Site == "Mianus River Gorge") %>%
    mutate(
      # Extract transect number (e.g., "MRG-1" from "MRG-1-Fen-4")
      Transect_Num = sub("^(MRG-\\d+).*", "\\1", Transect),
      # Line is the Transect column (e.g., "MRG-1-Fen-4")
      Line = Transect
    )
}

# Summarize hierarchical counts for Mianus
summarize_mianus_hierarchical <- function(data) {
  mianus_hierarchy <- create_mianus_hierarchy(data)

  list(
    transect = mianus_hierarchy %>%
      group_by(Transect_Num, Life_Stage) %>%
      summarize(Total = sum(Count), .groups = "drop"),
    line = mianus_hierarchy %>%
      group_by(Line, Life_Stage) %>%
      summarize(Total = sum(Count), .groups = "drop"),
    segment = data %>%
      filter(Site == "Mianus River Gorge") %>%
      group_by(Segment, Life_Stage) %>%
      summarize(Total = sum(Count), .groups = "drop")
  )
}

# 2. Get date range safely
get_date_range <- function(df) {
  if ("Date" %in% names(df) && nrow(df) > 0) {
    c(min(df$Date, na.rm = TRUE), max(df$Date, na.rm = TRUE))
  } else {
    c(NA, NA)
  }
}

# 3. Create a site summary UI block
generate_site_summary <- function(data, site_name, min_date, max_date) {
  site_data <- data %>% filter(Site == site_name)
  total_ticks <- sum(site_data$Count, na.rm = TRUE)
  unique_species <- length(unique(site_data$Species))
  
  tagList(
    p(HTML(paste0("<strong>Site:</strong> ", site_name)), class = "text-gray-600"),
    p(HTML(paste0("<strong>Total Ticks Counted:</strong> ", format(total_ticks, big.mark = ","))), class = "text-gray-600"),
    p(HTML(paste0("<strong>Unique Species Found:</strong> ", unique_species)), class = "text-gray-600"),
    p(paste0(
      "Data collected between ",
      ifelse(!is.na(min_date), min_date, "N/A"),
      " and ",
      ifelse(!is.na(max_date), max_date, "N/A"),
      "."
    ), class = "text-sm text-gray-500 mt-2")
  )
}

# 4. Generate stacked bar plot of tick species by life stage
generate_species_plot <- function(data, site_name) {
  plot_data <- data %>%
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

  ggplot(plot_data, aes(x = Species_Abbrev, y = Total, fill = Life_Stage)) +
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

# Initialize base leaflet map with provider tiles
init_leaflet_map <- function(lng, lat, zoom = 17, max_zoom = 20) {
  leaflet(options = leafletOptions(maxZoom = max_zoom)) %>%
    addProviderTiles(providers$OpenTopoMap, group = "Topographic",
                     options = providerTileOptions(maxZoom = max_zoom)) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite",
                     options = providerTileOptions(maxZoom = max_zoom)) %>%
    addTiles(group = "Default Map", options = tileOptions(maxZoom = max_zoom)) %>%
    setView(lng = lng, lat = lat, zoom = zoom)
}

# Add preserve boundary polygons to leaflet map
add_preserve_polygons <- function(map, polygons_data) {
  if (is.null(polygons_data)) return(map)

  for (i in seq_along(polygons_data$features)) {
    feature <- polygons_data$features[[i]]
    coords <- feature$geometry$coordinates

    # Get properties
    name <- feature$properties$Name
    acres <- feature$properties$Acres
    nhp_name <- feature$properties$NHP_Name

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
        label = ifelse(!is.null(name) && name != "", name, "Parcel"),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#34D399",
          fillOpacity = 0.4,
          bringToFront = FALSE
        )
      )
  }

  return(map)
}

# Add exclosure polygons to leaflet map
add_exclosure_polygons <- function(map, exclosures_data) {
  if (is.null(exclosures_data)) return(map)

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

  return(map)
}

# Add trail polylines to leaflet map
add_trail_polylines <- function(map, trails_data) {
  if (is.null(trails_data)) return(map)

  for (i in seq_along(trails_data$features)) {
    feature <- trails_data$features[[i]]
    coords <- feature$geometry$coordinates

    # Get properties
    track_name <- feature$properties$TRACK
    color_name <- feature$properties$Color

    # Add polyline in dark grey
    map <- map %>%
      addPolylines(
        lng = sapply(coords, function(x) x[[1]]),
        lat = sapply(coords, function(x) x[[2]]),
        color = "#4B5563",
        weight = 2,
        opacity = 0.7,
        group = "Trails",
        label = ifelse(!is.null(track_name), track_name, "Trail"),
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#1F2937",
          opacity = 1,
          bringToFront = TRUE
        )
      )
  }

  return(map)
}
