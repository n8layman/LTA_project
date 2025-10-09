# ---- helpers.R (Updated for New Schema) ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(htmltools)
library(jsonlite)
library(glue)

# --- Templates ---
# Make sure TICK_COUNT_TOOLTIP_TEMPLATE and TABLE_ROW_TEMPLATE are defined in app.R
# Load HTML templates for tooltips (hover labels)
TICK_COUNT_TOOLTIP_TEMPLATE <- paste(readLines("tick_count_tooltip.html"), collapse = "\n")
TABLE_ROW_TEMPLATE <- paste(readLines("table_row.html"), collapse = "\n")

# 1. Create table row for tooltip
create_table_row <- function(level, adults, nimphs, total, bgcolor = "") {
  glue(TABLE_ROW_TEMPLATE,
       bgcolor = if (bgcolor != "") glue(' style="background-color: {bgcolor};"') else "",
       level = level,
       adults = adults,
       nimphs = nimphs,
       total = total
  )
}

# 2. Create tooltip from multiple rows
create_tick_tooltip <- function(title, ...) {
  rows_list <- list(...)
  rows <- paste(rows_list, collapse = "\n")
  glue(TICK_COUNT_TOOLTIP_TEMPLATE,
       title = title,
       rows = rows
  )
}

# 3. Load tick data safely
load_tick_data <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("Tick data file not found at:", file_path))
    return(data.frame(
      Date = as.Date(character()),
      SiteName = character(),
      SegmCode = character(),
      Pair = character(),
      Treatm = character(),
      Transect = character(),
      Segment = character(),
      Adults = numeric(),
      Nimphs = numeric(),
      Total = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  tick_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Convert Date
  tick_data$Date <- lubridate::ymd(tick_data$Date)
  
  # Trim character columns
  tick_data$SegmCode <- trimws(tick_data$SegmCode)
  tick_data$SiteName <- trimws(tick_data$SiteName)
  
  # Ensure numeric columns
  tick_data$Adults <- as.numeric(tick_data$Adults)
  tick_data$Nimphs <- as.numeric(tick_data$Nimphs)
  tick_data$Total <- as.numeric(tick_data$Total)
  
  return(tick_data)
}

# 4. Load GeoJSON safely
load_geojson <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("GeoJSON file not found at:", file_path))
    return(NULL)
  }
  
  geojson_string <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
  
  data <- tryCatch({
    jsonlite::fromJSON(geojson_string, simplifyVector = FALSE)
  }, error = function(e) {
    warning(paste("Error parsing GeoJSON:", e$message))
    return(NULL)
  })
  
  return(data)
}

# 5. Safely get count from summary
get_count <- function(df, stage = "Total") {
  if (nrow(df) == 0) return(0)
  stage_col <- switch(stage,
                      "Adult" = "Adult",
                      "Nymph" = "Nymph",
                      "Total" = "Total",
                      "Total")
  sum(df[[stage_col]], na.rm = TRUE)
}

# 6. Summarize by transect
summarize_by_transect <- function(data, SiteName) {
  data %>%
    filter(SiteName == SiteName) %>%
    group_by(Transect) %>%
    summarize(
      adults = sum(Adults, na.rm = TRUE),
      nymphs = sum(Nimphs, na.rm = TRUE),
      total = sum(Total, na.rm = TRUE),
      .groups = "drop"
    )
}

# 7. Segment totals for Mianus
get_segment_tick_totals <- function(data) {
  data %>%
    filter(SiteName == "Mianus River Gorge") %>%
    group_by(SegmCode) %>%
    summarize(
      Adult_Count = sum(Adults, na.rm = TRUE),
      Nymph_Count = sum(Nimphs, na.rm = TRUE),
      Total_Ticks = sum(Total, na.rm = TRUE),
      .groups = "drop"
    )
}

# 8. Hierarchical summaries for Mianus
summarize_mianus_hierarchical <- function(data) {
  mianus_data <- data %>% filter(SiteName == "Mianus River Gorge")
  
  list(
    transect = mianus_data %>%
      group_by(Transect) %>%
      summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nimphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      ),
    
    line = mianus_data %>%
      group_by(Transect, Treatm, Segment) %>%
      summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nimphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Line_Code = paste0("MRG-", Transect, "-", Treatm, "-", Segment)),
    
    segment = mianus_data %>%
      group_by(SegmCode) %>%
      summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nimphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      )
  )
}

# 9. Get date range safely
get_date_range <- function(df) {
  if ("Date" %in% names(df) && nrow(df) > 0) {
    c(min(df$Date, na.rm = TRUE), max(df$Date, na.rm = TRUE))
  } else {
    c(NA, NA)
  }
}

# 10. Site summary UI block
generate_site_summary <- function(data, SiteName, min_date, max_date) {
  site_data <- data %>% filter(SiteName == SiteName)
  total_ticks <- sum(site_data$Total, na.rm = TRUE)
  unique_species <- length(unique(site_data$Species))
  
  tagList(
    p(HTML(paste0("<strong>Site:</strong> ", SiteName)), class = "text-gray-600"),
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

# 11. Generate stacked bar plot of tick species by life stage
generate_species_plot <- function(data, SiteName) {
  plot_data <- data %>%
    filter(SiteName == SiteName) %>%
    select(-Total) %>%   # drop original Total to avoid duplicate columns
    pivot_longer(
      cols = c(Adults, Nimphs),
      names_to = "Life_Stage",
      values_to = "Count"
    ) %>%
    group_by(Species, Life_Stage) %>%
    summarize(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Species_Abbrev = case_when(
        Species == "Ixodes scapularis (Blacklegged)" ~ "I. scap",
        Species == "Amblyomma americanum (Lone Star)" ~ "A. amer",
        Species == "Dermacentor variabilis (Dog Tick)" ~ "D. var",
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

# --- Leaflet helpers (unchanged) ---
init_leaflet_map <- function(lng, lat, zoom = 17, max_zoom = 20) {
  leaflet(options = leafletOptions(maxZoom = max_zoom)) %>%
    addProviderTiles(providers$OpenTopoMap, group = "Topographic",
                     options = providerTileOptions(maxZoom = max_zoom)) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite",
                     options = providerTileOptions(maxZoom = max_zoom)) %>%
    addTiles(group = "Default Map", options = tileOptions(maxZoom = max_zoom)) %>%
    setView(lng = lng, lat = lat, zoom = zoom)
}

add_preserve_polygons <- function(map, polygons_data) {
  if (is.null(polygons_data)) return(map)
  for (i in seq_along(polygons_data$features)) {
    feature <- polygons_data$features[[i]]
    coords <- feature$geometry$coordinates
    name <- feature$properties$Name
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
          weight = 3, color = "#34D399", fillOpacity = 0.4, bringToFront = FALSE
        )
      )
  }
  return(map)
}

add_exclosure_polygons <- function(map, exclosures_data) {
  if (is.null(exclosures_data)) return(map)
  for (i in seq_along(exclosures_data$features)) {
    feature <- exclosures_data$features[[i]]
    coords <- feature$geometry$coordinates
    done <- feature$properties$Done
    fill_color <- if (!is.null(done) && done == 1) "#9333EA" else "#C084FC"
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

add_trail_polylines <- function(map, trails_data) {
  if (is.null(trails_data)) return(map)
  for (i in seq_along(trails_data$features)) {
    feature <- trails_data$features[[i]]
    coords <- feature$geometry$coordinates
    map <- map %>%
      addPolylines(
        lng = sapply(coords, function(x) x[[1]]),
        lat = sapply(coords, function(x) x[[2]]),
        color = "#4B5563",
        weight = 2,
        opacity = 0.7,
        group = "Trails"
      )
  }
  return(map)
}
