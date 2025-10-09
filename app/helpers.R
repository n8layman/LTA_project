# ---- helpers.R ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(htmltools)

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
    summarize(Total = sum(Count), .groups = "drop")
  
  ggplot(plot_data, aes(x = Species, y = Total, fill = Life_Stage)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    scale_fill_manual(values = c("Adult" = "#3B82F6", "Nymph" = "#10B981")) +
    labs(x = NULL, y = "Count", fill = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 11, angle = 30, hjust = 1),
      axis.text.y = element_text(size = 11),
      legend.position = "bottom"
    )
}

# 5. Generate simplified Mianus transect map
generate_mianus_map <- function(transects_data, polygons_data, exclosures_data, trails_data, tick_data, site_coords) {
  mianus_data <- tick_data %>%
    filter(Site == "Mianus River Gorge") %>%
    group_by(Transect, Life_Stage) %>%
    summarize(Total = sum(Count), .groups = "drop")
  
  center_lat <- site_coords[1]
  center_lng <- site_coords[2]
  
  map <- leaflet(options = leafletOptions(maxZoom = 20)) %>%
    addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addTiles(group = "Default Map") %>%
    setView(lng = center_lng, lat = center_lat, zoom = 17)
  
  if (!is.null(transects_data)) {
    for (i in seq_along(transects_data$features)) {
      feature <- transects_data$features[[i]]
      coords <- feature$geometry$coordinates
      transect_id <- feature$properties$Transect
      
      t_adult <- mianus_data %>% filter(Transect == transect_id, Life_Stage == "Adult") %>% pull(Total)
      t_nymph <- mianus_data %>% filter(Transect == transect_id, Life_Stage == "Nymph") %>% pull(Total)
      
      t_adult <- ifelse(length(t_adult) > 0, t_adult, 0)
      t_nymph <- ifelse(length(t_nymph) > 0, t_nymph, 0)
      t_total <- t_adult + t_nymph
      
      line_color <- if (t_total == 0) "#9CA3AF" else "#7C3AED"
      
      tooltip_content <- sprintf(
        '<div style="font-family: sans-serif; font-size: 11px;">
         <strong>%s</strong><br>
         <table style="margin-top: 6px; border-collapse: collapse;">
         <tr><td>Adult</td><td align="right">%d</td></tr>
         <tr><td>Nymph</td><td align="right">%d</td></tr>
         <tr><td><b>Total</b></td><td align="right"><b>%d</b></td></tr>
         </table></div>',
        transect_id, t_adult, t_nymph, t_total
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
          labelOptions = labelOptions(permanent = FALSE, textOnly = FALSE)
        )
    }
  }
  
  map %>%
    addCircles(lng = center_lng, lat = center_lat, radius = 1000, color = "#0000FF", fillOpacity = 0.2, group = "Model Layer") %>%
    addLayersControl(
      baseGroups = c("Default Map", "Topographic", "Satellite"),
      overlayGroups = c("Transects", "Model Layer"),
      options = layersControlOptions(collapsed = TRUE)
    )
}
