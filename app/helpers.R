# ---- helpers.R (Updated for New Schema) ----
# Using explicit namespaces to minimize dependencies in shinylive export

# Load core packages
library(leaflet) # For mapping - too many functions to use explicit namespacing

# Load the pipe operator from dplyr (needed for %>% syntax)
library(dplyr)

# --- Templates ---
# Make sure TICK_COUNT_TOOLTIP_TEMPLATE and TABLE_ROW_TEMPLATE are defined in app.R
# Load HTML templates for tooltips (hover labels)
TICK_COUNT_TOOLTIP_TEMPLATE <- paste(readLines("tick_count_tooltip.html"), collapse = "\n")
TABLE_ROW_TEMPLATE <- paste(readLines("table_row.html"), collapse = "\n")

# 1. Create table row for tooltip
create_table_row <- function(level, adults, nymphs, total, bgcolor = "") {
  glue::glue(TABLE_ROW_TEMPLATE,
       bgcolor = if (bgcolor != "") glue::glue(' style="background-color: {bgcolor};"') else "",
       level = level,
       adults = adults,
       nymphs = nymphs,
       total = total
  )
}

# 2. Create tooltip from multiple rows
create_tick_tooltip <- function(title, ...) {
  rows_list <- list(...)
  rows <- paste(rows_list, collapse = "\n")
  glue::glue(TICK_COUNT_TOOLTIP_TEMPLATE,
       title = title,
       rows = rows
  )
}

# 3. Load tick data safely
load_tick_data <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("Tick data file not found at:", file_path))
    return(data.frame(
      SiteName = character(),
      SegmCode = character(),
      Pair = character(),
      Treatm = character(),
      Transect = character(),
      Segment = character(),
      Species = character(),
      Adults = numeric(),
      Nymphs = numeric(),
      Larva = numeric(),
      Total = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  tick_data <- read.csv(file_path, stringsAsFactors = FALSE)

  # Trim character columns
  tick_data$SegmCode <- trimws(tick_data$SegmCode)
  tick_data$SiteName <- trimws(tick_data$SiteName)
  if ("Species" %in% names(tick_data)) {
    tick_data$Species <- trimws(tick_data$Species)
  }

  # Ensure numeric columns
  tick_data$Adults <- as.numeric(tick_data$Adults)
  tick_data$Nymphs <- as.numeric(tick_data$Nymphs)
  tick_data$Total <- as.numeric(tick_data$Total)
  if ("Larva" %in% names(tick_data)) {
    tick_data$Larva <- as.numeric(tick_data$Larva)
  }

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
summarize_by_transect <- function(data, site_name) {
  data %>%
    dplyr::filter(SiteName == site_name) %>%
    dplyr::group_by(Transect) %>%
    dplyr::summarize(
      adults = sum(Adults, na.rm = TRUE),
      nymphs = sum(Nymphs, na.rm = TRUE),
      total = sum(Total, na.rm = TRUE),
      .groups = "drop"
    )
}

# 7. Segment totals for Mianus
get_segment_tick_totals <- function(data) {
  data %>%
    dplyr::filter(SiteName == "Mianus River Gorge") %>%
    dplyr::group_by(SegmCode) %>%
    dplyr::summarize(
      Adult_Count = sum(Adults, na.rm = TRUE),
      Nymph_Count = sum(Nymphs, na.rm = TRUE),
      Total_Ticks = sum(Total, na.rm = TRUE),
      .groups = "drop"
    )
}

# 8. Hierarchical summaries for Mianus
summarize_mianus_hierarchical <- function(data) {
  mianus_data <- data %>% dplyr::filter(SiteName == "Mianus River Gorge")

  list(
    # Transect = Pair + Treatment (e.g., "1-Unf", "1-Fen")
    # Extract pair from SegmCode since Pair column contains Site_location
    transect = mianus_data %>%
      dplyr::mutate(
        PairNum = sub("MRG-(\\d+)-.*", "\\1", SegmCode),
        Transect_Code = paste(PairNum, Treatm, sep = "-")
      ) %>%
      dplyr::group_by(Transect_Code) %>%
      dplyr::summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nymphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      ),

    # Line = the number within a transect (0, 1, 2, 3, 4)
    # Line_Code = Pair-Treatment-Number (e.g., "1-Unf-2")
    line = mianus_data %>%
      dplyr::mutate(
        PairNum = sub("MRG-(\\d+)-.*", "\\1", SegmCode),
        Line_Code = paste(PairNum, Treatm, Transect, sep = "-")
      ) %>%
      dplyr::group_by(Line_Code) %>%
      dplyr::summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nymphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      ),

    # Segment = individual segment (A, B, C, etc.)
    segment = mianus_data %>%
      dplyr::group_by(SegmCode) %>%
      dplyr::summarize(
        Adult = sum(Adults, na.rm = TRUE),
        Nymph = sum(Nymphs, na.rm = TRUE),
        Total = sum(Total, na.rm = TRUE),
        .groups = "drop"
      )
  )
}

# 9. Get date range safely (returns NA when no dates available)
get_date_range <- function(df) {
  if ("Date" %in% names(df) && nrow(df) > 0) {
    c(min(df$Date, na.rm = TRUE), max(df$Date, na.rm = TRUE))
  } else {
    c(NA, NA)
  }
}

# 10. Site summary UI block
generate_site_summary <- function(data, site_name, min_date = NA, max_date = NA) {
  site_data <- data %>% dplyr::filter(SiteName == site_name)
  total_ticks <- sum(site_data$Total, na.rm = TRUE)
  unique_species <- length(unique(site_data$Species))

  # Site descriptions with links
  site_description <- if (site_name == "Mohonk Preserve") {
    'An 8,000-acre preserve in the Shawangunk Ridge featuring mixed hardwood-conifer forests dominated by oak, hemlock, and pitch pine. The site includes experimental deer exclosures to study wildlife impacts on forest regeneration and tick populations. <a href="https://www.mohonkpreserve.org/" target="_blank" style="color: #3B82F6;">Learn more</a>'
  } else if (site_name == "Mianus River Gorge") {
    'A 1,100-acre preserve protecting old-growth hemlock-northern hardwood forest along steep ravines. The diverse canopy includes hemlock, oak, beech, and tulip trees, providing habitat for white-tailed deer, black bear, and numerous bird species. <a href="https://www.mianus.org/" target="_blank" style="color: #3B82F6;">Learn more</a>'
  } else {
    site_name
  }

  summary_items <- list(
    shiny::p(shiny::HTML(paste0("<strong>Site:</strong> ", site_description)), class = "text-gray-700 mb-3", style = "font-size: 0.95em;"),
    shiny::p(shiny::HTML(paste0("<strong>Total Ticks Counted:</strong> ", format(total_ticks, big.mark = ","))), class = "text-gray-600"),
    shiny::p(shiny::HTML(paste0("<strong>Unique Species Found:</strong> ", unique_species)), class = "text-gray-600")
  )

  # Only add date range if dates are available
  if (!is.na(min_date) && !is.na(max_date)) {
    summary_items <- c(summary_items, list(
      shiny::p(paste0(
        "Data collected between ",
        min_date,
        " and ",
        max_date,
        "."
      ), class = "text-sm text-gray-500 mt-2")
    ))
  }

  do.call(shiny::tagList, summary_items)
}

# 11. Generate stacked bar plot of tick species by life stage
generate_species_plot <- function(data, site_name) {
  plot_data <- data %>%
    dplyr::filter(SiteName == site_name) %>%
    dplyr::rename(CommonName = Common.Name) %>%  # R converts "Common Name" to "Common.Name"
    dplyr::select(-Total) %>%   # drop original Total to avoid duplicate columns
    tidyr::pivot_longer(
      cols = c(Adults, Nymphs),
      names_to = "Life_Stage",
      values_to = "Count"
    ) %>%
    dplyr::group_by(CommonName, Life_Stage) %>%
    dplyr::summarize(Total = sum(Count, na.rm = TRUE), .groups = "drop")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = CommonName, y = Total, fill = Life_Stage)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.6) +
    ggplot2::scale_fill_manual(
      values = c("Adults" = "#3B82F6", "Nymphs" = "#10B981"),
      labels = c("Adults", "Nymphs")
    ) +
    ggplot2::labs(x = NULL, y = "Count", fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 11, angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title.y = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
}

# 12. Create detailed tooltip for Mohonk locations showing transect breakdown
create_mohonk_location_tooltip <- function(data, site_location, exclosure_name) {
  # Get transect-level summary for this location
  transect_data <- data %>%
    dplyr::filter(SiteName == "Mohonk Preserve", Site_location == site_location) %>%
    dplyr::group_by(Transect) %>%
    dplyr::summarize(
      Adults = sum(Adults, na.rm = TRUE),
      Nymphs = sum(Nymphs, na.rm = TRUE),
      Total = sum(Total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Transect)

  # Calculate totals
  total_adults <- sum(transect_data$Adults, na.rm = TRUE)
  total_nymphs <- sum(transect_data$Nymphs, na.rm = TRUE)
  total_ticks <- sum(transect_data$Total, na.rm = TRUE)

  # Build rows for each transect
  transect_rows <- lapply(seq_len(nrow(transect_data)), function(i) {
    t <- transect_data[i, ]
    create_table_row(
      paste("Transect", t$Transect),
      t$Adults,
      t$Nymphs,
      t$Total,
      bgcolor = if (i %% 2 == 0) "#f9fafb" else ""
    )
  })

  # Add total row
  all_rows <- c(
    transect_rows,
    list(create_table_row("Total", total_adults, total_nymphs, total_ticks, bgcolor = "#e5e7eb"))
  )

  rows <- paste(all_rows, collapse = "\n")

  glue::glue(TICK_COUNT_TOOLTIP_TEMPLATE,
       title = exclosure_name,
       rows = rows
  )
}

# 13. Summarize trailside tick data (all trails combined)
summarize_trailside_data <- function(data) {
  data %>%
    dplyr::filter(SiteName == "Mianus River Gorge", Treatm == "Trailside") %>%
    dplyr::summarize(
      Adult = sum(Adults, na.rm = TRUE),
      Nymph = sum(Nymphs, na.rm = TRUE),
      Total = sum(Total, na.rm = TRUE),
      .groups = "drop"
    )
}

# 14. Add Mohonk transect lines to map (for when polygon data is available)
add_mohonk_transects <- function(map, transects_data, tick_data) {
  if (is.null(transects_data)) return(map)

  # Get segment totals for coloring
  segment_totals <- tick_data %>%
    dplyr::filter(SiteName == "Mohonk Preserve") %>%
    dplyr::group_by(SegmCode) %>%
    dplyr::summarize(
      Adult_Count = sum(Adults, na.rm = TRUE),
      Nymph_Count = sum(Nymphs, na.rm = TRUE),
      Total_Ticks = sum(Total, na.rm = TRUE),
      .groups = "drop"
    )

  # Get location-level summary for tooltips
  location_summary <- tick_data %>%
    dplyr::filter(SiteName == "Mohonk Preserve") %>%
    dplyr::group_by(Site_location) %>%
    dplyr::summarize(
      Adult = sum(Adults, na.rm = TRUE),
      Nymph = sum(Nymphs, na.rm = TRUE),
      Total = sum(Total, na.rm = TRUE),
      .groups = "drop"
    )

  for (i in seq_along(transects_data$features)) {
    feature <- transects_data$features[[i]]
    coords <- feature$geometry$coordinates
    props <- feature$properties

    segm_code <- trimws(props$SegmCode)
    site_location <- props$Site_location  # Assuming this will be in the GeoJSON

    # Get segment data for coloring
    segment_data <- segment_totals %>% dplyr::filter(SegmCode == segm_code)
    if (nrow(segment_data) == 0) {
      segment_data <- data.frame(Adult_Count = 0, Nymph_Count = 0, Total_Ticks = 0)
    }

    line_color <- if (segment_data$Total_Ticks[1] == 0) "#9CA3AF" else "#EF4444"

    # Get location totals for tooltip
    location_data <- location_summary %>% dplyr::filter(Site_location == site_location)
    if (nrow(location_data) == 0) {
      location_data <- data.frame(Adult = 0, Nymph = 0, Total = 0)
    }

    tooltip_content <- create_tick_tooltip(
      title = segm_code,
      create_table_row("Location", location_data$Adult[1], location_data$Nymph[1], location_data$Total[1]),
      create_table_row("Segment", segment_data$Adult_Count[1], segment_data$Nymph_Count[1], segment_data$Total_Ticks[1])
    )

    map <- map %>%
      addPolylines(
        lng = sapply(coords, function(x) x[[1]]),
        lat = sapply(coords, function(x) x[[2]]),
        color = line_color,
        weight = 7,
        opacity = 0.9,
        group = "Mohonk Transects",
        label = lapply(tooltip_content, shiny::HTML),
        highlightOptions = highlightOptions(
          weight = 9,
          color = "#FBBF24",
          opacity = 1,
          bringToFront = TRUE
        )
      )
  }

  return(map)
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
