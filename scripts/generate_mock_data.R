# R Script: generate_tick_data.R
#
# Reads segments directly from 'transects_data.geojson', generates the mock
# tick surveillance data, and saves the final result as 'tick_data.csv'.
#
# No intermediate segment list file is saved.
#
# Dependencies: dplyr, lubridate, readr, jsonlite

library(dplyr)
library(lubridate)
library(readr)
library(jsonlite)

# --- Configuration ---
GEOJSON_FILE <- "app/mianus_transects.geojson" # Assumes this file is in the current directory
TICK_DATA_FILE <- "app/tick_data.csv"
set.seed(42) # For reproducible mock data

# ----------------------------------------------------------------------------------
# --- 1. Load GeoJSON and Extract Mianus Segments ---
# ----------------------------------------------------------------------------------

if (!file.exists(GEOJSON_FILE)) {
  stop(paste("Error: GeoJSON file not found at", GEOJSON_FILE,
             ". Please ensure it is in the working directory."))
}

# Read the GeoJSON file
transects_data_list <- read_json(GEOJSON_FILE)

if (!is.null(transects_data_list$features) && length(transects_data_list$features) > 0) {
  # Extract the 'SegmCode' property from every feature
  mianus_transect_segments <- sapply(
    transects_data_list$features,
    function(f) f$properties$SegmCode
  )
} else {
  stop("GeoJSON loaded but contains no features. Cannot proceed with data generation.")
}

message(paste("Successfully extracted", length(mianus_transect_segments), "Mianus segments from GeoJSON."))

# Define all possible Mohonk transects (kept here as they are static and separate)
mohonk_transects <- c("Cedar Drive Loop", "Canaan Rd", "Glory Hill", "Undercliff Rd")

# ----------------------------------------------------------------------------------
# --- 2. Mock Data Generation (using the GeoJSON-derived segments) ---
# ----------------------------------------------------------------------------------

start_date <- ymd("2023-04-01")
end_date <- ymd("2024-09-30")
dates <- seq(start_date, end_date, by = "day")

# Poisson lambda chosen so P(X=0) = 50%
lambda_poisson <- 0.69

# Combine all segments
all_transect_segments <- c(mohonk_transects, mianus_transect_segments)
mock_data_list <- list()

for (segment in all_transect_segments) {
  # Poisson distribution: 50% chance of 0 detections per segment
  n_detections <- rpois(1, lambda = lambda_poisson)

  if (n_detections > 0) {
    # Determine site based on transect name
    site <- if (segment %in% mohonk_transects) "Mohonk Preserve" else "Mianus River Gorge"

    # For Mianus, store both the full segment code and base transect
    if (site == "Mianus River Gorge") {
      # Use regex to remove the final segment letter (-A, -B, etc.)
      transect <- sub("-[A-Z]$", "", segment)
      segment_code <- segment  # Full segment (e.g., MRG-1-Unf-0-A)
    } else {
      transect <- segment
      segment_code <- segment
    }

    mock_data_list[[length(mock_data_list) + 1]] <- data.frame(
      Date = sample(dates, n_detections, replace = TRUE),
      Site = site,
      Transect = transect,
      Segment = segment_code,
      Species = sample(
        c("Ixodes scapularis (Blacklegged)", "Amblyomma americanum (Lone Star)", "Dermacentmaus variabilis (Dog Tick)"),
        n_detections, replace = TRUE, prob = c(0.5, 0.3, 0.2)
      ),
      Life_Stage = sample(c("Adult", "Nymph"), n_detections, replace = TRUE, prob = c(0.4, 0.6)),
      # Ensure count is at least 1
      Count = round(rlnorm(n_detections, meanlog = 1.5, sdlog = 0.8)) + 1
    )
  }
}

mock_data <- bind_rows(mock_data_list) %>%
  arrange(Date, Site, Transect)

# ----------------------------------------------------------------------------------
# --- 3. Save Final Data to CSV ---
# ----------------------------------------------------------------------------------
write_csv(mock_data, TICK_DATA_FILE)
message(paste("Generated final mock data file:", TICK_DATA_FILE))

message("\n✅ Script complete. The 'tick_data.csv' file is ready for your Shiny app.")
