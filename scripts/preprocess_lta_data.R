# Preprocessing script to convert LTA_tick_data.csv to app-compatible format
library(dplyr)
library(tidyr)

# Read the raw LTA data with check.names=FALSE to preserve column names
lta_data <- read.csv("LTA_tick_data.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Clean column names manually
colnames(lta_data) <- gsub("#", "num", colnames(lta_data))
colnames(lta_data) <- gsub("/", "_", colnames(lta_data))
colnames(lta_data) <- gsub(" ", "_", colnames(lta_data))

# Convert all tick count columns to numeric (some may be character/factor)
lta_data <- lta_data %>%
  mutate(
    across(c(BL_Adults, BL_Nymphs, BL_Larva,
             Dg_Adults, Dg_Nymphs, Dg_Larva,
             ALH_Adults, ALH_Nymphs, ALH_Larva),
           ~as.numeric(as.character(.)))
  )

# Create SegmCode and other columns
lta_data <- lta_data %>%
  mutate(
    # Map treatment names to match GeoJSON
    Treatm_Code = case_when(
      Fence_Unf == "Fence" ~ "Fen",
      Fence_Unf == "Body Ticks" ~ "Body Ticks",
      Fence_Unf == "Trailside" ~ "Trailside",
      Fence_Unf == "Roadside" ~ "Roadside",
      TRUE ~ Fence_Unf
    ),
    # Build SegmCode in format: LT_Site-1-Treatm-Transect#-Segment (uppercase)
    # Always use "1" for the pair number in SegmCode to match GeoJSON
    # For MRG: MRG-1-Unf-0-A or MRG-1-Fen-0-A
    SegmCode = paste0(
      LT_Site, "-",
      "1", "-",  # GeoJSON uses 1 for all segments
      Treatm_Code, "-",
      `Transect_num`, "-",
      toupper(Segment)
    ),
    SiteName = case_when(
      LT_Site == "MRG" ~ "Mianus River Gorge",
      LT_Site == "MOH" ~ "Mohonk Preserve",
      TRUE ~ LT_Site
    ),
    Treatm = Treatm_Code,  # Use mapped treatment code (Fence -> Fen)
    # For MRG, Transect # is the transect
    Transect = as.character(`Transect_num`),
    # Keep Segment as uppercase
    Segment = toupper(as.character(Segment)),
    # Keep original Pair for reference
    Pair = as.character(Pair)
  )

# Reshape from wide to long format - create one row per species per segment
tick_data_long <- lta_data %>%
  # Select relevant columns and species data
  select(
    SiteName, SegmCode, Site_location, Pair, Treatm, Transect, Segment,
    # Blacklegged tick (Ixodes scapularis)
    BL_Adults, BL_Nymphs, BL_Larva,
    # Dog tick (Dermacentor variabilis)
    Dg_Adults, Dg_Nymphs, Dg_Larva,
    # Lone Star tick (Amblyomma americanum)
    ALH_Adults, ALH_Nymphs, ALH_Larva
  ) %>%
  # Pivot to long format
  pivot_longer(
    cols = c(BL_Adults:ALH_Larva),
    names_to = "SpeciesStage",
    values_to = "Count"
  ) %>%
  # Extract species and life stage
  mutate(
    Species_Code = sub("_.*", "", SpeciesStage),
    Life_Stage = sub(".*_", "", SpeciesStage),
    Species = case_when(
      Species_Code == "BL" ~ "Ixodes scapularis (Blacklegged)",
      Species_Code == "Dg" ~ "Dermacentor variabilis (Dog Tick)",
      Species_Code == "ALH" ~ "Amblyomma americanum (Lone Star)",
      TRUE ~ Species_Code
    )
  ) %>%
  # Pivot life stages back to columns
  select(-Species_Code, -SpeciesStage) %>%
  pivot_wider(
    names_from = Life_Stage,
    values_from = Count,
    values_fill = 0
  ) %>%
  # Calculate totals
  mutate(
    Total = Adults + Nymphs + Larva
  ) %>%
  # Select and order columns to match app expectations
  select(
    Species, SiteName, SegmCode, Site_location, Pair, Treatm, Transect, Segment,
    Adults, Nymphs, Larva, Total
  ) %>%
  # Remove rows where no ticks were found (optional - uncomment if desired)
  # filter(Total > 0) %>%
  # Sort for easier reading
  arrange(SiteName, Transect, Segment, Species)

# Write the transformed data
write.csv(tick_data_long, "tick_data_processed.csv", row.names = FALSE)

cat("Preprocessing complete!\n")
cat("Input rows:", nrow(lta_data), "\n")
cat("Output rows:", nrow(tick_data_long), "\n")
cat("File saved as: tick_data_processed.csv\n")
