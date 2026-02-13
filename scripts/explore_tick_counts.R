library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("app/LTA_tick_data.csv", check.names = FALSE) %>%
  filter(LT_Site == "MRG") %>%
  mutate(
    Adults_Nymphs = BL_Adults + BL_Nymphs + Dg_Adults + Dg_Nymphs + ALH_Adults + ALH_Nymphs,
    Treatment = `Fence/Unf`
  )

# Summarize
summary_df <- df %>%
  group_by(Site_location, Pair, Treatment) %>%
  summarize(Adults_Nymphs = sum(Adults_Nymphs, na.rm = TRUE), .groups = "drop") %>%
  arrange(Site_location, Pair, Treatment)

# Table
cat("=== Ticks (Adults + Nymphs) by Site x Pair x Treatment ===\n")
print(summary_df, n = Inf)

# Plot
ggplot(summary_df, aes(x = Pair, y = Adults_Nymphs, fill = Treatment)) +
  geom_col(position = "dodge") +
  facet_wrap(~Site_location) +
  labs(
    title = "Ticks (Adults + Nymphs) by Site, Pair, and Treatment",
    x = "Pair", y = "Count", fill = "Treatment"
  ) +
  theme_minimal()
