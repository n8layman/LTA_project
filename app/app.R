# Load required libraries
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(lubridate)
library(leaflet.providers)
library(readxl) 

# --- 1. Data Generation and Setup ---

# Define site coordinates
SITE_COORDS <- list(
  "Mohonk" = c(41.7711375, -74.135745),
  # Corrected coordinates for Mianus River Park (already accurate, but ensuring consistency)
  "Mianus River Park" = c(41.08561763020252, -73.58839283678192) 
)

# Generate mock data
set.seed(42)
start_date <- ymd("2023-04-01")
end_date <- ymd("2024-09-30")
dates <- seq(start_date, end_date, by = "day")
n <- 500

mock_data <- data.frame(
  Date = sample(dates, n, replace = TRUE),
  Site = sample(names(SITE_COORDS), n, replace = TRUE, prob = c(0.6, 0.4)),
  Species = sample(
    c("Ixodes scapularis (Blacklegged)", "Amblyomma americanum (Lone Star)", "Dermacentor variabilis (Dog Tick)"),
    n, replace = TRUE, prob = c(0.5, 0.3, 0.2)
  ),
  Count = round(rlnorm(n, meanlog = 1.5, sdlog = 0.8)) + 1
) %>%
  arrange(Date, Site)

# Define Exclosure data for Mohonk
exclosures_data <- data.frame(
  Exclosure_Name = c("Cedar Drive Loop", "Canaan Rd", "Glory Hill", "Undercliff Rd"),
  Latitude = c(41.79766, 41.78495, 41.74764, 41.75430),
  Longitude = c(-74.11761, -74.10986, -74.14738, -74.16813)
)

# Define initial date range
min_date <- min(mock_data$Date)
max_date <- max(mock_data$Date)

# --- 2. User Interface (UI) ---

ui <- fluidPage(
  tags$head(
    # Set custom font and basic styling using inline CSS
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;500;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f7f9fc;
      }
      /* Standard well style for the data table */
      .well {
        background-color: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 0.5rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -2px rgba(0, 0, 0, 0.1);
        padding: 1.5rem;
        margin-bottom: 1rem;
      }
      /* Custom style for the combined map/summary container */
      .map-summary-container {
        background-color: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 0.5rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -2px rgba(0, 0, 0, 0.1);
        padding: 1.5rem;
        margin-bottom: 1rem;
      }
      /* Leaflet map needs rounded corners on the left only */
      .leaflet-container {
        border-radius: 0.5rem 0 0 0.5rem; 
        box-shadow: none; /* Shadow handled by parent container */
        border: none;
        height: 400px; /* Fixed height for vertical alignment */
      }
      /* Style for the summary panel to match map height */
      .site-summary-panel {
          height: 400px; /* Matches map height */
          padding: 1rem;
          background-color: #fcfcfc; /* Slightly different background for distinction */
          border-left: 1px solid #e5e7eb;
      }
    "))
  ),
  
  titlePanel(
    div(
      h1("Regional Tick Surveillance Data Explorer", class = "text-2xl font-bold text-gray-800"),
      p("Visualizing data and locations for two key ecological study sites.", class = "text-gray-500")
    ),
    windowTitle = "Tick Study App"
  ),
  
  # --- Central Tabset Panel for Site-Specific Views ---
  tabsetPanel(
    id = "site_tabs",
    type = "pills",
    
    # Tab 1: Mohonk Site View
    tabPanel(
      title = "Mohonk Preserve, NY",
      value = "Mohonk",
      div(class = "map-summary-container",
          h3("Mohonk Preserve, NY - Data and Location", class = "text-xl font-semibold mb-4 text-gray-700"),
          fluidRow(
            # Left side: Mohonk Map (7 columns)
            column(7,
                   leafletOutput("mohonk_map", height = "400px")
            ),
            # Right side: Mohonk Summary (5 columns)
            column(5, class = "site-summary-panel", 
                   h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"),
                   uiOutput("site_summary_ui")
            )
          )
      )
    ),
    
    # Tab 2: Mianus River Park Site View
    tabPanel(
      title = "Mianus River Park, CT/NY",
      value = "Mianus River Park",
      div(class = "map-summary-container",
          h3("Mianus River Park - Data and Location", class = "text-xl font-semibold mb-4 text-gray-700"),
          fluidRow(
            # Left side: Mianus River Park Map (7 columns)
            column(7,
                   leafletOutput("mianus_map", height = "400px")
            ),
            # Right side: Mianus River Park Summary (5 columns)
            column(5, class = "site-summary-panel",
                   h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"),
                   uiOutput("site_summary_ui_mianus")
            )
          )
      )
    )
  ), # End of tabsetPanel
  
  # --- Global Data Table (Below the tabs) ---
  fluidRow(
    column(12,
           div(class = "well",
               h3("All Tick Data Points (Across Both Sites)", class = "text-xl font-semibold mt-6 mb-4 text-gray-700"),
               DTOutput("tick_data_table")
           )
    )
  )
)

# --- 3. Server Logic ---

server <- function(input, output, session) {

  # Reactive expression for all mock data (No date filtering)
  filtered_data <- reactive({
    mock_data
  })

  # --- Summary Generation for Mohonk and Mianus River ---
  
  # Function to generate summary text for a given site
  generate_site_summary <- function(site_name) {
    # Data is filtered only by site, not date
    data <- filtered_data() %>% filter(Site == site_name) 
    
    if (nrow(data) == 0) {
      return(
        p(paste("No data points found for", site_name, "."), class = "text-red-500 italic")
      )
    }
    
    total_ticks <- sum(data$Count)
    unique_species <- length(unique(data$Species))
    
    tagList(
      p(HTML(paste0("<strong>Site:</strong> ", site_name)), class = "text-gray-600"),
      p(HTML(paste0("<strong>Total Ticks Counted:</strong> ", format(total_ticks, big.mark = ","))), class = "text-gray-600"),
      p(HTML(paste0("<strong>Unique Species Found:</strong> ", unique_species)), class = "text-gray-600"),
      p(paste0("The summary above reflects all data collected between ", min_date, " and ", max_date, " at this site."), 
        class = "text-sm text-gray-500 mt-2")
    )
  }
  
  # Render Mohonk summary (output used in Tab 1)
  output$site_summary_ui <- renderUI({
    generate_site_summary("Mohonk")
  })
  
  # Render Mianus River Park summary (output used in Tab 2)
  output$site_summary_ui_mianus <- renderUI({
    generate_site_summary("Mianus River Park")
  })

  # --- Data Table (Global) ---
  
  # Render the data table (ALL data for ALL sites)
  output$tick_data_table <- renderDT({
    data <- filtered_data()
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'tip',
        searching = FALSE,
        scrollY = '300px'
      ),
      rownames = FALSE,
      caption = 'All data across both study sites.'
    )
  })

  # --- Leaflet Map Renderers ---

  # 1. Mohonk Map (output used in Tab 1)
  output$mohonk_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mohonk"]][1]
    lng <- SITE_COORDS[["Mohonk"]][2]
    
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles(group = "Default Map") %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"), 
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = lng, lat = lat, zoom = 12.25) %>%
      # Add the exclosure locations as markers
      addMarkers(
        data = exclosures_data,
        lat = ~Latitude,
        lng = ~Longitude,
        popup = ~paste("Exclosure:", Exclosure_Name),
        label = ~Exclosure_Name
      )
  })

  # 2. Mianus River Map (output used in Tab 2)
  output$mianus_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mianus River Park"]][1]
    lng <- SITE_COORDS[["Mianus River Park"]][2]
    
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      addTiles(group = "Default Map") %>%
      addLayersControl(
        baseGroups = c("Default Map", "Topographic", "Satellite"), 
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = lng, lat = lat, zoom = 14) %>%
      # FIX: Ensure lat and lng are used correctly for the marker
      addMarkers(
        lng = lng, 
        lat = lat,
        popup = "<b>Mianus River Park</b><br>Regional tick and wildlife study site.",
        label = "Mianus River Park"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
