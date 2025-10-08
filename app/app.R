# Load required libraries
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(lubridate)
library(leaflet.providers) # Required for addProviderTiles
library(readxl) # Added to simulate required library for reading exclosures data

# --- 1. Data Generation and Setup ---

# Define site coordinates
SITE_COORDS <- list(
  "Mohonk" = c(41.7711375, -74.135745),
  "Mianus River Park" = c(41.08356759923184, -73.57994874522173)
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

# Define Exclosure data for Mohonk (simulating read from 'app/exclosures_mohonk.xlsx')
exclosures_data <- data.frame(
  Exclosure_Name = c("Cedar Drive Loop", "Canaan Rd", "Glory Hill", "Undercliff Rd"),
  Latitude = c(41.79766, 41.78495, 41.74764, 41.75430),
  Longitude = c(-74.11761, -74.10986, -74.14738, -74.16813)
)

# Define initial date range (used for summary text only, slider is removed)
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
      .panel-heading {
        background-color: #3b82f6 !important;
        color: white;
        border-radius: 0.5rem 0.5rem 0 0;
        padding: 1rem;
      }
      .well {
        background-color = #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 0.5rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -2px rgba(0, 0, 0, 0.1);
        padding: 1.5rem;
      }
      .leaflet-container {
        border-radius: 0.5rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
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
  
  fluidRow(
    # Left Side: Summary and Data Table (6 Columns)
    column(6,
           div(class = "well",
               
               # REMOVED: Date Slider
               
               # Summary of Data 
               h3("Overall Data Summary", class = "text-xl font-semibold mb-4 text-gray-700"),
               uiOutput("site_summary_ui"),
               
               # Data Table
               h3("All Tick Data Points", class = "text-xl font-semibold mt-6 mb-4 text-gray-700"),
               DTOutput("tick_data_table")
           )
    ),
    
    # Right Side: Interactive Maps (6 Columns)
    column(6,
           # Tabset Panel to show two separate maps (Kept as requested)
           tabsetPanel(
             id = "site_tabs",
             type = "pills",
             # Tab 1: Mohonk Map
             tabPanel(
               title = "Mohonk Map",
               value = "Mohonk",
               div(class = "well",
                   h3("Mohonk Preserve, NY - Exclosure Sites Highlighted", class = "text-xl font-semibold mb-4 text-gray-700"),
                   leafletOutput("mohonk_map", height = 450)
               )
             ),
             # Tab 2: Mianus River Map
             tabPanel(
               title = "Mianus River Park Map",
               value = "Mianus River Park",
               div(class = "well",
                   h3("Mianus River Park, CT/NY", class = "text-xl font-semibold mb-4 text-gray-700"),
                   leafletOutput("mianus_map", height = 450)
               )
             )
           )
    )
  )
)

# --- 3. Server Logic ---

server <- function(input, output, session) {

  # Reactive expression for all mock data (No filtering since slider is removed)
  filtered_data <- reactive({
    mock_data
  })

  # Reactive summary generation
  site_summary_text <- reactive({
    # Only require the tab selection to be ready
    req(input$site_tabs) 
    
    current_site <- input$site_tabs
    # Data is filtered only by site, not date
    data <- filtered_data() %>% filter(Site == current_site) 
    
    if (nrow(data) == 0) {
      return(
        p(paste("No data points found for", current_site, "."), class = "text-red-500 italic")
      )
    }
    
    total_ticks <- sum(data$Count)
    unique_species <- length(unique(data$Species))
    
    tagList(
      p(HTML(paste0("<strong>Site:</strong> ", current_site)), class = "text-gray-600"),
      p(HTML(paste0("<strong>Total Ticks Counted:</strong> ", format(total_ticks, big.mark = ","))), class = "text-gray-600"),
      p(HTML(paste0("<strong>Unique Species Found:</strong> ", unique_species)), class = "text-gray-600"),
      # Updated text to reflect ALL data collected
      p(paste0("The summary above reflects all data collected between ", min_date, " and ", max_date, " at this site."), 
        class = "text-sm text-gray-500 mt-2")
    )
  })
  
  # Render the summary text
  output$site_summary_ui <- renderUI({
    site_summary_text()
  })
  
  # Render the data table (ALL data for ALL sites)
  output$tick_data_table <- renderDT({
    data <- filtered_data()
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'tip', # Show table, info, and pagination
        searching = FALSE, # Remove search bar to keep UI clean
        scrollY = '300px'
      ),
      rownames = FALSE,
      caption = 'All data across both study sites.'
    )
  })

  # --- Leaflet Map Renderers (Two separate outputs, with layer selector kept) ---

  # 1. Mohonk Map 
  output$mohonk_map <- renderLeaflet({
    req(input$site_tabs) 
    
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
      setView(lng = lng, lat = lat, zoom = 12.25) %>% # Zoomed out slightly to see all exclosures
      # Add the exclosure locations as red circle markers
      addMarkers(
        data = exclosures_data,
        lat = ~Latitude,
        lng = ~Longitude,
        popup = ~paste("Exclosure:", Exclosure_Name),
        label = ~Exclosure_Name
      )
  })

  # 2. Mianus River Map
  output$mianus_map <- renderLeaflet({
    req(input$site_tabs)

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
      addMarkers(
        lng = lng, lat = lat,
        popup = "<b>Mianus River Park</b><br>Regional tick and wildlife study site.",
        label = "Mianus River Park"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
