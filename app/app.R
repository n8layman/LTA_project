# Load required libraries
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(lubridate)
library(leaflet.providers)
library(leaflet.extras)
library(readxl) 
library(htmltools) 

# --- 2. Data Generation and Setup (UNCHANGED) ---
SITE_COORDS <- list(
  "Mohonk" = c(41.7711375, -74.135745),
  "Mianus River Park" = c(41.08561763020252, -73.58839283678192) 
)
set.seed(42)
start_date <- ymd("2023-04-01")
end_date <- ymd("2024-09-30")
dates <- seq(start_date, end_date, by = "day")
n <- 500

mock_data <- data.frame(
  Date = sample(dates, n, replace = TRUE),
  Site = sample(names(SITE_COORDS), n, replace = TRUE, prob = c(0.6, 0.4)),
  Species = sample(
    c("Ixodes scapularis (Blacklegged)", "Amblyomma americanum (Lone Star)", "Dermacentmaus variabilis (Dog Tick)"),
    n, replace = TRUE, prob = c(0.5, 0.3, 0.2)
  ),
  Count = round(rlnorm(n, meanlog = 1.5, sdlog = 0.8)) + 1
) %>%
  arrange(Date, Site)
exclosures_data <- data.frame(
  Exclosure_Name = c("Cedar Drive Loop", "Canaan Rd", "Glory Hill", "Undercliff Rd"),
  Latitude = c(41.79766, 41.78495, 41.74764, 41.75430),
  Longitude = c(-74.11761, -74.10986, -74.14738, -74.16813)
)
min_date <- min(mock_data$Date)
max_date <- max(mock_data$Date)

# --- 3. User Interface (UI) ---

ui <- fluidPage(
  tags$head(
  # Load Font Awesome 6 for icons
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
  ),
  
  # Include custom fonts and styles
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;500;700&display=swap');
    
    body {
      font-family: 'Inter', sans-serif;
      background-color: #f7f9fc;
    }

    /* --- Base UI Styling --- */
    .tab-content {
      background-color: #ffffff;
      border: 1px solid #e5e7eb;
      border-radius: 0 0.5rem 0.5rem 0.5rem;
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1),
                  0 2px 4px -2px rgba(0, 0, 0, 0.1);
      padding: 1.5rem;
      margin-bottom: 1rem;
    }

    .leaflet-container {
      border-radius: 0.5rem;
      box-shadow: none;
      border: 1px solid #e5e7eb;
      height: 500px;
    }

    .site-summary-panel {
      height: 500px;
      padding: 1.5rem;
      background-color: #f9f9f9;
      border: 1px solid #e5e7eb;
      border-radius: 0.5rem;
      margin-left: 1rem;
    }

    .data-table-styled-container {
      background-color: #f9f9f9;
      border: 1px solid #e5e7eb;
      border-radius: 0.5rem;
      padding: 1.5rem;
      margin-top: 1.5rem;
    }

    /* --- Enable Font Awesome inside Leaflet EasyButtons --- */
    .easy-button-button i.fa,
    .easy-button-button i.fas,
    .easy-button-button i.far,
    .easy-button-button i.fab {
      font-family: 'Font Awesome 6 Free' !important;
      font-weight: 900 !important;
      font-size: 18px !important;      /* Icon size */
      line-height: 36px !important;
      vertical-align: middle;
    }

    /* --- Resize and tighten EasyButton --- */
    .easy-button-button {
      width: 36px !important;
      height: 36px !important;
      line-height: 36px !important;
      padding: 0 !important;
      border-radius: 6px !important;
      font-size: 18px !important;
      color: #333;
      transition: all 0.2s ease;
      background-color: white !important;
      border: 1px solid #ddd !important;
    }

    .easy-button-button:hover {
      color: #007bff !important;
      background-color: #f0f0f0 !important;
    }

    .easy-button-button.active {
      color: #fff !important;
      background-color: #007bff !important;
      border-color: #007bff !important;
    }
  "))
),

  
  titlePanel(
    div(h1("Regional Tick Surveillance Data Explorer", class = "text-2xl font-bold text-gray-800")),    
    windowTitle = "Tick Study App"
  ),
  
  # --- Central Tabset Panel ---
  tabsetPanel(
    id = "site_tabs",
    
    # Tab 1: Mohonk Site View
    tabPanel(title = "Mohonk Preserve, NY", value = "Mohonk",
      h3("Mohonk Preserve, NY - Location and Data Overview", class = "text-xl font-semibold mb-4 text-gray-700"),
      fluidRow(
        column(8, leafletOutput("mohonk_map", height = "500px")),
        column(4, div(class = "site-summary-panel", h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), uiOutput("site_summary_ui")))
      ),
      div(class = "data-table-styled-container", h4("Mohonk Preserve Data", class = "text-lg font-semibold mb-3 text-gray-800"), DTOutput("mohonk_data_table"))
    ),
    
    # Tab 2: Mianus River Park Site View
    tabPanel(title = "Mianus River Park, CT/NY", value = "Mianus River Park",
      h3("Mianus River Park - Location and Data Overview", class = "text-xl font-semibold mb-4 text-gray-700"),
      fluidRow(
        column(8, leafletOutput("mianus_map", height = "500px")),
        column(4, div(class = "site-summary-panel", h4("Data Summary", class = "text-lg font-semibold mb-3 text-gray-800"), uiOutput("site_summary_ui_mianus")))
      ),
      div(class = "data-table-styled-container", h4("Raw Data: Mianus River Park", class = "text-lg font-semibold mb-3 text-gray-800"), DTOutput("mianus_data_table"))
    )
  )
)

# --- 4. Server Logic (UNCHANGED, except for the EasyButton call) ---

server <- function(input, output, session) {

  filtered_data <- reactive({ mock_data })
  
  # --- Toggle State Reactive Values ---
  mohonk_model_state <- reactiveVal(FALSE) 
  mianus_model_state <- reactiveVal(FALSE)
  
  # --- Mohonk Model Layer Toggle Logic ---
  observeEvent(input$mohonk_model_toggle, {
    new_state <- !mohonk_model_state()
    mohonk_model_state(new_state) 
    session$sendCustomMessage("set-attribute", list(id = "easy_button_mohonk"))
  })
  
  observeEvent(mohonk_model_state(), {
    proxy <- leafletProxy("mohonk_map", session)
    if (mohonk_model_state()) {
      proxy %>% addCircles(lng = SITE_COORDS[["Mohonk"]][2], lat = SITE_COORDS[["Mohonk"]][1], radius = 5000, color = "#FF0000", fillOpacity = 0.2, group = "Model Layer", layerId = "Model Layer")
    } else {
      proxy %>% removeShape(layerId = "Model Layer") 
    }
  })
  
  # --- Mianus Model Layer Toggle Logic ---
  observeEvent(input$mianus_model_toggle, {
    new_state <- !mianus_model_state()
    mianus_model_state(new_state) 
    session$sendCustomMessage("set-attribute", list(id = "easy_button_mianus"))
  })
  
  observeEvent(mianus_model_state(), {
    proxy <- leafletProxy("mianus_map", session)
    if (mianus_model_state()) {
      proxy %>% addCircles(lng = SITE_COORDS[["Mianus River Park"]][2], lat = SITE_COORDS[["Mianus River Park"]][1], radius = 1000, color = "#0000FF", fillOpacity = 0.2, group = "Mianus Model Layer", layerId = "Mianus Model Layer")
    } else {
      proxy %>% removeShape(layerId = "Mianus Model Layer") 
    }
  })

  # --- CUSTOM JAVASCRIPT FOR TOGGLING EASYBUTTON ACTIVE CLASS ---
  session$sendCustomMessage("set-attribute", message = list(code = 
    "Shiny.setInputValue('trigger', Math.random());
     Shiny.addCustomMessageHandler('set-attribute', function(message) {
       // Toggle the 'active' class on the EasyButton element itself
       $('#' + message.id).toggleClass('active');
     });"
  ))
  
  # --- Data and Summary Generation (UNCHANGED) ---
  
  generate_site_summary <- function(site_name) {
    data <- filtered_data() %>% filter(Site == site_name) 
    total_ticks <- sum(data$Count)
    unique_species <- length(unique(data$Species))
    
    tagList(
      p(HTML(paste0("<strong>Site:</strong> ", site_name)), class = "text-gray-600"),
      p(HTML(paste0("<strong>Total Ticks Counted:</strong> ", format(total_ticks, big.mark = ","))), class = "text-gray-600"),
      p(HTML(paste0("<strong>Unique Species Found:</strong> ", unique_species)), class = "text-gray-600"),
      p(paste0("The summary above reflects all data collected between ", min_date, " and ", max_date, " at this site."), class = "text-sm text-gray-500 mt-2")
    )
  }
  
  output$site_summary_ui <- renderUI({ generate_site_summary("Mohonk") })
  output$site_summary_ui_mianus <- renderUI({ generate_site_summary("Mianus River Park") })
  output$mohonk_data_table <- renderDT({ datatable(filtered_data() %>% filter(Site == "Mohonk") %>% select(-Site), options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'), rownames = FALSE) })
  output$mianus_data_table <- renderDT({ datatable(filtered_data() %>% filter(Site == "Mianus River Park") %>% select(-Site), options = list(pageLength = 10, dom = 'tip', searching = TRUE, scrollY = '300px'), rownames = FALSE, caption = 'All data points for Mianus River Park.') })
  
  # 1. Mohonk Map
  output$mohonk_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mohonk"]][1]
    lng <- SITE_COORDS[["Mohonk"]][2]
    
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles(group = "Default Map") %>%
      addLayersControl(baseGroups = c("Default Map", "Topographic", "Satellite"), options = layersControlOptions(collapsed = TRUE)) %>%
      setView(lng = lng, lat = lat, zoom = 12.25) %>%
      addMarkers(data = exclosures_data, lat = ~Latitude, lng = ~Longitude, popup = ~paste("Exclosure:", Exclosure_Name), label = ~Exclosure_Name) %>%
      
      addEasyButton(
        easyButton(
          id = "easy_button_mohonk", 
          icon = "fas fa-chart-column",
          title = "Show Predictive Modeling Results (TBD)",
          position = "bottomleft",
          onClick = JS("function(btn, map) { Shiny.setInputValue('mohonk_model_toggle', Math.random(), {priority: 'event'}); }")
        )
      )
  })
  
  # 2. Mianus River Map
  output$mianus_map <- renderLeaflet({
    lat <- SITE_COORDS[["Mianus River Park"]][1]
    lng <- SITE_COORDS[["Mianus River Park"]][2]
    
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      addTiles(group = "Default Map") %>%
      addLayersControl(baseGroups = c("Default Map", "Topographic", "Satellite"), options = layersControlOptions(collapsed = TRUE)) %>%
      setView(lng = lng, lat = lat, zoom = 14) %>%
      addMarkers(lng = lng, lat = lat, popup = "<b>Mianus River Park</b><br>Regional tick and wildlife study site.", label = "Mianus River Park") %>%
      
      addEasyButton(
        easyButton(
          id = "easy_button_mianus", 
          icon = "fas fa-chart-column",
          title = "Show Predictive Modeling Results (TBD)",
          position = "bottomleft",
          onClick = JS("function(btn, map) { Shiny.setInputValue('mianus_model_toggle', Math.random(), {priority: 'event'}); }")
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
