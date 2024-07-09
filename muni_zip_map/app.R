
library(shiny)
library(leaflet)
library(dplyr)
library(sf) 

# Load data
zip_map_df = readRDS("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//Data//zip_map_df.rds")
muni_map_sf = readRDS("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//Data//seller_map_df.rds")


# Define metric options and their readable names
zip_metric_options <- c(
  "Population" = "seller_zip_population",
  "Income per Capita" = "seller_zip_incomepc",
  "Unique Products Sold" = "unique_products_sold",
  "Average Price per Product Sold" = "avg_price_per_seller",
  "Total Sellers" = "total_sellers",
  "Total Seller Sales" = "total_sales",
  "Average Shipping Charge per Product" = "avg_freight_per_seller",
  "Total Seller Orders" = "total_orders",
  "Average Shipping Share of Product Price" = "avg_ship_share_per_seller",
  "Average Actual Shipping Time per Seller" = "avg_ship_time_per_seller",
  "Average Review Score" = "avg_review_score_per_seller",
  "Total Seller LTV to Olist" = "total_seller_LTV",
  "Average Seller LTV to Olist" = "avg_seller_LTV"
  # Add other metrics as needed
)

zip_metric_ranges <- list(
  seller_zip_population = c(15, 3252),
  seller_zip_income = c(10130, 6393591),
  seller_zip_incomepc = c(36.85, 7903.81),
  unique_products_sold = c(1, 2256),
  avg_price_per_seller = c(6.25, 3307),
  total_sellers = c(1, 47),
  total_sales = c(6.5, 625192),
  avg_freight_per_seller = c(0.03, 308.34),
  total_freight = c(0.03, 137451.45),
  total_revenue = c(15.2, 762643.4),
  total_orders = c(1, 6622),
  avg_ship_share_per_seller = c(0.0252, 325.3753),
  avg_expected_ship_time_per_seller = c(3, 82),
  avg_ship_time_per_seller = c(1, 190),
  avg_review_score_per_seller = c(1, 5),
  total_seller_LTV = c(100.3, 167905.4),
  avg_seller_LTV = c(100.3, 47396.2)
  # Add ranges for other metrics as needed
)

muni_metric_options <- c(
  "Muni Population" = "muni_population",
  "Muni Income per Capita" = "muni_incomepc",
  "Unique Products Sold" = "unique_products_sold",
  "Average Price per Product Sold" = "avg_price_per_seller",
  "Total Sellers" = "total_sellers",
  "Total Sales" = "total_sales",
  "Average Shipping Charge per Product" = "avg_freight_per_seller",
  "Total Orders" = "total_orders",
  "Average Shipping Share of Product Price" = "avg_ship_share_per_seller",
  "Average Shipping Time" = "avg_ship_time_per_seller",
  "Average Review Score" = "avg_review_score_per_seller",
  "Total Seller LTV to Olist" = "total_seller_LTV",
  "Average Seller LTV to Olist" = "avg_seller_LTV"
  
)


muni_metric_ranges <- list(
  muni_population = c(22, 342522),
  muni_income = c(14780, 493116456),
  unique_products_sold = c(1, 7672),
  avg_price_per_seller = c(6.25, 3133.32),
  total_sellers = c(1, 626),
  total_sales = c(10, 2588557),
  avg_freight_per_seller = c(9.512, 150.22),
  total_freight = c(11.2, 464474.6),
  total_revenue = c(25.7, 3053031.8),
  total_orders = c(1, 23856),
  avg_ship_share_per_seller = c(3.261, 192.634),
  avg_expected_ship_time_per_seller = c(7, 66.04),
  avg_ship_time_per_seller = c(2, 190),
  avg_review_score_per_seller = c(1, 5),
  total_seller_LTV = c(101, 946876.4),
  avg_seller_LTV = c(101, 47396.2),
  muni_incomepc = c(155.2, 3331.3)
)


# UI
ui <- fluidPage(
  titlePanel("Dynamic Maps: Zip Codes and Municipalities"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mapType", "Select Map Type:", 
                  choices = c("Zip Codes" = "zip", "Municipalities" = "muni")),
      selectInput("metric", "Select Metric:", 
                  choices = zip_metric_options),  # Default to zip metrics
      uiOutput("sliderUI")
    ),
    mainPanel(leafletOutput("map"))
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Observe changes in map type and update metric options
  observeEvent(input$mapType, {
    if (input$mapType == "zip") {
      updateSelectInput(session, "metric", choices = zip_metric_options)
      # Reset slider to default zip range
      range <- zip_metric_ranges[[names(zip_metric_options)[1]]]
      updateSliderInput(session, "valueRange", min = range[1], max = range[2], value = range)
    } else {
      updateSelectInput(session, "metric", choices = muni_metric_options)
      # Reset slider to default muni range
      range <- muni_metric_ranges[[names(muni_metric_options)[1]]]
      updateSliderInput(session, "valueRange", min = range[1], max = range[2], value = range)
    }
  })
  
  # Update slider range based on selected metric
  observe({
    if (input$mapType == "zip") {
      range <- zip_metric_ranges[[input$metric]]
    } else {
      range <- muni_metric_ranges[[input$metric]]
    }
    updateSliderInput(session, "valueRange", min = range[1], max = range[2], value = range)
  })
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    if (input$mapType == "zip") {
      zip_map_df %>%
        filter(get(input$metric) >= input$valueRange[1], get(input$metric) <= input$valueRange[2])
    } else {
      muni_map_sf %>%
        filter(get(input$metric) >= input$valueRange[1], get(input$metric) <= input$valueRange[2])
    }
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    data <- filteredData()
    if (input$mapType == "zip") {
      createZipMap(data, input$metric)
    } else {
      createMuniMap(data, input$metric)
    }
  })
}

# Function to create map for zip codes
createZipMap <- function(data, metric) {
  leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude,
      fillColor = ~colorNumeric("viridis", data[[metric]])(data[[metric]]),
      fillOpacity = 0.8, radius = 5,
      popup = ~paste(metric, ":", data[[metric]])
    )
}

# Function to create map for municipalities
createMuniMap <- function(data, metric) {
  leaflet(data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~colorNumeric("viridis", data[[metric]])(data[[metric]]),
      fillOpacity = 0.7,
      popup = ~paste(metric, ":", data[[metric]])
    )
}

# Run the application
shinyApp(ui, server)
