
library(shiny)
library(leaflet)
library(dplyr)
library(sf) 

# Load data
zip_map_df = readRDS("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//Data//zip_map_df.rds")
muni_map_sf = readRDS("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//Data//muni_map_sf.rds")

if (!inherits(muni_map_sf, "sf")) {
  muni_map_sf <- st_as_sf(muni_map_sf, wkt = "geometry_column")
}

print(head(zip_map_df))
print(head(muni_map_sf))


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
  muni_incomepc = c(155.1606, 3331.318)
)


# UI
ui <- fluidPage(
  titlePanel("Dynamic Maps: Sellers & Customers"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mapType", "Select Map Type:", 
                  choices = c("Zip Codes" = "zip", "Municipalities" = "muni")),
      selectInput("metric", "Select Metric:", 
                  choices = zip_metric_options),  # Default to zip metrics
      uiOutput("sliderUI")  # Placeholder for dynamic slider UI
    ),
    mainPanel(leafletOutput("map", width = "100%", height = "800px")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Render the dynamic slider UI based on selected metric
  output$sliderUI <- renderUI({
    if (input$mapType == "zip") {
      range <- zip_metric_ranges[[input$metric]]
    } else {
      range <- muni_metric_ranges[[input$metric]]
    }
    
    # Add a small buffer to the maximum value
    max_value <- max(range) + 0.01  
    
    # Creating the slider with the correct range
    sliderInput("valueRange", "Value Range:", 
                min = min(range), max = max(range), value = range)
  })
  
  # Observe changes in map type and update metric options
  observeEvent(input$mapType, {
    if (input$mapType == "zip") {
      updateSelectInput(session, "metric", choices = zip_metric_options)
    } else {
      updateSelectInput(session, "metric", choices = muni_metric_options)
    }
  })
  
  # Update slider range based on selected metric
  observeEvent(input$metric, {
    if (input$mapType == "zip") {
      range <- zip_metric_ranges[[input$metric]]
    } else {
      range <- muni_metric_ranges[[input$metric]]
    }
    
    # Add a small buffer to the maximum value
    max_value <- max(range) + 0.01
    
    updateSliderInput(session, "valueRange", min = min(range), max = max(range), value = range)
  })
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    data <- if (input$mapType == "zip") zip_map_df else muni_map_sf
    metric <- input$metric
    # Print for debugging
    print(paste("Filtering data for metric:", metric))
    print(paste("Data columns available:", toString(names(data))))
    
    if (metric %in% names(data)) {
      range <- input$valueRange
      data <- data %>% filter(get(metric) >= range[1], get(metric) <= range[2])
    }
    
    # Print the number of rows after filtering
    print(paste("Number of rows after filtering:", nrow(data)))
    
    data
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
  # Calculate log values for coloring
  log_color_values <- log(data[[metric]] + 1)  # Add 1 to avoid log(0)
  
  # Define color palette
  color_palette <- colorNumeric("viridis", log_color_values)
  
  # Create bins for log values using quantile style
  bins <- classInt::classIntervals(log_color_values, n = 5, style = "quantile")$brks
  
  # Map bins back to original scale
  original_scale_bins <- exp(bins) - 1  # Reverse of log(x + 1)
  # Label for bins in original scale
  bin_labels <- sprintf("%.1f - %.1f", original_scale_bins[-length(original_scale_bins)], original_scale_bins[-1])
  
  leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude,
      fillColor = ~color_palette(log_color_values),
      fillOpacity = 0.8, radius = 5,
      popup = ~paste("Zip Code:", data$seller_zip_code_prefix, "<br>",
                     metric, ":", round(data[[metric]], 2))
    ) %>%
    addLegend(
      position = "bottomright",
      colors = color_palette(bins[-length(bins)]),
      labels = bin_labels,
      title = metric
    )
}

# Function to create map for municipalities
createMuniMap <- function(data, metric) {
  # Ensure that data is an sf object
  if (!inherits(data, "sf")) {
    stop("Data must be an sf object")
  }
  
  # Initialize variables for color palette and bin labels
  bins <- NULL
  bin_labels <- NULL
  color_values <- NULL
  pal <- NULL
  
  
  # Apply different binning logic based on the metric
  if (metric == "muni_population") {
    # Logarithmic scaling for muni_population
    ccolor_values <- log(data[[metric]] + 1)
    bins <- classInt::classIntervals(color_values, n = 5, style = "quantile")$brks
    pal <- colorNumeric(palette = "viridis", domain = range(color_values))
    
    # Create labels using the original data scale
    original_bins <- exp(bins) - 1
    bin_labels <- sprintf("%.0f - %.0f", head(original_bins, -1), tail(original_bins, -1))
    
  } else if (metric == "total_sales") {
    # Custom binning for total_sales
    custom_bins <- c(0, 1000, 10000, 100000, 500000, max(data[[metric]]))
    bins <- log(custom_bins + 1)
    pal <- colorNumeric(palette = "viridis", domain = bins)
    bin_labels <- sprintf("%.0f - %.0f", custom_bins[-length(custom_bins)], custom_bins[-1])
    color_list <- pal(bins[-length(bins)])
    
  } else if (metric == "avg_price_per_seller") {
    # Quantile bins for avg_price_per_seller
    bins <- quantile(data[[metric]], probs = seq(0, 1, length.out = 6))
    pal <- colorNumeric(palette = "viridis", domain = bins)
    bin_labels <- sprintf("%.2f - %.2f", head(bins, -1), tail(bins, -1))
    color_list <- pal(bins[-length(bins)])
    
  } else if (metric == "avg_review_score_per_seller") {
    # Equal interval bins for avg_review_score_per_seller
    bins <- seq(min(data[[metric]]), max(data[[metric]]), length.out = 6)
    pal <- colorNumeric(palette = "viridis", domain = bins)
    bin_labels <- sprintf("%.1f - %.1f", head(bins, -1), tail(bins, -1))
    color_list <- pal(bins[-length(bins)])
    
  } else {
    # Default linear binning for other metrics
    color_values <- data[[metric]]
    bins <- classInt::classIntervals(color_values, n = 5, style = "quantile")$brks
    pal <- colorNumeric(palette = "viridis", domain = range(color_values))
    bin_labels <- sprintf("%.2f - %.2f", head(bins, -1), tail(bins, -1))
  }
  
  # Create the leaflet map
  leaflet(data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(ifelse(metric %in% c("muni_population", "total_sales"), 
                              log(data[[metric]] + 1), data[[metric]])),
      color = "transparent",
      fillOpacity = 0.7,
      popup = ~paste("Municipality:", data$muni_name, "<br>", metric, ":", data[[metric]])
    ) %>%
    addLegend(
      position = "bottomright",
      colors = color_list,  # Use the color list defined based on the appropriate scale
      labels = bin_labels,  # Use labels based on the appropriate scale of the data
      title = metric
    )
}




# Run the application
shinyApp(ui, server)
