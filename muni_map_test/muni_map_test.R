library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Load data
muni_map_sf = readRDS("muni_map_sf.rds")

# Check that muni map is in the correct format
if (!inherits(muni_map_sf, "sf")) {
  muni_map_sf <- st_as_sf(muni_map_sf, wkt = "geometry_column")
}

# Set metric options
muni_metric_options <- c(
  "Total Sales (BRL)" = "total_sales",
  "Total Sellers" = "total_sellers",
  "Population" = "muni_population",
  "Average Product Price (BRL)" = "avg_price_per_seller",
  "Total Seller LTV to Olist (BRL)" = "total_seller_LTV",
  "Average Seller LTV to Olist (BRL)" = "avg_seller_LTV",
  "Average Ship Cost Share of Total Order (%)" = "avg_ship_share_per_seller",
  "Average Expected Shipping Time (days)" = "avg_expected_ship_time_per_seller",
  "Average Review Score" = "avg_review_score_per_seller"
)



# UI
ui <- fluidPage(
  titlePanel("Dynamic Maps with Bin Selection"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Select Metric:", choices = muni_metric_options),
      checkboxInput("selectAll", "Select All", value = TRUE),
      uiOutput("checkboxesUI")
    ),
    mainPanel(leafletOutput("map"))
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Define the function to get bins for each metric
  getBinsForMetric <- function(metric) {
    switch(metric,
           "total_sales" = c("0-10k", "10k-20k", "20k-40k", "40k-100k", "100k-1m", "1m-3m"),
           "total_sellers" = c("0-5", "5-10", "10-20", "20-40", "40-50", "50-75"),
           "muni_population" = c("0-3k", "3k-6k", "6k-10k", "10k-60k", "60k-400k"),
           "avg_price_per_seller" = c("0-250", "250-500", "500-1000", "1k-2k", "2k-4k"),
           "total_seller_LTV" = c("0-5k", "5k-10k", "10k-30k", "30k-100k", "100k-200k", "200k-1m"),
           "avg_seller_LTV" = c("0-1k", "1k-3k", "3k-6k", "6k-10k", "10k-25k", "25k-50k"),
           "avg_ship_share_per_seller" = c("0-10%", "10%-20%", "20%-30%", "30%-40%", "40%-50%", "50%-100%"),
           "avg_expected_ship_time_per_seller" = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-75"),
           "avg_review_score_per_seller" = c("1-2", "2-3", "3-4", "4-5")
    )
  }
  
  
  filterDataBasedOnBins <- function(data, metric, selected_bins) {
    # Define bin ranges for each metric
    bin_ranges <- list(
      "total_sales" = list("0-10k" = c(0, 10000), "10k-20k" = c(10000, 20000), "20k-40k" = c(20000, 40000), "40k-100k" = c(40000, 100000), "100k-1m" = c(100000, 1000000), "1m-3m" = c(1000000, 3000000)),
      "total_sellers" = list("0-5" = c(0, 5), "5-10" = c(5, 10), "10-20" = c(10, 20), "20-40" = c(20, 40), "40-50" = c(40, 50), "50-75" = c(50, 75)),
      "muni_population" = list("0-3k" = c(0, 3000), "3k-6k" = c(3000, 6000), "6k-10k" = c(6000, 10000), "10k-60k" = c(10000, 60000), "60k-400k" = c(60000, 400000)),
      "avg_price_per_seller" = list("0-250" = c(0, 250), "250-500" = c(250, 500), "500-1000" = c(500, 1000), "1k-2k" = c(1000, 2000), "2k-4k" = c(2000, 4000)),
      "total_seller_LTV" = list("0-5k" = c(0, 5000), "5k-10k" = c(5000, 10000), "10k-30k" = c(10000, 30000), "30k-100k" = c(30000, 100000), "100k-200k" = c(100000, 200000), "200k-1m" = c(200000, 1000000)),
      "avg_review_score_per_seller" = list("1-2" = c(1, 2), "2-3" = c(2, 3), "3-4" = c(3, 4), "4-5" = c(4, 5)),
      "avg_seller_LTV" = list("0-1k" = c(0, 1000), "5k-10k" = c(5000, 10000), "3k-6k" = c(3000, 6000), "6k-10k" = c(6000, 10000), "10k-25k" = c(10000, 25000), "25k-50k" = c(25000, 50000)),
      "avg_ship_share_per_seller" = list("0-10%" = c(0, 10), "10%-20%" = c(10, 20), "25%-50%" = c(25, 50), "50%-75%" = c(50, 75), "75%-100%" = c(75, 100), "100%-200%" = c(100, 200)),
      "avg_expected_ship_time_per_seller" = list("0-10" = c(0, 10), "10-20" = c(10, 20), "20-30" = c(20, 30), "30-40" = c(30, 40), "40-50" = c(40, 50), "50-75" = c(50, 75))
    )
    
    
    # Map selected bin labels to numeric ranges
    bin_labels <- getBinsForMetric(metric)
    selected_ranges <- lapply(selected_bins, function(bin) bin_ranges[[metric]][[bin]])
    
    selected_ranges <- lapply(selected_bins, function(bin) bin_ranges[[metric]][[bin]])
    
    filtered_data <- list()
    for (range in selected_ranges) {
      if (!is.null(range) && length(range) == 2) {
        subset_data <- data[data[[metric]] >= range[1] & data[[metric]] <= range[2], ]
        if (nrow(subset_data) > 0) {
          filtered_data <- c(filtered_data, list(subset_data))
          print(paste("Subsetting for range:", range[1], "-", range[2], "Rows:", nrow(subset_data)))
        }
      }
    }
    
    if (length(filtered_data) > 0) {
      filtered_data <- do.call(rbind, filtered_data)
      print(paste("Returning filtered data with rows:", nrow(filtered_data)))
      return(st_as_sf(filtered_data, sf_column_name = "geometry"))
    } else {
      print("Returning original data as no rows were found in selected bins")
      return(data)
    }
  }
  
  # Dynamically generate checkboxes based on selected metric
  output$checkboxesUI <- renderUI({
    bins <- getBinsForMetric(input$metric)
    checkboxGroupInput("selectedBins", "Select Bins:", choices = bins, selected = bins)
  })
  
  # Handle "Select All" functionality
  observeEvent(input$selectAll, {
    # Get all available bins for the current metric
    all_bins <- getBinsForMetric(input$metric)
    
    if (input$selectAll) {
      # If "Select All" is checked, select all bins
      updateCheckboxGroupInput(session, "selectedBins", selected = all_bins)
    } else {
      # If "Select All" is unchecked, deselect all bins
      updateCheckboxGroupInput(session, "selectedBins", selected = character(0))
    }
  }, ignoreNULL = FALSE)
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    data <- muni_map_sf
    metric <- input$metric
    selected_bins <- input$selectedBins
    filterDataBasedOnBins(data, metric, selected_bins)
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    data <- filteredData()
    createMuniMap(data, input$metric)
  })
}

# Function to create map for municipalities
createMuniMap <- function(data, metric) {
  if (!inherits(data, "sf")) {
    stop("Data must be an sf object")
  }
  
  # Define custom bins based on the metric
  bins <- switch(metric,
                 "total_sales" = c(0, 10000, 20000, 40000, 100000, 1000000, 3000000),
                 "total_sellers" = c(0, 5, 10, 20, 40, 50, 75),  # Add bins for new metrics
                 "muni_population" = c(0, 3000, 6000, 10000, 60000, 400000),
                 "avg_price_per_seller" = c(0, 250, 500, 1000, 2000, 4000),
                 "total_seller_LTV" = c(0, 5000, 10000, 30000, 100000, 200000, 1000000),
                 "avg_review_score_per_seller" = c(1, 2, 3, 4, 5),
                 "avg_seller_LTV" = c(0, 1000, 5000, 6000, 10000, 25000, 50000),  # Add bins for new metrics
                 "avg_ship_share_per_seller" = c(0, 10, 20, 50, 75, 100, 200),  # Add bins for new metrics
                 "avg_expected_ship_time_per_seller" = c(0, 10, 20, 30, 40, 50, 75),  # Add bins for new metrics
                 stop("Unknown metric: ", metric)
  )
  
  # Create color palette
  pal <- colorBin(palette = "viridis", domain = bins, bins = bins, na.color = "transparent")
  
  # Generate labels for bins
  bin_labels <- if (metric == "avg_review_score") {
    sapply(1:(length(bins) - 1), function(i) paste(bins[i], "-", bins[i + 1]))
  } else {
    sapply(2:length(bins), function(i) paste(formatC(bins[i - 1], format = "f", big.mark = ","), "-", formatC(bins[i], format = "f", big.mark = ",")))
  }
  
  # Create the leaflet map
  leaflet(data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(get(metric)),
      color = "transparent",
      fillOpacity = 0.7,
      popup = ~paste("Municipality:", muni_name, "<br>", metric, ":", get(metric))
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~get(metric),
      labels = bin_labels,
      title = metric
    )
}



# Run the application
shinyApp(ui, server)

  