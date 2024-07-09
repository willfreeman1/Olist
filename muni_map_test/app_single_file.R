
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tidyr)

# Debugging
options(shiny.error = 'browser') # for debugging

#............................................................................... Load data
muni_map_sf = readRDS("muni_map_sf.rds")
custmetrics_state_geo = readRDS("custmetrics_state_geo.rds")
custmetrics_muni_geo = readRDS("custmetrics_muni_geo.rds")
custmetrics_zip_geo = readRDS("custmetrics_zip_geo.rds")

# Check seller DF
str(muni_map_sf)

# REMOVE NA and Empty values
muni_map_sf <- muni_map_sf %>% 
  filter(!st_is_empty(muni_geom)) %>%
  drop_na()

custmetrics_state_geo <- custmetrics_state_geo %>% 
  filter(!st_is_empty(geom)) %>%
  drop_na()

custmetrics_muni_geo <- custmetrics_muni_geo %>% 
  filter(!st_is_empty(geom)) %>%
  drop_na()

custmetrics_zip_geo <- custmetrics_zip_geo %>% 
  filter(!st_is_empty(geom)) %>%
  drop_na()


# Check that data is the correct SF format
if (!inherits(muni_map_sf, "sf")) {muni_map_sf <- st_as_sf(muni_map_sf)}
if (!inherits(custmetrics_state_geo, "sf")) {stop("custmetrics_state_geo is not an sf object")}
if (!inherits(custmetrics_muni_geo, "sf")) {stop("custmetrics_muni_geo is not an sf object")}
if (!inherits(custmetrics_zip_geo, "sf")) {stop("custmetrics_zip_geo is not an sf object")}


# Set muni (seller / muni_map_sf) metric labels & option
seller_muni_metric_options <- c(
  "Total Sales (BRL)" = "total_sales",
  "Total Sellers" = "total_sellers",
  "Average Product Price (BRL)" = "avg_price_per_seller",
  "Total Seller LTV to Olist (BRL)" = "total_seller_LTV",
  "Average Seller LTV to Olist (BRL)" = "avg_seller_LTV",
  "Average Ship Cost Share of Total Order (%)" = "avg_ship_share_per_seller",
  "Average Expected Shipping Time (days)" = "avg_expected_ship_time_per_seller",
  "Average Review Score" = "avg_review_score_per_seller")

# Customer metric options (common for all aggregation levels - custmetrics DFs)
customer_metric_options <- c(
  "Total Sales (BRL)" = "total_sales",
  "Total Orders" = "total_orders",
  "Average Order Value (BRL)" = "aov",
  "Average Delivery Time (days)" = "avg_delivery_time",
  "Average Expected Delivery Time (days)" = "avg_expected_delivery_time",
  "Average Shipping Share of Total Order Value (%)" = "avg_ship_share",
  "Total Unique Customers" = "total_unique_customers",
  "Total Repeat Customers" = "total_repeat_customers",
  "Average Review Score" = "avg_review_score")

#...............................................................................DEFINE NON-REACTIVE FUNCTIONS

# Function to choose the appropriate data frame
chooseDataFrame <- function(func_aggregation_input, func_datatype_input) {
  
  # Print the values for debugging
  print(paste("CDF aggregationLevel:", func_aggregation_input))
  print(paste("CDF dataType:", func_datatype_input))
  
  # Define the appropriate data frame
  data <- NULL
  if (func_datatype_input == "seller" && func_aggregation_input == "muni") {
    print("CDF Using muni_map_sf")
    data <- muni_map_sf
  } else if (func_datatype_input == "customer") {
    if (func_aggregation_input == "state") {
      print("CDF Using custmetrics_state_geo")
      data <- custmetrics_state_geo
    }
    else if (func_aggregation_input == "muni") {
      print("CDF Using custmetrics_muni_geo")
      data <- custmetrics_muni_geo
    }
    else if (func_aggregation_input == "zip") {
      print("CDF Using custmetrics_zip_geo")
      data <- custmetrics_zip_geo
    }
  }
  print("Returning data from chooseDataFrame:")
  print(head(data))
  return(data)
}


# Func - get bin ranges for each metric
getGlobalBinRanges <- function(func_datatype_input, func_aggregation_input, func_metric_input) {
  
  bin_info <- list(bin_ranges = list(), bin_labels = list())
  if (func_datatype_input == "seller" && func_aggregation_input == "muni") {
    if (func_metric_input == "total_sales") {
      bin_info$bin_ranges <- list(c(0, 10000), c(10001, 50000), c(50001, 100000), c(100001, 200000), c(200001, 500000), c(500001, 1000000))
      bin_info$bin_labels <- c("R$0-10,000", "R$10,000-50,000", "R$50,000-100,000", "R$100,000-200,000", "R$200,000-500,000", "R$500,000-1,000,000")
    } else if (func_metric_input == "total_sellers") {
      bin_info$bin_ranges <- list(c(5, 10), c(10, 20), c(20, 40), c(40, 50), c(50, 75))
      bin_info$bin_labels <- c("0-5", "5-10", "10-20", "20-40", "40-50", "50-75")
    } else if (func_metric_input == "avg_price_per_seller") {
      bin_info$bin_ranges <- list(c(0, 250), c(250, 500), c(500, 1000), c(1000, 2000), c(2000, 4000))
      bin_info$bin_labels <- c("R$0-250", "R$250-500", "R$500-1,000", "R$1,000-2,000", "R$2,000-4,000")
    } else if (func_metric_input == "total_seller_LTV") {
      bin_info$bin_ranges <- list(c(0, 5000), c(5000, 10000), c(10000, 30000), c(30000, 100000), c(100000, 200000), c(200000, 1000000))
      bin_info$bin_labels <- c("R$0-5,000", "R$5,000-10,000", "R$10,000-30,000", "R$30,000-100,000", "R$100,000-200,000", "R$200,000-1,000,000")
    } else if (func_metric_input == "avg_seller_LTV") {
      bin_info$bin_ranges <- list(c(0, 1000), c(5000, 10000), c(3000, 6000), c(6000, 10000), c(10000, 25000), c(25000, 50000))
      bin_info$bin_labels <- c("R$0-1,000", "R$5,000-10,000", "R$3,000-6,000", "R$6,000-10,000", "R$10,000-25,000", "R$25,000-50,000")
    } else if (func_metric_input == "avg_review_score_per_seller") {
      bin_info$bin_ranges <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 5))
      bin_info$bin_labels <- c("1-2", "2-3", "3-4", "4-5")
    }else if (func_metric_input == "avg_ship_share_per_seller") {
      bin_info$bin_ranges <- list(c(0, 10), c(10, 20), c(25, 50), c(50, 75), c(75, 100), c(100, 200))
      bin_info$bin_labels <- c("0-10%", "10%-20%", "25%-50%", "50%-75%", "75%-100%", "100%-200%")
    } else if (func_metric_input == "avg_expected_ship_time_per_seller") {
      bin_info$bin_ranges <- list(c(0, 10), c(10, 20), c(20, 30), c(30, 40), c(40, 50), c(50, 75))
      bin_info$bin_labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-75")
    }
  }
  if (func_datatype_input == "customer") {
    if (func_aggregation_input == "zip") {
      if (func_metric_input == "total_sales") {
        bin_info$bin_ranges <- list(c(0, 200), c(200, 500), c(500, 1200), c(1200, 3000), c(3000, 10000), c(10000, 22500))
        bin_info$bin_labels <- c("Up to R$200", "R$200 - R$500", "R$500 - R$1.2k", "R$1.2k - R$3k", "R$3k - R$10k", "Over R$10k")
      } else if (func_metric_input == "total_orders") {
        bin_info$bin_ranges <- list(c(0, 3), c(3, 5), c(5, 10), c(10, 20), c(20, 50), c(50, 150))
        bin_info$bin_labels <- c("Up to 3", "4 - 5", "6 - 10", "11 - 20", "21 - 50", "Over 50")
      } else if (func_metric_input == "aov") {
        bin_info$bin_ranges <- list(c(0, 80), c(80, 100), c(100, 130), c(130, 160), c(160, 200), c(200, 2400))
        bin_info$bin_labels <- c("Up to R$80", "R$80 - R$100", "R$100 - R$130", "R$130 - R$160", "R$160 - R$200", "Over R$200")
      } else if (func_metric_input == "avg_delivery_time") {
        bin_info$bin_ranges <- list(c(0, 8), c(8, 12), c(12, 16), c(16, 24), c(24, 35), c(35, 200))
        bin_info$bin_labels <- c("Up to 8 days", "8 - 12 days", "12 - 16 days", "16 - 24 days", "24 - 35 days", "Over 35 days")
      } else if (func_metric_input == "avg_expected_delivery_time") {
        bin_info$bin_ranges <- list(c(0, 20), c(20, 24), c(24, 28), c(28, 32), c(32, 40), c(40, 100))
        bin_info$bin_labels <- c("Up to 20 days", "20 - 24 days", "24 - 28 days", "28 - 32 days", "32 - 40 days", "Over 40 days")
      } else if (func_metric_input == "avg_ship_share") {
        bin_info$bin_ranges <- list(c(0, 20), c(20, 25), c(25, 30), c(30, 40), c(40, 60), c(60, 750))
        bin_info$bin_labels <- c("Up to 20%", "20% - 25%", "25% - 30%", "30% - 40%", "40% - 60%", "Over 60%")
      } else if (func_metric_input == "total_unique_customers") {
        bin_info$bin_ranges <- list(c(0, 3), c(3, 5), c(5, 10), c(10, 20), c(20, 50), c(50, 150))
        bin_info$bin_labels <- c("Up to 3", "4 - 5", "6 - 10", "11 - 20", "21 - 50", "Over 50")
      } else if (func_metric_input == "total_repeat_customers") {
        bin_info$bin_ranges <- list(c(0, 1), c(1, 2), c(2, 5), c(5, 10), c(10, 20), c(20, 30))
        bin_info$bin_labels <- c("0", "1", "2 - 5", "6 - 10", "11 - 20", "Over 20")
      } else if (func_metric_input == "avg_review_score") {
        bin_info$bin_ranges <- list(c(0, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
        bin_info$bin_labels <- c("0 - 2", "2 - 3", "3 - 4", "4 - 4.5", "4.5 - 5")
      }
    }
    
    if (func_aggregation_input == "muni") {
      if (func_metric_input == "total_sales") {
        bin_info$bin_ranges <- list(c(1, 500), c(500, 2000), c(2000, 5000), c(5000, 10000), c(10000, 50000), c(50000, 2000000))
        bin_info$bin_labels <- c("Up to R$500", "R$500 - R$2k", "R$2k - R$5k", "R$5k - R$10k", "R$10k - R$50k", "Over R$50k")
      } else if (func_metric_input == "total_orders") {
        bin_info$bin_ranges <- list(c(0, 3), c(3, 10), c(10, 30), c(30, 100), c(100, 1000), c(1000, 16000))
        bin_info$bin_labels <- c("Up to 3", "4 - 10", "11 - 30", "31 - 100", "101 - 1k", "Over 1k")
      } else if (func_metric_input == "aov") {
        bin_info$bin_ranges <- list(c(0, 80), c(80, 120), c(120, 160), c(160, 200), c(200, 500), c(500, 2300))
        bin_info$bin_labels <- c("Up to R$80", "R$80 - R$120", "R$120 - R$160", "R$160 - R$200", "R$200 - R$500", "Over R$500")
      } else if (func_metric_input == "avg_delivery_time") {
        bin_info$bin_ranges <- list(c(0, 10), c(10, 15), c(15, 20), c(20, 25), c(25, 35), c(35, 120))
        bin_info$bin_labels <- c("Up to 10 days", "10 - 15 days", "15 - 20 days", "20 - 25 days", "25 - 35 days", "Over 35 days")
      } else if (func_metric_input == "avg_expected_delivery_time") {
        bin_info$bin_ranges <- list(c(5, 20), c(20, 25), c(25, 30), c(30, 40), c(40, 60), c(60, 90))
        bin_info$bin_labels <- c("5 - 20 days", "20 - 25 days", "25 - 30 days", "30 - 40 days", "40 - 60 days", "60 - 90 days")
      } else if (func_metric_input == "avg_ship_share") {
        bin_info$bin_ranges <- list(c(0, 15), c(15, 25), c(25, 35), c(35, 50), c(50, 75), c(75, 450))
        bin_info$bin_labels <- c("Up to 15%", "15% - 25%", "25% - 35%", "35% - 50%", "50% - 75%", "Over 75%")
      } else if (func_metric_input == "total_unique_customers") {
        bin_info$bin_ranges <- list(c(0, 3), c(3, 10), c(10, 20), c(20, 50), c(50, 100), c(100, 16000))
        bin_info$bin_labels <- c("Up to 3", "4 - 10", "11 - 20", "21 - 50", "51 - 100", "Over 100")
      } else if (func_metric_input == "total_repeat_customers") {
        bin_info$bin_ranges <- list(c(0, 1), c(1, 2), c(2, 5), c(5, 10), c(10, 50), c(50, 2000))
        bin_info$bin_labels <- c("0", "1", "2 - 5", "6 - 10", "11 - 50", "Over 50")
      } else if (func_metric_input == "avg_review_score") {
        bin_info$bin_ranges <- list(c(0, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
        bin_info$bin_labels <- c("0 - 2", "2 - 3", "3 - 4", "4 - 4.5", "4.5 - 5")
      }
    }
    
    if (func_aggregation_input == "state") {
      if (func_metric_input == "total_sales") {
        bin_info$bin_ranges <- list(c(0, 100000), c(100000, 250000), c(250000, 500000), c(500000, 1000000), c(1000000, 3000000), c(3000000, 5200000))
        bin_info$bin_labels <- c("Up to R$100k", "R$100k - R$250k", "R$250k - R$500k", "R$500k - R$1M", "R$1M - R$3M", "Over R$3M")
      } else if (func_metric_input == "total_orders") {
        bin_info$bin_ranges <- list(c(0, 500), c(500, 1000), c(1000, 2500), c(2500, 5000), c(5000, 20000), c(20000, 42000))
        bin_info$bin_labels <- c("Up to 500", "500 - 1k", "1k - 2.5k", "2.5k - 5k", "5k - 20k", "Over 20k")
      } else if (func_metric_input == "aov") {
        bin_info$bin_ranges <- list(c(0, 140), c(140, 160), c(160, 180), c(180, 200), c(200, 215))
        bin_info$bin_labels <- c("Up to R$140", "R$140 - R$160", "R$160 - R$180", "R$180 - R$200", "Over R$200")
      } else if (func_metric_input == "avg_delivery_time") {
        bin_info$bin_ranges <- list(c(0, 15), c(15, 18), c(18, 21), c(21, 25), c(25, 30), c(30, 35))
        bin_info$bin_labels <- c("Up to 15 days", "15 - 18 days", "18 - 21 days", "21 - 25 days", "25 - 30 days", "Over 30 days")
      } else if (func_metric_input == "avg_expected_delivery_time") {
        bin_info$bin_ranges <- list(c(0, 25), c(25, 28), c(28, 31), c(31, 35), c(35, 40), c(40, 50))
        bin_info$bin_labels <- c("Up to 25 days", "25 - 28 days", "28 - 31 days", "31 - 35 days", "35 - 40 days", "Over 40 days")
      } else if (func_metric_input == "avg_ship_share") {
        bin_info$bin_ranges <- list(c(0, 17), c(17, 20), c(20, 23), c(23, 26), c(26, 30))
        bin_info$bin_labels <- c("Up to 17%", "17% - 20%", "20% - 23%", "23% - 26%", "Over 26%")
      } else if (func_metric_input == "total_unique_customers") {
        bin_info$bin_ranges <- list(c(0, 400), c(400, 900), c(900, 2300), c(2300, 4000), c(4000, 20000), c(20000, 42000))
        bin_info$bin_labels <- c("Up to 400", "400 - 900", "900 - 2.3k", "2.3k - 4k", "4k - 20k", "Over 20k")
      } else if (func_metric_input == "total_repeat_customers") {
        bin_info$bin_ranges <- list(c(0, 30), c(30, 100), c(100, 300), c(300, 1000), c(1000, 3000), c(3000, 5000))
        bin_info$bin_labels <- c("Up to 30", "30 - 100", "100 - 300", "300 - 1k", "1k - 3k", "Over 3k")
      } else if (func_metric_input == "avg_review_score") {
        bin_info$bin_ranges <- list(c(0, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
        bin_info$bin_labels <- c("0 - 2", "2 - 3", "3 - 4", "4 - 4.5", "4.5 - 5")
      }
    } 
      print(paste("CGBR Bin info:", bin_info))
      return(bin_info)
    }
  }


# Function to get the region name based on aggregation level
getRegionName <- function(func_datatype_input, func_aggregation_input, feature) {
  if (func_aggregation_input == "state") {
    return(feature$name_state)
  } else if (func_aggregation_input == "muni") {
    # Different column names for seller and customer data
    if (func_datatype_input == "seller") {
      return(feature$name_muni)  # For seller data
    } else {
      return(feature$muni_name)  # For customer data
    }
  } else if (func_aggregation_input == "zip") {
    return(feature$customer_zip_code_prefix)
  }
}


# Function to get the metric value based on data type
getMetricValue <- function(func_metric_input, feature) {
  return(feature[[func_metric_input]])
}


# Function to check if the metric is a currency metric
isCurrencyMetric <- function(func_metric_input) {
  # Add more currency metrics if needed
  currency_metrics <-
    c(
      "total_sales",
      "avg_price_per_seller",
      "total_seller_LTV",
      "avg_seller_LTV",
      "aov"
    )
  return(func_metric_input %in% currency_metrics)
}

#............................................................................... UI

# UI
ui <- fluidPage(
  titlePanel("Brazilian E-commerce Data from Olist"),
  
  sidebarLayout(
    sidebarPanel(
      # Data Type Selector
      selectInput("dataType", "Data Type:",
                  choices = c("Seller" = "seller", "Customer" = "customer"), selected = "Seller"),
      
      # Metric Selector
      selectInput("metricSelector", "Metric:", choices = seller_muni_metric_options, selected = "Total Sellers"),
      
      # Aggregation Level Selector
      selectInput("aggregationLevel", "Aggregation Level:",
                  choices = c("State" = "state", "Municipality" = "muni", "Zip Code" = "zip"), selected = "Municipality"),
      
      # Checkbox for 'Select All'
      checkboxInput("selectAll", "Select All", value = TRUE),
      
      # Dynamic UI Output for Bin Selection Checkboxes
      uiOutput("checkboxesUI")
    ),
    mainPanel(
      
      leafletOutput("map")
      
    )
  )
)

#............................................................................... server

server <- function(input, output, session) {
  
  # Capture User Inputs
  selected_metric <- reactive({input$metricSelector})
  selected_aggregation <- reactive({input$aggregationLevel})
  selected_data_type <- reactive({input$dataType})
  
  observe({
    print(paste("Observed DataType:", selected_data_type()))
    print(paste("Observed Metric:", selected_metric()))
    print(paste("Observed Aggregation Level:", selected_aggregation()))
  })
  
  # Leaflet map creation 
  createOlistMap <- function(data, func_metric_input, func_datatype_input, func_aggregation_input) {
    if (!inherits(data, "sf")) {
      stop("Data must be an sf object")
    }
    
    # Retrieve bin information
    bin_info <- getGlobalBinRanges(selected_data_type(), selected_aggregation(), selected_metric())
    bin_ranges_vector <- unlist(bin_info$bin_ranges)
    bin_labels <- bin_info$bin_labels
    
    # Ensure the bin ranges are unique and sorted
    unique_bin_ranges <- sort(unique(bin_ranges_vector))
    
    if (!is.null(unique_bin_ranges) && !is.null(bin_labels)) {
      
      # Create color palette
      pal <- colorBin(
        palette = "viridis",
        domain = range(unique_bin_ranges),  # Use the range of unique bin ranges
        bins = unique_bin_ranges,
        na.color = "transparent"
      )
      
      # Print the unique bin ranges for debugging
      print(paste("Unique bin ranges:", unique_bin_ranges))
      
      
      # Diagnostic: Check if any data values fall outside the bin ranges
      metric_values <- data[[func_metric_input]]
      outside_bin_range <- metric_values < min(unique_bin_ranges) | metric_values > max(unique_bin_ranges)
      print(paste("Values outside bin ranges:", sum(outside_bin_range, na.rm = TRUE)))
      
      # Create the leaflet map
      lft <- leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~ pal(metric_values),
          color = "transparent",
          fillOpacity = 0.7
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = metric_values,
          labels = bin_labels,
          title = func_metric_input
        )
      
      return(lft) 
    } else {
      print("The 'bin_ranges' and 'bin_labels' are NULL!")
    }
  }
  
  
  
  # DEFINE REACTIVE FUNCTIONS
  # dynamicPopupContent function (gets content for text popup when user scrolls over a region on the map)
  dynamicPopupContent <- function(func_metric_input,
                                  func_aggregation_input,
                                  func_datatype_input,
                                  feature) {
    
    # print("####################")
    # print(feature)
    
    region_name <- getRegionName(selected_aggregation(), feature)
    metric_value <- getMetricValue(selected_metric(), feature)
    
    # Format the metric value based on the metric type
    if (isCurrencyMetric(selected_metric())) {
      formatted_metric <- paste("R$", round(metric_value, 2))
    } else {
      formatted_metric <- format(metric_value, big.mark = ",")
    }
    
    popup_content <- paste(region_name, formatted_metric)
    
    return(popup_content)
  }
  
  
  # Observer to update selected_metric choices based on data type
  observe({
    if (selected_data_type() == "seller") {
      updateSelectInput(session, "metricSelector", choices = seller_muni_metric_options)
    } else {
      updateSelectInput(session, "metricSelector", choices = customer_metric_options)
    }
  })
  
  observe({print(paste("Updated Metric Selector to", input$metricSelector))})
  
  # Filter Data Based on User Selection
  filtered_data <- reactive({
    # Retrieve bin information for the selected selected_metric, data type, and aggregation level
    bin_info <- getGlobalBinRanges(selected_data_type(), selected_aggregation(), selected_metric())
    
    # Debugging: print bin ranges and labels
    print(paste("Bin Ranges:", toString(bin_info$bin_ranges)))
    print(paste("Bin Labels:", toString(bin_info$bin_labels)))
    
    # Extract the selected bins from user input
    selected_bins <- input$selectedBins
    
    # Debugging: print the values being sent to chooseDataFrame
    print(paste("Calling chooseDataFrame with DataType:", selected_data_type()))
    print(paste("Calling chooseDataFrame with Metric:", selected_metric()))
    print(paste("Calling chooseDataFrame with Aggregation Level:", selected_aggregation()))
    
    # Call the function with the selected values
    chosen_data <- chooseDataFrame(selected_aggregation(), selected_data_type())
    
    # Debugging: Check if chosen_data is NULL
    if (is.null(chosen_data)) {
      print("chooseDataFrame returned NULL")
    } else {
      print("chooseDataFrame returned data")
    }
    
    return(chosen_data)
  })
  
  
  # Debugging selectedBins
  observe({
    print(paste("Selected Bins:", input$selectedBins))
  })
  
  
  # Add the print statements here
  observe({
    fd <- filtered_data()
    print("Filtered Data:")
    print(head(fd))
  })
  
  
  # Dynamically generate checkboxes based on selected metric
  output$checkboxesUI <- renderUI({
    # Get bin information based on the current selection
    bin_info <-
      getGlobalBinRanges(selected_data_type(), selected_aggregation(), selected_metric())
    
    print(bin_info$bin_labels)
    
    # Create checkbox group with the labels from bin_info
    checkboxGroupInput(
      "selectedBins",
      "Select Bins:",
      choices = bin_info$bin_labels,
      selected = bin_info$bin_labels
    )
  })
  
  
  # Handle "Select All" functionality
  observeEvent(input$selectAll, {
    # Get all available bins for the current metric
    all_bins <- getGlobalBinRanges(selected_metric(), selected_aggregation(), selected_data_type())
    
    if (input$selectAll) {
      # If "Select All" is checked, select all bins
      updateCheckboxGroupInput(session, "selectedBins", selected = all_bins)
    } else {
      # If "Select All" is unchecked, deselect all bins
      updateCheckboxGroupInput(session, "selectedBins", selected = character(0))
    }
  }, ignoreNULL = FALSE)
  
  
  shiny::observeEvent(c(filtered_data()), {
    
    print("===============================")
    print(shiny::isTruthy(filtered_data()))
    
    if (shiny::isTruthy(filtered_data())) {
      
      data <- filtered_data()
      
      print("Data in renderLeaflet before calling createOlistMap:")
      print(data)
      # print(head(data))
      if (!is.null(data)) {
        if (inherits(data, "sf")) {
          
          # Output the Map: Replace or modify your existing map rendering code
          output$map <- renderLeaflet({
            createOlistMap(data, selected_metric(), selected_data_type(), selected_aggregation())
          })
        }
      } else {
        print("Data is either NULL or not an sf object in renderLeaflet.")
      }
    }
    
  })
  
  # # Output the Map: Replace or modify your existing map rendering code
  # output$map <- renderLeaflet({
  # 
  #   data <- filtered_data()
  # 
  #   print("Data in renderLeaflet before calling createOlistMap:")
  #   print(data)
  #   # print(head(data))
  #   if (!is.null(data)) {
  #     if (!inherits(data, "sf")) {
  #       createOlistMap(data, selected_metric(), selected_data_type(), selected_aggregation())
  #     }
  #   } else {
  #     print("Data is either NULL or not an sf object in renderLeaflet.")
  #   }
  # })
  
}




# Run the application
shinyApp(ui, server)
