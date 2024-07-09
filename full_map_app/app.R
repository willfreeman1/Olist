library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Server logic
options(shiny.error = 'browser') # for debugging


#............................................................................... Load data
muni_map_sf = readRDS(file.path("data", "muni_map_sf.rds"))
custmetrics_state_geo = readRDS(file.path("data", "custmetrics_state_geo.rds"))
custmetrics_muni_geo = readRDS(file.path("data", "custmetrics_muni_geo.rds"))  
custmetrics_zip_geo = readRDS(file.path("data", "custmetrics_zip_geo.rds"))

# Check that data is the correct SF format
if (!inherits(muni_map_sf, "sf")) {
  muni_map_sf <- st_as_sf(muni_map_sf, wkt = "geometry_column")
}

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
        bin_info$bin_ranges <- list(c(5, 200), c(200, 500), c(500, 1150), c(1150, 2000), c(2000, 5000), c(5000, 22200))
        bin_info$bin_labels <- c("R$5-200", "R$200-500", "R$500-1,150", "R$1,150-2,000", "R$2,000-5,000", "R$5,000-22,200")
      } else if (func_metric_input == "total_orders") {
        bin_info$bin_ranges <- list(c(0, 5), c(5, 10), c(10, 25), c(25, 50), c(50, 100), c(100, 150))
        bin_info$bin_labels <- c("0-5", "5-10", "10-25", "25-50", "50-100", "100-150")
      } else if (func_metric_input == "aov") {
        bin_info$bin_ranges <- list(c(3, 80), c(80, 120), c(120, 160), c(160, 200), c(200, 500), c(500, 2400))
        bin_info$bin_labels <- c("R$3-80", "R$80-120", "R$120-160", "R$160-200", "R$200-500", "R$500-2,400")
      } else if (func_metric_input == "avg_delivery_time") {
        bin_info$bin_ranges <- list(c(1, 10), c(10, 15), c(15, 20), c(20, 50), c(50, 100), c(100, 200))
        bin_info$bin_labels <- c("1-10", "10-15", "15-20", "20-50", "50-100", "100-200")
      } else if (func_metric_input == "avg_expected_delivery_time") {
        bin_info$bin_ranges <- list(c(2, 20), c(20, 25), c(25, 30), c(30, 50), c(50, 70), c(70, 100))
        bin_info$bin_labels <- c("2-20", "20-25", "25-30", "30-50", "50-70", "70-100")
      } else if (func_metric_input == "avg_ship_share") {
        bin_info$bin_ranges <- list(c(0, 25), c(25, 30), c(30, 40), c(40, 50), c(50, 100), c(100, 750))
        bin_info$bin_labels <- c("0-25", "25-30", "30-40", "40-50", "50-100", "100-750")
      } else if (func_metric_input == "total_unique_customers") {
        bin_info$bin_ranges <- list(c(1, 3), c(3, 5), c(5, 10), c(10, 20), c(20, 50), c(50, 150))
        bin_info$bin_labels <- c("1-3", "3-5", "5-10", "10-20", "20-50", "50-150")
      } else if (func_metric_input == "total_repeat_customers") {
        bin_info$bin_ranges <- list(c(0, 2), c(3, 6), c(7, 10), c(10, 20), c(20, 30))
        bin_info$bin_labels <- c("0-2", "3-6", "7-10", "10-20", "20-30")
      } else if (func_metric_input == "avg_review_score") {
        bin_info$bin_ranges <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
        bin_info$bin_labels <- c("1-2", "2-3", "3-4", "4-4.5", "4.5-5")
      }
    }
    if (func_aggregation_input == "muni") {
      if (func_metric_input == "total_sales") {
        bin_info$bin_ranges <- list(c(6, 200), c(200, 500), c(500, 1500), c(1500, 3000), c(3000, 10000), c(10000, 1913000))
        bin_info$bin_labels <- c("R$6-R$200", "R$200-R$500", "R$500-R$1,500", "R$1,500-R$3,000", "R$3,000-R$10,000", "R$10,000-R$1,913,000")
      } else if (func_metric_input == "total_orders") {
        bin_info$bin_ranges <- list(c(1, 4), c(4, 10), c(10, 20), c(20, 50), c(50, 100), c(100, 15400))
        bin_info$bin_labels <- c("1-4", "4-10", "10-20", "20-50", "50-100", "100-15,400")
      } else if (func_metric_input == "aov") {
        bin_info$bin_ranges <- list(c(5, 85), c(85, 125), c(125, 170), c(170, 200), c(200, 500), c(500, 2200))
        bin_info$bin_labels <- c("R$5-R$85", "R$85-R$125", "R$125-R$170", "R$170-R$200", "R$200-R$500", "R$500-R$2,200")
      } else if (func_metric_input == "avg_delivery_time") {
        bin_info$bin_ranges <- list(c(0, 12), c(12, 15), c(15, 20), c(20, 30), c(30, 60), c(60, 110))
        bin_info$bin_labels <- c("0-12", "12-15", "15-20", "20-30", "30-60", "60-110")
      } else if (func_metric_input == "avg_expected_delivery_time") {
        bin_info$bin_ranges <- list(c(8, 25), c(25, 30), c(30, 35), c(35, 50), c(50, 70), c(70, 90))
        bin_info$bin_labels <- c("8-25", "25-30", "30-35", "35-50", "50-70", "70-90")
      } else if (func_metric_input == "avg_ship_share") {
        bin_info$bin_ranges <- list(c(0, 20), c(20, 25), c(25, 35), c(35, 50), c(50, 100), c(100, 430))
        bin_info$bin_labels <- c("0-20", "20-25", "25-35", "35-50", "50-100", "100-430")
      } else if (func_metric_input == "total_unique_customers") {
        bin_info$bin_ranges <- list(c(1, 4), c(4, 10), c(10, 20), c(20, 50), c(50, 100), c(100, 15400))
        bin_info$bin_labels <- c("1-4", "4-10", "10-20", "20-50", "50-100", "100-15,400")
      } else if (func_metric_input == "total_repeat_customers") {
        bin_info$bin_ranges <- list(c(0, 1), c(1, 2), c(2, 5), c(5, 10), c(10, 1960))
        bin_info$bin_labels <- c("0-1", "1-2", "2-5", "5-10", "10-1960")
      } else if (func_metric_input == "avg_review_score") {
        bin_info$bin_ranges <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
        bin_info$bin_labels <- c("1-2", "2-3", "3-4", "4-4.5", "4.5-5")
      }
    }
    if (func_aggregation_input == "state") {
      if (func_metric_input == "total_sales") {
        bin_info$bin_ranges <- list(c(8000, 80000), c(80000, 170000), c(170000, 350000), c(350000, 500000), c(500000, 1000000), c(1000000, 5180000))
        bin_info$bin_labels <- c("R$8,000-R$80,000", "R$80,000-R$170,000", "R$170,000-R$350,000", "R$350,000-R$500,000", "R$500,000-R$1,000,000", "R$1,000,000-R$5,180,000")
      } else if (func_metric_input == "total_orders") {
        bin_info$bin_ranges <- list(c(50, 400), c(400, 1000), c(1000, 2500), c(2500, 5000), c(5000, 10000), c(10000, 41200))
        bin_info$bin_labels <- c("50-400", "400-1,000", "1,000-2,500", "2,500-5,000", "5,000-10,000", "10,000-41,200")
      } else if (func_metric_input == "aov") {
        bin_info$bin_ranges <- list(c(130, 150), c(150, 165), c(165, 180), c(180, 190), c(190, 215))
        bin_info$bin_labels <- c("R$130-R$150", "R$150-R$165", "R$165-R$180", "R$180-R$190", "R$190-R$215")
      } else if (func_metric_input == "avg_delivery_time") {
        bin_info$bin_ranges <- list(c(9, 16), c(16, 19), c(19, 22), c(22, 25), c(25, 30))
        bin_info$bin_labels <- c("9-16", "16-19", "19-22", "22-25", "25-30")
      } else if (func_metric_input == "avg_expected_delivery_time") {
        bin_info$bin_ranges <- list(c(20, 27), c(27, 31), c(31, 33), c(33, 40), c(40, 47))
        bin_info$bin_labels <- c("20-27", "27-31", "31-33", "33-40", "40-47")
      } else if (func_metric_input == "avg_ship_share") {
        bin_info$bin_ranges <- list(c(14, 18), c(18, 21), c(21, 24), c(24, 26), c(26, 29))
        bin_info$bin_labels <- c("14-18", "18-21", "21-24", "24-26", "26-29")
      } else if (func_metric_input == "total_unique_customers") {
        bin_info$bin_ranges <- list(c(50, 400), c(400, 1000), c(1000, 2500), c(2500, 5000), c(5000, 10000), c(10000, 41200))
        bin_info$bin_labels <- c("50-400", "400-1,000", "1,000-2,500", "2,500-5,000", "5,000-10,000", "10,000-41,200")
      } else if (func_metric_input == "total_repeat_customers") {
        bin_info$bin_ranges <- list(c(10, 50), c(50, 150), c(150, 300), c(300, 500), c(500, 1000), c(1000, 5100))
        bin_info$bin_labels <- c("10-50", "50-150", "150-300", "300-500", "500-1,000", "1,000-5,100")
      } else if (func_metric_input == "avg_review_score") {
        bin_info$bin_ranges <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
        bin_info$bin_labels <- c("1-2", "2-3", "3-4", "4-4.5", "4.5-5")
      }
      print(paste("CGBR Bin info:", bin_info))
      return(bin_info)
    }
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



server <- function(input, output, session) {
  
  # Capture User Inputs
  selected_metric <- reactive({input$metricSelector})
  selected_aggregation <- reactive({input$aggregationLevel})
  selected_data_type <- reactive({input$dataType})
  
  # Leaflet map creation function (creates the leaflet map from filtered data)
  createOlistMap <- function(data, func_metric_input, func_datatype_input,func_aggregation_input) {
    if (!inherits(data, "sf")) {
      stop("Data must be an sf object")
    }
    
    # Retrieve bin information for the selected metric, data type, and aggregation level
    bin_info <- getGlobalBinRanges(selected_data_type(),selected_aggregation(), selected_metric())
    
    # Extract bin ranges and labels from bin_info
    bin_ranges <- bin_info$bin_ranges
    bin_ranges <- as.vector(unlist(lapply(bin_ranges, function(x) x[2])))         # I modified this and kept the second item of each sublist
    
    bin_labels <- bin_info$bin_labels
    
    if (!is.null(bin_ranges) & !is.null(bin_labels)) {
      
      # Create color palette
      pal <-
        colorBin(
          palette = "viridis",
          domain = unlist(bin_ranges),
          bins = unlist(bin_ranges),
          na.color = "transparent"
        )
      
      print(list(bin_ranges = bin_ranges, bin_labels = bin_labels))
      
      # Create the leaflet map with dynamic popups and labels
      lft = leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~ pal(get(func_metric_input)),
          color = "transparent",
          fillOpacity = 0.7
          # popup = ~ dynamicPopupContent(selected_metric(), 
          #                               selected_aggregation(), 
          #                               selected_data_type(), .)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~ get(func_metric_input),
          labels = bin_labels,
          title = selected_metric()
          # title = metric
        )
      
      return(lft) 
    } else {
      print(message("The 'bin_ranges' and 'bin_labels' are NULL!")) 
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
  
  
  # Debugging prints to observe changes
  observe({
    print(paste("DB-Data type:", input$dataType))
    print(paste("DB-Metric:", input$metricSelector))
    print(paste("DB-Aggregation Level:", input$aggregationLevel))
  })
  
  
  
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
    
    # Add the print statements here
    print("B2 Bin Ranges:")
    print(bin_info$bin_ranges)
    print("B2 Bin Labels:")
    print(bin_info$bin_labels)
    
    # Extract the selected bins from user input
    selected_bins <- input$selectedBins
    
    # Filter the data based on the selected_metric, aggregation level, data type, and selected bins
    chooseDataFrame(selected_aggregation(), selected_data_type())
  })
  
  print(paste("B2",head(data)))
  
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
