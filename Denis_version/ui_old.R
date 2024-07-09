fluidPage(
  
  titlePanel("Brazilian E-commerce Data from Olist"),
  
  sidebarLayout(
    sidebarPanel(
      # Data Type Selector
      selectInput("dataType", "Data Type:",
                  choices = c("Seller" = "seller", "Customer" = "customer"), selected = "seller"),
      
      # Metric Selector
      selectInput("metricSelector", "Metric:", choices = seller_muni_metric_options, selected = "total_seller"),
      
      # Aggregation Level Selector
      selectInput("aggregationLevel", "Aggregation Level:",
                  choices = sell_choices, selected = "muni"),
      
      # Checkbox for 'Select All'
      checkboxInput("selectAll", "Select All", value = TRUE),
      
      # Dynamic UI Output for Bin Selection Checkboxes
      ## replaced uiOutput with checkboxGroupInput
      checkboxGroupInput("selectedBins", "Select Bins:",
                         choiceNames = getGlobalBinRanges("seller", "muni", "total_sales")$bin_labels, 
                         choiceValues = getGlobalBinRanges("seller", "muni", "total_sales")$bin_ranges, 
                         selected = getGlobalBinRanges("seller", "muni", "total_sales")$bin_ranges),
      
      # Add render map button
      actionButton("renderMap", "Render Map")
      
      
    ),
    
    mainPanel(
    leafletOutput("map", width = "100%", height = "800px")
    )
    )
  )