function(input, output, session) {
  
  ##...............................................................................update user choices
  # Observer to update selected_metric choices based on data type
  ## consolidated choice updates and converted to observeEvent to be more conservative
  observeEvent(input$dataType, {
    bounds <<- currentBounds() ## update current bounds on input change
    if (input$dataType == "seller") {
      updateSelectInput(session, "metricSelector", choices = seller_muni_metric_options, select = 'total_sales')
      print(1.1)
      updateSelectInput(session, "aggregationLevel", choices = sell_choices, select = "state")
      print(2.1)
    }
    else {
      updateSelectInput(session, "metricSelector", choices = customer_metric_options, select = 'total_sales')
      print(1.2)
      updateSelectInput(session, "aggregationLevel", choices = cust_choices, select = "state")
      print(2.2)
    }
    
    bin_info_control <<- getGlobalBinRanges(input$dataType, input$aggregationLevel, input$metricSelector)
    updateCheckboxGroupInput(session, "selectedBins", 
                             choiceNames = bin_info_control$bin_labels,
                             choiceValues = bin_info_control$bin_ranges,
                             selected = bin_info_control$bin_ranges)
    # print(3)
  })

  ##...............................................................................update based on user choices
  ## update bin checkboxes with different metric
  ## also update selectAll
  observeEvent(input$metricSelector, {
    bounds <<- currentBounds() ## update current bounds on input change
    bin_info <- getGlobalBinRanges(input$dataType, input$aggregationLevel, input$metricSelector)
    if (!setequal(bin_info_control, bin_info)) {
      updateCheckboxGroupInput(session, "selectedBins", 
                               choiceNames = bin_info$bin_labels,
                               choiceValues = bin_info$bin_ranges,
                               selected = bin_info$bin_ranges)
      # print("3-1")
      bin_info_control <<- bin_info
    }
    # updateCheckboxInput(session, "selectAll", value = TRUE)
    # print("0-1")
  })

  ## update bin checkboxes with different aggregation
  ## also update selectAll
  observeEvent(input$aggregationLevel, {
    bounds <<- currentBounds() ## update current bounds on input change
    bin_info <- getGlobalBinRanges(input$dataType, input$aggregationLevel, input$metricSelector)
    if (!setequal(bin_info_control, bin_info)) {
      updateCheckboxGroupInput(session, "selectedBins", 
                               choiceNames = bin_info$bin_labels,
                               choiceValues = bin_info$bin_ranges,
                               selected = bin_info$bin_ranges)
      # print("3-2")
      bin_info_control <<- bin_info
    }
    # updateCheckboxInput(session, "selectAll", value = TRUE)
    # print("0-2")
  })
  
  ##...............................................................................selectAll functionality
  ## update selectAll with bin selection 
  ## - select selectAll if all bins are selected, otherwise unselect selectAll
  observeEvent(input$selectedBins, {
    bounds <<- currentBounds() ## update current bounds on input change
    bin_info <- getGlobalBinRanges(input$dataType, input$aggregationLevel, input$metricSelector)
    if (!input$selectAll && (length(input$selectedBins) == length(bin_info$bin_labels))) {
      updateCheckboxInput(session, "selectAll", value = TRUE)
      # print("0-3")
    } 
    if (input$selectAll && (length(input$selectedBins) != length(bin_info$bin_labels))) {
      updateCheckboxInput(session, "selectAll", value = FALSE)
      # print("0-4")
    }
  })

  # Handle "Select All" functionality
  ## understanding how this observeEvent interacts with the above is key to making it work well
  observeEvent(input$selectAll, {
    bin_info <- getGlobalBinRanges(input$dataType, input$aggregationLevel, input$metricSelector)
    if (input$selectAll && (length(input$selectedBins) != length(bin_info$bin_labels))) {
      # If "Select All" is checked, select all bins
      updateCheckboxGroupInput(session, "selectedBins",
                               choiceNames = bin_info$bin_labels,
                               choiceValues = bin_info$bin_ranges,
                               selected = bin_info$bin_ranges)
      # print("3-3")
    }
    ## if all but one bin is selected, unselect "Select All"
    else if (length(input$selectedBins) == (length(bin_info$bin_labels) - 1)) {
      ## do nothing here since functionality is covered above
      ## this is a placeholder to prevent all bin selections from disappearing if one is unselected
      # updateCheckboxInput(session, "selectAll", value = FALSE)
      # print("0-5")
    }
    else if (!input$selectAll) {
      # If "Select All" is unchecked, deselect all bins
      updateCheckboxGroupInput(session, "selectedBins", selected = character(0))
      # print("0-6")
    }
  }, ignoreNULL = FALSE)

  ##...............................................................................filtering data
  
  # Filter Data Based on User Selection
  ## this section only chose the data set to use and did not filter for anything
  ## added in data filtering functionality
  filtered_data <- reactive({
    print("data filter start")
    
    ## extract the selected metric and bins from user input
    metric <- input$metricSelector
    selected_bins <- input$selectedBins

    # Call the function with the selected values
    chosen_data <- chooseDataFrame(input$aggregationLevel, input$dataType)

    ## function to check if a value falls within any of the ranges
    is_within_ranges <- function(value, ranges) {
      any(sapply(ranges, function(range) value > range[1] && value <= range[2]))
    }

    ## convert the list of selected ranges into a list of numeric vectors
    bins <- lapply(selected_bins, function(bin) eval(parse(text = bin)))

    ## apply the function to create a logical vector
    rows_to_keep <- sapply(chosen_data[[metric]], is_within_ranges, ranges = bins)

    print("data filter end")

    ## filter the dataframe
    return(chosen_data[rows_to_keep, ])
  })

  ##...............................................................................rendering Leaflet

  ## reactive value for storing initial bounds
  initialBounds <- reactiveValues(
    lng1 = -73, ## west
    lat1 = -34, ## south
    lng2 = -34, ## east
    lat2 = 5 ## north
  )
  
  ## reactive expression to store the current map bounds
  currentBounds <- reactive({
    if (!is.null(input$map_bounds)) {
      bounds <- input$map_bounds
      initialBounds$lng1 <- bounds$west
      initialBounds$lat1 <- bounds$south
      initialBounds$lng2 <- bounds$east
      initialBounds$lat2 <- bounds$north
    }
    return(c(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2))
  })
    
  # ## test
  # toListen <- reactive({
  #   list(input$dataType, input$metricSelector, input$aggregationLevel, input$selectedBins)
  # })
  
  ## render map in response to selectedBins options
  # observeEvent(toListen(), {
  # observeEvent(input$selectedBins, { ## the coordinates only stay if you change the bins
  
  # observe({ ## the coordinates stay with any change but the app crashes when moving from seller <-> customer
  observeEvent(input$renderMap,{ ## the coordinates stay with any change but the app crashes when moving from seller <-> customer
      
    ## choosing datatype/metric/aggregation causes 2x data filter + render
    
    
      # Ensure the bin ranges are unique and sorted
      ## consolidated code to make it easier to organize
      unique_bin_ranges <- sort(unique(unlist(bin_info_control$bin_ranges)))

      # Create color palette
      pal <- colorBin(
        palette = "viridis",
        domain = range(unique_bin_ranges),  # Use the range of unique bin ranges
        bins = unique_bin_ranges,
        na.color = "transparent"
      )

      #get labels correct
      #equation for popups
      dynamicPopupContent <- function(func_metric_input,
                                      func_aggregation_input,
                                      func_datatype_input,
                                      data) {
        region_name <- getRegionName(func_datatype_input,func_aggregation_input, data)
        metric_values <- data[[input$metricSelector]]

        # Format the metric value based on the metric type
        if (isCurrencyMetric(input$metricSelector)) { ## updated code
          formatted_metric <- paste("R$", round(metric_values, 2))
        } else {
          formatted_metric <- format(round(metric_values, 2), big.mark = ",")
        }

        popup_content <- paste(region_name, formatted_metric, sep = ":  ")

        return(popup_content)
      }

      #calculate
      popup_content <- dynamicPopupContent(input$metricSelector, input$aggregationLevel, input$dataType, filtered_data())
      metric_values <- filtered_data()[[input$metricSelector]]
      
      ## render the leaflet map
      output$map <- renderLeaflet({
        
        print('render leaflet start')
        
        leaflet(filtered_data()) %>%
          addProviderTiles(providers$CartoDB.Positron,  layerId = "tiles") %>%
          fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4]) %>%
          addPolygons(
            # data = filtered_data(),
            fillColor = ~pal(metric_values),
            color = "transparent",
            fillOpacity = 0.7,
            popup = ~popup_content,
            popupOptions = popupOptions(freezeAtZoom = T),
            group = "poly"
          ) %>%
          addScaleBar() %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = ~metric_values,
            labels = bin_info_control$bin_labels,
            title = input$metricSelector
          )
        
      })
      
    })

  }