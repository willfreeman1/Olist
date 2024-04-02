##! remove all print statements when done since they were inserted for debugging and flow tracking
##! remove all commented code since they were deemed extra
library(rlang)
library(ggplot2)
function(input, output, session) {
  
  #..................................DATA INSIGHTS TAB.....................................................................
  
  # Reactive expression to update chartType options based on dashboardType
  observe({
    chartOptions <- switch(input$dashboardType,
                           "Headline Data" = list("Olist & Seller Performance Index" = "Olist & Seller Performance Index", 
                                                  "Revenue with Forecast" = "Revenue with Forecast", 
                                                  "Orders vs AOV" = "Orders vs AOV", "Subscriptions vs Sales" = "Subscriptions vs Sales"),
                           "Seller Data" = list("Sellers New vs Old" = "Sellers New vs Old", 
                                                "Seller Churn" = "Seller Churn", 
                                                "Revenue by Product Category" = "Revenue by Product Category"),
                           "Shipping Data" = list("Monthly Delivery Times" = "Monthly Delivery Times", 
                                                  "Shipping Days vs Revenue" = "Shipping Days vs Revenue",
                                                  "Avg Freight Over Time" = "Avg Freight Over Time"),
                           "Customer Data" = list("New vs Repeat Customers" = "New vs Repeat Customers", 
                                                  "Urban vs Rural Customers" = "Urban vs Rural Customers",
                                                  "Delivery Review Scores" = "Delivery Review Scores")
    )
    updateSelectInput(session, "chartType", choices = chartOptions)
    print(paste("Updated chart options for dashboard type:", input$dashboardType))
  })
  
  # Dynamic plot rendering based on dashboard and chart type
  output$chartPlaceholder <- renderPlot({
    req(input$dashboardType, input$chartType) 
    print(paste("Rendering plot for Dashboard:", input$dashboardType, "Chart:", input$chartType))
    
    # Initialize an empty plot object
    p <- NULL
    
    # Check for "Olist & Seller Performance Index" chart
    if (input$dashboardType == "Headline Data" && input$chartType == "Olist & Seller Performance Index") {
      print("Filtering data for Olist & Seller Performance Index chart.")
      filtered_data <- index_summary_df %>%
        filter(month >= input$dateRange[1] & month <= input$dateRange[2])
      print(paste("Number of rows in filtered data:", nrow(filtered_data)))
      
      p <- ggplot(filtered_data, aes(x = month, y = Value, color = Category)) +
        geom_line(linewidth = 1) +
        labs(title = "Olist & Subscriber Performance",
             x = "Month", y = "Index (Base 100)", color = "Legend Title") +
        custom_theme() +
        theme(axis.text.x = element_text(hjust = 1))
      p <- apply_pastel1_palette(p, type = "colour")
    }
    
    # Check for "Revenue with Forecast" chart
    if (input$dashboardType == "Headline Data" && input$chartType == "Revenue with Forecast") {
      filtered_combined_data <- prophet_forecast_df %>%
        filter(ds >= input$dateRange[1] & ds <= input$dateRange[2])
      
      filtered_forecast_data <- prophet_forecast_2 %>%
        filter(ds >= input$dateRange[1] & ds <= input$dateRange[2])
      
      p <- ggplot() +
        geom_line(data = filtered_combined_data, aes(x = ds, y = y, color = type), linewidth = 1) +
        geom_ribbon(data = filtered_forecast_data, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2, fill = "lightblue") +
        labs(title = "Olist Revenue with Forecast",
             x = "Date", y = "Revenue (BRL)",
             color = "Type") +
        custom_theme()
      p <- apply_pastel1_palette(p, type = "colour")
    }
    
    # Check for "Orders vs AOV" chart
    if (input$dashboardType == "Headline Data" && input$chartType == "Orders vs AOV") {
      filtered_data <- orders_vs_aov %>%
        filter(date >= input$dateRange[1] & date <= input$dateRange[2])
      # Calculate the scale factor for the secondary axis
      scale_factor <- max(orders_vs_aov$total_orders) / max(orders_vs_aov$aov)
      
      # Create the plot
      p <- ggplot(filtered_data, aes(x = date)) +
        geom_line(aes(y = total_orders, colour = "Total Orders"), linewidth = 1) +
        geom_line(aes(y = aov * scale_factor, colour = "AOV"), linewidth = 1) +
        scale_y_continuous(name = "Total Orders",
                           sec.axis = sec_axis(~ . / scale_factor, name = "AOV")) +
        scale_colour_manual("", values = c("Total Orders" = "blue", "AOV" = "red")) +
        custom_theme() +
        labs(title = "Seller Revenue Drivers", x = "Date", colour = "Metric") +
        theme(axis.title.y.right = element_text(angle = 90))
      p <- apply_pastel1_palette(p, type = "colour")
    }
    
    # Check for "Subscriptions vs Sales" chart
    if (input$dashboardType == "Headline Data" && input$chartType == "Subscriptions vs Sales") {
      filtered_data <- rev_split %>%
        filter(order_purchase_timestamp >= input$dateRange[1] & order_purchase_timestamp <= input$dateRange[2])
      # Recreate the stacked area chart
      p <- ggplot(filtered_data, aes(x = order_purchase_timestamp, y = Amount, fill = `Revenue Type`)) +
        geom_area(position = "stack", alpha = 0.6) +
        scale_fill_manual(values = c("olist_rev_share_10DMA" = "lightcoral", 
                                     "daily_subscription_revenue_10DMA" = "steelblue")) +
        labs(title = "10-Day Moving Averages of Olist Revenue Share and Daily Subscription Revenue",
             x = "Date",
             y = "10-Day Moving Average (DMA)",
             fill = "Revenue Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        custom_theme()
      p <- apply_pastel1_palette(p, type = "fill")
  }
    
    # Check for "Sellers New vs Old" chart
    if (input$dashboardType == "Seller Data" && input$chartType == "Sellers New vs Old") {
      filtered_data <- sellers_new_old %>%
        filter(month >= input$dateRange[1] & month <= input$dateRange[2])
      p <- ggplot(filtered_data, aes(x = month, y = total_sellers, fill = seller_new_old)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_brewer(palette = "Pastel1") +
        labs(
          title = "Total Sellers per Month, New vs Old",
          x = "Month",
          y = "Total Sellers",
          fill = "Seller Type"
        ) +
        custom_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      p <- apply_pastel1_palette(p, type = "colour")
    }
    
    if (input$dashboardType == "Seller Data" && input$chartType == "Seller Churn") {
      filtered_data <- seller_churn_df %>%
        filter(month >= input$dateRange[1] & month <= input$dateRange[2])
    
      p <- ggplot(filtered_data, aes(x = month)) +
        geom_bar(aes(y = new_sellers, fill = "New Sellers"), stat = "identity") +
        geom_bar(aes(y = -lost_sellers, fill = "Churned Sellers"), stat = "identity") +
        geom_line(aes(y = net_subscriber_change, group = 1), color = "blue") +
        scale_fill_manual(values = c("New Sellers" = "green", "Churned Sellers" = "red")) +
        labs(title = "Monthly Seller Acquisition vs Churn (Jan 2017 - Jul 2018)",
             x = "Month",
             y = "Number of Sellers",
             fill = "Seller Type") +
        theme_minimal() +
        scale_y_continuous(labels = abs, sec.axis = sec_axis(~ ., name = "Net Subscriber Change"))
      p <- apply_pastel1_palette(p, type = "fill")
    }
    
    # Check for "Revenue by Product Category" chart
    if (input$dashboardType == "Seller Data" && input$chartType == "Revenue by Product Category") {
      monthly_revenue_grouped <- monthly_revenue_grouped %>%
        mutate(year_month_as_date = as.Date(paste0(year_month, "-01")))
      
      filtered_data <- monthly_revenue_grouped %>%
        filter(year_month_as_date >= input$dateRange[1] & year_month_as_date <= input$dateRange[2])
      
      p <- ggplot(filtered_data, aes(x = year_month_as_date, y = monthly_revenue, fill = category_grouped)) +
        geom_area(position = 'stack') +
        scale_fill_manual(values = unique(monthly_revenue_grouped$color)) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
        labs(title = "Monthly Revenue by Product Category", x = "Month", y = "Revenue") +
        custom_theme() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        guides(fill = guide_legend(ncol = 1)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }
    # Check for "Monthly Delivery Times" chart
    if (input$dashboardType == "Shipping Data" && input$chartType == "Monthly Delivery Times") {
      # Calculate the average delivery days for each month
      monthly_avg_delivery_days <- df_shipping_analysis %>%
        filter(order_purchase_timestamp >= as.Date("2017-01-01")) %>%
        group_by(year_month) %>%
        summarise(avg_delivery_days = mean(order_delivery_days, na.rm = TRUE)) %>%
        arrange(year_month)
      
      monthly_avg_delivery_days <- monthly_avg_delivery_days %>%
        mutate(year_month_as_date = as.Date(paste0(year_month, "-01")))
      
      filtered_data <- monthly_avg_delivery_days %>%
        filter(year_month_as_date >= input$dateRange[1] & year_month_as_date <= input$dateRange[2])
      
      # Plot the average delivery days by month
      p <- ggplot(filtered_data, aes(x = year_month_as_date, y = avg_delivery_days)) +
        geom_bar(stat = "identity", fill = "blue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Month-Year", y = "Average Delivery Days", title = "Average Order Delivery Days by Month") +
        custom_theme()
      p <- apply_pastel1_palette(p, type = "fill")
    }
    
    # Check for "Shipping Days vs Revenue" chart
    if (input$dashboardType == "Shipping Data" && input$chartType == "Shipping Days vs Revenue") {
      # Remove outliers (i.e., delivery days greater than 60)
      df_shipping_filtered <- df_shipping_analysis %>%
        filter(order_purchase_timestamp >= as.Date("2017-01-01"),order_delivery_days <= 60)
      
      # Create buckets of 2 days up to 50 days
      df_shipping_filtered <- df_shipping_filtered %>%
        mutate(delivery_day_bucket = cut(order_delivery_days, breaks = seq(0, 50, by = 2), right = FALSE, labels = seq(2, 50, by = 2)))
      
      # Aggregate data by delivery_day_bucket for total revenue and count zip codes
      bucketed_data <- df_shipping_filtered %>%
        group_by(delivery_day_bucket) %>%
        summarise(total_revenue = sum(price, na.rm = TRUE),
                  zip_code_count = n_distinct(customer_zip_code_prefix)) %>%
        ungroup()
      
      # Filter out the NA bucket
      bucketed_data <- bucketed_data %>% 
        filter(!is.na(delivery_day_bucket))
      
      # Reset to default theme settings
      theme_set(custom_theme())
      
      # Plot bar chart with adjusted annotations and corrected x-axis labels
      p <- ggplot(bucketed_data, aes(x = delivery_day_bucket, y = total_revenue)) +
        geom_bar(stat = "identity", fill = "blue") +
        geom_text(
          aes(label = zip_code_count), 
          nudge_y = max(bucketed_data$total_revenue) * 0.1, # Nudge the text above the bars by 5% of the max revenue
          angle = 90, # Rotate the text
          color = "black",
          size = 3.5
        ) +
        scale_x_discrete(labels = paste(seq(1, 49, by = 2), seq(2, 50, by = 2), sep = "-")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Add space at the top of the chart
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none"
        ) +
        labs(title = "Total Revenue by Delivery Days (Orders per Bucket Above Bars)",
             x = "Delivery Day Buckets (Days)",
             y = "Total Revenue") +
        custom_theme()
      p <- apply_pastel1_palette(p, type = "fill")
    }
    
    # Check for "Avg Freight Over Time" chart
    if (input$dashboardType == "Shipping Data" && input$chartType == "Avg Freight Over Time") {
      # Filter out rows where distance_km is zero to avoid division by zero errors
      geo_df_filtered <- df_shipping_analysis %>%
        filter(order_purchase_timestamp >= as.Date("2017-01-01") & distance_km > 0) %>%
        mutate(
          freight_per_kg_km = freight_value / (distance_km * final_weight_kg)
        )
      
      # Aggregate this data by Year-Month
      geo_df_aggregated <- geo_df_filtered %>%
        group_by(year_month) %>%
        summarise(
          avg_freight_per_kg_km = mean(freight_per_kg_km, na.rm = TRUE)
        )
      
      # Ensure year_month is in Date format for plotting
      geo_df_aggregated$year_month <- as.Date(paste0(geo_df_aggregated$year_month, "-01"))
      
      filtered_data <- geo_df_aggregated %>%
        filter(year_month >= input$dateRange[1] & year_month <= input$dateRange[2])
      
      
      p <- ggplot(filtered_data, aes(x = year_month, y = avg_freight_per_kg_km)) +
        geom_line(color = "blue", linewidth = 1) +
        geom_point(color = "red", size = 2) +
        labs(
          title = "Average Freight Cost per Kg-Km Over Time",
          x = "Month",
          y = "Average Freight Cost per Kg-Km",
          caption = "Data from Jan 2017 to Aug 2018"
        ) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        custom_theme()
      p <- apply_pastel1_palette(p, type = "colour")
      
    }
    
    # Check for "New vs Repeat Customers" chart
    if (input$dashboardType == "Customer Data" && input$chartType == "New vs Repeat Customers") {
      
      filtered_data <- monthly_customer_type_counts %>%
        filter(month >= input$dateRange[1] & month <= input$dateRange[2])
      
      p <- ggplot(filtered_data, aes(x = month, y = count, color = customer_type)) +
        geom_line(linewidth = 1) +
        labs(title = "New vs Repeat Customers Over Time",
             x = "Month",
             y = "Number of Customers",
             color = "Customer Type") +
        custom_theme()
      p <- apply_pastel1_palette(p, type = "colour")
      
    }
    
    # Check for "Urban vs Rural Customers" chart
    if (input$dashboardType == "Customer Data" && input$chartType == "Urban vs Rural Customers") {
      
      filtered_data <- monthly_revenue_by_zone %>%
        filter(month >= input$dateRange[1] & month <= input$dateRange[2])
      
      p <- ggplot(filtered_data, aes(x = month, y = revenue, color = cust_zone)) +
        geom_line(linewidth = 1) +
        labs(title = "Monthly Revenue, Urban vs Rural Customers",
             x = "Month",
             y = "Revenue",
             color = "Customer Area") +
        custom_theme()
      p <- apply_pastel1_palette(p, type = "colour")
    }
    
    # Check for "Delivery Review Scores" chart
    if (input$dashboardType == "Customer Data" && input$chartType == "Delivery Review Scores") {
      
      p <- ggplot(delivery_review_df, aes(x = review_score, y = Days, fill = Statistic)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(title = "Mean and Median Delivery Days per Review Score",
             x = "Review Score",
             y = "Delivery Days",
             fill = "Metric") +
        custom_theme()
      p <- apply_pastel1_palette(p, type = "fill")
    }
    
    if (is.null(p)) {
      print("Plot object 'p' is NULL. Check the conditions and data filtering.")
    } else {
      print("Plot object 'p' is ready for rendering.")
    }
    
    return(p)
  })
  

  #..................................INTERACTIVE MAPS TAB..........................
  
  values <- reactiveValues(noteKey = "Please select options to view map notes.")
  
  ##...............................................................................update user choices
  # Observer to update selected_metric choices based on data type
  #remove zip from seller data as there is none
  ## consolidated choice updates and converted to observeEvent to be more conservative
  observeEvent(input$dataType, {
    bounds <<- currentBounds() ## update current bounds on input change
    if (input$dataType == "seller") {
      updateSelectInput(session, "metricSelector", choices = seller_muni_metric_options)
      print(1.1)
      updateSelectInput(session, "aggregationLevel", choices = sell_choices, select = "muni")
      print(2.1)
    }
    else {
      updateSelectInput(session, "metricSelector", choices = customer_metric_options)
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
  
  # Place output$mapViewingNotes here
  output$mapViewingNotes <- renderUI({
    wellPanel(
      h4("Map Viewing Notes"),
      p(values$noteKey) # This will dynamically update based on the noteKey reactive value
    )
  })

  ##...............................................................................filtering data
  
  # Filter Data Based on User Selection
  ## this section only chose the data set to use and did not filter for anything
  ## added in data filtering functionality
  filtered_data <- eventReactive(input$renderButton, { ## [new] only load data if button is clicked
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
    
  
  # Create a reactive value to store the map viewing notes
  values <- reactiveValues(noteKey = "Please select options to view map notes.")
  
  ## render map in response to renderButton
  observeEvent(input$renderButton, { ## [new] only load map if button is clicked
    
    # Logic to determine the noteKey based on selections
    selectedNote <- if(input$dataType == "seller") {
      mapViewingNotes$seller[[input$metricSelector]] %||% "Please select options to view map notes."
    } else { # For customer with multiple aggregation levels
      mapViewingNotes$customer[[input$aggregationLevel]][[input$metricSelector]] %||% "Please select options to view map notes."
    }
    
    # Update the reactive value for noteKey
    values$noteKey <- selectedNote
    
  
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
      legend_title <- input$metricSelector ## [new] saving it as a value to be called later will prevent the map from loading without the button
      
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
            title = getDisplayName(legend_title, c(seller_muni_metric_options, customer_metric_options))[1] ## [new] retrieve metric's nice name
          )
        
      })
      
    })
  
  }