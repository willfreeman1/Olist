library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)
library(leaflet.extras)
library(ggplot2)

# Read DFs
rds_file_path <- "final_muni_geom_filtered_with_centroids.rds"
final_muni_geom_filtered_1 <- readRDS(rds_file_path)
final_muni_geom_filtered_sf <- st_as_sf(final_muni_geom_filtered_1)
final_muni_geom_filtered_sf <- st_transform(final_muni_geom_filtered_sf, crs = 4326) # WGS84 projection
final_muni_geom_filtered_df <- st_drop_geometry(final_muni_geom_filtered_sf)
final_muni_geom_filtered_df <- final_muni_geom_filtered_df %>%
  filter(
    latitude <= 5.27438888,
    longitude >= -73.98283055,
    latitude >= -33.75116944,
    longitude <= -34.79314722
  )

# Assuming 'max_quantiles_per_month.csv' is your processed data
max_quantiles_per_month <- read.csv("max_quantiles_per_month_2.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Olist Mock BI Dashboard"),
  tabsetPanel(
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataColumn", "Select Data Column", 
                  choices = c("Monthly Customer Sales" = "cust_sales_month", "Cumulative Customer Sales" = "cust_sales_cum",
                              "Monthly Customer Revenue" = "cust_revenue_month", "Cumulative Customer Revenue" = "cust_revenue_cum", 
                              "Count of Monthly Customers" = "total_cust_month", 
                              "Count of Monthly New Customers" = "new_cust_month", "Count of Monthly Returning Customers" = "returning_cust_month", 
                              "Cumulative Total Customers" = "total_cust_cum", "Avg Customer Lifetime Value" = "avg_clv",
                              "Monthly Seller Sales" = "seller_sales_month", "Cumulative Seller Sales" = "seller_sales_cum", 
                              "Monthly Seller Revenue" = "seller_revenue_month", "Cumulative Seller Revenue" = "seller_revenue_cum",
                              "Active Sellers Monthly" = "active_sellers_month", "Cumulative Active Sellers" = "active_sellers_cum",
                              "New Sellers Monthly" = "new_sellers_month", "Old Sellers Monthly" = "old_sellers_month",
                              "Total Seller Lifetime Value" = "total_seller_lv", "Average Seller Lifetime Value" = "avg_seller_lv")),
                sliderTextInput("monthSlider", "Select Month",
                      choices = format(seq(as.Date("2017-01-01"), as.Date("2018-08-01"), by="month"), "%Y-%m"),
                      selected = "2017-01")
          ),
          mainPanel(
            leafletOutput("map")
            )
          )
          ),
          tabPanel("Revenue",
           selectInput("chartType", "Select Data",
                       choices = c("Daily Olist Revenue", "Smoothed Revenue With Forecast", "Smoothed Revenue Streams")),
           dateRangeInput("dateRange", "Select Date Range",
                          start = as.Date("2017-01-01"), end = as.Date("2018-08-15"),
                          min = as.Date("2017-01-01"), max = as.Date("2018-08-15")), 

           plotOutput("revenueChart")
          )
  )
)


# Define server logic
server <- function(input, output, session) {
  #Load RDS Files
  historical_data <- readRDS("historical_data.rds")
  forecast_data <- readRDS("forecast_data.rds")
  rev_split <- readRDS("rev_split.rds")
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    selected_month <- as.Date(paste0(input$monthSlider, "-01"))
    data <- filter(final_muni_geom_filtered_df, year_month == selected_month)
    
    if(nrow(data) == 0) {
      return(NULL)
    }
    
    if(!is.numeric(data[[input$dataColumn]]) || length(data[[input$dataColumn]]) == 0) {
      print("Selected data column is not numeric or empty.")
      return(NULL)
    }
    
    list(data = data)
  })
  
  # Render the map
  output$map <- renderLeaflet({
    filtered <- filtered_data()
    
    if(is.null(filtered$data)) {
      print("Data is empty after filtering or no polygon geometries")
      return(NULL)
    }
    
    data <- filtered$data
    column_name <- input$dataColumn
    color_values <- data[[column_name]]
    
    # Define a custom gradient
    custom_gradient <- c(
      "0"   = "purple",    # Low intensity
      "0.5" = "blue",    # Medium intensity
      "0.66"   =  "green",
      "0.88" = "orange",
      "1" = "yellow" # High intensity
    )
    
    # Create a simple leaflet map
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    # Add a heatmap layer
    m <- m %>%
      addHeatmap(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        intensity = ~color_values,
        radius = 5,
        blur = 5,
        minOpacity = 0.01,
        gradient = custom_gradient
      )
  })
  output$revenueChart <- renderPlot({
    req(input$chartType)
    
    start_date <- input$dateRange[1]
    end_date <- max(historical_data$ds)
    
    if(input$chartType == "Daily Olist Revenue") {
      filtered_data <- rev_split %>%
        filter(order_purchase_timestamp >= start_date & order_purchase_timestamp <= end_date)
      ggplot(filtered_data, aes(x = order_purchase_timestamp, y = olist_total_rev)) +
        geom_line() +
        theme_minimal() +
        labs(title = "Daily Olist Revenue",
             x = "Order Date",
             y = "Revenue (BRL)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    else if(input$chartType == "Smoothed Revenue With Forecast") {
      filtered_historical_data <- historical_data %>%
        filter(ds >= start_date)
      window_size <- 5 # or your desired window size
      ggplot() +
        geom_line(data = filtered_historical_data, aes(x = ds, y = rolling_mean), color = "green", na.rm = TRUE) +
        geom_line(data = forecast_data, aes(x = ds, y = yhat), color = "red") +
        geom_ribbon(data = forecast_data, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2, fill = "orange") +
        labs(title = paste("Olist Revenue Forecast with", window_size, "Day Moving Average"),
             x = "Date", y = "Revenue (BRL)") +
        ylim(0, 20000)
      
    }
    else if(input$chartType == "Smoothed Revenue Streams") {
      filtered_data <- rev_split %>%
        filter(order_purchase_timestamp >= start_date & order_purchase_timestamp <= end_date)
      ggplot(filtered_data, aes(x = order_purchase_timestamp)) +
        geom_area(aes(y = olist_rev_share_ma, fill = "20% Revenue Share"), position = "stack") +
        geom_area(aes(y = daily_fee_ma, fill = "Subscription Revenue"), position = "stack") +
        scale_fill_manual(values = c("20% Revenue Share" = "blue", "Subscription Revenue" = "green")) +
        labs(title = "5-Day Moving Average of Olist Revenue Streams",
             x = "Date", y = "Revenue (BRL)", fill = "Revenue Type") +
        ylim(0, 15000)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
