library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)

# Read DFs
final_muni_geom_filtered <- readRDS("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//Scripts//final_muni_geom_filtered.rds")

# Convert final_muni_geom_filtered to an sf object
final_muni_geom_filtered_sf <- st_as_sf(final_muni_geom_filtered)

final_muni_geom_filtered_sf <- st_transform(final_muni_geom_filtered_sf, crs = 4326) # WGS84 projection


# Print data types of columns
print(sapply(final_muni_geom_filtered_sf, class))

# Define UI
ui <- fluidPage(
  titlePanel("Geographic Data Visualization"),
  
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
      leafletOutput("map", width = "100%")
    )
  )
)

# Assuming 'max_quantiles_per_month.csv' is your processed data
max_quantiles_per_month <- read.csv("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//Data//max_quantiles_per_month_2.csv")


# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    selected_month <- as.Date(paste0(input$monthSlider, "-01"))
    data <- filter(final_muni_geom_filtered_sf, year_month == selected_month)
    
    if(nrow(data) == 0) {
      return(NULL)
    }
    
    # Ensure the data column is numeric and non-empty
    if(!is.numeric(data[[input$dataColumn]]) || length(data[[input$dataColumn]]) == 0) {
      print("Selected data column is not numeric or empty.")
      return(NULL)
    }
    
    # Fetch the number of quantiles
    num_quantiles_data <- max_quantiles_per_month %>%
      filter(Column == input$dataColumn, Year_Month == as.character(selected_month))
    
    # Check if num_quantiles_data is empty
    if(nrow(num_quantiles_data) == 0) {
      num_quantiles <- 5  # Default number of quantiles
    } else {
      num_quantiles <- num_quantiles_data$Num_Quantiles
    }
    
    # Diagnostic print
    print(paste("Number of quantiles:", num_quantiles))
    
    # Apply square root transformation
    sqrt_columns <- c("total_cust_month", "cust_sales_month", "cust_revenue_month", "new_cust_month", "returning_cust_month", "total_cust_cum", "cust_sales_cum", "cust_revenue_cum", "seller_sales_month", "seller_revenue_month", "seller_sales_cum", "seller_revenue_cum", "total_seller_lv", "avg_clv", "avg_seller_lv")
    if (input$dataColumn %in% sqrt_columns) {
      data[[input$dataColumn]] <- sqrt(data[[input$dataColumn]] + 1)  # Adding 1 to avoid sqrt(0)
    }
    
    list(data = data, num_quantiles = num_quantiles)
  })
  
  
  output$map <- renderLeaflet({
    filtered <- filtered_data()
    
    if(is.null(filtered$data)) {
      print("Data is empty after filtering or no polygon geometries")
      return(NULL)
    }
    
    data <- filtered$data
    column_name <- input$dataColumn
    color_values <- data[[column_name]]
    
    # Calculate the breaks manually
    quantile_n <- min(length(unique(color_values)), filtered$num_quantiles)
    quantile_breaks <- unique(quantile(color_values, probs = seq(0, 1, length.out = quantile_n + 1), na.rm = TRUE))
    
    # Check and print the breaks
    print(paste("Calculated breaks:", paste(quantile_breaks, collapse = ", ")))
    
    # Use a color palette with the calculated breaks
    color_pal <- colorNumeric(palette = "YlOrRd", domain = quantile_breaks)
    
    # Leaflet map with adjusted color palette
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = data,
                  fillColor = ~color_pal(color_values),
                  weight = 1,
                  color = "#FFFFFF",
                  fillOpacity = 0.7,
                  popup = ~paste(column_name, ": ", round(color_values, 2))) %>%
      addLegend(position = "bottomright",  # Adjust the position as needed
                  pal = color_pal,
                  values = ~color_values,
                  title = "Legend Title",  # Replace with your desired title
                  opacity = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)