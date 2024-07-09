library(shiny)
library(leaflet)
library(sf)

final_muni_geom_simplified <- readRDS("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//final_muni_geom_simplified.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Map with Sales Data"),
  sliderInput("monthSlider", "Select Month",
              min = 1, max = 12, value = 1),
  leafletOutput("map")
)

# Define server logic
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # Extract the selected month
    unique_months <- sort(unique(format(final_muni_geom_simplified$year_month, "%Y-%m")))
    
    selected_month <- unique_months[input$monthSlider]
    
    # Filter data based on selected month
    selected_month_data <- final_muni_geom_simplified[format(final_muni_geom_simplified$year_month, "%Y-%m") == selected_month, ]
    
    leaflet(data = selected_month_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric(palette = "viridis", domain = selected_month_data$cust_sales_month)(cust_sales_month),
        weight = 1,
        color = "#FFFFFF",
        fillOpacity = 0.7,
        popup = ~paste("Sales: ", cust_sales_month)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
