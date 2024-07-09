# Olist test app

library(shiny)
library(ggplot2)
library(dplyr)

# Read initial dataframe
filtered_seller_indexes <- readRDS("C://Users//willf//OneDrive//Documents//NYDSA//R//Olist//R_Analysis//filtered_seller_indexes.rds")

ui <- fluidPage(
  titlePanel("Seller Revenue Analysis"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange",
                     label = "Select Date Range",
                     start = min(filtered_seller_indexes$month),
                     end = max(filtered_seller_indexes$month)
      )
    ),
    mainPanel(
      plotOutput("lineChart")
    )
  )
)

server <- function(input, output, session) {
  
  output$lineChart <- renderPlot({
    # Filter data based on the selected date range
    data <- filtered_seller_indexes %>%
      filter(month >= as.Date(input$dateRange[1]) & month <= as.Date(input$dateRange[2]))
    
    # Plotting code using ggplot2
    ggplot(data, aes(x = month)) +
      geom_line(aes(y = revenue_index, group = 1, color = "Total Revenue"), linewidth = 1) +
      geom_line(aes(y = sellers_index, group = 1, color = "Total Sellers"), linewidth = 1) +
      geom_line(aes(y = avg_revenue_index, group = 1, color = "Avg Revenue Per Seller"), linewidth = 1) +
      scale_color_manual(values = c("Total Revenue" = "blue", "Total Sellers" = "red", "Avg Revenue Per Seller" = "green")) +
      labs(title = "Total Revenue, Total Sellers and Avg Revenue Per Seller",
           x = "Month", y = "Index (Base 100)", color = "Metrics") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
