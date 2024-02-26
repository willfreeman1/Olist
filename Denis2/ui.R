ui <- navbarPage("Brazilian E-commerce Data from Olist",
                 tabPanel("Data Insights",
                          sidebarLayout(
                            sidebarPanel(
                              # Sidebar elements for the new charts
                              selectInput("dashboardType", "Dashboard:",
                                          choices = c("Headline Data", "Seller Data", "Shipping Data", "Customer Data")),
                              
                              selectInput("chartType", "Chart:",
                                          choices = c("Olist & Seller Performance Index", "Revenue with Forecast", "Orders vs AOV", 
                                                      "Subscriptions vs Sales", "Sellers New vs Old", "Seller Churn", 
                                                      "Revenue by Product Category","Monthly Delivery Times", "Shipping Days vs Revenue",
                                                      "Avg Freight Over Time", "New vs Repeat Customers", "Urban vs Rural Customers",
                                                      "Delivery Review Scores"), 
                                          selected = "Olist & Seller Performance Index"),
                              
                              # Assuming you will add the range slider here
                              sliderInput("dateRange", "Date Range",
                                          min = as.Date("2017-01-01"), 
                                          max = as.Date("2018-08-31"),
                                          value = c(as.Date("2017-01-01"), as.Date("2018-08-31")),
                                          timeFormat="%Y-%m-%d")
                            ),
                            mainPanel(
                              # Placeholder for charts, replace with your actual output elements
                              plotOutput("chartPlaceholder", width = "100%", height = "800px")
                            )
                          )
                 ),
                 tabPanel("Interactive Maps",
                          sidebarLayout(
                            sidebarPanel(
                              # Existing sidebar elements for the maps
                              selectInput("dataType", "Data Type:",
                                          choices = c("Seller" = "seller", "Customer" = "customer"), selected = "seller"),
                              
                              selectInput("metricSelector", "Metric:", choices = seller_muni_metric_options, selected = "total_seller"),
                              
                              selectInput("aggregationLevel", "Aggregation Level:",
                                          choices = sell_choices, selected = "muni"),
                              
                              checkboxInput("selectAll", "Select All", value = TRUE),
                              
                              checkboxGroupInput("selectedBins", "Select Bins:",
                                                 choiceNames = getGlobalBinRanges("seller", "muni", "total_sales")$bin_labels, 
                                                 choiceValues = getGlobalBinRanges("seller", "muni", "total_sales")$bin_ranges, 
                                                 selected = getGlobalBinRanges("seller", "muni", "total_sales")$bin_ranges),
                              
                              actionButton("renderButton", "Update Map"),
                              
                              uiOutput("mapViewingNotes")
                            ),
                            mainPanel(
                              leafletOutput("map", width = "100%", height = "800px")
                            )
                          )
                 )
                 
)