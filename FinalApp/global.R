## - these are my comments
##! - an error that needs your input

#............................................................................... Load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tidyr)
library(RColorBrewer)
library(lubridate)


#..............................DATA INSIGHTS TAB SECION.........................

#............................................................................... Load insights data
index_summary_df = readRDS("index_summary_df.rds")
prophet_forecast_df = readRDS("prophet_forecast_df.rds")
prophet_forecast_2 = readRDS("prophet_forecast_2.rds")
orders_vs_aov <- readRDS("orders_vs_aov.rds")
rev_split <- readRDS("rev_split.rds")
sellers_new_old <- readRDS("sellers_new_old.rds")
seller_churn_df <- readRDS("monthly_seller_stats_filtered.rds")
monthly_revenue_grouped <- readRDS("monthly_revenue_grouped_with_colors.rds")
df_shipping_analysis <- readRDS("df_shipping_analysis.rds")
monthly_customer_type_counts <- readRDS("monthly_customer_type_counts.rds")
monthly_revenue_by_zone <- readRDS("monthly_revenue_by_zone.rds")
delivery_review_df <- readRDS("delivery_review_df.rds")



#............................................................................... Define insights functions

library(scales)

# Custom theme
custom_theme <- function() {
  theme_gray(base_size = 14) +
    theme(
      plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
      plot.title.position = "plot",
      axis.title.x = element_text(size = 14), # X axis title
      axis.title.y = element_text(size = 14), # Y axis title
      axis.text.x = element_text(size = 14),  # X axis ticks
      axis.text.y = element_text(size = 14),  # Y axis ticks
      legend.text = element_text(size = 14)   # Legend text
    )
}


# Function to apply Pastel1 palette
apply_pastel1_palette <- function(p, type = "fill") {
  if (type == "fill") {
    p + scale_fill_brewer(palette = "Set1")
  } else if (type == "colour") {
    p + scale_colour_brewer(palette = "Set1")
  } else {
    p
  }
}



#...............................................................................
#..............................DATA INSIGHTS TAB SECTION.........................
#...............................................................................


#............................................................................... Load maps data
muni_map_sf = readRDS("muni_map_sf.rds")
custmetrics_state_geo = readRDS("custmetrics_state_geo.rds")
custmetrics_muni_geo = readRDS("custmetrics_muni_geo.rds")
# custmetrics_zip_geo = readRDS("custmetrics_zip_geo.rds")

# Set muni (seller / muni_map_sf) metric labels & option
seller_muni_metric_options <- c(
  "Total Sales (BRL)" = "total_sales",
  "Total Sellers" = "total_sellers",
  "Total Seller LTV to Olist (BRL)" = "total_seller_LTV")

# Customer metric options (common for all aggregation levels - custmetrics DFs)
customer_metric_options <- c(
  "Total Sales (BRL)" = "total_sales",
  "Average Order Value (BRL)" = "aov",
  "Average Delivery Time (days)" = "avg_delivery_time",
  "Average Shipping Share of Total Order Value (%)" = "avg_ship_share",
  "Total Unique Customers" = "total_unique_customers",
  "Total Repeat Customers" = "total_repeat_customers")

# Map Viewing Notes options
mapViewingNotes <- list(
  seller = list(
    total_sales = "Zoomed out, note that Brazil’s ecommerce market is concentrated in the Southeast region, especially around Sao Paulo and Rio. Note the concentration of sales in the Sao Paulo area as well as around Rio. As a SaaS provider to e-commerce sellers, Olist should have focused marketing efforts on major cities.",
    total_sellers = "Zoomed out, see that Brazil’s ecommerce market is concentrated in the Southeast region, especially around Sao Paulo and Rio. The number of Sellers is even more concentrated in Sao Paulo than seller revenue.",
    total_seller_LTV = "The lifetime value (LTV) for Olist is also concentrated in the Sao Paulo & Rio region of Brazil. Sao Paulo dwarfs all other cities in seller LTV, so Olist would have been wise to focus marketing efforts there."
    ),
  customer = list(
    state = list(
      total_sales = "This map is best viewed at the State aggregation level. Once again it shows that the Sao Paulo area dominates e-commerce in Brazil, followed by the rest of the eastern coastline.",
      aov = "Note that AOV gets higher as you move out from the Sao Paulo/Rio region. This is likely because customers that are outside of this region try to order in bulk to lower shipping costs.",
      avg_delivery_time = "This shows Brazil’s biggest problem as an e-commerce market in 2016-18: logistics. Shipping was extremely slow, especially for regions far from the Sao Paulo / Rio area.",
      avg_ship_share = "Note that the further from Sao Paulo and Rio, the higher the shipping costs a share of order value.Brazil was very much a single region e-commerce market at this time.",
      total_unique_customers = "Note that the Sao Paulo area dominates e-commerce in Brazil, followed by the rest of the eastern coastline.",
      total_repeat_customers = "Repeat customers are less than 10% of total customers - a sign of a very new e-commerce market. Like the rest of these statistics, repeat customers are in the major urban centers."
    ),
    muni = list(
      total_sales = "We recommend starting at the State aggregation level for this metric. At the municipality level, note that sales are focused around urban centers like Sao Paulo and Rio.",
      aov = "We recommend starting at the State aggregation level for this metric. At the municipality level, note that the pattern of higher AOV as you move out from the Sao Paulo & Rio area still generally holds true. Note that the vast majority of municipalities outside of major cities only have a handful of orders so AOV is not very definitive.",
      avg_delivery_time = "This shows Brazil’s biggest problem as an e-commerce market in 2016-18: logistics. Shipping was extremely slow, especially for regions far from the Sao Paulo / Rio area.",
      avg_ship_share = "This map is best viewed at the state level. But even at the municipality level, it is clear that shipping gets more expensive as you get further from the largest urban centers of Rio and Sao Paulo.",
      total_unique_customers = "This map is best viewed at the State aggregation level. At the Municipality level note that customers are mostly in and around urban centers like Sao Paulo and Rio.",
      total_repeat_customers = "This map is best viewed at the State aggregation level. At the Municipality level you can again see the dominance of the Sao Paulo and Rio areas."
    )
  )
)

#...............................................................................DEFINE NON-REACTIVE FUNCTIONS

## [new] function to retrieve metric's nice name
getDisplayName <- function(metric_code, options_list) {
  display_name <- names(options_list)[options_list == metric_code]
  if (length(display_name) == 0) {
    return(metric_code)  # Return the code itself if no display name found
  } else {
    return(display_name)
  }
}

# Function to choose the appropriate data frame
chooseDataFrame <- function(func_aggregation_input, func_datatype_input) {

  # Define the appropriate data frame
  data <- NULL
  
  ## 1. seller x municipality = muni_map_sf
  ## 2. customer x municipality = custmetrics_muni_geo
  ## 3. seller x state = custmetrics_state_geo
  ## 3. customer x state = custmetrics_state_geo
  ## 4. customer x zip code = custmetrics_zip_geo
  
  ## aggregated if statements
  if (func_datatype_input == "seller" & func_aggregation_input == "muni") {
    data <- muni_map_sf
  }
  else if (func_datatype_input == "customer" & func_aggregation_input == "muni") {
    data <- custmetrics_muni_geo
  }
  else if (func_aggregation_input == "state") {
    data <- custmetrics_state_geo
  }
  # else if (func_aggregation_input == "zip") {
  #   data <- custmetrics_zip_geo
  # }

  return(data)
}

# Func - get bin ranges for each metric
getGlobalBinRanges <- function(func_datatype_input, func_aggregation_input, func_metric_input) {
  
  bin_info <- list(bin_ranges = list(), bin_labels = list())
  
  ## seller_muni_metric_options <- c(
  ##   "Total Sales (BRL)" = "total_sales", <-- common with customer
  ##   "Total Sellers" = "total_sellers",
  ##   "Average Product Price (BRL)" = "avg_price_per_seller",
  ##   "Total Seller LTV to Olist (BRL)" = "total_seller_LTV",
  ##   "Average Seller LTV to Olist (BRL)" = "avg_seller_LTV",
  ##   "Average Ship Cost Share of Total Order (%)" = "avg_ship_share_per_seller",
  ##   "Average Expected Shipping Time (days)" = "avg_expected_ship_time_per_seller",
  ##   "Average Review Score" = "avg_review_score_per_seller")
  
  ## # Customer metric options (common for all aggregation levels - custmetrics DFs)
  ## customer_metric_options <- c(
  ## c   "Total Sales (BRL)" = "total_sales", <-- common with seller
  ##   "Total Orders" = "total_orders",
  ##   "Average Order Value (BRL)" = "aov",
  ##   "Average Delivery Time (days)" = "avg_delivery_time",
  ##   "Average Expected Delivery Time (days)" = "avg_expected_delivery_time",
  ##   "Average Shipping Share of Total Order Value (%)" = "avg_ship_share",
  ##   "Total Unique Customers" = "total_unique_customers",
  ##   "Total Repeat Customers" = "total_repeat_customers",
  ##   "Average Review Score" = "avg_review_score")
  
  ## the below if statements seem to be organized according to the data set being used
  ## aggregated, organized, and removed conflicts for the if statements (I think this is what you intended)
  ##! what's the best way to display bins and legend since they don't match?
  ## - keep overlaps or not?
  ## this type of structure should reduce run-time although this isn't the culprit for the app's slower speed
  
  ## 1. seller x municipality x func_metric_input
  if (func_datatype_input == "seller" & func_aggregation_input == "muni") {
    if (func_metric_input == "total_sales") {
      bin_info$bin_ranges <- list(c(0, 10000), c(10000, 50000), c(50000, 100000), c(100000, 200000), c(200000, 500000), c(500000, 1000000), c(1000000, 3000000)) ## updated code
      bin_info$bin_labels <- c("R$0-10,000", "R$10,001-50,000", "R$50,001-100,000", "R$100,001-200,000", "R$200,001-500,000", "R$500,001-1,000,000", "R$1,000,001-3,000,000") ## updated code
    } 
    else if (func_metric_input == "total_sellers") {
      bin_info$bin_ranges <- list(c(0, 10), c(10, 20), c(20, 40), c(40, 100), c(100, 500), c(500, 1000)) ## updated code
      bin_info$bin_labels <- c("0-10", "11-20", "21-40", "41-100", "101-5000", "501-1000") ## updated code
    } 
    else if (func_metric_input == "avg_price_per_seller") {
      bin_info$bin_ranges <- list(c(0, 250), c(250, 500), c(500, 1000), c(1000, 2000), c(2000, 4000))
      bin_info$bin_labels <- c("R$0-250", "R$251-500", "R$501-1,000", "R$1,001-2,000", "R$2,001-4,000") ## updated code
    } 
    else if (func_metric_input == "total_seller_LTV") {
      bin_info$bin_ranges <- list(c(0, 5000), c(5000, 10000), c(10000, 30000), c(30000, 100000), c(100000, 200000), c(200000, 1000000))
      bin_info$bin_labels <- c("R$0-5,000", "R$5,001-10,000", "R$10,001-30,000", "R$30,001-100,000", "R$100,001-200,000", "R$200,001-1,000,000") ## updated code
    } 
    else if (func_metric_input == "avg_seller_LTV") {
      bin_info$bin_ranges <- list(c(0, 1000), c(5000, 10000), c(3000, 6000), c(6000, 10000), c(10000, 25000), c(25000, 50000)) ##! why are there gaps in the ranges?
      bin_info$bin_labels <- c("R$0-1,000", "R$5,000-10,000", "R$3,000-6,000", "R$6,000-10,000", "R$10,000-25,000", "R$25,000-50,000") ## updated code
    } 
    else if (func_metric_input == "avg_ship_share_per_seller") {
      bin_info$bin_ranges <- list(c(0, 10), c(10, 20), c(25, 50), c(50, 75), c(75, 100), c(100, 200)) ##! why are there gaps?
      bin_info$bin_labels <- c("0-10%", "11%-20%", "25%-50%", "51%-75%", "76%-100%", "101%-200%") ## updated code
    } 
    else if (func_metric_input == "avg_expected_ship_time_per_seller") {
      bin_info$bin_ranges <- list(c(0, 10), c(10, 20), c(20, 30), c(30, 40), c(40, 50), c(50, 75))
      bin_info$bin_labels <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-75") ## updated code
    }
    else if (func_metric_input == "avg_review_score_per_seller") {
      bin_info$bin_ranges <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 5))
      bin_info$bin_labels <- c("1-2", "2-3", "3-4", "4-5")
    }
  }
  ## 2. customer x municipality x func_metric_input
  else if (func_datatype_input == "customer" & func_aggregation_input == "muni") {
    if (func_metric_input == "total_sales") {
      bin_info$bin_ranges <- list(c(0, 10000), c(10000, 50000), c(50000, 100000), c(100000, 200000), c(200000, 500000), c(500000, 1000000), c(1000000, 3000000)) ## updated code
      bin_info$bin_labels <- c("R$0-10,000", "R$10,001-50,000", "R$50,001-100,000", "R$100,001-200,000", "R$200,001-500,000", "R$500,001-1,000,000", "R$1,000,001-3,000,000") ## updated code
    } 
    else if (func_metric_input == "total_orders") {
      bin_info$bin_ranges <- list(c(0, 3), c(3, 10), c(10, 30), c(30, 100), c(100, 1000), c(1000, 10000), c(1000, 20000))
      bin_info$bin_labels <- c("Up to 3", "4 - 10", "11 - 30", "31 - 100", "101 - 1,000", "1,001 - 10,000", "10,001 - 20,000")
    } 
    else if (func_metric_input == "aov") {
      bin_info$bin_ranges <- list(c(0, 80), c(80, 120), c(120, 160), c(160, 200), c(200, 500), c(500, 2300))
      bin_info$bin_labels <- c("Up to R$80", "R$81 - R$120", "R$121 - R$160", "R$161 - R$200", "R$201 - R$500", "Over R$500") ## updated code
    } 
    else if (func_metric_input == "avg_delivery_time") {
      bin_info$bin_ranges <- list(c(0, 10), c(10, 15), c(15, 20), c(20, 25), c(25, 35), c(35, 120))
      bin_info$bin_labels <- c("Up to 10 days", "11 - 15 days", "16 - 20 days", "21 - 25 days", "26 - 35 days", "Over 35 days") ## updated code
    } 
    else if (func_metric_input == "avg_expected_delivery_time") {
      bin_info$bin_ranges <- list(c(5, 20), c(20, 25), c(25, 30), c(30, 40), c(40, 60), c(60, 90))
      bin_info$bin_labels <- c("5 - 20 days", "21 - 25 days", "26 - 30 days", "31 - 40 days", "41 - 60 days", "60 - 90 days") ## updated code
    } 
    else if (func_metric_input == "avg_ship_share") {
      bin_info$bin_ranges <- list(c(0, 15), c(15, 25), c(25, 35), c(35, 50), c(50, 75), c(75, 450))
      bin_info$bin_labels <- c("Up to 15%", "16% - 25%", "26% - 35%", "36% - 50%", "51% - 75%", "Over 75%") ## updated code
    } 
    else if (func_metric_input == "total_unique_customers") {
      bin_info$bin_ranges <- list(c(0, 3), c(3, 10), c(10, 100), c(100, 1000), c(1000, 10000), c(10000, 20000))
      bin_info$bin_labels <- c("Up to 3", "4 - 10", "11 - 20", "101 - 1,000", "1,001 - 10,000", "Over 10,000")
    } 
    else if (func_metric_input == "total_repeat_customers") {
      bin_info$bin_ranges <- list(c(0, 2), c(2, 10), c(10, 100), c(100, 500), c(500, 1000), c(1000, 2000))
      bin_info$bin_labels <- c("0 - 2", "3 - 10", "11 - 100", "101 - 500", "501 - 1,000", "Over")
    } 
    else if (func_metric_input == "avg_review_score") {
      bin_info$bin_ranges <- list(c(0, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
      bin_info$bin_labels <- c("0 - 2", "2 - 3", "3 - 4", "4 - 4.5", "4.5 - 5")
    }
  }
  ## 3. seller x state x func_metric_input
  ## 3. customer x state x func_metric_input
  ##! this section covers customer options well. what about the non-"total_sales" options for seller?
  else if (func_aggregation_input == "state") {
    if (func_metric_input == "total_sales") {
      bin_info$bin_ranges <- list(c(0, 100000), c(100000, 250000), c(250000, 500000), c(500000, 1000000), c(1000000, 3000000), c(3000000, 5200000))
      bin_info$bin_labels <- c("Up to R$100k", "R$100k - R$250k", "R$250k - R$500k", "R$500k - R$1M", "R$1M - R$3M", "Over R$3M")
    } 
    else if (func_metric_input == "total_orders") {
      bin_info$bin_ranges <- list(c(0, 500), c(500, 1000), c(1000, 2500), c(2500, 5000), c(5000, 20000), c(20000, 42000))
      bin_info$bin_labels <- c("Up to 500", "501 - 1k", "1k - 2.5k", "2.5k - 5k", "5k - 20k", "Over 20k")
    } 
    else if (func_metric_input == "aov") {
      bin_info$bin_ranges <- list(c(0, 140), c(140, 160), c(160, 180), c(180, 200), c(200, 215))
      bin_info$bin_labels <- c("Up to R$140", "R$141 - R$160", "R$161 - R$180", "R$181 - R$200", "Over R$200") ## updated code
    } 
    else if (func_metric_input == "avg_delivery_time") {
      bin_info$bin_ranges <- list(c(0, 15), c(15, 18), c(18, 21), c(21, 25), c(25, 30), c(30, 35))
      bin_info$bin_labels <- c("Up to 15 days", "16 - 18 days", "19 - 21 days", "22 - 25 days", "26 - 30 days", "Over 30 days") ## updated code
    } 
    else if (func_metric_input == "avg_expected_delivery_time") {
      bin_info$bin_ranges <- list(c(0, 25), c(25, 28), c(28, 31), c(31, 35), c(35, 40), c(40, 50))
      bin_info$bin_labels <- c("Up to 25 days", "26 - 28 days", "29 - 31 days", "32 - 35 days", "36 - 40 days", "Over 40 days") ## updated code
    } 
    else if (func_metric_input == "avg_ship_share") {
      bin_info$bin_ranges <- list(c(0, 17), c(17, 20), c(20, 23), c(23, 26), c(26, 30))
      bin_info$bin_labels <- c("Up to 17%", "18% - 20%", "21% - 23%", "24% - 26%", "Over 26%") ## updated code
    } 
    else if (func_metric_input == "total_unique_customers") {
      bin_info$bin_ranges <- list(c(0, 400), c(400, 900), c(900, 2300), c(2300, 4000), c(4000, 20000), c(20000, 42000))
      bin_info$bin_labels <- c("Up to 400", "401 - 900", "901 - 2.3k", "2.3k - 4k", "4k - 20k", "Over 20k") ## updated code
    } 
    else if (func_metric_input == "total_repeat_customers") {
      bin_info$bin_ranges <- list(c(0, 30), c(30, 100), c(100, 300), c(300, 1000), c(1000, 3000), c(3000, 5000))
      bin_info$bin_labels <- c("Up to 30", "31 - 100", "101 - 300", "301 - 1k", "1k - 3k", "Over 3k") ## updated code
    } 
    else if (func_metric_input == "avg_review_score") {
      bin_info$bin_ranges <- list(c(0, 2), c(2, 3), c(3, 4), c(4, 4.5), c(4.5, 5))
      bin_info$bin_labels <- c("0 - 2", "2 - 3", "3 - 4", "4 - 4.5", "4.5 - 5")
    }
  }
 
  return(bin_info)
}

# Function to get the region name based on aggregation level
## flipped your comments "for seller data" & "for customer data" since they were in the wrong spots
getRegionName <- function(func_datatype_input, func_aggregation_input, data) {
  if (func_aggregation_input == "state") {
    return(data$name_state)
  } else if (func_aggregation_input == "muni") {
    # Different column names for seller and customer data
    if (func_datatype_input == "customer") {
      return(data$name_muni)  # For customer data
    } else {
      return(data$muni_name)  # For seller data
    }
  # } else if (func_aggregation_input == "zip") {
  #   return(data$customer_zip_code_prefix)
  }
}

# Function to get the metric value based on data type
getMetricValue <- function(func_metric_input, data) {
  return(data[[func_metric_input]])
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

#choices for third level (no information on zip for seller)
## reordered choices to alphabetical order and so that municipality is the first option since it's pre-selected in UI
sell_choices = c("Municipality" = "muni")
cust_choices = c("Municipality" = "muni", "State" = "state")