## - these are my comments
##! - an error that needs your input

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tidyr)

#............................................................................... Load data
muni_map_sf = readRDS("muni_map_sf.rds")
custmetrics_state_geo = readRDS("custmetrics_state_geo.rds")
custmetrics_muni_geo = readRDS("custmetrics_muni_geo.rds")
custmetrics_zip_geo = readRDS("custmetrics_zip_geo.rds")

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
  else if (func_aggregation_input == "zip") {
    data <- custmetrics_zip_geo
  }

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
      bin_info$bin_labels <- c("R$0-10,000", "R$10,001-50,000", "R$50,001-100,000", "R$100,001-200,000", "R$200,001-500,000", "R$500,001-1,000,000", "R$1000,001-3,000,000") ## updated code
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
      bin_info$bin_ranges <- list(c(1, 500), c(500, 2000), c(2000, 5000), c(5000, 10000), c(10000, 50000), c(50000, 2000000))
      bin_info$bin_labels <- c("Up to R$500", "R$501 - R$2k", "R$2k - R$5k", "R$5k - R$10k", "R$10k - R$50k", "Over R$50k")
    } 
    else if (func_metric_input == "total_orders") {
      bin_info$bin_ranges <- list(c(0, 3), c(3, 10), c(10, 30), c(30, 100), c(100, 1000), c(1000, 16000))
      bin_info$bin_labels <- c("Up to 3", "4 - 10", "11 - 30", "31 - 100", "101 - 1k", "Over 1k")
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
      bin_info$bin_ranges <- list(c(0, 3), c(3, 10), c(10, 20), c(20, 50), c(50, 100), c(100, 16000))
      bin_info$bin_labels <- c("Up to 3", "4 - 10", "11 - 20", "21 - 50", "51 - 100", "Over 100")
    } 
    else if (func_metric_input == "total_repeat_customers") {
      bin_info$bin_ranges <- list(c(0, 1), c(1, 2), c(2, 5), c(5, 10), c(10, 50), c(50, 2000))
      bin_info$bin_labels <- c("0", "1", "2 - 5", "6 - 10", "11 - 50", "Over 50")
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
  ## 4. customer x zip code x func_metric_input
  else if (func_aggregation_input == "zip") {
    if (func_metric_input == "total_sales") {
      bin_info$bin_ranges <- list(c(0, 200), c(200, 500), c(500, 1200), c(1200, 3000), c(3000, 10000), c(10000, 22500))
      bin_info$bin_labels <- c("Up to R$200", "R$201 - R$500", "R$501 - R$1.2k", "R$1.2k - R$3k", "R$3k - R$10k", "Over R$10k") ## updated code
    } 
    else if (func_metric_input == "total_orders") {
      bin_info$bin_ranges <- list(c(0, 3), c(3, 5), c(5, 10), c(10, 20), c(20, 50), c(50, 150))
      bin_info$bin_labels <- c("Up to 3", "4 - 5", "6 - 10", "11 - 20", "21 - 50", "Over 50")
    } 
    else if (func_metric_input == "aov") {
      bin_info$bin_ranges <- list(c(0, 80), c(80, 100), c(100, 130), c(130, 160), c(160, 200), c(200, 2400))
      bin_info$bin_labels <- c("Up to R$80", "R$81 - R$100", "R$101 - R$130", "R$131 - R$160", "R$161 - R$200", "Over R$200") ## updated code
    } 
    else if (func_metric_input == "avg_delivery_time") {
      bin_info$bin_ranges <- list(c(0, 8), c(8, 12), c(12, 16), c(16, 24), c(24, 35), c(35, 200))
      bin_info$bin_labels <- c("Up to 8 days", "9 - 12 days", "13 - 16 days", "17 - 24 days", "25 - 35 days", "Over 35 days") ## updated code
    } 
    else if (func_metric_input == "avg_expected_delivery_time") {
      bin_info$bin_ranges <- list(c(0, 20), c(20, 24), c(24, 28), c(28, 32), c(32, 40), c(40, 100))
      bin_info$bin_labels <- c("Up to 20 days", "21 - 24 days", "25 - 28 days", "29 - 32 days", "33 - 40 days", "Over 41 days") ## updated code
    } 
    else if (func_metric_input == "avg_ship_share") {
      bin_info$bin_ranges <- list(c(0, 20), c(20, 25), c(25, 30), c(30, 40), c(40, 60), c(60, 750))
      bin_info$bin_labels <- c("Up to 20%", "21% - 25%", "26% - 30%", "31% - 40%", "41% - 60%", "Over 60%") ## updated code
    } 
    else if (func_metric_input == "total_unique_customers") {
      bin_info$bin_ranges <- list(c(0, 3), c(3, 5), c(5, 10), c(10, 20), c(20, 50), c(50, 150))
      bin_info$bin_labels <- c("Up to 3", "4 - 5", "6 - 10", "11 - 20", "21 - 50", "Over 50")
    } 
    else if (func_metric_input == "total_repeat_customers") {
      bin_info$bin_ranges <- list(c(0, 1), c(1, 2), c(2, 5), c(5, 10), c(10, 20), c(20, 30))
      bin_info$bin_labels <- c("0", "1", "2 - 5", "6 - 10", "11 - 20", "Over 20")
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
  } else if (func_aggregation_input == "zip") {
    return(data$customer_zip_code_prefix)
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
sell_choices = c("Municipality" = "muni", "State" = "state")
cust_choices = c("Municipality" = "muni", "State" = "state", "Zip Code" = "zip")