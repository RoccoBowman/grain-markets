# Reproduction Scripts for Bowman, Henderson, Ryavec, and Wu
#   Markets and Macroregions: Evidence from Qing Grain Prices, 1736-1842
#   2022

# Script 2/4: Exploring initial time series data and missingness

# Author: Rocco Bowman
# Contact: bowman.rocco@gmail.com

# Begin script

# 1. Loading data ---------------------------------------------------------

setwd("~/Grain Price Project")

data <- read.csv("output/02_monthly_means.csv")

# 1. Graphing present data in time series ----------------------------------

  library(tidyverse)
  
  # plotting histogram of data presence by year across all prefectures and
  # noting where historical events intersect with missing data
  ggplot(data,aes(year))+
    geom_bar() +
    scale_x_continuous(name ="Year", breaks=seq(min(data$year),max(data$year),10)) +
    scale_y_continuous(name = "Count") +
    geom_vline(xintercept = c(1820, 1850), lwd = 1.5) +
    annotate("text", x=1800, y=290, label= "Beginning of\n Daoguang Emperor's\n Reign") +
    annotate("text", x=1865, y=290, label= "Beginning of\n Taiping Rebellion") +
    ggtitle("") +
    theme_classic()

  # calculate how many years are reported by prefectures

  share <- data %>%
    group_by(W1_ID) %>%
    summarise(years = length(unique(year)))

  # join summary data to shapefile for map plot
  
  library(sf)
  
  shp <- st_read("data/final_thiessens_validated.shp") %>%
    left_join(share)
  
  shp
  
  # Map the data
  tm_shape(shp) + 
    tm_polygons(col = "years",
                border.col = "black",
                colorNA = "white",
                title = "No Data Freq.",
                style = "jenks",
                n = 5,) +
    tm_layout(legend.bg.color = "white",
              legend.frame = "black",
              aes.palette = list(seq = "Greys"))
  

# 2. Calculating predominate grains in each prefecture --------------------


  # create function to calculate mode of grain mentions
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # calculate grain mode for each prefecture
  all_grain <- tibble()
  for (i in unique(data$W1_ID)){
    fu <- data %>% filter(W1_ID == i)
    mode <- getmode(fu$grain)
    grain <- tibble(W1_ID = unique(fu$W1_ID), grain = mode)
    all_grain <- bind_rows(all_grain, grain)
  }
  
  # join grain modes to shapefile
  shp_grain <- shp %>%
    left_join(all_grain)
  
  # map predominate grains per prefecture
  ggplot() +
    geom_sf(data = shp_grain, aes(fill = grain)) +
    ggtitle("Predominating Grain for Each Prefecture") +
    theme_classic()


# 3. Visualizing Missing Values -------------------------------------------

  library(naniar)
  
  # any NA?
  any_na(data[34:45])
  
  # how many?
  n_miss(data[34:45])
  prop_miss(data[34:45])
  
  # which months are missing the most data?
  gg_miss_var(data %>% select(34:45)) # no real pattern
  
  # visualize overall missing position
  vis_miss(data %>% select(34:45)) + 
    ggtitle ("Missing Data Across Data Frame") + 
    theme(axis.text.x = element_text(angle=70)) # again, no real pattern
  
  time_series <- read_csv("output/03_full_time_series.csv")
  
  ts_pt1 <- time_series %>% 
    select(4:171)
  ts_pt2 <- time_series %>% 
    select(172:342)
  
  
  gg_miss_var(ts_pt1)
  gg_miss_var(ts_pt2)
  
  vis_miss(ts_pt1) + 
    ggtitle ("Missing Data Across Data Frame") + 
    theme(axis.text.x = element_text(angle=70)) # scattered long runs of missing values
  
  vis_miss(ts_pt2) + 
    ggtitle ("Missing Data Across Data Frame") + 
    theme(axis.text.x = element_text(angle=70)) # Xinjiang and Yunnan are missing most

# End Script
