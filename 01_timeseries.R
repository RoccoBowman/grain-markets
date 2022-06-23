# Reproduction Scripts for Bowman, Henderson, Ryavec, and Wu
#   Markets and Macroregions: Evidence from Qing Grain Prices, 1736-1842
#   2022

# Script 1/4: Creating monthly price times series for each prefecture

# Author: Rocco Bowman
# Contact: bowman.rocco@gmail.com

# Begin script

# 1. Read in data ---------------------------------------------------------

  library(readxl)
  
  # set whatever folder is holding the data on your machine
  setwd("~/Grain Price Project")
  
  # create a subdirectory to hold all future outputs if none exists
  if (dir.exists(file.path(getwd(), 'output')) == TRUE) {
    print("Sub-directory found")
  } else {
  dir.create(file.path(getwd(), 'output'))
  }
  
  # read in excel spreadsheet
  price_data <- read_xls("data/allgrain_W1.xls")
  
  # save out as csv
  write.csv(price_data,
            file = "output/01_grain_data.csv",
            row.names = FALSE)


# 2. Calculate monthly mean prices --------------------------------
  
  library(tidyverse)
  
  # create vector of months
  months <- c("Month1","Month2","Month3","Month4","Month5","Month6","Month7","Month8","Month9","Month10","Month11","Month12")
  
  # merge the columns to the full data set then make them of a numeric data type
  price_data[months] <- NA
  price_data[months] <- sapply(price_data[months], as.numeric)
  
  # split data by prefecture
  split <- price_data %>%
    group_by(W1_ID) %>%
    group_split()
  
  # iterate through each prefecture and calculate monthly means
  for (i in 1:length(split)){
    k <- 6
    for (j in 34:45) {
      k <- k + 1
      low <- split[[i]][,k]
      k <- k + 1
      high <- split[[i]][,k]
      split[[i]][,j] <- ((low + high) / 2)
      
    }
    print(paste0("Calculating means for ",split[[i]]$W1_ID[1]))
  }

  # compile results
  all_means <- do.call(rbind,split)

  # removing potentially unneeded columns
  # means_trimmed <- all_means %>%
  #   select(c(3,34:45))
  
  write.csv(all_means,file = "output/02_monthly_means.csv", row.names = FALSE)


# 3. Reshaping price data into monthly time series ------------------------
  
  # stretch year and month labels into long form to make a master list
  all_ts <- tibble(year = as.character(rep(min(all_means$year):max(all_means$year),each = 12, length = 2112)),
                   month = as.character(str_pad(rep(1:12,each = 1, length = 2112),2, side = "left", pad="0")))
  
  # iterate through each prefecture
  for (i in unique(all_means$W1_ID)){
    print(paste0("Pivoting ",i))
    
    # filter for prefecture id and select for columns of interest
    pre_slice <- all_means %>% 
      filter(W1_ID == i) %>%
      select(c(3,6,34:45))
    
    # stretch month-price matrix into vector time series
    prices <- tibble(prices = as.vector(t(pre_slice[,3:length(pre_slice)])))
    colnames(prices) <- pre_slice[1,1]
    
    # repeat year and month columns for 12 months per year
    rep <- pre_slice[rep(seq_len(nrow(pre_slice)), each = 12), ]
    months <- tibble(month = as.character(str_pad(rep(1:12,each = 1, length = nrow(rep)),2, side = "left", pad="0")))
    years <- tibble(year = as.character(rep$year))
    
    # bind years, month, and price columns together for one price per year-month combo
    prefecture_ts <- bind_cols(years,months,prices)
    
    # append prefecture time series to master list
    all_ts <- left_join(all_ts,prefecture_ts)
    
    print(paste0("Appending time series ",pre_slice[1,1],"..."))
    
  }    

  # removing first few years since it is a NA-laden half-decade
  all_ts_trim <- all_ts %>%
    filter(year >= 1740 & year <= 1909)
  
  number_of_decades <- nrow(all_ts_trim) / 120  

  # fill in decade info
  all_ts_decade <- all_ts_trim %>%
    mutate(decade = str_pad(rep(1:17,each = 120),2, side = "left","0"),
           .after = month)  
  
  # write out the time series series
  write.csv(all_ts_decade, file = "output/03_full_time_series.csv", row.names = FALSE)
  
# End script
  