# Reproduction Scripts for Bowman, Henderson, Ryavec, and Wu
#   Markets and Macroregions: Evidence from Qing Grain Prices, 1736-1842
#   2022

# Script 3/4: Calculating correlation coefficients between all prefectures
#             creating network graph, and detecting communities

# Author: Rocco Bowman
# Contact: bowman.rocco@gmail.com

# Begin script


# 1. Loading packages and data --------------------------------------------

  library(igraph) 
  library(sf) 
  library(rgdal) 
  library(tidyverse)
  library(spdep) 
  library(tmap) 
  library(tmaptools) 
  library(reshape2) 
  library(igraph) 
  
  print("Loading data...")

  # establish working directory
  # replace with your own directory where the data is held
  setwd("~/Grain Price Project")
  
  # load in initial spatial and tabular data
  thiessen_sp <- readOGR("data/final_thiessens_validated.shp")
  thiessen_sf <- st_read("data/final_thiessens_validated.shp")
  
  physiographic_macroregions <- st_read("data/v5_PMR_pgn_utf_xian.shp")
  
  all_timeseries <- read.csv("output/03_full_time_series.csv")
  
  # pad decade tags with zeroes for easier arrangement (smallest to largest)
  all_timeseries$decade <- str_pad(all_timeseries$decade,
                                   2,
                                   side = "left",
                                   pad = "0")
  
  print("Filtering data...")
  
  # filter all data up to 1842
  data_filter <- all_timeseries %>%
    filter(year <= 1842)
  
  # select out only the times series for prefectures and cuts out averages and duplicates
  want <- thiessen_sf$W1_ID 
  
  data_filter <- data_filter %>% 
    select(-c(1:4)) %>% 
    select(want)
  
  # iterate backwards through times series data and prune if needed
  all_na_sum <- tibble()
  for (i in seq(length(data_filter),1,-1)) { 
    na_sum <- sum(is.na(data_filter[i]))
    add <- tibble(na_sum = na_sum)
    all_na_sum <- bind_rows(all_na_sum, add)
    if (na_sum >= (nrow(data_filter) * 0.75)) {
      data_filter <- data_filter %>% select(-i)
    }
  }

  # count how many existing data points for each prefecture
  all_counts <- tibble()
  for (i in seq(1, length(data_filter))) {
    name <- colnames(data_filter)[i]
    na_sum <- sum(is.na(data_filter[i]))
    tibble <- tibble(W1_ID = name,
                     na = na_sum,
                     exist = nrow(data_filter) - na_sum)
    tibble <- tibble %>% 
      mutate(percent = signif(exist / nrow(data_filter),digits=2))
    all_counts <- bind_rows(all_counts,tibble)
  }
  
  counts_shp <- thiessen_sf %>% 
    left_join(all_counts)
  
  # tm_shape(counts_shp) + 
  #   tm_polygons(col = "percent",
  #               border.col = "black",
  #               colorNA = "grey",
  #               title = "Proportion of reporting\nmonths to total (n=1236)",
  #               style = "quantile",
  #               n = 5) +
  #   tm_layout(legend.bg.color = "white",
  #             legend.frame = "black",
  #             aes.palette = list(seq = "Reds")) +
  #   tm_add_legend(labels = c("22% to 79%", "79% to 81%", "81% to 82%", "82% to 84%", "84% to 88%")
  #                 )
  #   tm_add_legend(type = "fill", 
  #               labels = c("0", "1 to 20", "21 to 40", "41 to 60", "61 to 80", "81 to 100"),
  #               col = c("grey", "#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
  #               border.lwd = 0.5,
  #               title = "Count")
    
    tmap <- tm_shape(counts_shp) + 
      tm_polygons(col = "percent",
                  #border.col = "black",
                  colorNA = "grey",
                  title = "Proportion of reporting\nmonths to total (n=1236)",
                  style = "jenks",
                  n = 5,
                  legend.show = FALSE)+
      tm_layout(frame = TRUE,
                legend.frame = TRUE,
                legend.title.size = 1.5,
                legend.text.size = 1) +
      tm_add_legend(type = "fill",
                    #labels = c("Missing or Insufficient","20% to 79%", "79% to 81%", "81% to 82%", "82% to 84%", "84% to 88%"),
                    labels = c("Missing or insufficient", "22% to 36%", "37% to 50%", "51% to 70%", "70% to 82%", "82% to 88%"),
                    col = c("grey", "#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
                    border.lwd = 0.5,
                    border.col = "black",
                    title = "Data availability\n(out of 1236 possible)")
  tmap
  tmap_save(tmap, "figure_3.pdf", width=9, height=6, asp=0, units = "in")
    
  print("Reporting spatial missing data...")
  
  # transpose correlation data into spatial format 
  transpose <- data.frame(t(data_filter)) %>%
    select(1) %>% 
    mutate(X1 = rownames(.)) %>% 
    rename(W1_ID = X1)

  # right join transposed data to shapefile
  shp_trim <- thiessen_sf %>% 
    right_join(transpose)
  

# 2. Calculating correlation matrix ---------------------------------------

  # Unnecessary if you already have the correlation data to load in!

  # all_tests <- tibble() # Creates an empty set to collect info on all pairwise correlations
  # 
  # # Calculate pearson and cross correlations for each pairwise match of time series
  # for (i in 1:length(data_filter)) {
  #   print(paste0(i,"/",length(data_filter)))
  #   col1 <- data_filter[[i]]
  #   col1_name <- colnames(data_filter[i])
  #   auto_exclude <- data_filter %>% select(-i)
  #   for (j in 1:length(auto_exclude)){
  #     #print(paste0(j,"/",length(auto_exclude)))
  #     col2 <- auto_exclude[[j]]
  #     col2_name <- colnames(auto_exclude[j])
  # 
  #     no_na <- tibble(x = col1, y = col2) %>%
  #       drop_na()
  # 
  #     if (nrow(no_na) < 10) {
  #       next
  #     }
  # 
  #     ptest <- cor.test(no_na$x, no_na$y, method = "pearson", na.action = "na.omit")
  #     model <- summary(lm(no_na$y ~ no_na$x))
  # 
  #     ts1 <- ts(no_na$x)
  #     ts2 <- ts(no_na$y)
  # 
  #     diff1 <- diff(ts1)
  #     diff2 <- diff(ts2)
  # 
  #     ccf <- ccf(diff1, diff2, ylab = "Cross-correlation", plot = FALSE)
  #     ccf2 <- tibble(lag = ccf$lag, acf = ccf$acf) %>%
  #       filter(lag > -2 & lag < 2)
  #     cc_p_val <- 2 * (1 - pnorm(abs(ccf2$acf[2]), mean = 0, sd = 1/sqrt(ccf$n.used)))
  # 
  #     test_info <- tibble(x = col1_name,
  #                         y = col2_name,
  #                         t_stat_p = ptest[[1]],
  #                         df_p = ptest[[2]],
  #                         p_p = ptest[[3]],
  #                         r_p = ptest[[4]],
  #                         adj_r2 = model$adj.r.squared,
  #                         alt_p = ptest[[6]],
  #                         cc_before = ccf2$acf[1],
  #                         cc_same = ccf2$acf[2],
  #                         cc_after = ccf2$acf[3],
  #                         cc_p = cc_p_val)
  # 
  # 
  #     all_tests <- bind_rows(all_tests, test_info)
  #   }
  # }
  # 
  #  all_tests$diff <- abs(all_tests$r_p - all_tests$cc_same)
  # # 
  #  write.csv(all_tests, "output/all_tests_after_1842.csv", row.names = FALSE)
  # 
  # Begin here to simply load in the resulting correlations
  
  print("Loading test results...")
  
  all_tests <- read_csv("output/all_tests_before_1842.csv")
  #all_tests <- read_csv("output/all_tests_after_1842.csv")
  
  # transform correlation results into a matrix (to be multiplied to weight matrix later)
  tests_wider <- pivot_wider(all_tests %>% select(x,y,cc_same), names_from = y, values_from = cc_same)
  rownames <- tests_wider$x
  tests_wider <- tests_wider %>%
    select(-1)
  tests_mat <- as.matrix(tests_wider)
  rownames(tests_mat) <- rownames

# 3. Analysis parameters --------------------------------------------------

  # set the cutoff for cluster size (anything smaller than X)
  min_cluster_size <- 5 
  
  # set the cut-off level for correlation coefficients (higher means stricter filter)
  sig_level <- 0 
  

# 4. Generating spatial weights matrix and filtering final data -----------

  print("Generating spatial weights matrix...")
  
  # convert shapefile back to sp and generate spatial adjacency matrix
  shp_trim_sp <- as(shp_trim, "Spatial")
  
  queen <- poly2nb(shp_trim_sp, queen = TRUE)
  xy <- coordinates(shp_trim_sp)

  plot(thiessen_sp, col='white', border='grey', lwd=2)
  plot(queen, xy, col='black', lwd=2, add = TRUE)
  
  cardinality <- card(queen)
  hist(cardinality, main = paste0("Histogram of Neighbor Linkages\nMean = ",round(mean(cardinality), 2)), xlab = "Number of Neighbors")

  wm <- nb2mat(queen, style='B', zero.policy = TRUE)
  colnames(wm) <- colnames(data_filter)
  rownames(wm) <- colnames(data_filter)

  # This code chunk creates a k nearest neighbors matrix which should replace
  # the previous matrix but was not used in the final analysis
  
  # coords <- coordinates(shp_trim_sp)
  # knn <- knearneigh(coords, k = 8)
  # w <- knn2nb(knn)
  # xy <- coordinates(shp_trim_sp)
  # plot(shp_trim_sp, col='white', border='grey', lwd=2)
  # plot(w, xy, col='red', lwd=2, add=TRUE)
  # wm <- nb2mat(w, style='B')
  # wm_df <- as.data.frame(wm)
  # colnames(wm) <- colnames(data_filter)
  # rownames(wm) <- colnames(data_filter)
  # 
  # wm <- nb2mat(w, style='B', zero.policy = TRUE)
  # wm_df <- as.data.frame(wm)
  # colnames(wm) <- colnames(data_filter)
  # rownames(wm) <- colnames(data_filter)
  
  # arrange matrix of correlation data into long format
  tests_longer <- melt(tests_mat)
  tests_longer$Var1 <- as.character(tests_longer$Var1)
  tests_longer$Var2 <- as.character(tests_longer$Var2)
  tests_longer <- tests_longer %>% arrange(Var1, Var2)
  
  # Plot histogram of all correlation coefficients
  hist(tests_longer$value, main = "Histogram of all Cross-correlation Coefficients\nn = 43264", xlim = c(-1,1), xlab = "Coefficient")
  
  # arrange weight matrix into long format
  wm_longer <- melt(wm)
  wm_longer$Var1 <- as.character(wm_longer$Var1)
  wm_longer$Var2 <- as.character(wm_longer$Var2)
  wm_longer <- wm_longer %>% arrange(Var1, Var2)
  colnames(wm_longer) <- c("x","y","adj")
  
  # Connecting isolates to nearest neighbor
  
    # Taiwan
    wm_longer$adj[wm_longer$x == "FJ036" & wm_longer$y == "FJ025"] <- 1
    
    # Manchuria
    wm_longer$adj[wm_longer$x == "SJ019" & wm_longer$y == "SJ001"] <- 1  


  print("Applying weight matrix and other filters...")
  
  # filter out only positive correlations and those over the chosen threshold (networks don't like negative ones)
  combine <- all_tests %>%
    left_join(wm_longer)
  combine_neighbors <- combine %>% 
    filter(adj > 0)
  combine_significant <- combine_neighbors %>% 
    filter(cc_p < 0.05)
  combine_positive <- combine_significant %>%  
    filter(cc_same > 0)
  
    # grab a significance level based on quantiles
  # sig_level <- quantile(positive_coef$coef, na.rm = TRUE)[[1]] # set the cutoff for correlation coefficient filter (anything smaller or equal to 4th quintile)
  
  only_sig <- combine_positive %>%
    filter(cc_same >= sig_level)
  
  no_duplicates <- only_sig %>%
    mutate(normalized = map2_chr(x, y, ~paste(sort(c(.x, .y)), collapse = ""))) %>%
    group_by(normalized) %>%
    summarise(x = first(x),
                     y = first(y)) %>%
    select(-normalized) %>% 
    left_join(all_tests)
  
  hist(no_duplicates$cc_same, xlab = "Coefficent", main = "Histogram of Corr. Coefficients for Pre-1842")
  
  write.csv(no_duplicates, "output/05_network_edges.csv", row.names = FALSE)

# 5. Creating network graph and detecting communities ---------------------

  print("Generating network graph and detecting communities...")
  
  # drawing paths between nodes and creating undirected graph object
  initial_graph <- graph_from_data_frame(no_duplicates,
                                         directed = FALSE)
  weighted_graph <- set_edge_attr(initial_graph,
                                  "weight",
                                  value = no_duplicates$cc_same)
  
  # applying Louvin community detection algorithm to find clusters in the graph
  community <- cluster_louvain(weighted_graph,
                               weights = E(weighted_graph)$weight)
  algorithm <- algorithm(community)
  modularity <- round(modularity(community),2)
  transitivity <- transitivity(weighted_graph,
                               type = "global",
                               weights = E(weighted_graph)$weight)
  
  cluster_id <- tibble(
    W1_ID = community$names,
    cluster = as.factor(community$membership)
    )
  
  # trim clusters that are too small
  only_big <- cluster_id %>% 
    group_by(cluster) %>% 
    filter(n() >= min_cluster_size)
  
  # join graph data to shapefile
  cluster_spatial <- thiessen_sf %>%
    right_join(only_big)
  
  # reproject shapefile to Asia North Albers Equal Area Conic
  cp_t <- st_transform(cluster_spatial, "ESRI:102025")
  
  # intersecting Skinner's physiographic macroregion names with clustered prefectures
  pmr_transform <- st_transform(physiographic_macroregions,
                                crs = "ESRI:102025")
  union <- st_intersection(cp_t, pmr_transform) 
  cluster_union <- cluster_spatial %>%
    left_join(st_drop_geometry(union), by = c("W1_ID","cluster"))
  
  # rename clusters based on their pmr locations
  rename <- st_drop_geometry(cluster_union) %>%
    group_by(cluster) %>%
    count(SYS_NAME) %>%
    top_n(1) %>%
    arrange(SYS_NAME)
  
  print("Naming split clusters")
  rename$name_dup <- duplicated(rename$SYS_NAME)
  rename$clust_dup <- duplicated(rename$cluster)
  rename$na <- rowSums(is.na(rename))
  
  dup_counter <- 1
  for (i in 1:nrow(rename)){
    if (rename$na[i] == 1){
      rename$SYS_NAME[i] <- "Xinjiang*"
    }
    if (rename$name_dup[i] == TRUE){
      dup_counter <- dup_counter + 1
      rename$SYS_NAME[i] <- paste0(rename$SYS_NAME[i],"_",dup_counter)
    } else {
      dup_counter <- 1
    }
    print(rename$SYS_NAME[i])
  }
  
  rename_drop_tie <- rename %>%
    filter(clust_dup == FALSE)
  
  # join final results to shapefile and convert cluster info to factors
  clusters_complete <- cluster_spatial %>%
    left_join(rename_drop_tie) %>%
    drop_na()
  clusters_complete$SYS_NAME <- as.factor(clusters_complete$SYS_NAME)
  clusters_complete$cluster <- as.factor(as.numeric(clusters_complete$SYS_NAME))
  
  print("Plotting clusters...")
  
  # plot prefectures with cluster membership
  tmap <- tm_shape(thiessen_sf)+
      tm_borders() +
  tm_shape(clusters_complete) +
      tm_polygons(col = "SYS_NAME", border.col = "white") +
  tm_shape(physiographic_macroregions) +
      tm_borders(lwd = 2, col = "black") +
      tm_layout(title = "Louvain Communities Pre-1842",
                legend.bg.color = "white",
                legend.frame = "black",
                legend.outside = TRUE)
  
  tmap_save(tmap, "clusters_pre_1842.pdf", width=9, height=6, asp=0, units = "in")
  
  
  # save shapefile with louvin algorithm applied
  st_write(clusters_complete,
           dsn = paste0(getwd(), "/output"),
           layer = paste0("louvain_clusters_pre_1842",".shp"),
           driver = "ESRI Shapefile",
           delete_layer = TRUE)


# 6. Applying the Spinglass Algorithm -------------------------------------

  print("Applying Spinglass algorithm...")
    
  # Deleting unconnected isolates for spinglass algorithm
  wm_longer$adj[wm_longer$x == "SX044" & wm_longer$y == "SX038"] <- 0
  wm_longer$adj[wm_longer$x == "SX038" & wm_longer$y == "SX044"] <- 0
  
  # filter out only positive correlations and those over the chosen threshold (networks don't like negative ones)
  combine <- all_tests %>%
    left_join(wm_longer)
  combine_neighbors <- combine %>% 
    filter(adj > 0)
  combine_significant <- combine_neighbors %>% 
    filter(cc_p < 0.05)
  combine_positive <- combine_significant %>%  
    filter(cc_same > 0)
  
  #sig_level <- quantile(positive_coef$coef, na.rm = TRUE)[[1]] # set the cutoff for correlation coefficient filter (anything smaller or equal to 4th quintile)
  
  only_sig <- combine_positive %>%
    filter(cc_same >= sig_level)
  
  no_duplicates <- only_sig %>%
    mutate(normalized = map2_chr(x, y, ~paste(sort(c(.x, .y)), collapse = ""))) %>%
    group_by(normalized) %>%
    summarise(x = first(x),
              y = first(y)) %>%
    select(-normalized) %>% 
    left_join(all_tests)
  
  hist(no_duplicates$cc_same, xlab = "Coefficent", main = "")
  
  # drawing paths between nodes and creating undirected graph object
  
  initial_graph <- graph_from_data_frame(no_duplicates,
                                         directed = FALSE)
  weighted_graph <- set_edge_attr(initial_graph,
                                  "weight",
                                  value = no_duplicates$cc_same)
  
  # applying Spinglass algorithm to find communities
  community <- cluster_spinglass(
    weighted_graph,
    weights = E(weighted_graph)$weight,
    vertex = NULL,
    spins = 16,
    parupdate = FALSE,
    start.temp = 1,
    stop.temp = 0.01,
    cool.fact = 0.99,
    update.rule = "simple",
    gamma = 1,
    implementation = "orig",
    gamma.minus = 1
  )
  
  algorithm <- algorithm(community)
  modularity <- round(modularity(community),2)
  transitivity <- transitivity(weighted_graph,
                               type = "global",
                               weights = E(weighted_graph)$weight)
  
  cluster_id <- tibble(
    W1_ID = community$names,
    cluster = as.factor(community$membership)
  )
  
  # trim clusters that are too small
  only_big <- cluster_id %>% 
    group_by(cluster) %>% 
    filter(n() >= min_cluster_size)
  
  # join graph data to shapefile
  cluster_spatial <- thiessen_sf %>%
    inner_join(only_big)
  
  cp_t <- st_transform(cluster_spatial, "ESRI:102025")
  
  # intersecting pmr names with clusters
  pmr_transform <- st_transform(physiographic_macroregions,
                                crs = "ESRI:102025")
  
  union <- st_intersection(cp_t, pmr_transform) 
  
  cluster_union <- cluster_spatial %>%
    left_join(st_drop_geometry(union), by = c("W1_ID","cluster"))
  
  rename <- st_drop_geometry(cluster_union) %>%
    group_by(cluster) %>%
    count(SYS_NAME) %>%
    top_n(1) %>%
    arrange(SYS_NAME)
  
  print("Naming split clusters")
  rename$name_dup <- duplicated(rename$SYS_NAME)
  rename$clust_dup <- duplicated(rename$cluster)
  rename$na <- rowSums(is.na(rename))
  
  dup_counter <- 1
  for (i in 1:nrow(rename)){
    if (rename$na[i] == 1){
      rename$SYS_NAME[i] <- "Xinjiang*"
    }
    if (rename$name_dup[i] == TRUE){
      dup_counter <- dup_counter + 1
      rename$SYS_NAME[i] <- paste0(rename$SYS_NAME[i],"_",dup_counter)
    } else {
      dup_counter <- 1
    }
    print(rename$SYS_NAME[i])
  }
  
  rename_drop_tie <- rename %>%
    filter(clust_dup == FALSE)
  
  print("joining final data to shapefile")
  clusters_complete <- cluster_spatial %>%
    left_join(rename_drop_tie) %>%
    drop_na()
  
  clusters_complete$SYS_NAME <- as.factor(clusters_complete$SYS_NAME)
  clusters_complete$cluster <- as.factor(as.numeric(clusters_complete$SYS_NAME))
  
  print(
    tm_shape(thiessen_sf)+
      tm_borders() +
      tm_shape(clusters_complete) +
      tm_polygons(col = "cluster", border.col = "white") +
      tm_shape(physiographic_macroregions) +
      tm_borders(lwd = 2, col = "black") +
      tm_layout(title = "Spinglass Communities Pre-1842",
                legend.bg.color = "white",
                legend.frame = "black")
  )
  
  
  st_write(clusters_complete,
           dsn = "C:/Users/bowma/Documents/Grain Price Project/output",
           layer = paste0("spinglass_clusters_no_filter",".shp"),
           driver = "ESRI Shapefile",
           delete_layer = TRUE)


# 7. Extra analysis: No spatial weights -------------------------------------------------------


  combine <- all_tests %>%
    left_join(wm_longer)
  combine_significant <- combine %>% 
    filter(cc_p < 0.05)
  combine_positive <- combine_significant %>%  
    filter(cc_same > 0)
  
  only_sig <- combine_positive %>% 
    filter(cc_same >= sig_level)
  
  no_duplicates <- only_sig %>%
    mutate(normalized = map2_chr(x, y, ~paste(sort(c(.x, .y)), collapse = ""))) %>%
    group_by(normalized) %>%
    summarise(x = first(x),
              y = first(y)) %>%
    select(-normalized) %>% 
    left_join(all_tests) %>% 
    rename(coef = cc_same)
  
  
  hist(no_duplicates$coef, main = paste0("Histogram of Spatially-Constrained and Positive\nCross-correlation Coefficients\nn = ", nrow(no_duplicates)), xlab = "Coefficent")
  
  
  #Drawing paths between nodes and creating undirected graph object
  initial_graph <- graph_from_data_frame(no_duplicates, directed = FALSE)
  weighted_graph <- set_edge_attr(initial_graph, "weight", value = no_duplicates$coef)
  
  #Determining community and cluster membership
  community <- cluster_louvain(weighted_graph, weights = E(weighted_graph)$weight)
  algorithm <- algorithm(community)
  modularity <- round(modularity(community),2)
  transitivity <- transitivity(weighted_graph)
  
  cluster_id <- tibble(W1_ID = community$names, cluster = as.factor(community$membership))
  
  # Trim clusters that are too small
  only_big <- cluster_id %>% 
    group_by(cluster) %>% 
    filter(n() >= min_cluster_size)
  
  #Join graph data to shapefile
  cluster_spatial <- thiessen_sf %>%
    inner_join(only_big)
  
  # intersecting pmr names with clusters
  pmr_transform <- st_transform(physiographic_macroregions, crs = st_crs(cluster_spatial))
  union <- st_intersection(cluster_spatial, pmr_transform)
  cluster_union <- cluster_spatial %>%
    left_join(st_drop_geometry(union), by = c("W1_ID","cluster"))
  
  rename <- st_drop_geometry(cluster_union) %>%
    group_by(cluster) %>%
    count(SYS_NAME) %>%
    top_n(1) %>%
    arrange(SYS_NAME)
  
  print("Naming split clusters")
  rename$name_dup <- duplicated(rename$SYS_NAME)
  rename$clust_dup <- duplicated(rename$cluster)
  rename$na <- rowSums(is.na(rename))
  
  dup_counter <- 1
  for (i in 1:nrow(rename)){
    if (rename$na[i] == 1){
      rename$SYS_NAME[i] <- "Xinjiang*"
    }
    if (rename$name_dup[i] == TRUE){
      dup_counter <- dup_counter + 1
      rename$SYS_NAME[i] <- paste0(rename$SYS_NAME[i],"_",dup_counter)
    } else {
      dup_counter <- 1
    }
    print(rename$SYS_NAME[i])
  }
  
  rename_drop_tie <- rename %>%
    filter(clust_dup == FALSE)
  
  print("joining final data to shapefile")
  clusters_complete <- cluster_spatial %>%
    left_join(rename_drop_tie) %>%
    drop_na()
  
  print(
    tm_shape(thiessen_sf)+
      tm_borders() +
      tm_shape(clusters_complete) +
      tm_polygons(col = "SYS_NAME") +
      tm_shape(physiographic_macroregions) +
      tm_borders(lwd = 1.5, col = "black") +
      tm_layout(main.title = paste0("Disaggregated Cluster Map Using ", round(sig_level,2), " filter\n", "Queen's Case Spatial Adjacency\n",algorithm," Cluster Algorithm\n", modularity, " Modularity"),
                main.title.size = 1,
                legend.bg.color = "white",
                legend.frame = "black")
  )

# 8. Extra analysis: What if we just use Standard Pearson? --------------------------------

  # all_tests_lowp <- all_tests %>% filter(p_p < 0.05)
  # all_tests_goodr2 <- all_tests_lowp %>%  filter(adj_r2 >= 0.5)
  # tests_wider <- pivot_wider(all_tests_goodr2 %>% 
  #                              select(x,y, r_p), names_from = y, values_from = r_p)
  
  # rownames <- tests_wider$x
  # tests_wider <- tests_wider %>%
  #   select(-1)
  
  # tests_mat <- as.matrix(tests_wider)
  # rownames(tests_mat) <- rownames
  
  tests_longer <- melt(tests_mat)
  tests_longer$Var1 <- as.character(tests_longer$Var1)
  tests_longer$Var2 <- as.character(tests_longer$Var2)
  tests_longer <- tests_longer %>% arrange(Var1, Var2)
  colnames(tests_longer) <- c("Source","Target","coef")
  
  # Do ggplot
  hist(tests_longer$coef, main = "Histogram of all Cross-correlation Coefficients\nn = 43264", xlab = "Coefficient")
  
  # filter out only positive correlations and those over the chosen threshold (networks don't like negative ones)
  combine3 <- all_tests %>% filter(p_p < 0.05)
  combine4 <- combine3 %>% filter(adj_r2 >= 0.8)
  
  only_sig <- combine4 %>% 
    filter(r_p > sig_level)
  
  no_duplicates <- only_sig %>%
    mutate(normalized = map2_chr(x, y, ~paste(sort(c(.x, .y)), collapse = ""))) %>%
    group_by(normalized) %>%
    summarise(x = first(x),
              y = first(y)) %>%
    select(-normalized) %>% 
    left_join(all_tests)
  
  
  hist(no_duplicates$r_p, main = paste0("Histogram of Spatially-Constrained and Positive\nCross-correlation Coefficients\nn = ", nrow(no_duplicates)), xlab = "Coefficent")
  
  
  
  #Drawing paths between nodes and creating undirected graph object
  
  initial_graph <- graph_from_data_frame(no_duplicates, directed = FALSE)
  weighted_graph <- set_edge_attr(initial_graph, "weight", value = no_duplicates$r_p)
  
  #Determining community and cluster membership
  community <- cluster_louvain(weighted_graph, weights = E(weighted_graph)$weight)
  algorithm <- algorithm(community)
  modularity <- round(modularity(community),2)
  
  cluster_id <- tibble(W1_ID = community$names, cluster = as.factor(community$membership))
  
  # Trim clusters that are too small
  only_big <- cluster_id %>% 
    group_by(cluster) %>% 
    filter(n() >= min_cluster_size)
  
  #Join graph data to shapefile
  cluster_spatial <- thiessen_sf %>%
    inner_join(only_big)
  
  # intersecting pmr names with clusters
  pmr_transform <- st_transform(physiographic_macroregions, crs = st_crs(cluster_spatial))
  union <- st_intersection(cluster_spatial, pmr_transform)
  cluster_union <- cluster_spatial %>%
    left_join(st_drop_geometry(union), by = c("W1_ID","cluster"))
  
  rename <- st_drop_geometry(cluster_union) %>%
    group_by(cluster) %>%
    count(SYS_NAME) %>%
    top_n(1) %>%
    arrange(SYS_NAME)
  
  print("Naming split clusters")
  rename$name_dup <- duplicated(rename$SYS_NAME)
  rename$clust_dup <- duplicated(rename$cluster)
  rename$na <- rowSums(is.na(rename))
  
  dup_counter <- 1
  for (i in 1:nrow(rename)){
    if (rename$na[i] == 1){
      rename$SYS_NAME[i] <- "Xinjiang*"
    }
    if (rename$name_dup[i] == TRUE){
      dup_counter <- dup_counter + 1
      rename$SYS_NAME[i] <- paste0(rename$SYS_NAME[i],"_",dup_counter)
    } else {
      dup_counter <- 1
    }
    print(rename$SYS_NAME[i])
  }
  
  rename_drop_tie <- rename %>%
    filter(clust_dup == FALSE)
  
  print("joining final data to shapefile")
  clusters_complete <- cluster_spatial %>%
    left_join(rename_drop_tie) %>%
    drop_na()

  print(
    tm_shape(thiessen_sf)+
      tm_borders() +
      tm_shape(clusters_complete) +
      tm_polygons(col = "SYS_NAME") +
      tm_shape(physiographic_macroregions) +
      tm_borders(lwd = 1.5, col = "black") +
      tm_layout(main.title = paste0("Disaggregated Cluster Map Using ", round(sig_level,2), " filter\n", "Queen's Case Spatial Adjacency\n",algorithm," Cluster Algorithm\n", modularity, " Modularity"),
                main.title.size = 1,
                legend.bg.color = "white",
                legend.frame = "black")
  )


# 9. Extra analysis: What if we just use Pearson with WM? --------------------------------
# Transform correlation results into a matrix (to be multiplied to weight matrix later)

tests_longer <- melt(tests_mat)
tests_longer$Var1 <- as.character(tests_longer$Var1)
tests_longer$Var2 <- as.character(tests_longer$Var2)
tests_longer <- tests_longer %>% arrange(Var1, Var2)
colnames(tests_longer) <- c("Source","Target","coef")

# arrange weight matrix into 3 columns
wm_longer <- melt(wm)
wm_longer$Var1 <- as.character(wm_longer$Var1)
wm_longer$Var2 <- as.character(wm_longer$Var2)
wm_longer <- wm_longer %>% arrange(Var1, Var2)
colnames(wm_longer) <- c("x","y","adj")

combine <- all_tests %>% left_join(wm_longer)
combine2 <- combine %>% filter(adj > 0)
combine3 <- combine2 %>% filter(p_p < 0.05)
combine4 <- combine3 %>% filter(adj_r2 >= 0.2)

# Do ggplot
hist(combine4$r_p, main = "Histogram of all Cross-correlation Coefficients\nn = 43264",xlab = "Coefficient")

sig_level <- 0
only_sig <- combine4 %>% filter(r_p >= 0)

no_duplicates <- only_sig %>%
  mutate(normalized = map2_chr(x, y, ~paste(sort(c(.x, .y)), collapse = ""))) %>%
  group_by(normalized) %>%
  summarise(x = first(x),
            y = first(y)) %>%
  select(-normalized) %>% 
  left_join(combine4)


hist(no_duplicates$r_p, main = paste0("Histogram of Spatially-Constrained and Positive\nCross-correlation Coefficients\nn = ", nrow(no_duplicates)), xlab = "Coefficent")


#Drawing paths between nodes and creating undirected graph object

initial_graph <- graph_from_data_frame(no_duplicates, directed = FALSE)
weighted_graph <- set_edge_attr(initial_graph, "weight", value = no_duplicates$r_p)

#Determining community and cluster membership
community <- cluster_louvain(weighted_graph, weights = E(weighted_graph)$weight)
#community <- cluster_spinglass(weighted_graph, weights = E(weighted_graph)$weight)
#community <- cluster_edge_betweenness(weighted_graph)
#community <- cluster_leading_eigen(weighted_graph)
#community <- cluster_optimal(weighted_graph)
algorithm <- algorithm(community)
modularity <- round(modularity(community),2)

cluster_id <- tibble(W1_ID = community$names, cluster = as.factor(community$membership))

# Trim clusters that are too small
only_big <- cluster_id %>% 
  group_by(cluster) %>% 
  filter(n() >= min_cluster_size)

#Join graph data to shapefile
cluster_spatial <- thiessen_sf %>%
  inner_join(only_big)

# intersecting pmr names with clusters
pmr_transform <- st_transform(physiographic_macroregions, crs = st_crs(cluster_spatial))
union <- st_intersection(cluster_spatial, pmr_transform)
cluster_union <- cluster_spatial %>%
  left_join(st_drop_geometry(union), by = c("W1_ID","cluster"))

rename <- st_drop_geometry(cluster_union) %>%
  group_by(cluster) %>%
  count(SYS_NAME) %>%
  top_n(1) %>%
  arrange(SYS_NAME)

print("Naming split clusters")
rename$name_dup <- duplicated(rename$SYS_NAME)
rename$clust_dup <- duplicated(rename$cluster)
rename$na <- rowSums(is.na(rename))

dup_counter <- 1
for (i in 1:nrow(rename)){
  if (rename$na[i] == 1){
    rename$SYS_NAME[i] <- "Xinjiang*"
  }
  if (rename$name_dup[i] == TRUE){
    dup_counter <- dup_counter + 1
    rename$SYS_NAME[i] <- paste0(rename$SYS_NAME[i],"_",dup_counter)
  } else {
    dup_counter <- 1
  }
  print(rename$SYS_NAME[i])
}

rename_drop_tie <- rename %>%
  filter(clust_dup == FALSE)

print("joining final data to shapefile")
clusters_complete <- cluster_spatial %>%
  left_join(rename_drop_tie) %>%
  drop_na()

# just_clusters <- st_drop_geometry(clusters_complete) %>% 
#   select(1,2)
# 
# just_clusters$cluster <- as.numeric(just_clusters$cluster)
# 
# colnames(just_clusters) <- c("W1_ID", cluster_name)
# 
# 
# 
# cluster_comparison <- cluster_comparison %>% 
#   left_join(just_clusters)

print(
  tm_shape(thiessen_sf)+
    tm_borders() +
    tm_shape(clusters_complete) +
    tm_polygons(col = "SYS_NAME") +
    tm_shape(physiographic_macroregions) +
    tm_borders(lwd = 1.5, col = "black") +
    tm_layout(main.title = paste0("Disaggregated Cluster Map Using ", round(sig_level,2), " filter\n", "Queen's Case Spatial Adjacency\n",algorithm," Cluster Algorithm\n", modularity, " Modularity"),
              main.title.size = 1,
              legend.bg.color = "white",
              legend.frame = "black")
)

all_runs <- tibble()

for (i in 1:100) {
  print(i)
  community <- cluster_spinglass(
    weighted_graph,
    weights = E(weighted_graph)$weight,
    vertex = NULL,
    spins = 25,
    parupdate = FALSE,
    start.temp = 1,
    stop.temp = 0.01,
    cool.fact = 0.99,
    update.rule = "simple",
    gamma = 1,
    implementation = "orig",
    gamma.minus = 1
  )
  
  clusters <- tibble(W1_ID = cluster_id$W1_ID,
                       cluster = membership(community))
  
  only_big <- clusters %>% 
    group_by(cluster) %>% 
    filter(n() >= min_cluster_size)
  
  max <- tibble(i = i, max = length(unique(only_big$cluster)))
  
  all_runs <- all_runs %>% bind_rows(max)
}

tabulation <- data.frame(table(all_runs$max))

