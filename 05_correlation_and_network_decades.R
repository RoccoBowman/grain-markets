# Reproduction Scripts for ...

# Script 1/1: Calculating correlation coefficients between all prefectures
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
setwd("~/qing_prices_decades")

# load in initial spatial and tabular data
thiessen_sp <- readOGR("data/final_thiessens_validated.shp")
thiessen_sf <- st_read("data/final_thiessens_validated.shp")

physiographic_macroregions <- st_read("data/v5_PMR_pgn_utf_xian.shp")

all_timeseries <- read.csv("data/03_full_time_series.csv")

# pad decade tags with zeroes for easier arrangement (smallest to largest)
all_timeseries$decade <- str_pad(all_timeseries$decade,
                                 2,
                                 side = "left",
                                 pad = "0")

all_tests <- tibble() # Creates an empty set to collect info on all pairwise correlations
all_graphs <- tibble(W1_ID = thiessen_sf$W1_ID)
all_graph_stats <- tibble()

for (i in unique(all_timeseries$decade)){ # Start of level 1
  #print(i)
  print(paste0("Filtering data for decade...",i))
  decade_filter <- all_timeseries %>% 
    filter(decade == i)
  #print(length(decade_filter))
  
  # select out only the times series for prefectures and cuts out averages and duplicates
  want <- thiessen_sf$W1_ID 
  
  decade_filter <- decade_filter %>% 
    select(-c(1:4)) %>% 
    select(want)
  
  # iterate backwards through times series data and prune if needed
  all_na_sum <- tibble()
  for (j in seq(length(decade_filter),1,-1)) { 
    na_sum <- sum(is.na(decade_filter[j]))
    add <- tibble(na_sum = na_sum)
    all_na_sum <- bind_rows(all_na_sum, add)
    if (na_sum >= 60) {
      decade_filter <- decade_filter %>% select(-j)
    }
  }

  
  print("Reporting spatial missing data...")
  
  # transpose correlation data into spatial format 
  transpose <- data.frame(t(decade_filter)) %>%
    select(1) %>% 
    mutate(X1 = rownames(.)) %>% 
    rename(W1_ID = X1)
  
  # right join transposed data to shapefile
  shp_trim <- thiessen_sf %>% 
    right_join(transpose)

  
  # 2. Calculating correlation matrix ---------------------------------------
  
  # Unnecessary if you already have the correlation data to load in!
  
    #Calculate pearson and cross correlations for each pairwise match of time series
  all_tests <- tibble()
  
  
  for (k in 1:length(decade_filter)) {
    print(paste0(k,"/",length(decade_filter)))
    col1 <- decade_filter[[k]]
    col1_name <- colnames(decade_filter[k])
    auto_exclude <- decade_filter %>% select(-k)
    for (l in 1:length(auto_exclude)){
      #print(paste0(j,"/",length(auto_exclude)))
      col2 <- auto_exclude[[l]]
      col2_name <- colnames(auto_exclude[l])

      no_na <- tibble(x = col1, y = col2) %>%
        drop_na()

      if (nrow(no_na) < 10) {
        next
      }

      #ptest <- cor.test(no_na$x, no_na$y, method = "pearson", na.action = "na.omit")
      #model <- summary(lm(no_na$y ~ no_na$x))

      ts1 <- ts(no_na$x)
      ts2 <- ts(no_na$y)

      diff1 <- diff(ts1)
      diff2 <- diff(ts2)

      ccf <- ccf(diff1, diff2, ylab = "Cross-correlation", plot = FALSE)
      ccf2 <- tibble(lag = ccf$lag, acf = ccf$acf) %>%
        filter(lag > -2 & lag < 2)
      cc_p_val <- 2 * (1 - pnorm(abs(ccf2$acf[2]), mean = 0, sd = 1/sqrt(ccf$n.used)))

      test_info <- tibble(decade = i,
                          x = col1_name,
                          y = col2_name,
                          #t_stat_p = ptest[[1]],
                          #df_p = ptest[[2]],
                          #p_p = ptest[[3]],
                          #r_p = ptest[[4]],
                          #adj_r2 = model$adj.r.squared,
                          #alt_p = ptest[[6]],
                          cc_before = ccf2$acf[1],
                          cc_same = ccf2$acf[2],
                          cc_after = ccf2$acf[3],
                          cc_p = cc_p_val)


      all_tests <- bind_rows(all_tests, test_info)
    }
  } # end of correlation test loop

  write.csv(all_tests, paste0("output/decade_",i,"_tests.csv"), row.names = FALSE)
  
  # Begin here to simply load in the resulting correlations
  
  print("Loading test results...")
  
 # all_tests <- read_csv(paste0("output/decade_",i,"_tests.csv"))
  
  decade_tests <- all_tests %>% 
    filter(decade == i)
  
  # transform correlation results into a matrix (to be multiplied to weight matrix later)
  tests_wider <- pivot_wider(decade_tests %>% select(x,y,cc_same), names_from = y, values_from = cc_same)
  rownames <- tests_wider$x
  tests_wider <- tests_wider %>%
    select(-1)
  tests_mat <- as.matrix(tests_wider)
  rownames(tests_mat) <- rownames
  
  # 3. Analysis parameters --------------------------------------------------
  
  # set the cutoff for cluster size (anything smaller than X)
  min_cluster_size <- 5 
  
  # set the cut-off level for correlation coefficients (higher means stricter filter)
  sig_level <- 0.20 
  
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
  colnames(wm) <- colnames(decade_filter)
  rownames(wm) <- colnames(decade_filter)
  
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
  
  # Taiwan
  wm_longer$adj[wm_longer$x == "FJ036" & wm_longer$y == "FJ025"] <- 1
  
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
    left_join(combine_positive)
  
  hist(no_duplicates$cc_same, xlab = "Coefficent", main = "")
  
  write.csv(no_duplicates, paste0("output/network_edges",i,".csv"), row.names = FALSE)
  
  # 5. Creating network graph and detecting communities ---------------------
  
  print("Generating network graph and detecting communities...")
  
  # drawing paths between nodes and creating undirected graph object
  initial_graph <- graph_from_data_frame(no_duplicates,
                                         directed = FALSE)
  weighted_graph <- set_edge_attr(initial_graph,
                                  "weight",
                                  value = no_duplicates$cc_same)
  
  # applying Louvain community detection algorithm to find clusters in the graph
  community <- cluster_louvain(weighted_graph,
                               weights = E(weighted_graph)$weight)
  community_df <- tibble(W1_ID = community$names,
                         membership = community$membership) 
  colnames(community_df) <- c("W1_ID",paste0("membership",i))
  
  all_graphs <- left_join(all_graphs, community_df)
  
  algorithm <- algorithm(community)
  modularity <- round(modularity(community),2)
  transitivity <- transitivity(weighted_graph,
                               type = "global",
                               weights = E(weighted_graph)$weight)
  assortativity <- assortativity.degree(weighted_graph)
  graph_stats <- tibble(i = i,
                        algorithm = algorithm,
                        modularity = modularity,
                        transitivity = transitivity,
                        assortativity = assortativity)
  all_graph_stats <- bind_rows(all_graph_stats, graph_stats)
  

  
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
  for (m in 1:nrow(rename)){
    if (rename$na[m] == 1){
      rename$SYS_NAME[m] <- "Xinjiang*"
    }
    if (rename$name_dup[m] == TRUE){
      dup_counter <- dup_counter + 1
      rename$SYS_NAME[m] <- paste0(rename$SYS_NAME[m],"_",dup_counter)
    } else {
      dup_counter <- 1
    }
    print(rename$SYS_NAME[m])
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
  print(tm_shape(thiessen_sf)+
    tm_borders() +
    tm_shape(clusters_complete) +
    tm_polygons(col = "SYS_NAME", border.col = "white") +
    tm_shape(physiographic_macroregions) +
    tm_borders(lwd = 2, col = "black") +
    tm_layout(main.title = paste0("Clusters for decade ",i),
              legend.bg.color = "white",
              legend.frame = "black"))

  # save shapefile with louvin algorithm applied
  st_write(clusters_complete,
           dsn = paste0(getwd(), "/output"),
           layer = paste0("louvain_clusters_decade_",i,".shp"),
           driver = "ESRI Shapefile",
           delete_layer = TRUE)
  
} # End of level 1


# Saving Test Results -----------------------------------------------------

write.csv(all_tests, "testout/all_tests_decades.csv", row.names = FALSE)
library(staplr)
staple_pdf(
  input_directory = paste0(getwd(),"/images"),
  output_filepath = paste0(getwd(),"/staples.pdf"),
  overwrite = FALSE
)

# 7. Extra analysis: No spatial weights -------------------------------------------------------

combine <- all_tests %>%
  left_join(wm_longer)
combine_significant <- combine %>% 
  filter(cc_p < 0.05)
combine_positive <- combine_significant %>%  
  filter(cc_same > 0.6)

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


series1 <- decade_filter$GX076
series2 <- decade_filter$AH013

ts1 <- ts(series1)
ts2 <- ts(series2)

ts.plot(ts1)
ts.plot(ts2)

plot(ts2, ylim = c(0,450))

lines(ts1, col = "red")

all_graphs <- tibble(W1_ID = thiessen_sf$W1_ID)

for (i in unique(all_timeseries$decade)) {
  clusters <- st_drop_geometry(st_read(paste0("output/louvain_clusters_decade_",i,".shp"))) %>% 
    select(W1_ID,cluster) 
  colnames(clusters) <- c("W1_ID", paste0("clus",i))
  
  all_graphs <- all_graphs %>% 
    left_join(clusters)
}

