# Reproduction Scripts for Bowman, Henderson, Ryavec, and Wu
#   Markets and Macroregions: Evidence from Qing Grain Prices, 1736-1842
#   2022

# Script 4/4: Exporting network to Gephi and GIS

# Author: Rocco Bowman
# Contact: bowman.rocco@gmail.com

# Begin script

# 1. Read in data ---------------------------------------------------------

  library(tidyverse)
  library(sf)
  library(igraph)
  
  # set working directory
  setwd("~/Grain Price Project")
  
  # read in data
  edges <- read_csv("output/05_network_edges.csv")
  nodes <- st_read("output/louvain_clusters_pre_1842.shp") %>% 
    st_drop_geometry() %>% 
    select(W1_ID, cluster) %>% 
    arrange(W1_ID)


# 2. Recreate network graph -----------------------------------------------

  initial_graph <- graph_from_data_frame(edges,
                                         directed = FALSE)
  
  weighted_graph <- set_edge_attr(initial_graph,
                                  "weight",
                                  value = edges$cc_same)
  

# 3. Develop nodes --------------------------------------------------------
  
  degree <- tibble(W1_ID = names(degree(weighted_graph)),
                    degree = degree(weighted_graph))
  
  # calculate degree of notes
  degree <- nodes %>% 
    left_join(degree)
  
  # calculate a normalized measure of degree within clusters
  
  all_local_degree <- tibble()
  
  for (i in unique(degree$cluster)) {
    cluster_filter <- degree %>% 
      filter(cluster == i) %>% 
      mutate(local_deg = signif(degree / max(degree), digits = 2))
    all_local_degree <- bind_rows(all_local_degree, cluster_filter)
  }
  
  points <- st_read("data/final_points.shp")

  points_info <- points %>% 
    left_join(all_local_degree)

  degree_points <- st_drop_geometry(points_info) %>%
    arrange(W1_ID) %>%
    mutate(id = rownames(.), .before = W1_ID) %>% 
    rename(label = W1_ID) %>% 
    replace_na(list(cluster = "Unclustered",
                    degree = 0,
                    local_deg = 0))
  
  write.csv(degree_points, "output/nodes.csv", row.names = FALSE)


# 4. Develop edges --------------------------------------------------------

  edges_trim <- edges %>%
    select(1,2,10) %>% 
    rename(Source = x, Target = y, coef = cc_same) %>% 
    left_join(nodes, by = c("Source" = "W1_ID"))
  
  join1 <- left_join(edges_trim,degree_points, by = c("Source" = "label"))
  join1$Source <- join1$id
  
  join2 <- left_join(join1, degree_points, by = c("Target" = "label"))
  join2$Target <- join2$id.y
  join3 <- join2 %>% 
    select(1:4) %>%
    mutate(weight = (join2$coef-min(join2$coef))/(max(join2$coef)-min(join2$coef))) %>% 
    rename(cluster = cluster.x)
  
  join3$coef <- round(join3$coef,2)
  join3$weight <- round(join3$weight,2)

  all_local_coef <- tibble()
  
  for (i in unique(join3$cluster)) {
    cluster_filter <- join3 %>% 
      filter(cluster == i) %>% 
      mutate(local_coef = round(coef / max(coef), 2),
             local_weight = round(weight / max(weight), 2),
             label = coef)
    all_local_coef <- bind_rows(all_local_coef, cluster_filter)
  }
  
  write.csv(all_local_coef, "output/edges_before.csv", row.names = FALSE)

# 5. Import ---------------------------------------------------------------

# How to import into Gephi

  # create new project
  # open data laboratory
  # import spreadsheet of nodes
  # accept and click finish
  # change graph type to undirected
  # select append to current workspace

  # then click edges
  # import spreadsheet of edges
  # accept and click finish
  # change graph type to undirected
  # select append
  # geolayout via plugin

#uploading to QGIS

  # import nodes and edges as csv
  # create a virtual layer and input the following SQL query
  
  # SELECT Source, Target, coef, local_coef, weight, local_weight,
  #     make_line(a.geometry, b.geometry)
  # FROM edges
  # JOIN nodes a ON edges.Source = a.ID
  # JOIN nodes b ON edges.Target = b.ID
  # WHERE a.id != b.id

all_cases <- tibble()
  
north_china_edges <- all_local_coef %>% filter(cluster == 8)
north_china_points <- degree_points %>% filter(cluster == 8)
mean(north_china$coef)  
mean(north_china$local_coef)
mean(north_china_points$degree)
mean(north_china_points$local_deg)
north_china_summary <- tibble(cluster = 8,
                              sys_name = "North China",
                              avg_coef = mean(north_china_edges$coef),
                              avg_degree = mean(north_china_points$degree))

all_cases <- bind_rows(all_cases, north_china_summary)

henan_shandong_edges <- all_local_coef %>% filter(cluster == 6)
henan_shandong_points <- degree_points %>% filter(cluster == 6)
mean(henan_shandong_edges$coef)  
mean(henan_shandong_edges$local_coef)
mean(henan_shandong_points$degree)
mean(henan_shandong_points$local_deg)
henan_shandong_summary <- tibble(cluster = 6,
                              sys_name = "Henan-Shandong",
                              avg_coef = mean(henan_shandong_edges$coef),
                              avg_degree = mean(henan_shandong_points$degree))

all_cases <- bind_rows(all_cases, henan_shandong_summary)

shanxi_edges <- all_local_coef %>% filter(cluster == 7)
shanxi_points <- degree_points %>% filter(cluster == 7)
mean(shanxi_edges$coef)  
mean(shanxi_edges$local_coef)
mean(shanxi_points$degree)
mean(shanxi_points$local_deg)
shanxi_summary <- tibble(cluster = 7,
                                 sys_name = "Shanxi",
                                 avg_coef = mean(shanxi_edges$coef),
                                 avg_degree = mean(shanxi_points$degree))

all_cases <- bind_rows(all_cases, shanxi_summary)

gansu_edges <- all_local_coef %>% filter(cluster == 10)
gansu_points <- degree_points %>% filter(cluster == 10)
mean(gansu_edges$coef)  
mean(gansu_edges$local_coef)
mean(gansu_points$degree)
mean(gansu_points$local_deg)
gansu_summary <- tibble(cluster = 10,
                         sys_name = "Gansu",
                         avg_coef = mean(gansu_edges$coef),
                         avg_degree = mean(gansu_points$degree))

all_cases <- bind_rows(all_cases, gansu_summary)

upper_yangzi_edges <- all_local_coef %>% filter(cluster == 9)
upper_yangzi_points <- degree_points %>% filter(cluster == 9)
mean(upper_yangzi_edges$coef)  
mean(upper_yangzi_edges$local_coef)
mean(upper_yangzi_points$degree)
mean(upper_yangzi_points$local_deg)
upper_yangzi_summary <- tibble(cluster = 9,
                        sys_name = "Upper Yangzi",
                        avg_coef = mean(upper_yangzi_edges$coef),
                        avg_degree = mean(upper_yangzi_points$degree))

all_cases <- bind_rows(all_cases, upper_yangzi_summary)

yungui_edges <- all_local_coef %>% filter(cluster == 11)
yungui_points <- degree_points %>% filter(cluster == 11)
mean(yungui_edges$coef)  
mean(yungui_edges$local_coef)
mean(yungui_points$degree)
mean(yungui_points$local_deg)
yungui_summary <- tibble(cluster = 11,
                               sys_name = "Yungui",
                               avg_coef = mean(yungui_edges$coef),
                               avg_degree = mean(yungui_points$degree))

all_cases <- bind_rows(all_cases, yungui_summary)

lingnan_edges <- all_local_coef %>% filter(cluster == 2)
lingnan_points <- degree_points %>% filter(cluster == 2)
mean(lingnan_edges$coef)  
mean(lingnan_edges$local_coef)
mean(lingnan_points$degree)
mean(lingnan_points$local_deg)
lingnan_summary <- tibble(cluster = 2,
                         sys_name = "Lingnan",
                         avg_coef = mean(lingnan_edges$coef),
                         avg_degree = mean(lingnan_points$degree))

all_cases <- bind_rows(all_cases, lingnan_summary)

se_coast_edges <- all_local_coef %>% filter(cluster == 5)
se_coast_points <- degree_points %>% filter(cluster == 5)
mean(se_coast_edges$coef)  
mean(se_coast_edges$local_coef)
mean(se_coast_points$degree)
mean(se_coast_points$local_deg)
se_coast_summary <- tibble(cluster = 5,
                          sys_name = "Southeast Coast",
                          avg_coef = mean(se_coast_edges$coef),
                          avg_degree = mean(se_coast_points$degree))

all_cases <- bind_rows(all_cases, se_coast_summary)

middle_yangzi_1_edges <- all_local_coef %>% filter(cluster == 1)
middle_yangzi_1_points <- degree_points %>% filter(cluster == 1)
mean(middle_yangzi_1_edges$coef)  
mean(middle_yangzi_1_edges$local_coef)
mean(middle_yangzi_1_points$degree)
mean(middle_yangzi_1_points$local_deg)
middle_yangzi_1_summary <- tibble(cluster = 1,
                           sys_name = "Middle Yangzi 1",
                           avg_coef = mean(middle_yangzi_1_edges$coef),
                           avg_degree = mean(middle_yangzi_1_points$degree))

all_cases <- bind_rows(all_cases, middle_yangzi_1_summary)

middle_yangzi_2_edges <- all_local_coef %>% filter(cluster == 4)
middle_yangzi_2_points <- degree_points %>% filter(cluster == 4)
mean(middle_yangzi_2_edges$coef)  
mean(middle_yangzi_2_edges$local_coef)
mean(middle_yangzi_2_points$degree)
mean(middle_yangzi_2_points$local_deg)
middle_yangzi_2_summary <- tibble(cluster = 4,
                                  sys_name = "Middle Yangzi 2",
                                  avg_coef = mean(middle_yangzi_2_edges$coef),
                                  avg_degree = mean(middle_yangzi_2_points$degree))

all_cases <- bind_rows(all_cases, middle_yangzi_2_summary)

lower_yangzi_edges <- all_local_coef %>% filter(cluster == 3)
lower_yangzi_points <- degree_points %>% filter(cluster == 3)
mean(lower_yangzi_edges$coef)  
mean(lower_yangzi_edges$local_coef)
mean(lower_yangzi_points$degree)
mean(lower_yangzi_points$local_deg)
lower_yangzi_summary <- tibble(cluster = 3,
                                  sys_name = "Lower Yangzi",
                                  avg_coef = mean(lower_yangzi_edges$coef),
                                  avg_degree = mean(lower_yangzi_points$degree))

all_cases <- bind_rows(all_cases, lower_yangzi_summary)

