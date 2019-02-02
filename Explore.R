library(RODBC)
library(data.table)
library(MetroTransitr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(sp)
library(RColorBrewer)
library(leaflet)
library(rgeos)
#install.packages('poweRlaw')
library(poweRlaw)
#install.packages('tnet')
#library(tnet)
#install.packages('qgraph')
library(qgraph)

#Pull data ----------------------------------------------------------------------------------------
#Get OD Data
block_OD <- readRDS('data/blocks_flowmap.RDS')
setDT(block_OD)
block_OD[, weight := N]

taz_OD <- readRDS('data/taz_fm.RDS')
setDT(taz_OD)
taz_OD[, weight := .N, by = .(dest_taz, orig_taz)]
taz_OD <- taz_OD[!duplicated(taz_OD[, .(dest_taz, orig_taz)]), ]
taz_OD <- taz_OD[!is.na(orig_taz) & !is.na(dest_taz)]

tracts_OD <- readRDS('data/tracts_flowmap.RDS')
setDT(tracts_OD)
tracts_OD[, weight := N]

#tbi_OD <- readRDS('data/tbi_OD.RDS')
#setDT(tbi_OD)

#Get tract, block, taz polygons (class is Spatial Polygons Data Frame)
blocks <- readRDS('data/blocks.RDS')
taz <- readRDS('data/taz.RDS')
tracts <- readRDS('data/tracts.RDS')

#Eventually get transit taz polygons
#ttaz <- readRDS('data/ttaz')
#ttaz2 <- readRDS('data/ttaz2')
#ttaz3 <- readRDS('data/ttaz3')

#Create graphs ------------------------------------------------------------------------------------
tract_graph <- graph_from_data_frame(tracts_OD[, .(orig_tracts, dest_tracts, weight)], directed = TRUE)
tract_graph_ud <- graph_from_data_frame(tracts_OD[, .(orig_tracts, dest_tracts, weight)], directed = FALSE)

taz_graph <- graph_from_data_frame(taz_OD[, .(orig_taz, dest_taz, weight)], directed = TRUE)
taz_graph_ud <- graph_from_data_frame(taz_OD[, .(orig_taz, dest_taz, weight)], directed = FALSE)

block_graph <- graph_from_data_frame(block_OD[, .(orig_blocks, dest_blocks, weight)], directed = TRUE)
block_graph_ud <- graph_from_data_frame(block_OD[, .(orig_blocks, dest_blocks, weight)], directed = FALSE)

#Compute Betweenness Centrality (ALL DATA) ---------------------------------------------------------------------------

#Compute betweenness centrality and plot
#C(k) = Sum_{(i,j)} (#shortest paths btw i,j passing through k)/(#shortest paths btw i,j)
bet_tract <- betweenness(tract_graph)
bet_tract <- data.table(tract = names(bet_tract), BC = bet_tract)
tracts <- merge(tracts, bet_tract, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

bet_taz <- betweenness(taz_graph)
bet_taz <- data.table(taz = names(bet_taz), BC = bet_taz)
taz <- merge(taz, bet_taz, by.x = "TAZ", by.y = 'taz', all.y = TRUE)

bet_block <- betweenness(block_graph)
bet_block <- data.table(block = names(bet_block), BC = bet_block)
blocks <- merge(blocks, bet_block, by.x = 'GEOID10', by.y = 'block', all.y = TRUE)

pal_tract <- colorNumeric(palette = "Reds", domain = tracts$BC)
tract_BC_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                            attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                            under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                            Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                            under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tracts, fillColor = ~pal_tract(BC), fillOpacity = .7, weight = 1)

tract_BC_leaf

pal_taz <- colorNumeric(palette = "Reds", domain = taz$BC)
taz_BC_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                        attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                        under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                        Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                        under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = taz, fillColor = ~pal_taz(BC), fillOpacity = .7, weight = 1)

taz_BC_leaf

pal_block <- colorNumeric(palette = "Reds", domain = blocks$BC)
block_BC_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = blocks, fillColor = ~pal_block(BC), fillOpacity = .7, weight = 1)

block_BC_leaf

#Compute Page Rank (ALL DATA) and visualize------------------------------------------------------------------------------
#directed
pr_tract <- page_rank(tract_graph, vids = V(tract_graph), weights = edge_attr(tract_graph, name = 'weight', index = E(tract_graph)))
pr_tract <- data.table(tract = names(pr_tract$vector), PR = pr_tract$vector)
tracts <- merge(tracts, pr_tract, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

pr_taz <- page_rank(taz_graph, vids = V(taz_graph), weights = edge_attr(taz_graph, name = 'weight', index = E(taz_graph)))
pr_taz <- data.table(taz = names(pr_taz$vector), PR = pr_taz$vector)
taz <- merge(taz, pr_taz, by.x = "TAZ", by.y = 'taz', all.y = TRUE)

pr_block <- page_rank(block_graph, vids = V(block_graph), weights = edge_attr(block_graph, name = 'weight', index = E(block_graph)))
pr_block <- data.table(block = names(pr_block$vector), PR = pr_block$vector)
blocks <- merge(blocks, pr_block, by.x = "GEOID10", by.y = 'block', all.y = TRUE)

pal_tract_PR <- colorNumeric(palette = "Reds", domain = tracts$PR)
tract_PR_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tracts, fillColor = ~pal_tract_PR(PR), fillOpacity = .7, weight = 1)

tract_PR_leaf

pal_taz_PR <- colorNumeric(palette = "Reds", domain = taz$PR)
taz_PR_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                        attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                        under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                        Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                        under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = taz, fillColor = ~pal_taz_PR(PR), fillOpacity = .7, weight = 1)

taz_PR_leaf

pal_block_PR <- colorNumeric(palette = "Reds", domain = blocks$PR)
block_PR_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = blocks, fillColor = ~pal_block_PR(PR), fillOpacity = .7, weight = 1)

block_PR_leaf


#Betweenness Centrality with N > 1 --------------
#IGNORE THIS SECTION FOR NOW
tracts_OD_1 <- tracts_OD[N > 1]
tract_graph_1 <- as_tbl_graph(tracts_OD_1[, .(orig_tracts, dest_tracts, weight)], directed = TRUE)
tract_graph_1_ud <- as_tbl_graph(tracts_OD_1[, .(orig_tracts, dest_tracts, weight)], directed = FALSE)
bet_tract_1 <- betweenness(tract_graph_1)
bet_tract_1 <- data.table(tract = names(bet_tract_1), BC_1 = bet_tract_1)
tracts_1 <- merge(tracts, bet_tract_1, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

pal_tract_1 <- colorNumeric(palette = "Reds", domain = tracts_1$BC_1)
tracts_1_BC_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                        attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                        under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                        Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                        under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tracts_1, fillColor = ~pal_tract_1(BC_1), fillOpacity = .7, weight = 1)

tracts_1_BC_leaf

block_OD_1 <- block_OD[N > 1]
block_graph_1 <- as_tbl_graph(block_OD_1[, .(orig_blocks, dest_blocks, weight)], directed = TRUE)
bet_block_1 <- betweenness(block_graph_1)
bet_block_1 <- data.table(block = names(bet_block_1), BC_1 = bet_block_1)
blocks_1 <- merge(blocks, bet_block_1, by.x = 'GEOID10', by.y = 'block', all.y = TRUE)

pal5 <- colorNumeric(palette = "Reds", domain = blocks_1$BC_1)

blocks_1_BC_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                           under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                           Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                           under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = blocks_1, fillColor = ~pal5(BC_1), fillOpacity = .7, weight = 1)

blocks_1_BC_leaf


#Start clustering ---------------------------------------------------------------------------------
tract_clust_1 <- cluster_infomap(tract_graph_1_ud)
tract_memb_1 <- membership(tract_clust_1)
tract_memb_1 <- data.table(tract = names(tract_memb_1), clust2 = tract_memb_1)
tracts_1 <- merge(tracts_1, tract_memb_1, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

pal6 <- colorFactor(palette = "Paired", domain = tracts_1$clust)
tracts_1_comm_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                           under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                           Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                           under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts_1, weight = 1) %>% addPolygons(data = subset(tracts_1, clust == 40), fillColor = '#FF0000', fillOpacity = .9, weight = 1)

tracts_1_comm_leaf

tracts_OD_2 <- tracts_OD[N > 2]
tract_graph_2_ud <- graph_from_data_frame(tracts_OD_2[, .(orig_tracts, dest_tracts, weight)], directed = FALSE)
tract_clust_2 <- cluster_infomap(tract_graph_2_ud)
tract_memb_2 <- membership(tract_clust_2)
tract_memb_2 <- data.table(tract = names(tract_memb_2), clust2 = tract_memb_2)
tracts_2 <- merge(tracts, tract_memb_2, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

tracts_2_comm_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                             attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                             under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                             Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                             under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, weight = 1) %>% addPolygons(data = subset(tracts_2, clust2 == 4), fillColor = '#FF0000', fillOpacity = .9, weight = 1)

tracts_2_comm_leaf

tracts_OD_10 <- tracts_OD[N>10]
tract_graph_10_ud <- graph_from_data_frame(tracts_OD_10[, .(orig_tracts, dest_tracts, weight)], directed = FALSE)
tract_clust_10 <- cluster_infomap(tract_graph_10_ud)
tract_memb_10 <- membership(tract_clust_10)
tract_memb_10 <- data.table(tract = names(tract_memb_10), clust10 = tract_memb_10)
tracts_10 <- merge(tracts, tract_memb_10, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

tracts_10_comm_leaf <- leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                             attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                             under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                             Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                             under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, weight = 1) %>% addPolygons(data = subset(tracts_10, clust10 == 7), fillColor = '#FF0000', fillOpacity = .9, weight = 1)

tracts_10_comm_leaf

block_OD_1 <- block_OD[N > 1]
block_graph_1_ud <- graph_from_data_frame(block_OD_1[, .(orig_blocks, dest_blocks, weight)], directed = FALSE)
block_clust_1 <- cluster_infomap(block_graph_1_ud)

block_OD_2 <- block_OD[N > 2]
block_graph_2_ud <- graph_from_data_frame(block_OD_2[, .(orig_blocks, dest_blocks, weight)], directed = FALSE)
block_clust_2 <- cluster_infomap(block_graph_2_ud)

#Create new polygons based on clusters ----------
clust_1 <- subset(blocks, clust == 1)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = blocks, weight = 1) %>% addPolygons(data = clust_1, fillColor = "#008144", fillOpacity = .7, weight = 1)

clust_8 <- subset(blocks, clust == 8)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = blocks, weight = 1) %>% addPolygons(data = clust_8, fillColor = "#008144", fillOpacity = .7, weight = 1)

tract_clust <- cluster_infomap(tract_gph_ud)
tract_memb <- membership(tract_clust)
tract_memb <- data.table(tract = names(tract_memb), clust = tract_memb)
tracts <- merge(tracts, tract_memb, by.x = "GEOID10", by.y = 'tract', all.y = TRUE)
clust_size_tr <- tract_memb[, .N, by = clust]


leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, weight = 1) %>% addPolygons(data = subset(tracts, GEOID10 == 27053104900 | GEOID10 == 27053125600), fillColor = "#FFEF00", fillOpacity = 1, weight = 1)

#Walktrap clustering-----------------------
#Interesting results about St. Paul separating from surrounding areas and a few tracts strongly associated with St. Paul
tract_wt <- cluster_walktrap(tract_graph, steps = 3, weights = E(tract_graph)$weight)
tract_wt
tract_wt_memb <- membership(tract_wt)
tract_wt_memb <- data.table(tract = names(tract_wt_memb), clust = tract_wt_memb)
tract_wt <- merge(tracts, tract_wt_memb, by.x = 'GEOID10', by.y = 'tract', all.x = TRUE)

pal <- colorFactor(palette = "Dark2", domain = subset(tract_wt, clust <= 4)$clust)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tract_wt, weight = 1) %>% addPolygons(data = subset(tract_wt, clust <= 4), fillColor = ~pal(clust), fillOpacity = .7, weight = 1)

taz_wt <- cluster_walktrap(taz_graph, steps = 6)
taz_wt
taz_wt_memb <- membership(taz_wt)
taz_wt_memb <- data.table(taz = names(taz_wt_memb), clust = taz_wt_memb)
taz_wt <- merge(taz, taz_wt_memb, by.x = 'TAZ', by.y = 'taz', all.x = TRUE)

pal1 <- colorFactor(palette = "Paired", domain = taz_wt$clust)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = taz_wt, fillColor = ~pal1(clust), fillOpacity = .7, weight = 1)

#Fast and Greedy clustering
cluster_infomap(tract_graph)
cluster_infomap(block_graph)


#Coreness ------------------------------
core_tract <- coreness(tract_gph_ud)
tract_coreness <- data.table(tract = names(core_tract), core = core_tract)
tract_cr <- merge(tracts, tract_coreness, by.x = 'GEOID10', by.y = 'tract', all.x = TRUE)

pal2 <- colorNumeric(palette = "Reds", domain = tract_cr$core)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tract_cr, fillColor = ~pal2(core), fillOpacity = .7, weight = 1)

#Degree ----------------------------------------------------
tot_degrees <- degree(tract_graph, loops = TRUE, mode = 'total')
tot_degrees <- data.table(tract = names(tot_degrees), tot_degree = tot_degrees)
tract_deg <- merge(tracts, tot_degrees, by.x = 'GEOID10', by.y = 'tract')

pal <- colorNumeric(palette = "Reds", domain = tract_deg$tot_degree)

leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tract_deg, fillColor = ~pal(tot_degree), fillOpacity = .7, weight = 1)

out_degrees <- degree(tract_graph, loops = TRUE, mode = 'out')
out_degrees <- data.table(tract = names(out_degrees), out_degree = out_degrees)
tract_deg <- merge(tracts, out_degrees, by.x = 'GEOID10', by.y = 'tract')
pal <- colorNumeric(palette = "Reds", domain = tract_deg$out_degree)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tract_deg, fillColor = ~pal(out_degree), fillOpacity = .7, weight = 1)

in_degrees <- degree(tract_graph, loops = TRUE, mode = 'in')
in_degrees <- data.table(tract = names(in_degrees), in_degree = in_degrees)
tract_deg <- merge(tracts, in_degrees, by.x = 'GEOID10', by.y = 'tract')
pal <- colorNumeric(palette = "Reds", domain = tract_deg$in_degree)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tract_deg, fillColor = ~pal(in_degree), fillOpacity = .7, weight = 1)

degree_dist <- degree.distribution(tract_graph, cumulative = FALSE)
cum_degree <- cumsum(degree_dist)
degs <- seq(0, 1312, by = 1)
plot(degs, cum_degree)

#Try fitting power law
degs <- unname(tot_degrees)
degs_pl <- displ$new(degs)
est <- estimate_xmin(degs_pl)
degs_pl$setXmin(est)
plot(degs_pl)
lines(degs_pl, col=2)

#Compare to Actual connectivity of transit system (current)
ch_as <- odbcConnect('db_prod_allstop', uid = "sdreport", pwd = "tdr3port")
stops <- as.data.table(sqlQuery(ch_as, "select * from stop_sequence ss
                                inner join site s on ss.site_id = s.site_id", stringsAsFactors = F))
stops <- stops[, .(site_id, site_longitude, site_latitude, line_id, direction_number, 
                   stop_sequence_number, site_at, site_on, node_id)]
stops[grepl("84F", line_id), line_id := '84']
stops[, line_id := as.numeric(gsub(" ", "", line_id))]

stops_sp <- SpatialPointsDataFrame(data = stops, coords = stops[, .(site_longitude, site_latitude)], proj4string = CRS(proj4string(tracts)))

stop_tract <- over(stops_sp, tracts)

stops <- data.table(cbind(stops, stop_tract))

#Current stops with lat, lon, and census tract ID
stops <- stops[, .(site_id, site_longitude, site_latitude, line_id, direction_number, stop_sequence_number,
                   site_at, site_on, node_id, GEOID10)]

keycols <- c('line_id', 'direction_number', 'stop_sequence_number')
setkeyv(stops, keycols)
saveRDS(stops, 'data/stops.RDS')

#subset a single direction - can we assume it always stops/ends in the same tract? (Redo this taking into account stop sequence)
rte2_combos <- combn(stops[line_id == 2 & direction_number == 0, GEOID10], 2)
rte2_combos <- as.data.table(aperm(rte2_combos, c(2,1)))
rte2_combos[, line_id := 2]

OD <- data.table()

#for (i in unique(stops[, line_id])) {
#  for (j in unique(stops[line_id == i, direction_number])) {
#    print(paste(i, j))
#   temp <- as.data.table(aperm(combn(stops[line_id == i & direction_number == j, GEOID10], 2), c(2,1)))
#   temp[, `:=` (line_id = i, direction_number = j)]
#   OD <- rbind(OD, temp)
#  }
#}

#I hate how much I love for loops :(
for (i in unique(stops[, line_id])) {
  for (j in unique(stops[line_id == i, direction_number])) {
    for (k in 1:max(stops[line_id == i & direction_number == j, stop_sequence_number])-1) {
      print(paste(i, j))
      temp <- as.data.table(expand.grid(stops[line_id == i & direction_number == j & stop_sequence_number == k, GEOID10],
                          stops[line_id == i & direction_number == j & stop_sequence_number > k, GEOID10]))
      temp[, `:=` (line_id = i, direction_number = j)]
      OD <- rbind(OD, temp)
    }
  }
}

OD[, `:=` (node_1 = V1, node_2 = V2, V1 = NULL, V2 = NULL)]

OD_count <- OD[, .N, by = .(node_1, node_2)]
OD_count[, `:=` (weight = N, N = NULL)]

for (k in 1:max(stops[line_id == i & direction_number == j, stop_sequence_number])-1) {
  print(k)
}

OD <- unique(OD)
OD[, weight := .N, by = .(Var1, Var2)]

OD <- OD[!is.na(Var1) & !is.na(Var2)]

OD[, `:=` (Var1 = as.character(Var1), Var2 = as.character(Var2))]

saveRDS(OD, 'data/OD.RDS')

#Make graph from route connectivity
OD_graph <- graph_from_data_frame(OD[, .(Var1, Var2, weight)], directed = FALSE)

tract_wt <- cluster_walktrap(OD_graph, steps = 4, weights = E(OD_graph)$weight)
#tract_wt
tract_wt_memb <- membership(tract_wt)
tract_wt_memb <- data.table(tract = names(tract_wt_memb), clust = tract_wt_memb)
tract_wt <- merge(tracts, tract_wt_memb, by.x = 'GEOID10', by.y = 'tract', all.x = TRUE)

pal <- colorFactor(palette = "Paired", domain = tract_wt$clust)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tract_wt, fillColor = ~pal(clust), fillOpacity = .7, weight = 1)


#Try some edge prediction stuff
pred_egde <- predict_edges(tract_graph)
edges <- pred_egde[["edges"]]
edges <- as.data.table(edges)

probs <- pred_egde[["prob"]]
probs <- as.data.table(probs)

pred_edges <- cbind(edges, probs)
saveRDS(pred_edges, 'data/pred_edges.RDS')

tract_hrg <- pred_edge[["hrg"]]
saveRDS(tract_hrg, 'data/tract_hrg.RDS')


#Graph difference as a simple way to look at missing/unused routes
OD_no_dup <- unique(OD[, .(Var1, Var2)])
tract_no_dup <- unique(tracts_OD[, .(dest_tracts, orig_tracts)])

tract_for_diff <- graph_from_data_frame(tract_no_dup, directed = FALSE)
OD_for_diff <- graph_from_data_frame(OD_no_dup, directed = FALSE)

route_minus_tbi <- difference(OD_for_diff, tract_for_diff, byname = TRUE)
tbi_minus_route <- difference(tract_for_diff, OD_for_diff, byname = TRUE)


#Hennepin County clustering by tract
Henn_tracts <- subset(tracts, startsWith(GEOID10, '27053'))
Rams_tracts <- subset(tracts, startsWith(GEOID10, '27123'))
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, fillOpacity = .1, weight = 1) %>% addPolygons(data = Rams_tracts, fillOpacity = .5, weight = 1)

Henn_tracts_OD <- tracts_OD[dest_tracts %in% Henn_tracts$GEOID10 & orig_tracts %in% Henn_tracts$GEOID10]
Rams_tracts_OD <- tracts_OD[dest_tracts %in% Rams_tracts$GEOID10 & orig_tracts %in% Rams_tracts$GEOID10]

Henn_tract_graph <- graph_from_data_frame()