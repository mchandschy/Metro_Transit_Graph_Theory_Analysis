#Hennepin County clustering by tract ------
tracts <- readRDS('data/tracts.RDS')
Henn_tracts <- subset(tracts, startsWith(GEOID10, '27053'))

Henn_tracts_OD <- tracts_OD[dest_tracts %in% Henn_tracts$GEOID10 & orig_tracts %in% Henn_tracts$GEOID10]

Henn_tracts_OD[, weight := N]

Henn_tract_graph <- graph_from_data_frame(Henn_tracts_OD[, .(orig_tracts, dest_tracts, weight)], directed = TRUE)
Henn_tract_graph_ud <- graph_from_data_frame(Henn_tracts_OD[, .(orig_tracts, dest_tracts, weight)], directed = FALSE)

#BC
bet_tract <- betweenness(Henn_tract_graph)
bet_tract <- data.table(tract = names(bet_tract), BC = bet_tract)
Henn_tracts <- merge(Henn_tracts, bet_tract, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

pal_tract <- colorNumeric(palette = "Reds", domain = Henn_tracts$BC)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, fillOpacity = 0, weight = 1) %>% addPolygons(data = Henn_tracts, fillOpacity = .9, fillColor = ~pal_tract(BC), weight = 1)

#PR
Henn_pr_tract <- page_rank(Henn_tract_graph, vids = V(Henn_tract_graph), weights = edge_attr(Henn_tract_graph, name = 'weight', index = E(Henn_tract_graph)))
Henn_pr_tract <- data.table(tract = names(Henn_pr_tract$vector), PR = Henn_pr_tract$vector)
Henn_tracts <- merge(Henn_tracts, Henn_pr_tract, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

pal_tract_PR <- colorNumeric(palette = "Reds", domain = Henn_tracts$PR)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                        attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                        under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                        Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                        under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tracts, fillOpacity = 0, weight = 1) %>%
  addPolygons(data = Henn_tracts, fillColor = ~pal_tract_PR(PR), fillOpacity = .9, weight = 1)

#WTC
Henn_tract_wt <- cluster_walktrap(Henn_tract_graph, steps = 5, weights = E(Henn_tract_graph)$weight)
tract_wt_memb <- membership(Henn_tract_wt)
tract_wt_memb <- data.table(tract = names(tract_wt_memb), clust = tract_wt_memb)
Henn_tract_wt <- merge(Henn_tracts, tract_wt_memb, by.x = 'GEOID10', by.y = 'tract', all.x = TRUE)

pal <- colorFactor(palette = "Dark2", domain = subset(Henn_tract_wt)$clust)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, weight = 1, fillOpacity = 0) %>% addPolygons(data = subset(Henn_tract_wt), fillColor = ~pal(clust), fillOpacity = .7, weight = 1)

#Ramsey County clustering by tract ------
tracts <- readRDS('data/tracts.RDS')
Rams_tracts <- subset(tracts, startsWith(GEOID10, '27123'))

Rams_tracts_OD <- tracts_OD[dest_tracts %in% Rams_tracts$GEOID10 & orig_tracts %in% Rams_tracts$GEOID10]

Rams_tracts_OD[, weight := N]

Rams_tract_graph <- graph_from_data_frame(Rams_tracts_OD[, .(orig_tracts, dest_tracts, weight)], directed = TRUE)
Rams_tract_graph_ud <- graph_from_data_frame(Rams_tracts_OD[, .(orig_tracts, dest_tracts, weight)], directed = FALSE)

#BC
bet_tract <- betweenness(Rams_tract_graph)
bet_tract <- data.table(tract = names(bet_tract), BC = bet_tract)
Rams_tracts <- merge(Rams_tracts, bet_tract, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

pal_tract <- colorNumeric(palette = "Reds", domain = Rams_tracts$BC)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, fillOpacity = 0, weight = 1) %>% addPolygons(data = Rams_tracts, fillOpacity = .9, fillColor = ~pal_tract(BC), weight = 1)

#PR
Rams_pr_tract <- page_rank(Rams_tract_graph, vids = V(Rams_tract_graph), weights = edge_attr(Rams_tract_graph, name = 'weight', index = E(Rams_tract_graph)))
Rams_pr_tract <- data.table(tract = names(Rams_pr_tract$vector), PR = Rams_pr_tract$vector)
Rams_tracts <- merge(Rams_tracts, Rams_pr_tract, by.x = 'GEOID10', by.y = 'tract', all.y = TRUE)

pal_tract_PR <- colorNumeric(palette = "Reds", domain = Rams_tracts$PR)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, fillOpacity = 0, weight = 1) %>% addPolygons(data = Rams_tracts, fillOpacity = .9, fillColor = ~pal_tract_PR(PR), weight = 1)

#WTC
Rams_tract_wt <- cluster_walktrap(Rams_tract_graph, steps = 12, weights = E(Rams_tract_graph)$weight)
tract_wt_memb <- membership(Rams_tract_wt)
tract_wt_memb <- data.table(tract = names(tract_wt_memb), clust = tract_wt_memb)
Rams_tract_wt <- merge(Rams_tracts, tract_wt_memb, by.x = 'GEOID10', by.y = 'tract', all.x = TRUE)

pal <- colorFactor(palette = "Dark2", domain = subset(Rams_tract_wt)$clust)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, weight = 1, fillOpacity = 0) %>% addPolygons(data = subset(Rams_tract_wt), fillColor = ~pal(clust), fillOpacity = .7, weight = 1)

#Don't do this part - blocks are still too small to be meaningful for the TBI data for an entire county
#Hennepin County by Block ----
blocks <- readRDS('data/blocks.RDS')
blocks_OD <- readRDS('data/blocks_flowmap.RDS')
Henn_blocks <- subset(blocks, startsWith(GEOID10, '27053'))

Henn_blocks_OD <- blocks_OD[dest_blocks %in% Henn_blocks$GEOID10 & orig_blocks %in% Henn_blocks$GEOID10]

Henn_blocks_OD[, weight := N]

Henn_block_graph <- graph_from_data_frame(Henn_blocks_OD[, .(orig_blocks, dest_blocks, weight)], directed = TRUE)
Henn_block_graph_ud <- graph_from_data_frame(Henn_blocks_OD[, .(orig_blocks, dest_blocks, weight)], directed = FALSE)

#BC
bet_block <- betweenness(Henn_block_graph)
bet_block <- data.table(block = names(bet_block), BC = bet_block)
Henn_blocks <- merge(Henn_blocks, bet_block, by.x = 'GEOID10', by.y = 'block', all.y = TRUE)

pal_block <- colorNumeric(palette = "Reds", domain = Henn_blocks$BC)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, fillOpacity = 0, weight = 1) %>% addPolygons(data = Henn_blocks, fillOpacity = .9, fillColor = ~pal_block(BC), weight = 1)

#PR
Henn_pr_block <- page_rank(Henn_block_graph, vids = V(Henn_block_graph), weights = edge_attr(Henn_block_graph, name = 'weight', index = E(Henn_block_graph)))
Henn_pr_block <- data.table(block = names(Henn_pr_block$vector), PR = Henn_pr_block$vector)
Henn_blocks <- merge(Henn_blocks, Henn_pr_block, by.x = 'GEOID10', by.y = 'block', all.y = TRUE)

pal_block_PR <- colorNumeric(palette = "Reds", domain = Henn_blocks$PR)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                                        under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                                        Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                                        under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tracts, fillOpacity = 0, weight = 1) %>%
  addPolygons(data = Henn_blocks, fillColor = ~pal_block_PR(PR), fillOpacity = .9, weight = 1)

#WTC
Henn_block_wt <- cluster_walktrap(Henn_block_graph, steps = 15, weights = E(Henn_block_graph)$weight)
block_wt_memb <- membership(Henn_block_wt)
block_wt_memb <- data.table(block = names(block_wt_memb), clust = block_wt_memb)
Henn_block_wt <- merge(Henn_blocks, block_wt_memb, by.x = 'GEOID10', by.y = 'block', all.x = TRUE)

pal <- colorFactor(palette = "Dark2", domain = subset(Henn_block_wt)$clust)
leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
  addPolygons(data = tracts, weight = 1, fillOpacity = 0) %>% addPolygons(data = subset(Henn_block_wt), fillColor = ~pal(clust), fillOpacity = .7, weight = 1)




#Minneapolis ----
MPLS_tracts_ID <- c('27053000101', '27053000102', '27053100200', '27053100400', '27053100700', 
                    '27053100800', '27053100900', '27053100500', '27053000601', '27053000603',
                    '27053101200', '27053101800', '27053101900', '27053101600', '27053125700',
                    '27053101300', '27053102000', '27053102100', '27053102300', '27053102500',
                    '27053102100', '27053102300', '27053102800', '27053103000', '27053102600',
                    '27053102900', '27053103100', '27053104000', '27053103400', '27053103600',
                    '27053104100', '27053105100', '27053106500', '27053109100', '27053109800',
                    '27053105500', '27053106600', '27053108000', '27053109900', '27053111300',
                    '27053111400', '27053012001', '27053000300', '27053001100', '27053001700',
                    '27053001700', '27053002400', '27053002200', '27053002700', '27053003200',
                    '27053003300', '27053010600', '27053126200', '27053126100', '27053104400',
                    '27053103700', '27053003800', '27053103900', '27053104900', '27053125600',
                    '27053104800', '27053105400', '27053105201', '27053105204', '27053105600',
                    '27053105700', '27053005901', '27053005902', '27053106000', '27053106200',
                    '27053106700', '27053006800', '27053107000', '27053106900', '27053007801',
                    '27053106400', '27053106200', '27053107500', '27053125900', '27053125800',
                    '27053126000', '27053007700', '27053008100', '27053008200', '27053008300',
                    '27053008400', '27053008500', '27053109200', '27053109300', '27053109400',
                    '27053010700', '27053110800', '27053110000', '27053009500', '27053009600',
                    '27053107400', '27053107600', '27053108800', '27053109800', '27053108700',
                    '27053108900', '27053109000', '27053108600', '27053109700', '27053110400',
                    '27053111500', '27053012003', '27053111600', '27053011703', '27053011704',
                    '27053110900', '27053011000', '27053110100', '27053110200', '27053111100',
                    '27053110500', '27053011998', '27053012102', '27053012101', '27053011800',
                    '27053111200'
                    )
MPLS_tracts <- subset(tracts, GEOID10 %in% MPLS_tracts_ID)

leaflet() %>% addTiles(urlTemplate = 'http://tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                       attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>,
                       under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>.
                       Data by <a href="http://openstreetmap.org">OpenStreetMap</a>,
                       under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.') %>%
  setView(lat = 44.963, lng = -93.22, zoom = 11) %>%
  addPolygons(data = tracts, fillOpacity = .1, weight = 1) %>% addPolygons(data = MPLS_tracts, fillOpacity = .7, weight = 1)

list <- tracts$GEOID10
