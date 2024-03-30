

library(tidyr)
library(dplyr)
library(spdep)
library(igraph)




load("./01-Data/02-Analytic-Data/tree_summaries.rdata")


load("./01-Data/01-Processed-Data/spatial.rdata")
load("./01-Data/01-Processed-Data/acs.rds")

source("./02-Scripts/02-Helper-Functions/generate_Spatial_Lag_Variable.R")











## temporal lag


trees.df.imputed <- trees.df.imputed %>%
  group_by(subtype, location) %>%
  mutate(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,1), .names = "{.col}_lag1"), 
         across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,2), .names = "{.col}_lag2")) %>%
  ungroup()














## spatial lag

# us.shape.state <- us.shape.state %>%
#   arrange(NAME)
# state.neighbors.mat <- spdep::poly2nb(us.shape.state) %>%
#   spdep::nb2mat(style = "B", zero.policy = TRUE)



state.neighbors.mat <- us.shape.state %>% 
  filter(NAME%in%unique(trees.df.imputed$location)) %>%
  arrange(NAME)

state.neighbors <- state.neighbors.mat$NAME
  
state.neighbors.mat <- state.neighbors.mat %>%
  spdep::poly2nb() %>% 
  spdep::nb2mat(style = "B", zero.policy = TRUE)


# spdep::nblag()

these.vars <- c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder")

for(i in 1:length(these.vars)){
  
  # i=1
  # this.subtype="H3"
  # this.subtype="BVic"
  
  slag <- lapply(c("BVic", "BYam", "H1", "H3"), 
                 function(this.subtype){
                   generate_Spatial_Lag(these.neighbors = state.neighbors, 
                                        this.neighbor.matrix = state.neighbors.mat, 
                                        this.subtype = this.subtype, 
                                        this.variable = these.vars[i], 
                                        new.var.suffix = "_spatial_lag1")
                 }) %>% bind_rows()
  
  trees.df.imputed <- full_join(trees.df.imputed, 
                                slag, 
                                by = c("subtype", "season", "location"))
  
}







acs1115 <- acs %>%
  filter(period%in%c("2011-2015") & !is.na(`Workers in Commuting Flow`)) %>%
  select(from = `State Residence`, to = `State Work`, weight = `Workers in Commuting Flow`) %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify() %>%
  as_adjacency_matrix(type = "both", attr = "weight", sparse = FALSE)


acs1620 <- acs %>%
  filter(period%in%c("2016-2020") & !is.na(`Workers in Commuting Flow`)) %>%
  select(from = `State Residence`, to = `State Work`, weight = `Workers in Commuting Flow`) %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify() %>%
  as_adjacency_matrix(type = "both", attr = "weight", sparse = FALSE)






these.vars <- c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder")

for(i in 1:length(these.vars)){
  
  slag <- lapply(c("BVic", "BYam", "H1", "H3"), 
                 function(this.subtype){
                   generate_Spatial_Lag(these.neighbors = rownames(acs1115), 
                                        this.neighbor.matrix = acs1115, 
                                        this.subtype = this.subtype, 
                                        this.variable = these.vars[i], 
                                        new.var.suffix = "_network_lag1_1115")
                 }) %>% bind_rows()
  
  trees.df.imputed <- full_join(trees.df.imputed, 
                                slag, 
                                by = c("subtype", "season", "location"))
  
  
  
  slag <- lapply(c("BVic", "BYam", "H1", "H3"), 
                 function(this.subtype){
                   generate_Spatial_Lag(these.neighbors = rownames(acs1620), 
                                        this.neighbor.matrix = acs1620, 
                                        this.subtype = this.subtype, 
                                        this.variable = these.vars[i], 
                                        new.var.suffix = "_network_lag1_1620")
                 }) %>% bind_rows()
  
  trees.df.imputed <- full_join(trees.df.imputed, 
                                slag, 
                                by = c("subtype", "season", "location"))
  
  
  
  trees.df.imputed <- trees.df.imputed %>%
    mutate('{paste0(these.vars[i], "_network_lag1")}' := ifelse(season%in%c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015"), 
                                                                .data[[paste0(these.vars[i], "_network_lag1_1115")]], 
                                                                ifelse(season%in%c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020"), 
                                                                       .data[[paste0(these.vars[i], "_network_lag1_1620")]], 
                                                                       NA)))
  

  
  trees.df.imputed <- trees.df.imputed %>%
    select(-matches("1115|1620"))
  
  
  
  
  
}







trees.df.imputed <- trees.df.imputed %>%
  group_by(subtype, location) %>%
  arrange(season.num) %>%
  mutate(across(matches("spatial|network"), 
                list(tlag1 = ~lag(.x, 1), 
                     tlag2 = ~lag(.x, 2)))) %>%
  ungroup()


treesdf <- trees.df.imputed

save(treesdf, file = "./01-Data/02-Analytic-Data/treesdf.rds")


rm(list=ls())
gc()



