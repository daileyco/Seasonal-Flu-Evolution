#script to compute correlations using small trees data

## load data
load("./01-Data/01-Processed-Data/spatial.rdata")
load("./01-Data/01-Processed-Data/acs.rds")

load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")


## helper functions
source("./02-Scripts/02-Helper-Functions/generate_Spatial_Lag_Variable.R")


## packages
library(dplyr)
library(tidyr)
library(spdep)
library(igraph)
# library(ppcor)






## clean data to include only necessary variables

smalltrees <- smalltrees.df %>%
  select(subtype, season, location, n, rep, mpd)




## generate adjacency matrices for spatial lags

# us.shape.state <- us.shape.state %>%
#   arrange(NAME)
# state.neighbors.mat <- spdep::poly2nb(us.shape.state) %>%
#   spdep::nb2mat(style = "B", zero.policy = TRUE)

state.neighbors.mat <- us.shape.state %>% 
  filter(NAME%in%unique(smalltrees$location)) %>%
  arrange(NAME)

state.neighbors <- state.neighbors.mat$NAME

# state.neighbors.mat <- state.neighbors.mat %>%
#   spdep::poly2nb() %>% 
#   spdep::nb2mat(style = "B", zero.policy = TRUE)


state.nb.lags <- nblag(poly2nb(state.neighbors.mat), maxlag = 3)

# all.equal(c(state.neighbors.mat), c(nb2mat(state.nb.lags[[1]], style = "B", zero.policy = TRUE)))


state.nb.lags <- lapply(state.nb.lags, 
                             (\(x){
                               # nb2listw(x, style = "B", zero.policy = TRUE)
                               nb2mat(x, style = "B", zero.policy = TRUE)
                             }))





## network neighbors

acs1115 <- acs %>%
  filter(period%in%c("2011-2015") & !is.na(`Workers in Commuting Flow`)) %>%
  select(from = `State Residence`, to = `State Work`, weight = `Workers in Commuting Flow`) %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify() %>%
  as_adjacency_matrix(type = "both", attr = "weight", sparse = FALSE) 
# %>%
#   mat2listw(style = "W")




acs1620 <- acs %>%
  filter(period%in%c("2016-2020") & !is.na(`Workers in Commuting Flow`)) %>%
  select(from = `State Residence`, to = `State Work`, weight = `Workers in Commuting Flow`) %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify() %>%
  as_adjacency_matrix(type = "both", attr = "weight", sparse = FALSE) 
# %>%
#   mat2listw(style = "W")


















## set seed for reproducibility
# Sys.time()
# # "2024-03-12 13:37:03 EDT"
# as.numeric(Sys.time())
# # 1710265023
set.seed(1710265023)

## set up loop to 
### 1 resample data for single metric per subtype*season*location
### 2 create time lagged variables
### 3 create space lagged variables
### 4 create space time lagged variables
### 5 compute correlation coefficients




bootlist <- list()





for(i in 1:1000){
  
  cat("\n", i, "%\n")
  
  this.data <- smalltrees %>%
    
    group_by(subtype, location, season) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    
    arrange(subtype, location, season)
  
  
  
  # this.data.cc <- smalltrees %>%
  #   
  #   group_by(subtype, location, season) %>%
  #   slice_sample(n = 1) %>%
  #   ungroup() %>%
  #   
  #   arrange(subtype, location, season) %>% 
  #   rename(mpd_cc=mpd) %>%
  #   select(-n,-rep)
  
  # %>%
  #   
  #   split(., ~ subtype + season)
  
  
  
  nbs <- c(state.nb.lags, list(acs1115, acs1620))
  suffixes <- c(paste0("_slag", 1:3), paste0("_nlag", c("1115", "1620")))
  
  for(j in 1:length(nbs)){
    
    
    this.data <<- full_join(this.data, 
                           
                           lapply(c("BVic", "BYam", "H1", "H3"), 
                                  function(this.subtype){
                                    generate_Spatial_Lag(this.df = this.data, 
                                                         these.neighbors = state.neighbors, 
                                                         this.neighbor.matrix = nbs[[j]], 
                                                         this.subtype = this.subtype, 
                                                         this.variable = "mpd", 
                                                         new.var.suffix = suffixes[j])
                                  }) %>% 
                             bind_rows(), 
                           
                           by = c("subtype", "season", "location")
                           
                           )
    
    
    
  }
  
  
  
  
  
  
  this.data <- this.data %>%
    
    mutate(mpd_nlag1 = ifelse(season %in% c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015"), 
                              mpd_nlag1115, 
                              ifelse(season %in% c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020"), 
                                     mpd_nlag1620, 
                                     NA))) %>%
    
    select(-matches("1115|1620")) %>%
    
    # full_join(., 
    #           this.data.cc, 
    #           by = c("subtype", "location", "season")) %>%
    
    mutate(across(matches("mpd"), 
                  list(tlag1 = ~lag(.x,1), 
                       tlag2 = ~lag(.x,2), 
                       tlag3 = ~lag(.x,3)), 
                  .names = "{.col}_{.fn}"))
  
  
  
  
  tdfw <- this.data %>%
    select(subtype, season, location, matches("mpd")) %>%
    pivot_wider(names_from = subtype, values_from = matches("mpd"))
  
  
  
  combos <- expand.grid(x = paste0("mpd_", c("BVic", "BYam", "H1", "H3")), 
                        y = names(tdfw)[-2:-1]) %>%
    mutate(across(1:2, ~as.character(.x)))%>%
    # filter(substr(x,nchar(x)-4, nchar(x))!=substr(y,nchar(y)-4, nchar(y)))%>%
    # filter(substr(x,nchar(x)-2, nchar(x))!=substr(y,nchar(y)-2, nchar(y)))%>%
    arrange(x) %>%
    mutate(cor = NA, p = NA)
  
  
  
  for(k in 1:nrow(combos)){
    temp <- cor.test(unlist(tdfw[,combos$x[k]]), unlist(tdfw[,combos$y[k]]))
    combos$cor[k] <- temp$estimate
    combos$p[k] <- temp$p.value
  }
  
  
  bootlist[[i]] <- combos %>%
    mutate(iteration = i)
  
  
}




cordf <- bind_rows(bootlist) %>%
  group_by(x, y) %>%
  summarise(cor50 = median(cor), 
            cor2.5 = quantile(cor, probs = 0.025), 
            cor97.5 = quantile(cor, probs = 0.975)) %>%
  ungroup()




## save
save(cordf, file = "./01-Data/02-Analytic-Data/correlations.rds")



## clean environment
rm(list=ls())
gc()






