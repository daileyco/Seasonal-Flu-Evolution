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

state.nb.lags1 <- nblag_cumul(state.nb.lags[c(1,2)])
state.nb.lags2 <- nblag_cumul(state.nb.lags[c(1,2,3)])


# state.nb.lags <- list(state.nb.lags[[1]], state.nb.lags1, state.nb.lags2) 
                        
state.nb.lags <- lapply(list(state.nb.lags[[1]], state.nb.lags1, state.nb.lags2),
                             (\(x){
                               # nb2listw(x, style = "B", zero.policy = TRUE)
                               mat <- nb2mat(x, style = "B", zero.policy = TRUE)
                               mat
                               # diag(mat) <- 1
                               # mat <- mat2listw(mat, style = "B")
                             }))




sts <- smalltrees.df %>%
  group_by(subtype, season, location) %>% 
  summarise(lmean = mean(mpd, na.rm = T)) %>%
  ungroup()


nbs <- c(state.nb.lags)
suffixes <- c(paste0("_slag", 1:3))

for(j in 1:length(nbs)){
  
  
  sts <<- full_join(sts, 
                          
                          lapply(c("BVic", "BYam", "H1", "H3"), 
                                 function(this.subtype){
                                   generate_Spatial_Lag(this.df = sts, 
                                                        these.neighbors = state.neighbors, 
                                                        this.neighbor.matrix = nbs[[j]], 
                                                        this.subtype = this.subtype, 
                                                        this.variable = "lmean", 
                                                        new.var.suffix = suffixes[j])
                                 }) %>% 
                            bind_rows(), 
                          
                          by = c("subtype", "season", "location")
                          
  )
  
  
  
}


wss <- sts %>%
  
  full_join(., 
            smalltrees, 
            by = c("subtype", "season", "location")) %>% 
  
  group_by(subtype, season) %>%
  mutate(lmean_slagus = mean(lmean[which(!duplicated(c(location, lmean)))], na.rm = T), 
         mpd_slagus = mean(mpd, na.rm = T))%>%
  ungroup() %>%
  
  group_by(subtype) %>% 
  mutate(smean = mean(lmean[which(!duplicated(c(season, location, lmean)))], na.rm = T), 
         mpd_smean = mean(mpd, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(allmean = mean(lmean[which(!duplicated(c(subtype, season, location, lmean)))], na.rm = T), 
         mpd_allmean = mean(mpd, na.rm = T)) %>%
  
  
  mutate(ls = (mpd-lmean)^2, 
         ls1 = (mpd-lmean_slag1)^2, 
         ls2 = (mpd-lmean_slag2)^2,
         ls3 = (mpd-lmean_slag3)^2,
         lsus = (mpd-lmean_slagus)^2, 
         ss = (mpd-smean)^2, 
         as = (mpd-allmean)^2, 
         
         mlsus = (mpd-mpd_slagus)^2, 
         mss = (mpd-mpd_smean)^2, 
         mas = (mpd-mpd_allmean)^2
         ) %>% 
  
  select(subtype, season, location, mpd, everything())
  




  # anova(aov(mpd~1, data = smalltrees), aov(mpd~subtype, data = smalltrees), aov(mpd~subtype*season, data = smalltrees), aov(mpd~subtype*season*location, data = smalltrees))

ws <- wss %>% 
  filter(!location%in%c("Alaska", "Hawaii") & !is.na(lmean) & lmean!=0)%>%
  summarise(across(c(as, ss, lsus, 
                     #ls3, ls2, ls1, 
                     ls
                     # , mas, mss, mlsus
                     ), ~sum(.x, na.rm = TRUE))) %>% 
  unlist()








png(filename = "./03-Output/02-Figures/ss_barplot.png", width = 4, height = 3, units = "in", res = 300, pointsize = 10)
barplot(ws, width = 1, space = 0.5, xaxt = 'n', main = "Mean Pairwise Patristic Distance\nSum of Squares")
text(c(1,2.5,4,5.5), y = ws, labels = round(ws), adj = c(0.5,-0.25), xpd = T)
axis(1, at = seq(1,6,by=1.5), labels = F, tick = T)
axis(1, at = seq(1,6,by=1.5), labels = c("Total SS", "-Subtype", "-Season", "-Location"), tick = F)
dev.off()






## clean environment
rm(list=ls())
gc()






