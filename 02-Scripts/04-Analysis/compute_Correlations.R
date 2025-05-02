



load("./01-Data/02-Analytic-Data/treesdf.rds")


library(dplyr)
library(tidyr)



tdfw <- treesdf %>%
  select(subtype, season, location, matches("mpd")) %>%
  pivot_wider(names_from = subtype, values_from = matches("mpd"))



combos <- expand.grid(x = paste0("mpd_", c("BVic", "BYam", "H1", "H3")), 
                      y = names(tdfw)[3:38]) %>%
  mutate(across(1:2, ~as.character(.x)))%>%
  # filter(substr(x,nchar(x)-4, nchar(x))!=substr(y,nchar(y)-4, nchar(y)))%>%
  # filter(substr(x,nchar(x)-2, nchar(x))!=substr(y,nchar(y)-2, nchar(y)))%>%
  arrange(x) %>%
  mutate(cor = NA, p = NA)



for(i in 1:nrow(combos)){
  temp <- cor.test(unlist(tdfw[,combos$x[i]]), unlist(tdfw[,combos$y[i]]))
  combos$cor[i] <- temp$estimate
  combos$p[i] <- temp$p.value
}




combos <- combos %>%
  mutate(corr.round = as.character(round(cor, 3))) %>%
  mutate(corr.round = case_when(p<0.001 ~ paste0(corr.round, "***"), 
                          p<0.01 ~ paste0(corr.round, "**"), 
                          p<0.05 ~ paste0(corr.round, "*"), 
                          TRUE ~ corr.round)) %>%
  mutate(var = substr(y, 1,3), 
         y1 = substr(y,5,nchar(y)), 
         subtypex = substr(x,5,nchar(x)), 
         subtypey = sub(".+_{1}(.+)$", "\\1", y)) %>%
  mutate(cortype = ifelse(subtypex==subtypey, "Auto", "Cross"), 
         y1 = substr(y1, 1, nchar(y1)-nchar(subtypey)-1)) %>%
  mutate(nbtype = case_when(grepl("network", y) ~ "Network", 
                            grepl("spatial", y) ~ "Spatial",
                            TRUE ~ NA), 
         y1 = sub("spatial_lag1[_]*|network_lag1[_]*", "", y1)) %>%
  mutate(tlag = gsub("[a-z]", "", y1)%>%as.numeric(), 
         tlag = ifelse(is.na(tlag), 0, tlag)) %>%
  select(-y1)


mycorrs <- combos


save(mycorrs, 
     file = "./01-Data/02-Analytic-Data/mycorrs.rds")


rm(list=ls())
gc()


