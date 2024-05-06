



load("./01-Data/02-Analytic-Data/treesummaries_spacetime.rds")


library(dplyr)
library(PerformanceAnalytics)

library(tidyr)

tdfw <- trees.df.imputed %>%
  select(subtype, season, location, matches("mpd")) %>%
  pivot_wider(names_from = subtype, values_from = matches("mpd"))



lapply(c("BVic", "BYam", "H1", "H3"), 
       function(this.subtype){
         svg(paste0("./03-Output/02-Figures/cor_mat_stlag_", this.subtype, ".svg"), height = 15, width = 15, pointsize = 10)
         chart.Correlation(trees.df.imputed%>%
                             filter(subtype == this.subtype) %>%
                             select(-matches("[l|k|f|t][_]lag1$")) %>%
                             select(matches("mpd"))#%>%mutate(across(everything(), ~log(.x)))
                           , 
                           histogram = TRUE, 
                           pch = 16)
         dev.off()
       })



combos <- expand.grid(x = paste0("mpd_", c("BVic", "BYam", "H1", "H3")), 
                      y = names(tdfw)[31:62]) %>%
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
  mutate(corr = as.character(round(cor, 3))) %>%
  mutate(corr = case_when(p<0.001 ~ paste0(corr, "***"), 
                          p<0.01 ~ paste0(corr, "**"), 
                          p<0.05 ~ paste0(corr, "*"), 
                          TRUE ~ corr)) %>%
  mutate(nbtype = sub("^mpd[_](.+)[_].+[_].+[_].+$", "\\1", y), 
         tlag = sub("tlag", "", sub("^mpd[_].+[_].+[_](.+)[_].+$", "\\1", y)), 
         subtype = sub("^mpd[_].+[_].+[_].+[_](.+)$", "\\1", y)) %>%
  select(1,5:8)%>%
  pivot_wider(names_from = subtype, values_from = corr) %>%
  pivot_wider(names_from = tlag, values_from = 4:7)





rm(list=ls())
gc()
































