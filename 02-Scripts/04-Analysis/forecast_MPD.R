



library(tidyr)

tdfw <- trees.df.imputed %>%
  select(subtype, season, location, matches("mpd")) %>%
  pivot_wider(names_from = subtype, values_from = matches("mpd"))



fit1 <- lm(log(mpd_H3)~
             log(mpd_lag1_H3)+
             log(mpd_lag2_H3)+
             log(mpd_spatial_lag1_tlag1_H3)+
             log(mpd_spatial_lag1_tlag2_H3)+
             log(mpd_lag1_H1)+
             log(mpd_lag2_H1)+
             log(mpd_spatial_lag1_tlag1_H1)+
             log(mpd_spatial_lag1_tlag2_H1)+
             # log(mpd_lag1_BYam)+
             # log(mpd_lag2_BYam)+
             # log(mpd_spatial_lag1_tlag1_BYam)+
             # log(mpd_spatial_lag1_tlag2_BYam)+
             # log(mpd_lag1_BVic)+
             # log(mpd_lag2_BVic)+
             # log(mpd_spatial_lag1_tlag1_BVic)+
             # log(mpd_spatial_lag1_tlag2_BVic)+
             season
             , 
             data = tdfw)

summary(fit1)




































