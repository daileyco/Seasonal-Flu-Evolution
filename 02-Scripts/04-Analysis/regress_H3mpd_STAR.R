


load("./01-Data/02-Analytic-Data/tree_summaries_clean.rdata")
load("./01-Data/02-Analytic-Data/treesummaries_spacetime.rds")


library(dplyr)




fit1 <- lm(mpd~
             mpd_lag1+
             # mpd_lag2+
             mpd_spatial_lag1_tlag1+
             # mpd_spatial_lag1_tlag2+
             season, 
           data = trees.df.imputed[which(trees.df.imputed$subtype=="H3"),])

summary(fit1)
plot(fit1)


fit1 <- lm(sqrt(sqrt(mpd))~
             sqrt(sqrt(mpd_lag1))+
             # mpd_lag2+
             sqrt(sqrt(mpd_spatial_lag1_tlag1))+
             # mpd_spatial_lag1_tlag2+
             season, 
           data = trees.df.imputed[which(trees.df.imputed$subtype=="H3"),])

summary(fit1)

fit1 <- lm(log(mpd)~
             log(mpd_lag1)+
             # mpd_lag2+
             log(mpd_spatial_lag1_tlag1)+
             # mpd_spatial_lag1_tlag2+
             season, 
           data = trees.df.imputed[which(trees.df.imputed$subtype=="H3"),])

summary(fit1)


fit1 <- lm(log(mpd)~
             # log(mpd_lag1)+
             log(mpd_lag2)+
             # log(mpd_spatial_lag1_tlag1)+
             # log(mpd_spatial_lag1_tlag2)+
             season, 
           data = trees.df.imputed[which(trees.df.imputed$subtype=="H1"),])

summary(fit1)


fit1 <- lm(log(mpd)~
             # log(mpd_lag1)+
             # log(mpd_lag2)+
             # log(mpd_spatial_lag1_tlag1)+
             # log(mpd_spatial_lag1_tlag2)+
             season, 
           data = trees.df.imputed[which(trees.df.imputed$subtype=="BYam"),])

summary(fit1)


fit1 <- lm(log(mpd)~
             # log(mpd_lag1)+
             # log(mpd_lag2)+
             log(mpd_spatial_lag1_tlag1)+
             # log(mpd_spatial_lag1_tlag2)+
             season, 
           data = trees.df.imputed[which(trees.df.imputed$subtype=="BVic"),])

summary(fit1)







library(tidyr)

tdfw <- trees.df.imputed %>%
  select(subtype, season, location, matches("mpd")) %>%
  pivot_wider(names_from = subtype, values_from = matches("mpd"))





fit1 <- lm(log(mpd_H3)~
             log(mpd_lag1_H3)+
             # log(mpd_lag2_H3)+
             # log(mpd_spatial_lag1_tlag1_H3)+
             # log(mpd_spatial_lag1_tlag2_H3)+
             log(mpd_lag1_H1)+
             # log(mpd_lag2_H1)+
             # log(mpd_spatial_lag1_tlag1_H1)+
             # log(mpd_spatial_lag1_tlag2_H1)+
             log(mpd_lag1_BYam)+
             # log(mpd_lag2_BYam)+
             # log(mpd_spatial_lag1_tlag1_BYam)+
             # log(mpd_spatial_lag1_tlag2_BYam)+
             log(mpd_lag1_BVic)#+
             # log(mpd_lag2_BVic)+
             # log(mpd_spatial_lag1_tlag1_BVic)+
             # log(mpd_spatial_lag1_tlag2_BVic)+
             # season
           , 
           data = tdfw)

summary(fit1)















library(dplyr)
library(lme4)



trees.reps1 <- full_join(trees.reps%>%select(-matches("lag")), 
                        trees.df.imputed[,-c(4:14,27:50)], 
                        by = c("subtype", "season", "location"))  %>%
  full_join(., 
            tdfw, 
            by = c("season", "location"))

# fit1 <- lmer(mpd~
#                mpd_lag1+
#                mpd_lag2+
#                mpd_spatial_lag1_tlag1+
#                # mpd_spatial_lag1_tlag2+
#                season+
#                (1|location), 
#              data = trees.reps1[which(trees.reps1$subtype=="H3"),])
fit1 <- lmer(log(mpd)~
               # ntips+
               log(mpd_lag1)+
               log(mpd_lag2)+
               log(mpd_spatial_lag1_tlag1)+
               log(mpd_spatial_lag1_tlag2)+
               season+
               (1|location), 
             data = trees.reps1[which(trees.reps1$subtype=="H3"),])



fit1 <- lmer(log(mpd)~
               ntips+
               log(mpd_lag1)+
               # log(mpd_lag2)+
               # log(mpd_spatial_lag1_tlag1)+
               log(mpd_spatial_lag1_tlag2)+
               # log(mpd_lag1_BVic)+
               # log(mpd_lag2_BVic)+
               # log(mpd_spatial_lag1_tlag1_BVic)+
               # log(mpd_spatial_lag1_tlag2_BVic)+
               log(mpd_lag1_BYam)+
               # log(mpd_lag2_BYam)+
               # log(mpd_spatial_lag1_tlag1_BYam)+
               # log(mpd_spatial_lag1_tlag2_BYam)+
               log(mpd_lag1_H1)+
               # log(mpd_lag2_H1)+
               # log(mpd_spatial_lag1_tlag1_H1)+
               # log(mpd_spatial_lag1_tlag2_H1)+
               season+
               (1|location), 
             data = trees.reps1[which(trees.reps1$subtype=="H3"),])






summary(fit1)
plot(fit1)
car::Anova(fit1, type = 3)



fit1 <- lmer(log(mpd)~
               ntips+
               log(mpd_lag1)+
               # log(mpd_lag2)+
               # log(mpd_spatial_lag1_tlag1)+
               # log(mpd_spatial_lag1_tlag2)+
               season+
               (1|location), 
             data = trees.reps1[which(trees.reps1$subtype=="H1"),])



summary(fit1)
plot(fit1)
car::Anova(fit1, type = 3)





fit1 <- lmer(log(mpd)~
               ntips+
               log(mpd_lag1)+
               # log(mpd_lag2)+
               # log(mpd_spatial_lag1_tlag1)+
               # log(mpd_spatial_lag1_tlag2)+
               season+
               (1|location), 
             data = trees.reps1[which(trees.reps1$subtype=="BYam"),])



summary(fit1)
plot(fit1)
car::Anova(fit1, type = 3)




fit1 <- lmer(log(mpd)~
               ntips+
               log(mpd_lag1)+
               # log(mpd_lag2)+
               # log(mpd_spatial_lag1_tlag1)+
               # log(mpd_spatial_lag1_tlag2)+
               season+
               (1|location), 
             data = trees.reps1[which(trees.reps1$subtype=="BVic"),])



summary(fit1)
plot(fit1)
car::Anova(fit1, type = 3)









fit1 <- lmer(log(mpd)~
               # subtype+
               subtype*ntips+
               log(mpd_lag1)+
               subtype*log(mpd_lag2)+
               log(mpd_spatial_lag1_tlag1)+
               subtype*log(mpd_spatial_lag1_tlag2)+
               subtype*season+
               (1|location), 
             data = trees.reps1)



summary(fit1)
plot(fit1)
car::Anova(fit1, type = 3)








fit1 <- lmer(log(mpd)~
               # subtype+
               subtype*ntips+
               subtype*log(mpd_lag1)+
               subtype*log(mpd_lag2)+
               subtype*log(mpd_spatial_lag1_tlag1)+
               subtype*log(mpd_spatial_lag1_tlag2)+
               subtype*log(mpd_network_lag1_tlag1)+
               subtype*log(mpd_network_lag1_tlag2)+
               subtype*season+
               (1|location), 
             data = trees.reps1)



summary(fit1)
plot(fit1)
car::Anova(fit1, type = 3)







# 
# 
# library(tidyr)
# 
# 
# h3 <- trees.reps1 %>%
#   filter(subtype=="H3") %>%
#   select(1:3, matches("mpd"))
# 
# 
# h1 <- trees.reps1 %>%
#   filter(subtype=="H1") %>%
#   select(1:3, matches("mpd"))
# names(h1)[4:14] <- paste0(names(h1)[4:14], "_h1")
# 
# by <- trees.reps1 %>%
#   filter(subtype=="BYam") %>%
#   select(1:3, matches("mpd"))
# names(by)[4:14] <- paste0(names(by)[4:14], "_byam")
# 
# bv <- trees.reps1 %>%
#   filter(subtype=="BVic") %>%
#   select(1:3, matches("mpd"))
# names(bv)[4:14] <- paste0(names(bv)[4:14], "_bvic")
# 
# 
# 
# 
# mpd <- full_join(h3, h1, by = c("season", "location"), )
# 
# 
# tdfw <- trees.reps1 %>%
#   select(subtype, season, location, matches("mpd")) %>%
#   pivot_wider(names_from = subtype, values_from = matches("mpd"))
# 
# 
# 
# 
# fit1 <- lmer(log(mpd)~
#                # subtype+
#                subtype*ntips+
#                log(mpd_lag1)+
#                subtype*log(mpd_lag2)+
#                log(mpd_spatial_lag1_tlag1)+
#                subtype*log(mpd_spatial_lag1_tlag2)+
#                subtype*season+
#                (1|location), 
#              data = trees.reps1)
# 
# 
# 
# summary(fit1)
# plot(fit1)
# car::Anova(fit1, type = 3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# load("./01-Data/02-Analytic-Data/tree_summaries_clean.rdata")
# 
# 
# 
# library(tidyr)
# library(dplyr)
# library(spdep)
# 
# 
# 
# 
# combos <- expand.grid(season = unique(iqtree$season), 
#                       location = unique(iqtree$location))
# 
# 
# 
# 
# iqtree.wide <- iqtree %>%
#   select(subtype, season, location, mpd, rep) %>%
#   pivot_wider(., names_from = "subtype", values_from = "mpd")
# 
# iqtree.wide <- iqtree.wide %>% 
#   group_by(season, location) %>% 
#   summarise(across(c(H3, H1, BVic, BYam), ~mean(.x, na.rm = T)))%>%
#   ungroup()
# 
# iqtree.wide <- iqtree.wide %>% full_join(combos, by = c("season", "location"))
# 
# 
# iqtree.wide <- iqtree.wide %>%
#   group_by(season) %>%
#   mutate(H3 = case_when(is.na(H3) | is.nan(H3) ~ median(H3, na.rm = TRUE), 
#                         TRUE ~ H3))%>%
#   ungroup()
# 
# 
# 
# iqtree.wide <- iqtree.wide %>%
#   mutate(season.num = as.numeric(substr(season, 1, 4))) %>%
#   arrange(location, season.num)%>%
#   group_by(location)%>%
#   mutate(H3.lag1 = lag(H3, 1),
#          H3.lag2 = lag(H3, 2),
#          H1.lag1 = lag(H1, 1),
#          H1.lag2 = lag(H1, 2),
#          BVic.lag1 = lag(BVic, 1),
#          BVic.lag2 = lag(BVic, 2),
#          BYam.lag1 = lag(BYam, 1), 
#          BYam.lag2 = lag(BYam, 2))%>%
#   ungroup()
# 
# 
# 
# 
# 
# load("./01-Data/01-Processed-Data/spatial.rdata")
# load("./01-Data/01-Processed-Data/acs_state_network.rdata")
# 
# us.shape.state.contig <- us.shape.state.contig %>% arrange(NAME)
# state.neighbors.nb <- spdep::poly2nb(us.shape.state.contig)
# state.neighbors.mat <- spdep::nb2mat(state.neighbors.nb, style = "B")
# 
# 
# h3.wide <- iqtree.wide %>%
#   select(season, location, H3) %>% 
#   filter(!location%in%c("Hawaii", "Alaska"))%>%
#   pivot_wider(names_from = season, values_from = H3)
# 
# h3.mat <- data.matrix(h3.wide[,2:11])
# 
# 
# h3.spatial.lag1 <- solve(diag(apply(state.neighbors.mat,1,sum)))%*%state.neighbors.mat%*%h3.mat
# 
# h3.spatial.lag1 <- h3.spatial.lag1 %>% 
#   as.data.frame()
# 
# h3.spatial.lag1$location <- h3.wide$location
# 
# h3.spatial.lag1 <- h3.spatial.lag1 %>%
#   select(location, everything()) %>%
#   pivot_longer(3:11, names_to = "season", values_to = "H3mpd.spatial.lag1") %>%
#   arrange(season)
# 
# 
# h3.long <- h3.wide %>%
#   pivot_longer(3:11, names_to = "season", values_to = "H3mpd") %>%
#   arrange(season)
# 
# h3.temporal.lag1 <- h3.wide %>%
#   pivot_longer(2:10, names_to = "season", values_to = "H3mpd") %>%
#   arrange(season)
# 
# 
# 
# fit1 <- lm(h3.long$H3mpd~h3.temporal.lag1$H3mpd+h3.spatial.lag1$H3mpd.spatial.lag1)
# summary(fit1)
# 
# 
# 
# 
# xmat <- kronecker(diag(9), rep(1,49))[,-9]# %>% 
#   # as.data.frame() %>%
#   # setNames(., nm = unique(iqtree$season)[c(-1, -9)])
# 
# 
# fit2 <- lm(h3.long$H3mpd~h3.temporal.lag1$H3mpd+h3.spatial.lag1$H3mpd.spatial.lag1+xmat)
# summary(fit2)
# 
