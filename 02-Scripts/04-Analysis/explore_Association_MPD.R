

load("./01-Data/02-Analytic-Data/tree_summaries_clean.rdata")



library(tidyr)
library(dplyr)

library(PerformanceAnalytics)



iqtree.wide <- trees.df.imputed%>%
  select(subtype, mpd, mpd_lag1, mpd_lag2, season, location)%>%
  pivot_wider(names_from = subtype, values_from = c(mpd, mpd_lag1, mpd_lag2))



# boxplot(iqtree$mpd~iqtree$subtype, ylab = "Mean Pairwise Distance", xlab = "Influenza Subtype")
# 
# boxplot(iqtree$mpd[which(iqtree$subtype=="H3")]~as.numeric(substr(iqtree$season[which(iqtree$subtype=="H3")],1,4)))
# boxplot(iqtree$mpd[which(iqtree$subtype=="H1")]~as.numeric(substr(iqtree$season[which(iqtree$subtype=="H1")],1,4)))
# boxplot(iqtree$mpd[which(iqtree$subtype=="BVic")]~as.numeric(substr(iqtree$season[which(iqtree$subtype=="BVic")],1,4)))
# boxplot(iqtree$mpd[which(iqtree$subtype=="BYam")]~as.numeric(substr(iqtree$season[which(iqtree$subtype=="BYam")],1,4)))
# 
# 
# boxplot(iqtree$imbalance.collessnorm~iqtree$subtype, ylab = "Colless Imbalance", xlab = "Influenza Subtype")
# boxplot(iqtree$avgladder~iqtree$subtype, ylab = "Colless Imbalance", xlab = "Influenza Subtype")
# boxplot(iqtree$stairs1~iqtree$subtype, ylab = "Colless Imbalance", xlab = "Influenza Subtype")
# boxplot(iqtree$stairs2~iqtree$subtype, ylab = "Colless Imbalance", xlab = "Influenza Subtype")




# ts0 <- iqtree %>% filter(rep==0)
# 
# 
# tapply(ts0, ts0$subtype, summary)
# 
# boxplot(ts0$mpd~ts0$ntips)
# 
# ts <- iqtree %>%
#   group_by(subtype, season, location) %>%
#   mutate(nreps=n()) %>%
#   ungroup() %>%
#   filter(!{rep==0&nreps>1})
# 
# boxplot(ts$mpd~ts$ntips)
# 
# ts1 <- ts %>% 
#   group_by(subtype, season, location) %>% 
#   summarise(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~mean(.x)))%>%
#   ungroup()
# 
# boxplot(ts1$mpd~ts1$ntips)
# 
# 
# iqtree.wide <- iqtree %>%
#   select(subtype, season, location, mpd, rep) %>%
#   pivot_wider(., names_from = "subtype", values_from = "mpd")%>%
#   group_by(season, location)%>%
#   mutate(nreps = n())%>%
#   ungroup()
# 
# 
# 
# iqtree.wide.full.trees <- iqtree.wide %>%
#   filter(rep==0)
# 
# 
# 
# 
# iqtree.wide <- iqtree.wide %>% 
#   filter(!{rep==0 & nreps>1})%>%
#   group_by(season, location) %>% 
#   summarise(across(c(H3, H1, BVic, BYam), ~mean(.x, na.rm = T)))%>%
#   ungroup()
# 
# 
# 
# hist(iqtree.wide$H3)
# hist(iqtree.wide$H1)
# hist(iqtree.wide$BVic)
# hist(iqtree.wide$BYam)


svg(filename = "./03-Output/02-Figures/correlation_matrix_stats.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(trees.df.imputed[5:10], histogram = TRUE, pch = 16)
dev.off()



svg(filename = "./03-Output/02-Figures/correlation_matrix_subtypes.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[3:6], histogram = TRUE, pch = 16)
dev.off()



svg(filename = "./03-Output/02-Figures/correlation_matrix_BVic_wlags.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[grep("BVic", names(iqtree.wide))], histogram = TRUE, pch = 16)
dev.off()


svg(filename = "./03-Output/02-Figures/correlation_matrix_BYam_wlags.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[grep("BYam", names(iqtree.wide))], histogram = TRUE, pch = 16)
dev.off()


svg(filename = "./03-Output/02-Figures/correlation_matrix_H1_wlags.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[grep("H1", names(iqtree.wide))], histogram = TRUE, pch = 16)
dev.off()


svg(filename = "./03-Output/02-Figures/correlation_matrix_H3_wlags.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[grep("H3", names(iqtree.wide))], histogram = TRUE, pch = 16)
dev.off()










svg(filename = "./03-Output/02-Figures/correlation_matrix_H3_wlagsubtypes.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[c(6:9, 11:13)], histogram = TRUE, pch = 16)
dev.off()




svg(filename = "./03-Output/02-Figures/correlation_matrix_H1_wlagsubtypes.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[c(5,7:8,10, 11:12,14)], histogram = TRUE, pch = 16)
dev.off()



svg(filename = "./03-Output/02-Figures/correlation_matrix_BYam_wlagsubtypes.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[c(4,7,9:11,13:14)], histogram = TRUE, pch = 16)
dev.off()


svg(filename = "./03-Output/02-Figures/correlation_matrix_BVic_wlagsubtypes.svg", height = 8, width = 8, pointsize = 10)
chart.Correlation(iqtree.wide[c(3,8:10,12:14)], histogram = TRUE, pch = 16)
dev.off()


# 
# cor(iqtree.wide[,c("H3", "H1", "BVic", "BYam")], use = "pairwise.complete.obs")
# 
# plot(iqtree.wide$H3, iqtree.wide$H1)
# plot(iqtree.wide$H3, iqtree.wide$BVic)
# plot(iqtree.wide$H3, iqtree.wide$BYam)
# 
# plot(iqtree.wide$H1, iqtree.wide$BVic)
# plot(iqtree.wide$H1, iqtree.wide$BYam)
# 
# plot(iqtree.wide$BVic, iqtree.wide$BYam)
# 
# 
# 
# 
# cor.test(iqtree.wide$H3, iqtree.wide$H1)
# cor.test(iqtree.wide$H3, iqtree.wide$BVic)
# cor.test(iqtree.wide$H3, iqtree.wide$BYam)
# 
# cor.test(iqtree.wide$H1, iqtree.wide$BVic)
# cor.test(iqtree.wide$H1, iqtree.wide$BYam)
# 
# cor.test(iqtree.wide$BVic, iqtree.wide$BYam)
# 
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
# plot(iqtree.wide$H3~iqtree.wide$H3.lag1)
# cor.test(iqtree.wide$H3, iqtree.wide$H3.lag1)
# 
# plot(iqtree.wide$H3~iqtree.wide$H3.lag2)
# cor.test(iqtree.wide$H3, iqtree.wide$H3.lag2)
# 
# 
# 
# 
# plot(iqtree.wide$H1~iqtree.wide$H1.lag1)
# cor.test(iqtree.wide$H1, iqtree.wide$H1.lag1)
# 
# plot(iqtree.wide$H1~iqtree.wide$H1.lag2)
# cor.test(iqtree.wide$H1, iqtree.wide$H1.lag2)
# 
# 
# 
# 
# plot(iqtree.wide$BVic~iqtree.wide$BVic.lag1)
# cor.test(iqtree.wide$BVic, iqtree.wide$BVic.lag1)
# 
# plot(iqtree.wide$BVic~iqtree.wide$BVic.lag2)
# cor.test(iqtree.wide$BVic, iqtree.wide$BVic.lag2)
# 
# 
# 
# 
# plot(iqtree.wide$BYam~iqtree.wide$BYam.lag1)
# cor.test(iqtree.wide$BYam, iqtree.wide$BYam.lag1)
# 
# plot(iqtree.wide$BYam~iqtree.wide$BYam.lag2)
# cor.test(iqtree.wide$BYam, iqtree.wide$BYam.lag2)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot(iqtree.wide$H3~iqtree.wide$H1.lag1)
# cor.test(iqtree.wide$H3, iqtree.wide$H1.lag1)
# 
# plot(iqtree.wide$H3~iqtree.wide$H1.lag2)
# cor.test(iqtree.wide$H3, iqtree.wide$H1.lag2)
# 
# 
# plot(iqtree.wide$H3~iqtree.wide$BVic.lag1)
# cor.test(iqtree.wide$H3, iqtree.wide$BVic.lag1)
# 
# plot(iqtree.wide$H3~iqtree.wide$BVic.lag2)
# cor.test(iqtree.wide$H3, iqtree.wide$BVic.lag2)
# 
# 
# plot(iqtree.wide$H3~iqtree.wide$BYam.lag1)
# cor.test(iqtree.wide$H3, iqtree.wide$BYam.lag1)
# 
# plot(iqtree.wide$H3~iqtree.wide$BYam.lag2)
# cor.test(iqtree.wide$H3, iqtree.wide$BYam.lag2)
# 
# 
# 
# 
# 
# 
# 
# plot(iqtree.wide$H1~iqtree.wide$BVic.lag1)
# cor.test(iqtree.wide$H1, iqtree.wide$BVic.lag1)
# 
# plot(iqtree.wide$H1~iqtree.wide$BVic.lag2)
# cor.test(iqtree.wide$H1, iqtree.wide$BVic.lag2)
# 
# 
# plot(iqtree.wide$H1~iqtree.wide$BYam.lag1)
# cor.test(iqtree.wide$H1, iqtree.wide$BYam.lag1)
# 
# plot(iqtree.wide$H1~iqtree.wide$BYam.lag2)
# cor.test(iqtree.wide$H1, iqtree.wide$BYam.lag2)
# 
# 
# 
# 
# 
# 
# plot(iqtree.wide$BVic~iqtree.wide$BYam.lag1)
# cor.test(iqtree.wide$BVic, iqtree.wide$BYam.lag1)
# 
# plot(iqtree.wide$BVic~iqtree.wide$BYam.lag2)
# cor.test(iqtree.wide$BVic, iqtree.wide$BYam.lag2)
# 
# 
# 
# 
# 
# 







