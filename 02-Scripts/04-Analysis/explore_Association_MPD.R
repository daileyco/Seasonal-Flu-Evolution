

load("./01-Data/02-Analytic-Data/tree_summaries.rds")



library(tidyr)
library(dplyr)




boxplot(iqtree$mpd~iqtree$subtype)




iqtree.wide <- iqtree %>%
  select(subtype, season, location, mpd, rep) %>%
  pivot_wider(., names_from = "subtype", values_from = "mpd")

iqtree.wide <- iqtree.wide %>% 
  group_by(season, location) %>% 
  summarise(across(c(H3, H1, BVic, BYam), ~mean(.x, na.rm = T)))%>%
  ungroup()



hist(iqtree.wide$H3)
hist(iqtree.wide$H1)
hist(iqtree.wide$BVic)
hist(iqtree.wide$BYam)





cor(iqtree.wide[,c("H3", "H1", "BVic", "BYam")], use = "pairwise.complete.obs")

plot(iqtree.wide$H3, iqtree.wide$H1)
plot(iqtree.wide$H3, iqtree.wide$BVic)
plot(iqtree.wide$H3, iqtree.wide$BYam)

plot(iqtree.wide$H1, iqtree.wide$BVic)
plot(iqtree.wide$H1, iqtree.wide$BYam)

plot(iqtree.wide$BVic, iqtree.wide$BYam)




cor.test(iqtree.wide$H3, iqtree.wide$H1)
cor.test(iqtree.wide$H3, iqtree.wide$BVic)
cor.test(iqtree.wide$H3, iqtree.wide$BYam)

cor.test(iqtree.wide$H1, iqtree.wide$BVic)
cor.test(iqtree.wide$H1, iqtree.wide$BYam)

cor.test(iqtree.wide$BVic, iqtree.wide$BYam)




iqtree.wide <- iqtree.wide %>%
  mutate(season.num = as.numeric(substr(season, 1, 4))) %>%
  arrange(location, season.num)%>%
  group_by(location)%>%
  mutate(H3.lag1 = lag(H3, 1),
         H3.lag2 = lag(H3, 2),
         H1.lag1 = lag(H1, 1),
         H1.lag2 = lag(H1, 2),
         BVic.lag1 = lag(BVic, 1),
         BVic.lag2 = lag(BVic, 2),
         BYam.lag1 = lag(BYam, 1), 
         BYam.lag2 = lag(BYam, 2))%>%
  ungroup()


plot(iqtree.wide$H3~iqtree.wide$H3.lag1)
cor.test(iqtree.wide$H3, iqtree.wide$H3.lag1)

plot(iqtree.wide$H3~iqtree.wide$H3.lag2)
cor.test(iqtree.wide$H3, iqtree.wide$H3.lag2)




plot(iqtree.wide$H1~iqtree.wide$H1.lag1)
cor.test(iqtree.wide$H1, iqtree.wide$H1.lag1)

plot(iqtree.wide$H1~iqtree.wide$H1.lag2)
cor.test(iqtree.wide$H1, iqtree.wide$H1.lag2)




plot(iqtree.wide$BVic~iqtree.wide$BVic.lag1)
cor.test(iqtree.wide$BVic, iqtree.wide$BVic.lag1)

plot(iqtree.wide$BVic~iqtree.wide$BVic.lag2)
cor.test(iqtree.wide$BVic, iqtree.wide$BVic.lag2)




plot(iqtree.wide$BYam~iqtree.wide$BYam.lag1)
cor.test(iqtree.wide$BYam, iqtree.wide$BYam.lag1)

plot(iqtree.wide$BYam~iqtree.wide$BYam.lag2)
cor.test(iqtree.wide$BYam, iqtree.wide$BYam.lag2)









plot(iqtree.wide$H3~iqtree.wide$H1.lag1)
cor.test(iqtree.wide$H3, iqtree.wide$H1.lag1)

plot(iqtree.wide$H3~iqtree.wide$H1.lag2)
cor.test(iqtree.wide$H3, iqtree.wide$H1.lag2)


plot(iqtree.wide$H3~iqtree.wide$BVic.lag1)
cor.test(iqtree.wide$H3, iqtree.wide$BVic.lag1)

plot(iqtree.wide$H3~iqtree.wide$BVic.lag2)
cor.test(iqtree.wide$H3, iqtree.wide$BVic.lag2)


plot(iqtree.wide$H3~iqtree.wide$BYam.lag1)
cor.test(iqtree.wide$H3, iqtree.wide$BYam.lag1)

plot(iqtree.wide$H3~iqtree.wide$BYam.lag2)
cor.test(iqtree.wide$H3, iqtree.wide$BYam.lag2)







plot(iqtree.wide$H1~iqtree.wide$BVic.lag1)
cor.test(iqtree.wide$H1, iqtree.wide$BVic.lag1)

plot(iqtree.wide$H1~iqtree.wide$BVic.lag2)
cor.test(iqtree.wide$H1, iqtree.wide$BVic.lag2)


plot(iqtree.wide$H1~iqtree.wide$BYam.lag1)
cor.test(iqtree.wide$H1, iqtree.wide$BYam.lag1)

plot(iqtree.wide$H1~iqtree.wide$BYam.lag2)
cor.test(iqtree.wide$H1, iqtree.wide$BYam.lag2)






plot(iqtree.wide$BVic~iqtree.wide$BYam.lag1)
cor.test(iqtree.wide$BVic, iqtree.wide$BYam.lag1)

plot(iqtree.wide$BVic~iqtree.wide$BYam.lag2)
cor.test(iqtree.wide$BVic, iqtree.wide$BYam.lag2)













