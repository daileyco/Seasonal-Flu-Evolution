


# load("./01-Data/01-Processed-Data/tree_summaries.rds")
load("./01-Data/01-Processed-Data/smalltrees_summaries.rds")




library(dplyr)


# trees.full <- iqtree %>% filter(rep==0)
trees.full <- smalltrees %>% filter(rep==0)



trees.season <- smalltrees %>%
  filter(!is.na(smalltree))



combos <- expand.grid(season = unique(smalltrees$season), 
                      location = unique(smalltrees$location), 
                      subtype = unique(smalltrees$subtype))




trees.full <- trees.full %>% 
  full_join(combos, by = c("season", "location", "subtype")) %>%
  mutate(season.num = as.numeric(substr(season, 1, 4))) %>% 
  arrange(subtype, location, season.num)


smalltrees.df <- trees.season %>% 
  full_join(combos, by = c("season", "location", "subtype")) %>%
  mutate(season.num = as.numeric(substr(season, 1, 4))) %>% 
  arrange(subtype, location, season.num)




save(smalltrees.df, trees.full, file = "./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")


rm(list=ls())
gc()








# tapply(ts0, ts0$subtype, summary)

# boxplot(trees.full$mpd~trees.full$ntips)

# trees.reps <- iqtree %>%
#   group_by(subtype, season, location) %>%
#   mutate(nreps=n()) %>%
#   ungroup() %>%
#   filter(!{rep==0&nreps>1})

# boxplot(trees.reps$mpd~trees.reps$ntips)

# trees.simple <- trees.reps %>% 
#   group_by(subtype, season, location) %>% 
#   summarise(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~mean(.x)), 
#             ntips = round(mean(ntips)), 
#             nreps = n(),
#             across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~paste0(.x, collapse=", "), .names = "{.col}_all"))%>%
#   ungroup()

# boxplot(trees.simple$mpd~trees.simple$ntips)

# trees.nested <- trees.reps %>% nest(., .by = c(subtype, season, location))


# trees.nested <- trees.nested %>% 
#   mutate(means = map(data, 
#                      function(this.df){
#                        this.df%>%
#                          select(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder", "ntips"))%>%
#                          summarise(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                                           ~mean(.x, na.rm = T)), 
#                                    nreps = n(), 
#                                    ntips.mean = round(mean(ntips, na.rm = T)), 
#                                    missing.flag = sum(is.na(ntips)))
#                        })
#          ) %>%
#   unnest(means)




# test <- trees.season %>% 
#   group_by(subtype, season, location) %>% 
#   summarise(mpdmean = mean(mpd), mpdsd = sd(mpd), n = n()) %>% 
#   ungroup()
# 
# plot(mpdmean ~ as.numeric(substr(season,1,4)), 
#      data = test %>% 
#        group_by(subtype, season) %>% 
#        summarise(mpdmean = mean(mpdmean)) %>% 
#        ungroup(), 
#      col = c("red", "purple", "blue", "darkgreen")[match(subtype, c("H3", "H1", "BVic", "BYam"))], 
#      pch = c(15:18)[match(subtype, c("H3", "H1", "BVic", "BYam"))])
# 
# sapply(c("H3", "H1", "BVic", "BYam"), 
#        (\(st){
#          
#          
#          lines(mpdmean ~ as.numeric(substr(season,1,4)), 
#                data = test %>% 
#                  group_by(subtype, season) %>% 
#                  summarise(mpdmean = mean(mpdmean)) %>% 
#                  ungroup() %>% 
#                  filter(subtype%in%c(st)), 
#                col = c("red", "purple", "blue", "darkgreen")[match(subtype, c("H3", "H1", "BVic", "BYam"))])
#          
#          
#        }))
# 
# boxplot(mpdmean ~ season + subtype, data = test)



# combos <- expand.grid(season = unique(iqtree$season), 
#                       location = unique(iqtree$location), 
#                       subtype = unique(iqtree$subtype))




# trees.full <- trees.full %>%
#   group_by(subtype, location) %>%
#   mutate(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,1), .names = "{.col}_lag1"), 
#          across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,2), .names = "{.col}_lag2")) %>%
#   ungroup()




# trees.reps <- trees.reps %>% 
#   full_join(combos, by = c("season", "location", "subtype")) %>%
#   mutate(season.num = as.numeric(substr(season, 1, 4))) %>% 
#   arrange(subtype, location, season.num)

# trees.reps <- trees.reps %>%
#   group_by(subtype, location) %>%
#   mutate(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,1), .names = "{.col}_lag1"), 
#          across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,2), .names = "{.col}_lag2")) %>%
#   ungroup()




# trees.df <- trees.nested %>% 
#   full_join(combos, by = c("season", "location", "subtype")) %>%
#   mutate(season.num = as.numeric(substr(season, 1, 4))) %>% 
#   arrange(subtype, location, season.num)





# missingness
# # smalltrees.df %>% 
# #   group_by(subtype, season, location) %>% 
# #   summarise(mpd = mean(mpd)) %>% 
# #   ungroup() %>% 
# #   group_by(subtype, season) %>% 
# #   summarise(n = n(), 
# #             nmiss = sum(is.na(mpd)), 
# #             pmiss=round(nmiss/n*100, 2)) %>% 
# #   ungroup() %>% 
# #   View()




# trees.df <- trees.df %>%
#   group_by(subtype, location) %>%
#   mutate(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,1), .names = "{.col}_lag1"), 
#          across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~lag(.x,2), .names = "{.col}_lag2")) %>%
#   ungroup()




# trees.df.imputed <- trees.nested %>% 
#   full_join(combos, by = c("season", "location", "subtype")) %>%
#   mutate(season.num = as.numeric(substr(season, 1, 4))) %>% 
#   arrange(subtype, location, season.num) %>% 
#   group_by(subtype, season) %>%
#   mutate(across(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), ~ifelse(is.na(.x) & subtype=="H3", median(.x, na.rm = TRUE), .x))) %>%
#   ungroup()


# save(trees.df, trees.reps, trees.full, trees.df.imputed, file = "./01-Data/02-Analytic-Data/tree_summaries.rdata")



