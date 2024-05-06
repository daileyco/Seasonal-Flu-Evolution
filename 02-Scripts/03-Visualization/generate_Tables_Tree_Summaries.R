




# load("./01-Data/02-Analytic-Data/tree_summaries.rdata")
load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")

source("./02-Scripts/02-Helper-Functions/tabulator.R")


library(tidyr)
library(dplyr)
library(lubridate)
# library(flextable)


trees.full <- trees.full %>% 
  mutate(tmrca1 = decimal_date(as.Date(tMRCA))-season.num-1, 
         rate = as.numeric(rate))

table.full <- create.Table.1(c("ntips", "mpd", "imbalance.collessnorm", "avgladder", #"diameter", "cherries", 
                               "tmrca1", "rate"), 
                             "subtype", 
                             trees.full, 
                             the.methods = c("medians", "means", "means", "means", "medians", "means"))

# tapply(trees.full$tmrca1[which(trees.full$tmrca1>-10)], trees.full$subtype[which(trees.full$tmrca1>-10)], summary);
# tapply(trees.full$rate[which(trees.full$tmrca1>-5)], trees.full$subtype[which(trees.full$tmrca1>-5)], summary)

trees.full1520 <- trees.full %>% 
  mutate(tmrca1 = decimal_date(as.Date(tMRCA))-season.num-1, 
         rate = as.numeric(rate))%>%
  filter(season.num>2014)

table.full1520 <- create.Table.1(c("ntips", "mpd", "imbalance.collessnorm", "avgladder", #"diameter", "closeness", "cherries", 
                                   "tmrca1", "rate"), 
                             "subtype", 
                             trees.full1520, 
                             the.methods = c("medians", "means", "means", "means", "medians", "means"))





smalltrees.df <- smalltrees.df %>%
  mutate(tmrca1 = tmrca-season.num-1)

table.sub <- create.Table.1(c("ntips", "mpd", "imbalance.collessnorm", "avgladder", #"diameter", "closeness", "cherries", 
                              "tmrca1"), 
                             "subtype", 
                             smalltrees.df, 
                            the.methods = c("medians", "means", "means", "means", "medians"))


smalltrees.df1520 <- smalltrees.df %>%
  mutate(tmrca1 = tmrca-season.num-1) %>%
  filter(season.num>2014)

table.sub1520 <- create.Table.1(c("ntips", "mpd", "imbalance.collessnorm", "avgladder", #"diameter", "closeness", "cherries", 
                                  "tmrca1"), 
                            "subtype", 
                            smalltrees.df1520, 
                            the.methods = c("medians", "means", "means", "means", "medians"))





save(list = ls(pattern = "^table[.]"), 
     file = "./03-Output/01-Tables/tables_tree_summaries.rdata")




rm(list=ls())
gc()










# 
# # create.Table.1(c("ntips", "mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "subtype", 
# #                trees.full)[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_full.pptx")
# 
# 
# table.reps <- create.Table.1(c("ntips", "mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                              "subtype", 
#                              trees.reps)[-c(7,8,10)]
# 
# # create.Table.1(c("ntips", "mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "subtype", 
# #                trees.reps)[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_reps.pptx")
# 
# 
# table.simple <- create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                                "subtype", 
#                                trees.df)[-c(7,8,10)]
# 
# 
# # create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "subtype", 
# #                trees.df)[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple.pptx")
# 
# 
# table.simple.imputed <- create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                                        "subtype", 
#                                        trees.df.imputed)[-c(7,8,10)]
# 
# # create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "subtype", 
# #                trees.df.imputed)[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_imputed.pptx")
# 
# 
# 
# 
# table.bvic.byseason <- create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                                       "season", 
#                                       trees.df[which(trees.df$subtype=="BVic"),])[-c(7,8,10)]
# 
# table.byam.byseason <- create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                                       "season", 
#                                       trees.df[which(trees.df$subtype=="BYam"),])[-c(7,8,10)]
# 
# table.ah1.byseason <- create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                                       "season", 
#                                       trees.df[which(trees.df$subtype=="H1"),])[-c(7,8,10)]
# 
# table.ah3.byseason <- create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
#                                       "season", 
#                                       trees.df[which(trees.df$subtype=="H3"),])[-c(7,8,10)]
# 
# # create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "season", 
# #                trees.df[which(trees.df$subtype=="BVic"),])[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_BVicbyseason.pptx")
# # 
# # 
# # 
# # create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "season", 
# #                trees.df[which(trees.df$subtype=="BYam"),])[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_BYambyseason.pptx")
# # 
# # 
# # 
# # create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "season", 
# #                trees.df[which(trees.df$subtype=="H1"),])[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_H1byseason.pptx")
# # 
# # 
# # 
# # create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
# #                "season", 
# #                trees.df[which(trees.df$subtype=="H3"),])[-c(7,8,10)]%>%
# #   flextable()%>%
# #   autofit()%>%
# #   save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_H3byseason.pptx")
# 
# 
# 
























