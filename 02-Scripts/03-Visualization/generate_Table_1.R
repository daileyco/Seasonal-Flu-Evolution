




load("./01-Data/02-Analytic-Data/tree_summaries_clean.rdata")

source("./02-Scripts/02-Helper-Functions/tabulator.R")


library(tidyr)
library(dplyr)
library(flextable)




create.Table.1(c("ntips", "mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "subtype", 
               trees.full)[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_full.pptx")




create.Table.1(c("ntips", "mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "subtype", 
               trees.reps)[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_reps.pptx")



create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "subtype", 
               trees.df)[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple.pptx")


create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "subtype", 
               trees.df.imputed)[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_imputed.pptx")




create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "season", 
               trees.df[which(trees.df$subtype=="BVic"),])[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_BVicbyseason.pptx")



create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "season", 
               trees.df[which(trees.df$subtype=="BYam"),])[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_BYambyseason.pptx")



create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "season", 
               trees.df[which(trees.df$subtype=="H1"),])[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_H1byseason.pptx")



create.Table.1(c("mpd", "closeness", "colless", "stairs1", "stairs2", "avgladder"), 
               "season", 
               trees.df[which(trees.df$subtype=="H3"),])[-c(7,8,10)]%>%
  flextable()%>%
  autofit()%>%
  save_as_pptx(path = "./03-Output/01-Tables/tree_summaries_simple_H3byseason.pptx")





rm(list=ls())
gc()
























