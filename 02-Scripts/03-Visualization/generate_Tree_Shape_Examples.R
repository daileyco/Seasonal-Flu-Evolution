

load("./01-Data/02-Analytic-Data/tree_summaries_clean.rdata")

source("./02-Scripts/02-Helper-Functions/root_Tree.R")

library(tidyr)
library(dplyr)
library(ape)


svg(filename = "./03-Output/02-Figures/mpd_ex.svg", width = 9, height = 4.5, pointsize = 10)
par(mfrow = c(1,2))
plot(root_Tree(trees.reps$tree.newick[which.max(trees.reps$mpd)]), 
     main = paste0(paste0(unlist(trees.reps[which.max(trees.reps$mpd), c("subtype", "season")]), collapse = ", "), "\nMax Mean Pairwise Distance = ", "\n", round(trees.reps$mpd[which.max(trees.reps$mpd)],4)), 
     show.tip.label = TRUE, 
     tip.color = "red", 
     cex = 0.75)
plot(root_Tree(trees.reps$tree.newick[which.min(trees.reps$mpd)]), 
     main = paste0(paste0(unlist(trees.reps[which.min(trees.reps$mpd), c("subtype", "season")]), collapse = ", "), "\nMin Mean Pairwise Distance = ", "\n", trees.reps$mpd[which.min(trees.reps$mpd)]), 
     x.lim = max(trees.reps$mpd, na.rm = T), 
     show.tip.label = TRUE, 
     tip.color = "blue", 
     cex = 0.75)
dev.off()



svg(filename = "./03-Output/02-Figures/closeness_ex.svg", width = 9, height = 4.5, pointsize = 10)
par(mfrow = c(1,2))
plot(root_Tree(trees.reps$tree.newick[which.max(trees.reps$closeness)]), 
     main = paste0(paste0(unlist(trees.reps[which.max(trees.reps$closeness), c("subtype", "season")]), collapse = ", "), "\nMax Closeness = ", "\n", round(trees.reps$closeness[which.max(trees.reps$closeness)],4)), 
     show.tip.label = TRUE, 
     tip.color = "red", 
     cex = 0.75)
plot(root_Tree(trees.reps$tree.newick[which.min(trees.reps$closeness)]), 
     main = paste0(paste0(unlist(trees.reps[which.min(trees.reps$closeness), c("subtype", "season")]), collapse = ", "), "\nMin Closeness = ", "\n", trees.reps$closeness[which.min(trees.reps$closeness)]), 
     x.lim = max(trees.reps$closeness, na.rm = T), 
     show.tip.label = TRUE, 
     tip.color = "blue", 
     cex = 0.75)
dev.off()






svg(filename = "./03-Output/02-Figures/colless_ex.svg", width = 9, height = 4.5, pointsize = 10)
par(mfrow = c(1,2))
plot(root_Tree(trees.reps$tree.newick[which.max(trees.reps$colless)]), 
     main = paste0(paste0(unlist(trees.reps[which.max(trees.reps$colless), c("subtype", "season")]), collapse = ", "), "\nMax Colless Imbalance = ", "\n", round(trees.reps$colless[which.max(trees.reps$colless)],4)), 
     show.tip.label = TRUE, 
     tip.color = "red", 
     cex = 0.75)
plot(root_Tree(trees.reps$tree.newick[which.min(trees.reps$colless)]), 
     main = paste0(paste0(unlist(trees.reps[which.min(trees.reps$colless), c("subtype", "season")]), collapse = ", "), "\nMin Colless Imbalance = ", "\n", trees.reps$colless[which.min(trees.reps$colless)]), 
     x.lim = max(trees.reps$colless, na.rm = T), 
     show.tip.label = TRUE, 
     tip.color = "blue", 
     cex = 0.75)
dev.off()







svg(filename = "./03-Output/02-Figures/stairs1_ex.svg", width = 9, height = 4.5, pointsize = 10)
par(mfrow = c(1,2))
plot(root_Tree(trees.reps$tree.newick[which.max(trees.reps$stairs1)]), 
     main = paste0(paste0(unlist(trees.reps[which.max(trees.reps$stairs1), c("subtype", "season")]), collapse = ", "), "\nMax stairs1 = ", "\n", round(trees.reps$stairs1[which.max(trees.reps$stairs1)],4)), 
     show.tip.label = TRUE, 
     tip.color = "red", 
     cex = 0.75)
plot(root_Tree(trees.reps$tree.newick[which.min(trees.reps$stairs1)]), 
     main = paste0(paste0(unlist(trees.reps[which.min(trees.reps$stairs1), c("subtype", "season")]), collapse = ", "), "\nMin stairs1 = ", "\n", trees.reps$stairs1[which.min(trees.reps$stairs1)]), 
     x.lim = max(trees.reps$stairs1, na.rm = T), 
     show.tip.label = TRUE, 
     tip.color = "blue", 
     cex = 0.75)
dev.off()





svg(filename = "./03-Output/02-Figures/stairs2_ex.svg", width = 9, height = 4.5, pointsize = 10)
par(mfrow = c(1,2))
plot(root_Tree(trees.reps$tree.newick[which.max(trees.reps$stairs2)]), 
     main = paste0(paste0(unlist(trees.reps[which.max(trees.reps$stairs2), c("subtype", "season")]), collapse = ", "), "\nMax stairs2 = ", "\n", round(trees.reps$stairs2[which.max(trees.reps$stairs2)],4)), 
     show.tip.label = TRUE, 
     tip.color = "red", 
     cex = 0.75)
plot(root_Tree(trees.reps$tree.newick[which.min(trees.reps$stairs2)]), 
     main = paste0(paste0(unlist(trees.reps[which.min(trees.reps$stairs2), c("subtype", "season")]), collapse = ", "), "\nMin stairs2 = ", "\n", trees.reps$stairs2[which.min(trees.reps$stairs2)]), 
     x.lim = max(trees.reps$stairs2, na.rm = T), 
     show.tip.label = TRUE, 
     tip.color = "blue", 
     cex = 0.75)
dev.off()






svg(filename = "./03-Output/02-Figures/avgladder_ex.svg", width = 9, height = 4.5, pointsize = 10)
par(mfrow = c(1,2))
plot(root_Tree(trees.reps$tree.newick[which.max(trees.reps$avgladder)]), 
     main = paste0(paste0(unlist(trees.reps[which.max(trees.reps$avgladder), c("subtype", "season")]), collapse = ", "), "\nMax avgladder = ", "\n", round(trees.reps$avgladder[which.max(trees.reps$avgladder)],4)), 
     show.tip.label = TRUE, 
     tip.color = "red", 
     cex = 0.75)
plot(root_Tree(trees.reps$tree.newick[which.min(trees.reps$avgladder)]), 
     main = paste0(paste0(unlist(trees.reps[which.min(trees.reps$avgladder), c("subtype", "season")]), collapse = ", "), "\nMin avgladder = ", "\n", trees.reps$avgladder[which.min(trees.reps$avgladder)]), 
     x.lim = max(trees.reps$avgladder, na.rm = T), 
     show.tip.label = TRUE, 
     tip.color = "blue", 
     cex = 0.75)
dev.off()



rm(list=ls())
gc()











