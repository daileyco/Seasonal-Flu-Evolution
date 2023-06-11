

load("./01-Data/01-Processed-Data/iqtree_results.rds")


source("./02-Scripts/02-Helper-Functions/root_Tree.R")
source("./02-Scripts/02-Helper-Functions/summarize_Tree.R")


library(ape)
library(dplyr)

treesummaries <- lapply(iqtree$tree.newick, 
                        function(this.tree.text){
                          this.tree <- try(root_Tree(this.tree.text))
                          this.tree.summary <- try(summarize_Tree(this.tree))
                          return(this.tree.summary)
                          }
                        )%>%
  bind_rows()




iqtree <- cbind(iqtree, treesummaries)


save(iqtree, file = "./01-Data/02-Analytic-Data/tree_summaries.rds")


rm(list = ls())
gc()




