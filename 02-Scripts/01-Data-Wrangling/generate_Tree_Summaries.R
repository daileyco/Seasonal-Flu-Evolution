

load("./01-Data/01-Processed-Data/iqtree_results.rds")
load("./01-Data/01-Processed-Data/subtrees_df.rds")

source("./02-Scripts/02-Helper-Functions/root_Tree.R")
source("./02-Scripts/02-Helper-Functions/summarize_Tree.R")


library(ape)
library(dplyr)
# 
# treesummaries <- lapply(iqtree$tree.newick, 
#                         function(this.tree.text){
#                           this.tree <- try(root_Tree(this.tree.text))
#                           this.tree.summary <- try(summarize_Tree(this.tree))
#                           return(this.tree.summary)
#                           }
#                         )%>%
#   bind_rows()



library(foreach)
library(doParallel)

cl <- makeCluster(mc <- getOption("cl.cores", 19))

clusterExport(cl=cl, varlist=c("root_Tree", "summarize_Tree"))
clusterEvalQ(cl, {library(ape);library(dplyr)})

registerDoParallel(cl)

treesummaries.list <- parLapplyLB(cl,
                                iqtree$tree.newick,
                                function(this.tree.text){
                                  this.tree <- try(root_Tree(this.tree.text))
                                  this.tree.summary <- try(summarize_Tree(this.tree))
                                  return(this.tree.summary)
                                })

stopCluster(cl)


treesummaries <- bind_rows(treesummaries.list)





iqtree <- cbind(iqtree, treesummaries)


save(iqtree, file = "./01-Data/01-Processed-Data/tree_summaries.rds")


rm(list = ls())
gc()




