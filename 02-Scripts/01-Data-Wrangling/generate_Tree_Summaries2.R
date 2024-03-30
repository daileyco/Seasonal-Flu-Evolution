

# load("./01-Data/01-Processed-Data/iqtree_results.rds")
load("./01-Data/01-Processed-Data/subtrees_df.rds")
iqtree <- tree.df
# source("./02-Scripts/02-Helper-Functions/root_Tree.R")
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

# clusterExport(cl=cl, varlist=c("root_Tree", "summarize_Tree"))
clusterExport(cl=cl, varlist=c("iqtree", "summarize_Tree"))
clusterEvalQ(cl, {library(ape);library(dplyr)})

registerDoParallel(cl)

treesummaries.list <- parLapplyLB(cl,
                                # iqtree$tree.newick,
                                1:nrow(iqtree),
                                # iqtree$tree.newick.timescaled,
                                function(this.index){
                                  this.tree.text <- iqtree$tree.newick.timescaled[this.index]
                                  # this.tree <- try(root_Tree(this.tree.text))
                                  this.tree <- try(read.tree(text = this.tree.text))
                                  this.tree.summary <- try(summarize_Tree(this.tree))
                                  
                                  this.output <- cbind(iqtree[this.index,which(names(iqtree)%in%c("subtype", "season", "location", "n", "rep", "tree.newick.timescaled"))], 
                                                       this.tree.summary)
                                  return(this.output)
                                })

stopCluster(cl)


treesummaries <- bind_rows(treesummaries.list)


smalltrees <- full_join(treesummaries, 
                        tree.df, 
                        by = c("subtype", "season", "location", "n", "rep", "tree.newick.timescaled"))


# iqtree <- cbind(iqtree, treesummaries)

save(smalltrees, file = "./01-Data/01-Processed-Data/smalltrees_summaries.rds")
# save(iqtree, file = "./01-Data/01-Processed-Data/tree_summaries2.rds")


rm(list = ls())
gc()




