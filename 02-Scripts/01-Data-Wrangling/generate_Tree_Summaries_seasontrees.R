

# load("./01-Data/01-Processed-Data/iqtree_results.rds")
load("./01-Data/01-Processed-Data/subtrees_df.rds")
seasontrees <- tree.df


load("./01-Data/01-Processed-Data/subtrees_mp_df.rds")
localtrees <- tree.df

# source("./02-Scripts/02-Helper-Functions/root_Tree.R")
source("./02-Scripts/02-Helper-Functions/summarize_Tree.R")


library(ape)
library(dplyr)



all.trees <- bind_rows(seasontrees %>% mutate(scope = ifelse(is.na(smalltree), "season", "cluster")), 
                       localtrees %>% filter(rep!=0) %>% mutate(scope = "local")) %>%
  mutate(tree = ifelse(is.na(smalltree), tree.newick2, smalltree))






library(foreach)
library(doParallel)

cl <- makeCluster(mc <- getOption("cl.cores", 19))

# clusterExport(cl=cl, varlist=c("root_Tree", "summarize_Tree"))
clusterExport(cl=cl, varlist=c("all.trees", "summarize_Tree"))
clusterEvalQ(cl, {library(ape);library(dplyr)})

registerDoParallel(cl)

treesummaries.list <- parLapplyLB(cl,
                                # iqtree$tree.newick,
                                1:nrow(all.trees),
                                # iqtree$tree.newick.timescaled,
                                function(this.index){
                                  # this.tree.text <- iqtree$tree.newick.timescaled[this.index]
                                  this.tree.text <- all.trees$tree[this.index]
                                  # this.tree <- try(root_Tree(this.tree.text))
                                  this.tree <- try(read.tree(text = this.tree.text))
                                  this.tree.summary <- try(summarize_Tree(this.tree))
                                  
                                  if(class(this.tree.summary)=="try-error"){
                                    this.output <- NULL
                                  }else{
                                    
                                    this.output <- cbind(all.trees[this.index,which(names(all.trees)%in%c("subtype", "season", "n", "rep", "scope", "tree"))], 
                                                       this.tree.summary)
                                  }
                                  
                                  return(this.output)
                                })

stopCluster(cl)


treesummaries <- bind_rows(treesummaries.list)


smalltrees <- full_join(treesummaries, 
                        all.trees, 
                        by = c("subtype", "season", "n", "rep", "scope", "tree"))


save(smalltrees, file = "./01-Data/01-Processed-Data/smalltrees_summaries.rds")


rm(list = ls())
gc()




