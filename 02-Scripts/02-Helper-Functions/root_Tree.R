



root_Tree <- function(this.tree.newick){
  
  require(dplyr)
  require(ape)
  
  
  this.tree <- read.tree(text=this.tree.newick)
  
  these.labels <- this.tree$tip.label
  
  these.labels.dates <- sapply(these.labels, 
                               function(this.label){
                                 this.sampling.date <- strsplit(this.label, split = "_")[[1]][4]
                               }) %>% 
    as.Date(., "%Y%m%d")
  
  this.tree.rooted <- rtt(this.tree, as.numeric(these.labels.dates))
  
  
  return(this.tree.rooted)
  
}


