# script to partition full phylogenetic trees based on estimated date of internal nodes to identify subtrees whose mrca falls within collection season

## load data
load("./01-Data/01-Processed-Data/iqtree_results.rds")


## helper function
# source("./02-Scripts/02-Helper-Functions/root_Tree.R")


## packages
library(ape)
library(dplyr)
library(tidyr)
library(lubridate)
library(treedater)




## create dataframe lookup table with dates specifying the start and end of each influenza season, 
### start = epiweek 30 and end = following year epiweek 18
seasonsdf <- data.frame(date = seq(as.Date("2010-01-01"), 
                                   as.Date("2020-12-31"), 
                                   by = "day")) %>% 
  mutate(epiweek = epiweek(date), 
         year = year(date)) %>% 
  mutate(season = ifelse(epiweek<30, 
                         paste0(year-1, "-", year), 
                         paste0(year, "-", year+1))) %>% 
  filter(epiweek%in%c(18,30)) %>% 
  group_by(epiweek, season) %>% 
  mutate(keep = ifelse(date==max(date) & epiweek==18, 
                       1, 
                       ifelse(date==min(date) & epiweek == 30, 
                              1, 
                              0))) %>% 
  ungroup() %>% 
  filter(keep==1) %>% 
  select(-keep, -year) %>% 
  mutate(epiweek = ifelse(epiweek==18, 
                          "end", 
                          "start")) %>% 
  filter(!season%in%c("2009-2010", "2020-2021")) %>% 
  pivot_wider(names_from = epiweek, 
              values_from = date)



## subset data to include only full trees
iqtree.full <- iqtree %>% 
  filter(rep == 0)




## set up list to hold output
tree.list <- list()




## loop to iterate through all full trees

for(i in 1:nrow(iqtree.full)){
  
  ##track progress
  cat("\ni = ", i, " (", round(i/nrow(iqtree.full)*100, 2), "%)\n")
  
  
  ### isolate single row in df for single tree
  this.row <- iqtree.full[i,]
  
  ### extract data into objects
  #### season
  this.season <- this.row$season
  #### seq length
  this.seqlength <- this.row$input.sites.per.seq
  
  
  #### read tree text from df
  this.tree <- read.tree(text = this.row$tree.newick)
  
  
  
  
  
  
  
  #### convert tip labels to dates
  these.labels <- this.tree$tip.label
  
  these.labels.dates <- sapply(these.labels, 
                               function(this.label){
                                 this.sampling.date <- strsplit(this.label, split = "_")[[1]][4]
                               }) %>% 
    as.Date(., "%Y%m%d")
  
  if(length(unique(these.labels.dates))==1){
    
    tree.list[[i]] <- this.row %>% mutate(errorts = "All sample dates identical.")
    next
    
  }
  
  
  ### time scale phylogeny (& root)
  #### may want to revisit to provide initial estimates for mutation rate, uses strict clock
  this.tree.scaled <- try(dater(tre = this.tree, 
                                sts = decimal_date(these.labels.dates), 
                                s = this.seqlength
                                ,omega0 = c(2e-4, 13e-4, 2e-3)
                                # ,numStartConditions = 0 
                                # ,clock = "uncorrelated"
                                , meanRateLimits = c(1e-4, 3e-3)
                                # , quiet = FALSE
                                
  ))
  
  # # Nobusawa & Sato (2006) estimated rates
  # # # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1440390/
  # # a c(1.8e-3, 2.2e-3)
  # # b c(0.45e-3, 0.8e-3)
  # {
  #   plot(c(1e-4, 3e-3), c(1,1));
  #   points(c(1.8e-3, 2.2e-3), c(1,1), col = "red", pch = "A");
  #   points(c(0.45e-3, 0.8e-3), c(1,1), col = "blue", pch = "B");
  #   points(c(1.3e-3), c(1), col = "green", pch = 16, cex = 2);
  # }
  
  if("try-error"%in%class(this.tree.scaled)){
    
    tree.list[[i]] <- this.row %>% mutate(errorts = as.character(attr(this.tree.scaled, "condition")))
    next
    
  }
  
  
  this.row$tree.newick.timescaled <- write.tree(this.tree.scaled, file = "")
  
  this.row$season.start <- decimal_date(seasonsdf$start[match(this.season, seasonsdf$season)])
  this.row$season.end <- decimal_date(seasonsdf$end[match(this.season, seasonsdf$season)])
  
  this.row$tmrca <- this.tree.scaled$timeOfMRCA
  this.row$mean.rate <- this.tree.scaled$mean.rate
  this.row$adjusted.mean.rate <- this.tree.scaled$adjusted.mean.rate
  
  
  ### add node labels
  #### begin index at Ntips +1
  this.tree.scaled$node.label <- (1:(this.tree.scaled$Nnode))+Ntip(this.tree.scaled)
  
  
  ### extract estimated times for each node
  these.node.times <- this.tree.scaled$Ti
  
  ### determine whether each internal node date estimate falls within the given influenza season
  node.in.season <- these.node.times>=decimal_date(seasonsdf$start[match(this.season, seasonsdf$season)]) & 
                    these.node.times<=decimal_date(seasonsdf$end[match(this.season, seasonsdf$season)])
  
  
  
  # #diagnostics
  # plot(this.tree)
  # plot(this.tree.scaled)
  
  # nodelabels(text = this.tree.scaled$Ti, node = (1:this.tree.scaled$Nnode)+Ntip(this.tree.scaled), xpd = TRUE)
  
  # nodelabels(text = (1:this.tree.scaled$Nnode)+Ntip(this.tree.scaled), node = (1:this.tree.scaled$Nnode)+Ntip(this.tree.scaled), xpd = TRUE)
  
  # nodelabels(text = node.in.season, node = (1:this.tree.scaled$Nnode)+Ntip(this.tree.scaled), xpd = TRUE)
  
  
  if(length(node.in.season)==sum(node.in.season) | sum(node.in.season)==0){
    
    
    tree.list[[i]] <- this.row
    
    
  }else{
    
    

    these.subtrees <- subtrees(this.tree.scaled)[which(node.in.season)]
    
    
    if(length(these.subtrees)==1){
      
      trees.to.keep <- 1
      
    }else{
      
      
      # scaffold <- combn(1:3, 2) %>% t() %>% as.data.frame()
      
      scaffold <- expand.grid(n = 1:length(these.subtrees), 
                              m = 1:length(these.subtrees), 
                              is.nested = NA) %>%
        filter(n!=m)
      
      
      for(j in 1:nrow(scaffold)){
        
        scaffold$is.nested[j] <- all(these.subtrees[[scaffold$n[j]]]$tip.label %in% these.subtrees[[scaffold$m[j]]]$tip.label)
        
      }
      
      trees.to.keep <- scaffold %>% 
        group_by(n) %>%
        mutate(is.nested = sum(is.nested)>0) %>%
        ungroup() %>%
        filter(is.nested==FALSE) %>%
        select(n) %>%
        unlist() %>%
        unique() %>%
        .[order(.)]
      
    }
    
    
    
    this.row.plus <- this.row
    
    for(k in 1:length(trees.to.keep)){
      
      this.row.plus <- bind_rows(this.row.plus,
                                 cbind(this.row.plus[1,c(1:3, 21:22)], 
                                       data.frame(rep = as.numeric(paste0(0, ".", k)), 
                                                  tree.newick.timescaled = write.tree(these.subtrees[trees.to.keep[k]], file = ""), 
                                                  n = Ntip(these.subtrees[[trees.to.keep[k]]]), 
                                                  tmrca = these.node.times[node.in.season][trees.to.keep[k]])))
      
      
    }
    
    tree.list[[i]] <- this.row.plus
    
  }
  
}



##combine all full trees and their chosen subtrees from list into a df
tree.df <- bind_rows(tree.list) %>%
  mutate(in.season = season.start <= tmrca & season.end >= tmrca)


##save
save(tree.df, file = "./01-Data/01-Processed-Data/subtrees_df.rds")


##clean environment
rm(list = ls())
gc()















# 
# 
# ### identify oldest within-season internal node
# first.node.in.season <- match(these.node.times[node.in.season][which.min(these.node.times[node.in.season])], 
#                               these.node.times)
# 
# # 
# # 
# # ### partition tree for first node in season
# # this.subtree <- extract.clade(this.tree.scaled, first.node.in.season+Ntip(this.tree))
# # 
# # this.subtree2 <- drop.tip(this.tree.scaled, tip = phangorn::Children(this.tree.scaled, first.node.in.season))
# #         
# #         #### convert tip labels to dates
# #         these.labels <- this.subtree$tip.label
# #         
# #         these.labels.dates <- sapply(these.labels, 
# #                                      function(this.label){
# #                                        this.sampling.date <- strsplit(this.label, split = "_")[[1]][4]
# #                                      }) %>% 
# #           as.Date(., "%Y%m%d")
# #         
# #         
# #         ### time scale phylogeny (& root)
# #         #### may want to revisit to provide initial estimates for mutation rate, uses strict clock
# #         this.subtree.scaled <- dater(tre = this.subtree, 
# #                                   sts = decimal_date(these.labels.dates), 
# #                                   s = this.seqlength
# #                                   ,omega0 = c(10e-4)
# #                                   ,numStartConditions = 0 
# #                                   # ,clock = "uncorrelated"
# #         )
# #         
# #         
# #         ### add node labels
# #         #### begin index at Ntips +1
# #         this.tree.scaled$node.label <- (1:(this.tree.scaled$Nnode))+Ntip(this.tree.scaled)
# #         
# #         
# #         ### extract estimated times for each node
# #         these.node.times <- this.tree.scaled$Ti
# #         
# #         ### determine whether each internal node date estimate falls within the given influenza season
# #         node.in.season <- these.node.times>=decimal_date(seasonsdf$start[match(this.season, seasonsdf$season)]) & 
# #           these.node.times<=decimal_date(seasonsdf$end[match(this.season, seasonsdf$season)])
# #         
# #         
# #         
# #         
# #         
# #         
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # #### remove tips in subtree from original tree
# # this.tree <- drop.tip(this.tree.scaled, this.subtree$tip.label)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# temp <- iqtree[1,]
# this.season <- temp$season
# 
# 
# this.tree <- read.tree(text = temp$tree.newick)
# this.tree$node.label <- (1:(this.tree$Nnode))+Ntip(this.tree)
# # this.tree <- root_Tree(temp$tree.newick)
# plot(this.tree, show.node.label = TRUE)
# # this.tree <- makeNodeLabel(this.tree, prefix = "")
# 
# these.labels <- this.tree$tip.label
# 
# these.labels.dates <- sapply(these.labels, 
#                              function(this.label){
#                                this.sampling.date <- strsplit(this.label, split = "_")[[1]][4]
#                              }) %>% 
#   as.Date(., "%Y%m%d")
# 
# this.tree <- dater(tre = this.tree, 
#                    sts = decimal_date(these.labels.dates), 
#                    s = temp$input.sites.per.seq)
# this.tree$node.label <- (1:(this.tree$Nnode))+Ntip(this.tree)
# 
# 
# # this.tree <- makeNodeLabel(this.tree, prefix = "")
# plot(this.tree, show.node.label = TRUE)
# nodelabels(text = this.tree$Ti, node = this.tree$node.label, xpd = TRUE)
# 
# 
# these.node.times <- this.tree$Ti
# 
# node.in.season <- these.node.times>=decimal_date(seasonsdf$start[match(this.season, seasonsdf$season)]) & these.node.times<=decimal_date(seasonsdf$end[match(this.season, seasonsdf$season)])
# 
# 
# 
# 
# # the.ace <- ace(as.numeric(these.labels.dates), this.tree)
# 
# 
# 
# # node.in.season <- as.Date(the.ace$ace)>=seasonsdf$start[match(this.season, seasonsdf$season)] & as.Date(the.ace$ace)<=seasonsdf$end[match(this.season, seasonsdf$season)]
# 
# # if(length(node.in.season)==sum(node.in.season)){
# #   return(temp)
# # }
# 
# first.node.in.season <- match(these.node.times[node.in.season][which.min(these.node.times[node.in.season])], these.node.times)
# 
# first.node.in.season <- match(TRUE, node.in.season)
# 
# 
# 
# 
# # ?ape::subtrees()
# # 
# # 
# # lapply(subtrees(this.tree), mrca)
# 
# 
# test <- extract.clade(this.tree, first.node.in.season+Ntip(this.tree))
# plot(test, show.node.label = TRUE)
# # nodelabels(text = test$Ti)
# 
# 
# 
# # plot.phylo(testtree, show.node.label = TRUE)
# # 
# # nodelabels(text = as.Date(testace$ace), node = as.numeric(names(testace$ace)), xpd = TRUE)
# # nodelabels(text = as.numeric(names(testace$ace)), node = as.numeric(names(testace$ace)), xpd = TRUE)
# 
# 
# 





















