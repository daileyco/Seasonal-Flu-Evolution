# script to partition full phylogenetic trees based on estimated date of internal nodes to identify subtrees whose mrca falls within collection season

## load data
load("./01-Data/01-Processed-Data/alt/IQTREE_results.rds")
# iqtreealt <- iqtree
# names(iqtreealt)[4:ncol(iqtreealt)] <- paste0(names(iqtreealt)[4:ncol(iqtreealt)], "_alt")
# 
# 
# load("./01-Data/01-Processed-Data/iqtree_results.rds")


## helper function


makeDecimalDate <- function(y){
  require(lubridate)
  case_when(nchar(y)==10 ~ decimal_date(as.Date(y, "%Y-%m-%d")), 
            nchar(y)==7 ~ as.numeric(substr(y,1,4))+(as.numeric(substr(y,6,7))-1)/12, 
            nchar(y)==4 ~ as.numeric(y), 
            TRUE ~ NA)
}

vsubstr <- Vectorize(substr, c("start", "stop"))




## packages
library(ape)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)





# ## identify better tree from runs with outgroup and those without
# ### use whichever tMRCA is 
# both <- full_join(iqtree, 
#                   iqtreealt) %>% 
#   select(subtype, season, location, matches("tMRCA")) %>% 
#   mutate(across(matches("tMRCA"), ~makeDecimalDate(trimws(.x)), .names = "{.col}_dd"), 
#          seasonnum = as.numeric(substr(season, 1,4)), 
#          across(matches("_dd"), ~.x-seasonnum-1, .names = "{.col}_diff2season"), 
#          dddiff = abs(tMRCA_dd-tMRCA_alt_dd), 
#          altcloser = (tMRCA_dd_diff2season < tMRCA_alt_dd_diff2season) & dddiff > 3)
# 
# alt.keep <- which(both$altcloser)
# outgroup.keep <- which(!(1:nrow(both) %in% alt.keep))
# 




# dates <- makeDecimalDate(trimws(iqtree.full$tMRCA));
# tmrcas <- c(both$tMRCA_dd_diff2season[outgroup.keep], both$tMRCA_alt_dd_diff2season[alt.keep]);
# subtypes <- c(both$subtype[outgroup.keep], both$subtype[alt.keep]);
# tms <- c(both$tMRCA_dd[outgroup.keep], both$tMRCA_alt_dd[alt.keep]);
# tapply(tmrcas, subtypes, summary);
# tapply(tmrcas[which(tms>2005)], subtypes[which(tms>2005)], summary)





# 
# 
# names(iqtreealt) <- sub("_alt", "", names(iqtreealt))
# 
# 
# ## subset data to include chosen time trees
# iqtree.full <- bind_rows(iqtree[outgroup.keep,], 
#                          iqtreealt[alt.keep,]) %>% 
#   arrange(subtype, season, location)
# 
# 


iqtree.full <- iqtree %>%
  arrange(subtype, season, location)







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
  
  
  #### read tree text from df
  this.tree <- read.tree(text = this.row$tree.newick)
  this.tree2 <- read.tree(text = this.row$tree.newick2)
  
  if(is.na(this.row$timetreenexusfile)){next}
  this.timetree <- read.nexus(file = this.row$timetreenexusfile)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### drop tips out of season
  
  #### convert tip labels to dates
  these.labels <- this.timetree$tip.label
  
  these.labels.dates <- sapply(these.labels, 
                               function(this.label){
                                 this.sampling.date <- strsplit(this.label, split = "[|]")[[1]][2]
                               })
  
  these.numdates <- sapply(these.labels.dates, 
    (\(x){
      
      # case_when(grepl("[_]", x) ~ strsplit(x, split = "_") %>%
      #                               unlist() %>%
      #                               as.Date(., "%Y-%m-%d") %>%
      #                               mean()%>%
      #                               decimal_date(), 
      #           nchar(x)==10 ~ decimal_date(as.Date(x, "%Y-%m-%d")), 
      #           nchar(x)==7 ~ as.numeric(substr(x,1,4))+(as.numeric(substr(x,6,7))-1)/12, 
      #           nchar(x)==4 ~ as.numeric(x))
      
      case_when(grepl("[_]", x) ~ strsplit(x, split = "_") %>%
                  unlist() %>%
                  makeDecimalDate() %>%
                  mean(), 
                TRUE ~ makeDecimalDate(x))
      
    })
  )


  
  
  pathdf <- nodepath(this.timetree) %>% 
    lapply(., 
           (\(x){
             y <- data.frame(start = x[-length(x)], stop = x[-1])
           })) %>% 
    bind_rows(.id = "tip")
  
  
  pathdf$branchlength <- sapply(1:nrow(pathdf), 
                                (\(x){
                                  this.timetree$edge.length[which(this.timetree$edge[,1]==pathdf$start[x]&this.timetree$edge[,2]==pathdf$stop[x])]
                                }))
  
  nodes <- pathdf %>% 
    group_by(tip) %>%
    summarise(pathbl = sum(branchlength)) %>% 
    ungroup() %>%
    full_join(., 
              data.frame(label = these.labels, 
                         tipdate = these.numdates) %>%
                mutate(tip = as.character(match(label, this.timetree$tip.label))), 
              by = "tip") %>%
    full_join(., 
              pathdf, 
              by = "tip") %>%
    mutate(rootage = tipdate - pathbl) %>% 
    (\(x){
      tapply(x, ~tip, (\(y){y <- y%>%arrange(start);y$cbl = cumsum(y$branchlength);y}))
    }) %>%
    bind_rows()%>%
    mutate(terminalage = rootage + cbl) %>% 
    (\(x){
      rootdist <- x %>% select(rootage) %>% unlist() %>% unname() %>% mean()
      
      nodedist <- x %>% filter(stop>min(start)) %>% split(., ~stop) %>% lapply(., (\(y){y %>% select(terminalage) %>% unlist() %>% unname() %>% mean()}))
      
      return(c(list(rootdist)%>%setNames(., nm = min(x$start)), nodedist))
    }) %>% 
    unlist() %>%
    as.matrix() %>%
    as.data.frame() %>% 
    setNames(., nm = c("age")) %>%
    rownames_to_column("node")
  
  
  
  
  
  
  # #### pull dates from nexus files (not done with read.nexus())
  # these.node.dates <- data.frame(datestring = vsubstr(readLines(this.row$timetreenexusfile)[3], 
  #                                                     unlist(gregexpr("\\[", readLines(this.row$timetreenexusfile)[3])), 
  #                                                     unlist(gregexpr("\\]", readLines(this.row$timetreenexusfile)[3]))), 
  #                                prechar = vsubstr(readLines(this.row$timetreenexusfile)[3], 
  #                                                  unlist(gregexpr("\\[", readLines(this.row$timetreenexusfile)[3]))-1,
  #                                                  unlist(gregexpr("\\[", readLines(this.row$timetreenexusfile)[3]))-1))  %>% 
  #   mutate(nodetype = ifelse(prechar==")", "internal", "tip"), 
  #          datestring1 = gsub('\\[|\\&|[a-z]|\\"|=|\\]', "", datestring), 
  #          date = makeDecimalDate(datestring1))
  # # %>% 
  # #   mutate(nodenum = match(datestring1, these.labels.dates)) %>% 
  # #   arrange(desc(nodetype)) %>% 
  # #   mutate(nodenum = ifelse(is.na(nodenum), row_number(), nodenum))
  
  
  
  # these.labels.is <- these.labels[which(these.labels.dates>=seasonsdf$start[match(this.season, seasonsdf$season)] & 
  #                                       these.labels.dates<=seasonsdf$end[match(this.season, seasonsdf$season)])]
  # 
  
  these.labels.is <- these.labels[which(these.numdates>=decimal_date(seasonsdf$start[match(this.season, seasonsdf$season)]) & 
                                          these.numdates<=decimal_date(seasonsdf$end[match(this.season, seasonsdf$season)]))]
  
  these.labesl.oos <- these.labels[which(!these.labels%in%these.labels.is)]
  
  
  
  
  ### determine whether each internal node date estimate falls within the given influenza season
  nodes$node.in.season <- nodes$age>=decimal_date(seasonsdf$start[match(this.season, seasonsdf$season)]) & 
    nodes$age<=decimal_date(seasonsdf$end[match(this.season, seasonsdf$season)])
  
  
  
  
  if(length(nodes$node.in.season)==sum(nodes$node.in.season) | sum(nodes$node.in.season)==0){
    
    
    tree.list[[i]] <- this.row
    
    
  }else{
    
    these.subtrees <- subtrees(this.timetree)[which(nodes$node.in.season)]
    
    
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
    
    
    kept.subtrees <- these.subtrees[trees.to.keep]
    
    
    this.row.plus <- this.row
    
    for(k in 1:length(trees.to.keep)){
      
      this.smalltree <- read.tree(text = this.row$tree.newick2) %>%
        keep.tip(., tip = kept.subtrees[[k]]$tip.label) %>%
        drop.tip(., tip = these.labesl.oos)
      
      
      if(!is.null(this.smalltree) && Ntip(this.smalltree)>1){
        
        
        this.row.plus <- bind_rows(this.row.plus,
                                   cbind(this.row.plus[1,c(1:3, 21:22)], 
                                         data.frame(rep = as.numeric(paste0(0, ".", k)), 
                                                    smalltree = write.tree(this.smalltree, file = ""), 
                                                    n = Ntip(this.smalltree), 
                                                    tmrca = nodes$age[nodes$node.in.season][trees.to.keep[k]])))
        
      }
      
      
    }
    
    tree.list[[i]] <- this.row.plus
    
    
    
  }
  
  
  
}



##combine all full trees and their chosen subtrees from list into a df
tree.df <- bind_rows(tree.list)


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





















