# script to read output files of iqtree into an r df

## load data

## packages
library(dplyr)

## helper functions



## iqtree files
iqtree.list <- c(list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3", full.names = T, pattern = ".iqtree"), 
                 list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H1", full.names = T, pattern = ".iqtree"), 
                 list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BVic", full.names = T, pattern = ".iqtree"), 
                 list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BYam", full.names = T, pattern = ".iqtree"))


### pull info from filenames
metadata <- lapply(iqtree.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta[.]iqtree$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "season", "n", "rep"))%>%
                       t()%>%
                       as.data.frame()
                   })%>%
  bind_rows() %>%
  bind_cols(., filename = iqtree.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep)))




### add info in file
iqtree.df <- lapply(1:nrow(metadata), function(i){
  
  iqtree.output <- readLines(metadata$filename[i])
  
  
  df <- data.frame(
    filename = metadata$filename[i],
    input.data = iqtree.output[grep("Input data: ", iqtree.output)],
    n.constant.sites = iqtree.output[grep("Number of constant sites: ", iqtree.output)], 
    n.invariant.sites = iqtree.output[grep("Number of invariant \\(constant or ambiguous constant\\) sites: ", iqtree.output)],
    n.inform.sites = iqtree.output[grep("Number of parsimony informative sites: ", iqtree.output)],
    n.distinct.site.patterns = iqtree.output[grep("Number of distinct site patterns: ", iqtree.output)],
    p.a = iqtree.output[grep("pi\\(A\\)", iqtree.output)], 
    p.c = iqtree.output[grep("pi\\(C\\)", iqtree.output)], 
    p.g = iqtree.output[grep("pi\\(G\\)", iqtree.output)], 
    p.t = iqtree.output[grep("pi\\(T\\)", iqtree.output)],
    tree.newick = iqtree.output[grep("Tree in newick format:", iqtree.output)+2], 
    tree.newick2 = paste0(readLines(sub("[.]iqtree", ".treefile", metadata$filename[i])), collapse = "")
  )
  
  return(df)
  
}) %>% 
  bind_rows() %>%
  mutate(input.n.seqs = as.numeric(sub(".*Input data: ([0-9]{1,4}) sequences.+$", "\\1", input.data)), 
         input.sites.per.seq = as.numeric(sub(".*Input data: [0-9]{1,4} sequences .+ ([0-9]{1,5}).+$", "\\1", input.data)), 
         n.constant.sites = as.numeric(sub(".+[:] ([0-9]{1,4}) \\(.+", "\\1", n.constant.sites)), 
         n.invariant.sites = as.numeric(sub(".+[:] ([0-9]{1,4}) \\(.+", "\\1", n.invariant.sites)), 
         n.inform.sites = as.numeric(sub("^.+ ([0-9]+)$", "\\1", n.inform.sites)), 
         n.distinct.site.patterns = as.numeric(sub("^.+ ([0-9]+)$", "\\1", n.distinct.site.patterns)), 
         p.a = as.numeric(sub("^.+= (.+)$", "\\1", p.a)), 
         p.c = as.numeric(sub("^.+= (.+)$", "\\1", p.c)), 
         p.g = as.numeric(sub("^.+= (.+)$", "\\1", p.g)),
         p.t = as.numeric(sub("^.+= (.+)$", "\\1", p.t))) %>%
  select(-input.data)





## same for log files

log.list <- c(list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3", full.names = T, pattern = ".log"),
              list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H1", full.names = T, pattern = ".log"),
              list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BVic", full.names = T, pattern = ".log"),
              list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BYam", full.names = T, pattern = ".log"))



metadata.log <- lapply(log.list, 
                       function(x){
                         sub(".+/(.+)[.]fasta[.]log$", "\\1", x)%>%
                           strsplit(., "_")%>%
                           unlist()%>%
                           setNames(., nm = c("subtype", "season", "n", "rep"))%>%
                           t()%>%
                           as.data.frame()
                       })%>%
  bind_rows() %>%
  bind_cols(., filename = log.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep)))


log.df <- lapply(1:nrow(metadata.log), function(i){
  
  iqtree.output <- readLines(metadata.log$filename[i])
  
  
  df <- data.frame(
    filename = metadata.log$filename[i],
    errors = paste0(iqtree.output[grep("ERROR: ", iqtree.output)], collapse = "")
  )
  
  return(df)
  
}) %>% bind_rows()





## same for lsd files

lsd.list <- c(list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3", full.names = T, pattern = ".lsd"),
              list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H1", full.names = T, pattern = ".lsd"),
              list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BVic", full.names = T, pattern = ".lsd"),
              list.files("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BYam", full.names = T, pattern = ".lsd"))



metadata.lsd <- lapply(lsd.list, 
                       function(x){
                         sub(".+/(.+)[.]fasta[.]timetree[.]lsd$", "\\1", x)%>%
                           strsplit(., "_")%>%
                           unlist()%>%
                           setNames(., nm = c("subtype", "season", "n", "rep"))%>%
                           t()%>%
                           as.data.frame()
                       })%>%
  bind_rows() %>%
  bind_cols(., filename = lsd.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep)))


lsd.df <- lapply(1:nrow(metadata.lsd), function(i){
  
  iqtree.output <- readLines(metadata.lsd$filename[i])
  
  
  df <- data.frame(
    filename = metadata.lsd$filename[i],
    datingresults = paste0(iqtree.output[grep("- Dating results:", iqtree.output)+1], collapse = ""), 
    timetreenexusfile = sub("[.]lsd", ".nex", metadata.lsd$filename[i])
  ) %>% 
    mutate(rate = sub("^.*rate[:blank:]*(.+),{1}.+,{1}.+$", "\\1", datingresults), 
           tMRCA = sub("^.+,{1}.*tMRCA[:blank:]*(.+),{1}.+$", "\\1", datingresults), 
           objfn = sub("^.+,{1}.+,{1}.*objective function[:blank:]*(.+)$", "\\1", datingresults))
  
  return(df)
  
}) %>% bind_rows()











## put them all together


iqtree.df <- full_join(metadata, iqtree.df, by = "filename")%>%rename(filename.iqtree = filename)

log.df <- full_join(metadata.log, log.df, by = "filename")%>%rename(filename.log = filename)

lsd.df <- full_join(metadata.lsd, lsd.df, by = "filename")%>%rename(filename.lsd = filename)



iqtree <- full_join(log.df, 
                    iqtree.df, 
                    by = c("subtype", "season", "n", "rep")) %>% 
  full_join(., 
            lsd.df, 
            by = c("subtype", "season", "n", "rep"))







## save
save(iqtree, file = "./01-Data/01-Processed-Data/SeasonTrees/IQTREE_results.rds")



## clean environment
rm(list=ls())
gc()
