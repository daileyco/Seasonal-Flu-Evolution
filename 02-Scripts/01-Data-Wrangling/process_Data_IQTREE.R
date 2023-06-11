
library(dplyr)

iqtree.list <- c(list.files("./01-Data/01-Processed-Data/IQTREE/H3", full.names = T, pattern = ".iqtree"), 
                   list.files("./01-Data/01-Processed-Data/IQTREE/H1", full.names = T, pattern = ".iqtree"), 
                   list.files("./01-Data/01-Processed-Data/IQTREE/BVic", full.names = T, pattern = ".iqtree"), 
                   list.files("./01-Data/01-Processed-Data/IQTREE/BYam", full.names = T, pattern = ".iqtree"))


metadata <- lapply(iqtree.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta[.]iqtree$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "season", "location", "n", "rep"))%>%
                       t()%>%
                       as.data.frame()
                     })%>%
  bind_rows() %>%
  bind_cols(., filename = iqtree.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep)))




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
    tree.newick = iqtree.output[grep("Tree in newick format:", iqtree.output)+2]
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




iqtree.df <- full_join(metadata, iqtree.df, by = "filename")








log.list <- c(list.files("./01-Data/01-Processed-Data/IQTREE/H3", full.names = T, pattern = ".log"),
              list.files("./01-Data/01-Processed-Data/IQTREE/H1", full.names = T, pattern = ".log"),
              list.files("./01-Data/01-Processed-Data/IQTREE/BVic", full.names = T, pattern = ".log"),
              list.files("./01-Data/01-Processed-Data/IQTREE/BYam", full.names = T, pattern = ".log"))



metadata.log <- lapply(log.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta[.]log$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "season", "location", "n", "rep"))%>%
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



log.df <- full_join(metadata.log, log.df, by = "filename")



iqtree <- full_join(log.df, iqtree.df, by = c("subtype", "season", "location", "n", "rep"))



save(iqtree, file = "./01-Data/01-Processed-Data/iqtree_results.rds")



rm(list=ls())
gc()
