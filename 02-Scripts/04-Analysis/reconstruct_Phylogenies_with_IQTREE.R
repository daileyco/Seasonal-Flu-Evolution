
library(dplyr)

fasta.list <- c(list.files("./01-Data/01-Processed-Data/Sequences/H3", full.names = T), 
                list.files("./01-Data/01-Processed-Data/Sequences/H1", full.names = T), 
                list.files("./01-Data/01-Processed-Data/Sequences/BVic", full.names = T), 
                list.files("./01-Data/01-Processed-Data/Sequences/BYam", full.names = T))


if(!dir.exists("./01-Data/01-Processed-Data/IQTREE")){
  dir.create("./01-Data/01-Processed-Data/IQTREE")
  dir.create("./01-Data/01-Processed-Data/IQTREE/H3")
  dir.create("./01-Data/01-Processed-Data/IQTREE/H1")
  dir.create("./01-Data/01-Processed-Data/IQTREE/BVic")
  dir.create("./01-Data/01-Processed-Data/IQTREE/BYam")
}else{
    if(!dir.exists("./01-Data/01-Processed-Data/IQTREE/H3")){
      dir.create("./01-Data/01-Processed-Data/IQTREE/H3")
      dir.create("./01-Data/01-Processed-Data/IQTREE/H1")
      dir.create("./01-Data/01-Processed-Data/IQTREE/BVic")
      dir.create("./01-Data/01-Processed-Data/IQTREE/BYam")
    }
  }





metadata <- lapply(fasta.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "season", "location", "n", "rep"))%>%
                       t()%>%
                       as.data.frame()
                     })%>%
  bind_rows() %>%
  bind_cols(., filename = fasta.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep)))




for(i in 1:nrow(metadata)){
  cat("\n", i, " of ", nrow(metadata))
  system(paste0('"C:/Program Files/iqtree-1.6.12-Windows/bin/iqtree.exe" -s "', 
                file.path(getwd(), 
                          substr(metadata$filename[i], 
                                 3, 
                                 nchar(metadata$filename[i]))), 
                '" -pre "', 
                file.path(getwd(), 
                          sub("Sequences", 
                              "IQTREE", 
                              substr(metadata$filename[i], 
                                     3, 
                                     nchar(metadata$filename[i])))), 
                '" -m GTR'# -nt AUTO
                ))
  
}



rm(list=ls())
gc()
