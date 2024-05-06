# script to iteratively call iqtree from command line


## load data


## packages
library(dplyr)

## helper functions



## list fasta files
fasta.list <- c(list.files("./01-Data/01-Processed-Data/alt/Sequences/H3", full.names = T), 
                list.files("./01-Data/01-Processed-Data/alt/Sequences/H1", full.names = T), 
                list.files("./01-Data/01-Processed-Data/alt/Sequences/BVic", full.names = T), 
                list.files("./01-Data/01-Processed-Data/alt/Sequences/BYam", full.names = T))


## set up directory
if(!dir.exists("./01-Data/01-Processed-Data/alt/IQTREE")){
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/H3")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/H1")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/BVic")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/BYam")
}else{
  if(!dir.exists("./01-Data/01-Processed-Data/alt/IQTREE/H3")){
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/H3")
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/H1")
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/BVic")
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/BYam")
  }
}




## extract info from filenames
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






# 
# for(i in 1:nrow(metadata)){
#   cat("\n\n\n\n\n", i, " of ", nrow(metadata), " (", round(i/nrow(metadata)*100,1), "%)\n\n\n")
#   Sys.sleep(1)
#   
#   
#   fasta <- readLines(fasta.list[i])
#   fastalabels <- fasta[which(substr(fasta, 1,1)==">")]
#   
#   datetable <- data.frame(id = sub(":", "_", sub(">", "", fastalabels)), 
#                           # id = sub("^>(.+)[|].+$", "\\1", fastalabels),
#                           date = sub("^.+[|](.+)$", "\\1", fastalabels))
#   
#   write.table(datetable,
#               file = "./01-Data/01-Processed-Data/alt/IQTREE/datefile.tsv",
#               sep = "\t",
#               row.names = F,
#               quote = F, 
#               col.names = F)
#   
#   # hung at i = c(613), rerunning fixed
#   
#   system(paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
#                 file.path(getwd(),
#                           substr(metadata$filename[i],
#                                  3,
#                                  nchar(metadata$filename[i]))),
#                 '" -pre "',
#                 file.path(getwd(),
#                           sub("Sequences",
#                               "IQTREE",
#                               substr(metadata$filename[i],
#                                      3,
#                                      nchar(metadata$filename[i])))),
#                 
#                 '" -m GTR --date ',
#                 '"',
#                 file.path(getwd(), "01-Data/01-Processed-Data/alt/IQTREE/datefile.tsv"),
#                 '"',
#                 
#                 ' --date-outlier 3 --date-options "-t 1e-4"'
#                 
#   ))
#   
#   
#   
#   
# }

for(j in 1:(ceiling(nrow(metadata)/10))){
  
  cat("\n\n\n\n\n", j, " of ", ceiling(nrow(metadata)/10), " (", round(j/ceiling(nrow(metadata)/10)*100,1), "%)\n\n\n")
  Sys.sleep(0.5)
  
  start <- (j-1)*10+1
  stop <- start + ifelse(j==ceiling(nrow(metadata)/10), nrow(metadata)-start, 9)
  
  commands <- c()
  
  for(i in start:stop){
    
    fasta <- readLines(fasta.list[i])
    fastalabels <- fasta[which(substr(fasta, 1,1)==">")]
    
    datetable <- data.frame(id = sub(":", "_", sub(">", "", fastalabels)), 
                            # id = sub("^>(.+)[|].+$", "\\1", fastalabels),
                            date = sub("^.+[|](.+)$", "\\1", fastalabels))
    
    write.table(datetable,
                file = paste0("./01-Data/01-Processed-Data/alt/IQTREE/datefile", i, ".tsv"),
                sep = "\t",
                row.names = F,
                quote = F, 
                col.names = F)
    
    # hung at i = c(613), rerunning fixed
    
    commands <- c(commands,
                  paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
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
                         
                         '" -m GTR --date ',
                         '"',
                         file.path(getwd(), paste0("01-Data/01-Processed-Data/alt/IQTREE/datefile", i, ".tsv")),
                         '"',
                         
                         ifelse(i%in%c(665:666, 739), '', ' --date-outlier 3'), 
                         ' --date-options "-t 1e-4"'
                         
                  ))
    
    
    
    
  }
  
  writeLines(c("#!/bin/bash", 
               paste(commands, " &"), 
               "wait"), "./01-Data/01-Processed-Data/alt/smalltrees.sh")
  
  
  system('bash "G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/alt/smalltrees.sh"')
  
  # Sys.sleep(1)
}

















## altogether

fasta.list <- c(list.files("./01-Data/01-Processed-Data/alt/Sequences/all/H3", full.names = T), 
                list.files("./01-Data/01-Processed-Data/alt/Sequences/all/H1", full.names = T), 
                list.files("./01-Data/01-Processed-Data/alt/Sequences/all/BVic", full.names = T), 
                list.files("./01-Data/01-Processed-Data/alt/Sequences/all/BYam", full.names = T))


## set up directory
if(!dir.exists("./01-Data/01-Processed-Data/alt/IQTREE/all")){
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/H3")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/H1")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/BVic")
  dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/BYam")
}else{
  if(!dir.exists("./01-Data/01-Processed-Data/alt/IQTREE/all/H3")){
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/H3")
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/H1")
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/BVic")
    dir.create("./01-Data/01-Processed-Data/alt/IQTREE/all/BYam")
  }
}




## extract info from filenames
metadata <- lapply(fasta.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "n"))%>%
                       t()%>%
                       as.data.frame()
                   })%>%
  bind_rows() %>%
  bind_cols(., filename = fasta.list) %>%
  mutate(n = as.numeric(sub("n", "", n)))



for(j in 1:(ceiling(nrow(metadata)/10))){
  
  cat("\n\n\n\n\n", j, " of ", ceiling(nrow(metadata)/10), " (", round(j/ceiling(nrow(metadata)/10)*100,1), "%)\n\n\n")
  Sys.sleep(0.5)
  
  start <- (j-1)*10+1
  stop <- start + ifelse(j==ceiling(nrow(metadata)/10), nrow(metadata)-start, 9)
  
  commands <- c()
  
  for(i in start:stop){
    
    fasta <- readLines(fasta.list[i])
    fastalabels <- fasta[which(substr(fasta, 1,1)==">")]
    
    datetable <- data.frame(id = sub(":", "_", sub(">", "", fastalabels)), 
                            # id = sub("^>(.+)[|].+$", "\\1", fastalabels),
                            date = sub("^.+[|](.+)$", "\\1", fastalabels))
    
    write.table(datetable,
                file = paste0("./01-Data/01-Processed-Data/alt/IQTREE/all/datefile", i, ".tsv"),
                sep = "\t",
                row.names = F,
                quote = F, 
                col.names = F)
    
    # hung at i = c(613), rerunning fixed
    
    commands <- c(commands,
                  paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
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
                         
                         '" -m GTR -nt AUTO --date ',
                         '"',
                         file.path(getwd(), paste0("01-Data/01-Processed-Data/alt/IQTREE/all/datefile", i, ".tsv")),
                         '"',
                         
                         ifelse(i%in%c(665:666, 739), '', ' --date-outlier 3'), 
                         ' --date-options "-t 1e-4"'
                         
                  ))
    
    
    
    
  }
  
  writeLines(c("#!/bin/bash", 
               paste(commands, " &"), 
               "wait"), "./01-Data/01-Processed-Data/alt/bigtrees.sh")
  
  
  system('bash "G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/alt/bigtrees.sh"')
  
  # Sys.sleep(1)
}




















## save


## clean environment
rm(list=ls())
gc()
