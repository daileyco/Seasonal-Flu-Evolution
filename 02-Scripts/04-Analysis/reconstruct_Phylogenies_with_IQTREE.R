# script to iteratively call iqtree from command line


## load data


## packages
library(dplyr)

## helper functions



## list fasta files
fasta.list <- c(list.files("./01-Data/01-Processed-Data/Sequences/H3", full.names = T), 
                list.files("./01-Data/01-Processed-Data/Sequences/H1", full.names = T), 
                list.files("./01-Data/01-Processed-Data/Sequences/BVic", full.names = T), 
                list.files("./01-Data/01-Processed-Data/Sequences/BYam", full.names = T))


## set up directory
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





## create lookup table for outgroups
outgroups <- data.frame(subtype = c("BYam", "H3", "H1", "BVic"), 
                        id = c("EPI_ISL_6587", "EPI_ISL_6726", "EPI_ISL_7047", "EPI_ISL_20973"))




## loop calls to iqtree
### run in batches of 10
### write dates to file for each of 10
### create bash script so that each command in batch can be run in parallel
### execute bash script before next iteration

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
                file = paste0("./01-Data/01-Processed-Data/IQTREE/datefile", i, ".tsv"),
                sep = "\t",
                row.names = F,
                quote = F, 
                col.names = F)
    
    
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
                         file.path(getwd(), paste0("01-Data/01-Processed-Data/IQTREE/datefile", i, ".tsv")),
                         '"',
                         
                         # ifelse(i%in%c(665:666, 739), '', ' --date-outlier 3'), 
                         ifelse(i%in%c(665), '', ' --date-outlier 3'), 
                         ' --date-options "-G -t 1e-4"', 
                         paste0(' -o "', paste0(sub(">", "", fastalabels[(length(fastalabels)):length(fastalabels)]), collapse = ","), '"')
                         
                  ))
    
    
    
    
  }
  
  writeLines(c("#!/bin/bash", 
               paste(commands, " &"), 
               "wait"), "./01-Data/01-Processed-Data/IQTREE/smalltrees.sh")
  
  
  system('bash "G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/IQTREE/smalltrees.sh"')
  
  # Sys.sleep(1)
}





## save


## clean environment
rm(list=ls())
gc()








#old

# # outgroups <- data.frame(subtype = rep(c("BVic", "BYam", "H1", "H3"), each = 3),
# #                         id = c(c("EPI_ISL_71453", "EPI_ISL_71454", "EPI_ISL_71455"),
# #                                c("EPI_ISL_76940", "EPI_ISL_77930", "EPI_ISL_76942"),
# #                                c("EPI_ISL_71398", "EPI_ISL_71406", "EPI_ISL_71431"),
# #                                c("EPI_ISL_158599", "EPI_ISL_74088", "EPI_ISL_76705")))
# 
# 
# outgroups <- data.frame(subtype = rep(c("BVic", "BYam", "H1", "H3"), each = 1), 
#                         id = c("EPI_ISL_71453", #"EPI_ISL_71454", "EPI_ISL_71455", 
#                                "EPI_ISL_76940", #"EPI_ISL_77930", "EPI_ISL_76942", 
#                                "EPI_ISL_71398", #"EPI_ISL_71406", "EPI_ISL_71431", 
#                                "EPI_ISL_158599" #,"EPI_ISL_74088", "EPI_ISL_76705"
#                         ))
# 
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
#               file = "./01-Data/01-Processed-Data/IQTREE/datefile.tsv",
#               sep = "\t",
#               row.names = F,
#               quote = F, 
#               col.names = F)
# 
#   
#   if(i==665){
#     
#     
#     # i=665 H1 Idaho 2013-2014; issues, remove outgroup specification and okay
#     system(paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
#                   file.path(getwd(),
#                             substr(metadata$filename[i],
#                                    3,
#                                    nchar(metadata$filename[i]))),
#                   '" -pre "',
#                   file.path(getwd(),
#                             sub("Sequences",
#                                 "IQTREE",
#                                 substr(metadata$filename[i],
#                                        3,
#                                        nchar(metadata$filename[i])))),
#                   # '" -m GTR --date TAXNAME',
#                   '" -m GTR --date ',
#                   '"',
#                   file.path(getwd(), "01-Data/01-Processed-Data/IQTREE/datefile.tsv"),
#                   '"',
# 
#                   ' --date-outlier 3 --date-options "-G -t 1e-4"'
#                   
#     ))
#     
#     
#   }else{
#     
#     system(paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
#                   file.path(getwd(),
#                             substr(metadata$filename[i],
#                                    3,
#                                    nchar(metadata$filename[i]))),
#                   '" -pre "',
#                   file.path(getwd(),
#                             sub("Sequences",
#                                 "IQTREE",
#                                 substr(metadata$filename[i],
#                                        3,
#                                        nchar(metadata$filename[i])))),
#                   
#                   # '" -m GTR --date TAXNAME',
#                   '" -m GTR --date ',
#                   '"',
#                   file.path(getwd(), "01-Data/01-Processed-Data/IQTREE/datefile.tsv"),
#                   '"',
#                   
#                   ' --date-outlier 3 --date-options "-G -t 1e-4"'
#                   ,
#                   
#                   ifelse(metadata$season[i]%in%c("2010-2011", "2011-2012"),
#                          "",
#                          paste0(' -o "', paste0(sub(">", "", fastalabels[(length(fastalabels)):length(fastalabels)]), collapse = ","), '"'))
#                   
#     ))
#     
#   }
# 
# 
#   
# 
# }