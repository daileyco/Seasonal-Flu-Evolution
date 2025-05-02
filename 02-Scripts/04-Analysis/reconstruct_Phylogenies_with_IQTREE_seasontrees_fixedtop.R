# script to iteratively call iqtree from command line


## load data


## packages
library(dplyr)

## helper functions



## list tempest files
tempest.list <- c(list.files("./01-Data/01-Processed-Data/SeasonTrees/TempEst", pattern = "tempest", full.names = T, recursive = T))



## list fasta files
fasta.list <- c(list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/H3", full.names = T), 
                list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/H1", full.names = T), 
                list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/BVic", full.names = T), 
                list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/BYam", full.names = T))






# ## set up directory
# if(!dir.exists("./01-Data/01-Processed-Data/SeasonTrees/IQTREE")){
#   dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE")
#   dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3")
#   dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H1")
#   dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BVic")
#   dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BYam")
# }else{
#   if(!dir.exists("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3")){
#     dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3")
#     dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H1")
#     dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BVic")
#     dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BYam")
#   }
# }




## extract info from filenames
metadata <- lapply(tempest.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta[.]treefile[.]tempest.*$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "season", "n", "rep"))%>%
                       t()%>%
                       as.data.frame()
                   })%>%
  bind_rows() %>%
  bind_cols(., filename = tempest.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep)), 
         rootmetric = ifelse(grepl("r2", filename), "r2", "hrmse"))

##for fastas
metadatafa <- lapply(fasta.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "season", "n", "rep"))%>%
                       t()%>%
                       as.data.frame()
                   })%>%
  bind_rows() %>%
  bind_cols(., filenamefa = fasta.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep)), 
         datetable = file.path(getwd(), paste0("01-Data/01-Processed-Data/SeasonTrees/IQTREE/datefile", row_number(), ".tsv")))







metadata <- full_join(metadatafa, metadata)


# 
# write.table(c(2e-3),
#             file = "./01-Data/01-Processed-Data/SeasonTrees/IQTREE/rate_a.tsv",
#             sep = "\t",
#             row.names = F,
#             quote = F, 
#             col.names = F)
# write.table(c(6e-4),
#             file = "./01-Data/01-Processed-Data/SeasonTrees/IQTREE/rate_b.tsv",
#             sep = "\t",
#             row.names = F,
#             quote = F, 
#             col.names = F)
# 
# 








for(j in 1:(ceiling(nrow(metadata)/10))){
  
  cat("\n\n\n\n\n", j, " of ", ceiling(nrow(metadata)/10), " (", round(j/ceiling(nrow(metadata)/10)*100,1), "%)\n\n\n")
  Sys.sleep(0.5)
  
  start <- (j-1)*10+1
  stop <- start + ifelse(j==ceiling(nrow(metadata)/10), nrow(metadata)-start, 9)
  
  commands <- c()
  
  for(i in start:stop){
    
    # fasta <- readLines(metadata$filenamefa[i])
    # fastalabels <- fasta[which(substr(fasta, 1,1)==">")]
    # 
    # datetable <- data.frame(id = sub(":", "_", sub(">", "", fastalabels)), 
    #                         # id = sub("^>(.+)[|].+$", "\\1", fastalabels),
    #                         date = sub("^.+[|](.+)$", "\\1", fastalabels))
    # 
    # write.table(datetable,
    #             file = paste0("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/datefile", i, ".tsv"),
    #             sep = "\t",
    #             row.names = F,
    #             quote = F, 
    #             col.names = F)
    # 
    # # hung at i = c(613), rerunning fixed
    
    commands <- c(commands,
                  paste0('"C:/Program Files/iqtree-2.4.0-Windows/bin/iqtree2.exe" -s "',
                         file.path(getwd(),
                                   substr(metadata$filenamefa[i],
                                          3,
                                          nchar(metadata$filenamefa[i]))),
                         '" -pre "',
                         file.path(getwd(),
                                   substr(metadata$filename[i],
                                          3,
                                          nchar(metadata$filename[i]))),
                         
                         '" -m GTR --date ',
                         # '" -m HKY --date ',
                         '"',
                         file.path(metadata$datetable[i]),
                         '"',
                         ' -te "', 
                         file.path(metadata$filename[i]),
                         
                         '" --date-outlier 3'#,
                         
                         # ifelse(metadata$subtype[i]%in%c("H3", "H1"), ' --date-options "-w G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/SeasonTrees/IQTREE/rate_a.tsv"', ' --date-options "-w G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/SeasonTrees/IQTREE/rate_b.tsv"')
                         # ' --date-options "-t 1e-4"'
                         
                  ))
    
    
    
    
  }
  
  writeLines(c("#!/bin/bash", 
               paste(commands, " &"), 
               "wait"), "./01-Data/01-Processed-Data/SeasonTrees/TempEst/seasontrees.sh")
  
  
  system('bash "G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/SeasonTrees/TempEst/seasontrees.sh"')
  
  # Sys.sleep(1)
}





## save


## clean environment
rm(list=ls())
gc()
