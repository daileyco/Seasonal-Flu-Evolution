# script to iteratively call iqtree from command line


## load data


## packages
library(dplyr)

## helper functions



## list fasta files
fasta.list <- c(list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/H3", full.names = T), 
                list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/H1", full.names = T), 
                list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/BVic", full.names = T), 
                list.files("./01-Data/01-Processed-Data/SeasonTrees/Sequences/BYam", full.names = T))


## set up directory
if(!dir.exists("./01-Data/01-Processed-Data/SeasonTrees/IQTREE")){
  dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE")
  dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3")
  dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H1")
  dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BVic")
  dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BYam")
}else{
  if(!dir.exists("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3")){
    dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H3")
    dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/H1")
    dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BVic")
    dir.create("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/BYam")
  }
}




## extract info from filenames
metadata <- lapply(fasta.list, 
                   function(x){
                     sub(".+/(.+)[.]fasta$", "\\1", x)%>%
                       strsplit(., "_")%>%
                       unlist()%>%
                       setNames(., nm = c("subtype", "season", "n", "rep"))%>%
                       t()%>%
                       as.data.frame()
                   })%>%
  bind_rows() %>%
  bind_cols(., filename = fasta.list) %>%
  mutate(n = as.numeric(sub("n", "", n)), 
         rep = as.numeric(sub("rep", "", rep))) %>% 
  mutate(s1 = as.numeric(substr(season,1,4)), 
         rootdate = paste0("b(", s1-2, ",", s1-1, ")"))





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
    
    fasta <- readLines(fasta.list[i])
    fastalabels <- fasta[which(substr(fasta, 1,1)==">")]
    
    datetable <- data.frame(id = sub(":", "_", sub(">", "", fastalabels)), 
                            # id = sub("^>(.+)[|].+$", "\\1", fastalabels),
                            date = sub("^.+[|](.+)$", "\\1", fastalabels))
    
    write.table(datetable,
                file = paste0("./01-Data/01-Processed-Data/SeasonTrees/IQTREE/datefile", i, ".tsv"),
                sep = "\t",
                row.names = F,
                quote = F, 
                col.names = F)
    

    
    # "C:/Program Files/iqtree-2.4.0-Windows/bin/iqtree2.exe" 
    ## -s "C:/Users/daileyco/Desktop/iqtreepractice/H3_2015-2016_n3336_rep0.fasta" 
    ## -pre "C:/Users/daileyco/Desktop/iqtreepractice/H3_2015-2016_n3336_rep0.fasta" 
    ## -m GTR --date "C:/Users/daileyco/Desktop/iqtreepractice/datefile6.tsv" 
    ## --date-outlier 3 
    ## --date-root b(2013,2015)
    
    
    
    commands <- c(commands,
                  paste0('"C:/Program Files/iqtree-2.4.0-Windows/bin/iqtree2.exe" -s "',
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
                         # '" -m HKY --date ',
                         '"',
                         file.path(getwd(), paste0("01-Data/01-Processed-Data/SeasonTrees/IQTREE/datefile", i, ".tsv")),
                         '"',
                         
                         ' --date-outlier 3 --date-root "', 
                         metadata$rootdate[i], 
                         '"'#,
                         
                         # ifelse(metadata$subtype[i]%in%c("H3", "H1"), ' --date-options "-w G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/SeasonTrees/IQTREE/rate_a.tsv"', ' --date-options "-w G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/SeasonTrees/IQTREE/rate_b.tsv"')
                         # ' --date-options "-t 1e-4"'
                         
                  ))
    
    
    
    
  }
  
  writeLines(c("#!/bin/bash", 
               paste(commands, " &"), 
               "wait"), "./01-Data/01-Processed-Data/SeasonTrees/seasontrees.sh")
  
  
  system('bash "G:/Research/Seasonal-Flu-Evolution/01-Data/01-Processed-Data/SeasonTrees/seasontrees.sh"')
  
  # Sys.sleep(1)
}





## save


## clean environment
rm(list=ls())
gc()
