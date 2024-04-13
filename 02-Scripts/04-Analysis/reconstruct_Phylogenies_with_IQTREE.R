# script to iteratively call iqtree from command line


## load data


## packages
library(dplyr)
# library(doParallel)

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




# outgroups <- data.frame(subtype = rep(c("BVic", "BYam", "H1", "H3"), each = 3),
#                         id = c(c("EPI_ISL_71453", "EPI_ISL_71454", "EPI_ISL_71455"),
#                                c("EPI_ISL_76940", "EPI_ISL_77930", "EPI_ISL_76942"),
#                                c("EPI_ISL_71398", "EPI_ISL_71406", "EPI_ISL_71431"),
#                                c("EPI_ISL_158599", "EPI_ISL_74088", "EPI_ISL_76705")))


outgroups <- data.frame(subtype = rep(c("BVic", "BYam", "H1", "H3"), each = 1), 
                        id = c("EPI_ISL_71453", #"EPI_ISL_71454", "EPI_ISL_71455", 
                               "EPI_ISL_76940", #"EPI_ISL_77930", "EPI_ISL_76942", 
                               "EPI_ISL_71398", #"EPI_ISL_71406", "EPI_ISL_71431", 
                               "EPI_ISL_158599" #,"EPI_ISL_74088", "EPI_ISL_76705"
                        ))



## calls in parallel

# cl <- makeCluster(getOption("cl.cores", 19))
# 
# clusterExport(cl, varlist = c("metadata"))
# clusterEvalQ(cl, expr = {library(dplyr)})
# 
# iqtreecalls <- parLapplyLB(cl, 
#                            1:nrow(metadata), 
#                            (\(index){
#                              
#                              try({
#                                system(paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "', 
#                                              file.path(getwd(), 
#                                                        substr(metadata$filename[i], 
#                                                               3, 
#                                                               nchar(metadata$filename[i]))), 
#                                              '" -pre "', 
#                                              file.path(getwd(), 
#                                                        sub("Sequences", 
#                                                            "IQTREE", 
#                                                            substr(metadata$filename[i], 
#                                                                   3, 
#                                                                   nchar(metadata$filename[i])))), 
#                                              '" -m GTR --date TAXNAME --date-outlier 3'
#                                              
#                                ))
#                              })
#                              
#                            }))
# 
# stopCluster(cl)


#i=1346, BVic Pennsylvania, 2018-2019, issues with dating when outgroup specified, remove specification and removed as outlier and dating is fine
# i=1346
# system(paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
#               file.path(getwd(),
#                         substr(metadata$filename[i],
#                                3,
#                                nchar(metadata$filename[i]))),
#               '" -pre "',
#               file.path(getwd(),
#                         sub("Sequences",
#                             "IQTREE",
#                             substr(metadata$filename[i],
#                                    3,
#                                    nchar(metadata$filename[i])))),
#               '" -m GTR --date TAXNAME',
#               # file.path(getwd(), "01-Data/01-Processed-Data/IQTREE/datefile.tsv"),
#               ' --date-outlier 3'
# ))


# # i=665 H1 Idaho 2013-2014; issues, remove outgroup specification and okay
# system(paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
#               file.path(getwd(),
#                         substr(metadata$filename[i],
#                                3,
#                                nchar(metadata$filename[i]))),
#               '" -pre "',
#               file.path(getwd(),
#                         sub("Sequences",
#                             "IQTREE",
#                             substr(metadata$filename[i],
#                                    3,
#                                    nchar(metadata$filename[i])))),
#               '" -m GTR --date TAXNAME',
#               # file.path(getwd(), "01-Data/01-Processed-Data/IQTREE/datefile.tsv"),
#               ' --date-outlier 3 --date-options "-t 1e-4"'))


# 
for(i in 665:nrow(metadata)){
# for(i in 1:nrow(metadata)){
  cat("\n", i, " of ", nrow(metadata))


  fasta <- readLines(fasta.list[i])
  fastalabels <- fasta[which(substr(fasta, 1,1)==">")]
  #
  # datetable <- data.frame(id = sub("^>(.+)[|].+$", "\\1", fastalabels),
  #                         date = sub("^.+[|](.+)$", "\\1", fastalabels))
  #
  # write.table(datetable,
  #             file = "./01-Data/01-Processed-Data/IQTREE/datefile.tsv",
  #             sep = "\t",
  #             row.names = F,
  #             quote = F)



  system(paste0('"C:/Program Files/iqtree-2.3.1-Windows/bin/iqtree2.exe" -s "',
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
                '" -m GTR --date TAXNAME',
                # file.path(getwd(), "01-Data/01-Processed-Data/IQTREE/datefile.tsv"),
                ' --date-outlier 3 --date-options "-G -t 1e-4"'
                ,

                ifelse(metadata$season[i]%in%c("2010-2011", "2011-2012"),
                       "",
                       paste0(' -o "', paste0(sub(">", "", fastalabels[(length(fastalabels)):length(fastalabels)]), collapse = ","), '"'))

                ))

}


## save


## clean environment
rm(list=ls())
gc()
