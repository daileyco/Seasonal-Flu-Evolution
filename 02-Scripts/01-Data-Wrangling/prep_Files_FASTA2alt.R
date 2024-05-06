# Script to write fasta files


## load data
load("./01-Data/01-Processed-Data/metadata.rds")
load("./01-Data/01-Processed-Data/seqsdf_clean.rds")


## packages
library(dplyr)
library(lubridate)


## helper functions







# set up

subtypes <- unique(seqs.df$subtype)

ustates <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
             "Colorado", "Connecticut", "Delaware", "District of Columbia",
             "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
             "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
             "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
             "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
             "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
             "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
             "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
             "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin",
             "Wyoming"
)

seasons <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015",
             "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")


combos <- expand.grid(subtype = subtypes,
                      location = ustates, 
                      season = seasons) %>%
  mutate(season1 = substr(season, 1,4), 
         season2 = substr(season, 6,9))


# write in loop



for(ii in 1:nrow(combos)){
  
  
  if(!dir.exists(paste0("./01-Data/01-Processed-Data/alt/Sequences/", combos$subtype[ii]))){
    dir.create(paste0("./01-Data/01-Processed-Data/alt/Sequences/", combos$subtype[ii]))
  }
  

  
  these.seqs <- seqs.df %>% 
    filter(subtype%in%combos$subtype[ii] & Location3%in%combos$location[ii]) %>%
    filter(Collection_year %in% c(combos$season1[ii], combos$season2[ii])) %>%
    mutate(Label = paste0(">", ID, "|", Collection_Date)) %>%
    select(Label, seq) 
  
  nseqs <- nrow(these.seqs)

  
  if(nseqs>2){
    
    write.table(these.seqs, 
                file=paste0("./01-Data/01-Processed-Data/alt/Sequences/", 
                            combos$subtype[ii], "/", 
                            combos$subtype[ii], "_", 
                            combos$season[ii], "_", 
                            combos$location[ii], 
                            "_n", nseqs, "_rep0", ".fasta"), 
                sep="\n", 
                col.names = FALSE, 
                row.names = FALSE, 
                quote=FALSE)
    
    
    
  }
  
  
}


for(ii in 1:length(subtypes)){
  
  dir.create("./01-Data/01-Processed-Data/alt/Sequences/all")
  if(!dir.exists(paste0("./01-Data/01-Processed-Data/alt/Sequences/all/", combos$subtype[ii]))){
    dir.create(paste0("./01-Data/01-Processed-Data/alt/Sequences/all/", combos$subtype[ii]))
  }
  
  
  
  these.seqs <- seqs.df %>% 
    filter(subtype%in%combos$subtype[ii]) %>%
    mutate(Label = paste0(">", ID, "|", Collection_Date)) %>%
    select(Label, seq) 
  
  nseqs <- nrow(these.seqs)
  
  
  if(nseqs>2){
    
    write.table(these.seqs, 
                file=paste0("./01-Data/01-Processed-Data/alt/Sequences/all/", 
                            combos$subtype[ii], "/", 
                            combos$subtype[ii], 
                            "_n", nseqs, ".fasta"), 
                sep="\n", 
                col.names = FALSE, 
                row.names = FALSE, 
                quote=FALSE)
    
    
    
  }
  
  
}




## save


## clean environment
rm(list = ls())
gc()






