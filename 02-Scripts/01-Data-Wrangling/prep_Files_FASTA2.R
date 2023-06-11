# Script to write fasta files from subset of sequence IDs
## subsets here are locations and seasons



load("./01-Data/01-Processed-Data/metadata_us1020.rds")

library(dplyr)


fasta.files <- c("./01-Data/01-Processed-Data/Sequences/BVic_n5778.mo.fasta", 
                 "./01-Data/01-Processed-Data/Sequences/BYam_n4937.mo.fasta", 
                 "./01-Data/01-Processed-Data/Sequences/H1_n11256.mo.fasta", 
                 "./01-Data/01-Processed-Data/Sequences/H3_n22027.mo.fasta")


seqs.df.list <- list()


for(i in 1:length(fasta.files)){
  
  

  seqs <- readLines(fasta.files[i])
  
  seqs.df <- data.frame(label = seqs[which(substr(seqs, 1,1)==">")]) %>%
    mutate(ID = gsub("^>(.+)\\|[0-9]{8}\\|[A-Z].+$", "\\1", label))
  
  start.indices <- which(substr(seqs, 1,1)==">")+1
  stop.indices <- c(which(substr(seqs, 1,1)==">")[-1]-1, length(seqs))
  
  seqs.df$seq <- NA
  
  for(k in seq_along(start.indices)){
    seqs.df$seq[k] <- paste0(seqs[start.indices[k]:stop.indices[k]], collapse = "")
  }
  
  
  
  
  seqs.df <- seqs.df %>% 
    filter(ID %in% meta.df.us1020$Isolate_Id) %>%
    left_join(meta.df.us1020%>%select(ID=Isolate_Id, Collection_Date, Collection_season, Location3, Subtype, Lineage, Passage_History, Isolate_Name, Location, Host, Host_Age, Host_Age_Unit, Host_Gender), 
              by = c("ID")) %>%
    filter(complete.cases(ID, Collection_Date, Location3, Subtype))
  
  # seqs.df %>% group_by(Collection_season, Location) %>% summarise(n=n()) %>% ungroup() %>% View()
  
  
  seqs.df.list[[i]] <- seqs.df
  
}




seqs.df <- seqs.df.list %>% 
  bind_rows() %>%
  mutate(Subtype = ifelse(Subtype%in%c("B"), paste0(Subtype, substr(Lineage,1,3)), ifelse(grepl("H1", Subtype), "H1", ifelse(grepl("H3", Subtype), "H3", NA)))) %>%
  filter(!duplicated(ID))
# View(meta.df.us1020[which(!meta.df.us1020$Isolate_Id%in%seqs.df$ID),])








subtypes <- unique(seqs.df$Subtype)

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
                      season = seasons)


set.seed(20230530)


for(ii in 1:nrow(combos)){
  
  
  if(!dir.exists(paste0("./01-Data/01-Processed-Data/Sequences/", combos$subtype[ii]))){
    dir.create(paste0("./01-Data/01-Processed-Data/Sequences/", combos$subtype[ii]))
  }

  
  these.seqs <- seqs.df %>% 
    filter(Subtype%in%combos$subtype[ii] & Collection_season%in%combos$season[ii] & Location3%in%combos$location[ii]) %>%
    mutate(Label = paste0(">", ID, "|", gsub("-", "", Collection_Date), "|", Location3)) %>%
    select(Label, seq)
  
  if(nrow(these.seqs)>3){
    
    if(nrow(these.seqs)>15){
      
      for(iii in 1:{ceiling(nrow(these.seqs)/15)*2}){
        
        this.subsample <- sample(1:nrow(these.seqs), 15, replace = TRUE)
        
        these.seqs.resampled <- these.seqs[this.subsample,]
        
        for(j in 1:length(this.subsample)){
          these.seqs.resampled$Label[j] <- sub("\\|", paste0("0", j, "\\|"), these.seqs.resampled$Label[j])
        }
        
        
        write.table(these.seqs.resampled, 
                    file=paste0("./01-Data/01-Processed-Data/Sequences/", 
                                combos$subtype[ii], "/", 
                                combos$subtype[ii], "_", 
                                combos$season[ii], "_", 
                                combos$location[ii], 
                                "_n", nrow(these.seqs), "_rep", iii, ".fasta"), 
                    sep="\n", 
                    col.names = FALSE, 
                    row.names = FALSE, 
                    quote=FALSE)
        
        
      }
      
      
    }else{
      write.table(these.seqs, 
                file=paste0("./01-Data/01-Processed-Data/Sequences/", 
                            combos$subtype[ii], "/", 
                            combos$subtype[ii], "_", 
                            combos$season[ii], "_", 
                            combos$location[ii], 
                            "_n", nrow(these.seqs), "_rep0", ".fasta"), 
                sep="\n", 
                col.names = FALSE, 
                row.names = FALSE, 
                quote=FALSE)
      
    }
    
    
  }
  
  
}





rm(list = ls())
gc()






