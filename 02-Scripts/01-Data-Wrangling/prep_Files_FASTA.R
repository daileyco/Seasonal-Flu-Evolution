# Script to write fasta files from subset of sequence IDs
## subsets here are locations and seasons



load("./01-Data/01-Processed-Data/metadata_us1020.rds")

library(dplyr)









raw.fasta.files <- c("./01-Data/00-Raw-Data/B-vic/raw_data/B-VicHA.fasta",

                     "./01-Data/00-Raw-Data/B-yam/raw_data/B-yamHA.fasta",

                     "./01-Data/00-Raw-Data/H1/raw_data/HA-sequence/H1-2009-2016-HA.fasta",
                     "./01-Data/00-Raw-Data/H1/raw_data/HA-sequence/H1-2016-2018-HA.fasta",
                     "./01-Data/00-Raw-Data/H1/raw_data/HA-sequence/H1-2018-2019-HA.fasta",
                     "./01-Data/00-Raw-Data/H1/raw_data/HA-sequence/H1-2019on-HA.fasta",

                     "./01-Data/00-Raw-Data/H3/raw_data/HA-sequence/old/H3-2015.fasta",
                     "./01-Data/00-Raw-Data/H3/raw_data/HA-sequence/old/H3-2015-2017.fasta",
                     "./01-Data/00-Raw-Data/H3/raw_data/HA-sequence/old/H3-2017-2019.fasta",
                     "./01-Data/00-Raw-Data/H3/raw_data/HA-sequence/old/H3-2019on.fasta"
                     )





if(!dir.exists("./01-Data/00-Raw-Data/Sequences")){
  dir.create("./01-Data/00-Raw-Data/Sequences")
}







seqs.df.list <- list()


for(i in 1:length(raw.fasta.files)){
  
  

  seqs <- readLines(raw.fasta.files[i])
  
  seqs.df <- data.frame(label = seqs[which(substr(seqs, 1,1)==">")]) %>%
    mutate(ID = gsub("^>(.+)\\|[A-Z]/.+", "\\1", label))
  
  
  
  
  
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
  mutate(Subtype = ifelse(Subtype%in%c("B"), paste0(Subtype, substr(Lineage,1,3)), ifelse(grepl("H1", Subtype), "H1", ifelse(grepl("H3", Subtype), "H3", NA))))
# View(meta.df.us1020[which(!meta.df.us1020$Isolate_Id%in%seqs.df$ID),])








subtypes <- unique(seqs.df$Subtype)


for(ii in 1:length(subtypes)){
  
  these.seqs <- seqs.df %>% 
    filter(Subtype%in%subtypes[ii]) %>%
    mutate(Label = paste0(">", ID, "|", gsub("-", "", Collection_Date), "|", Location3)) %>%
    select(Label, seq)
  
  if(nrow(these.seqs)>0){
    write.table(these.seqs, 
                file=paste0("./01-Data/00-Raw-Data/Sequences/", 
                            subtypes[ii],
                            "_n", nrow(these.seqs), ".fasta"), 
                sep="\n", 
                col.names = FALSE, 
                row.names = FALSE, 
                quote=FALSE)
  }
  
  
}





rm(list = ls())
gc()






