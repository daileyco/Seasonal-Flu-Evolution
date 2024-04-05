# Script to write fasta files from subset of sequence IDs
## subsets here are locations and seasons



load("./01-Data/01-Processed-Data/metadata_us1020.rds")

library(dplyr)
library(lubridate)

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
  mutate(Subtype = ifelse(Subtype%in%c("B"), 
                          paste0(Subtype, substr(Lineage,1,3)), 
                          ifelse(grepl("H1", Subtype), 
                                 "H1", 
                                 ifelse(grepl("H3", Subtype), 
                                        "H3", 
                                        NA))), 
         cy = year(Collection_Date), 
         ew = epiweek(Collection_Date),
         Collection_season = case_when(#ew<30 & ew>18 ~ "off season",
                                       ew<30 ~ paste0(cy-1, "-", cy), 
                                       TRUE ~ paste0(cy, "-", cy+1)),)





## identify isolates with multiple sequences

multipleseqs <- seqs.df$Isolate_Name%in%seqs.df$Isolate_Name[which(duplicated(seqs.df$Isolate_Name))]


uniqueiso <- seqs.df[which(!multipleseqs),]

miso <- seqs.df[which(multipleseqs),] %>%
  arrange(Isolate_Name)%>%
  mutate(dupd = duplicated(seq))%>%
  group_by(Isolate_Name)%>%
  mutate(anydupdseqs = sum(dupd)>0)%>%
  ungroup()%>%
  split(., ~Isolate_Name) %>% 
  lapply(., 
         (\(x){
           x$whichones <- sapply(x$seq, 
                             (\(y){
                               which(x$seq%in%y) %>% unlist() %>% unname() %>% paste0(., collapse = ",")
                             }))
           x
         })
  ) %>%
  bind_rows(.id = "isoid") %>%
  mutate(isoidnum = as.numeric(factor(isoid))) %>%
  group_by(Isolate_Name) %>%
  mutate(alldupdseqs = length(unique(whichones))==1, 
         samemeta = length(unique(Collection_Date))==1 & length(unique(Location3))==1, 
         completemeta = !is.na(Collection_Date) & !is.na(Location3), 
         yearmatch = sub("^.+/{1}([0-9]{4})$", "\\1", Isolate_Name)==as.character(year(Collection_Date))) %>%
  ungroup()



miso.alldupdseqs <- miso %>% 
  filter(alldupdseqs)

# > miso.alldupdseqs[which(!miso.alldupdseqs$samemeta), c(1,3)]
# # A tibble: 6 × 2
# isoid              ID            
# <chr>              <chr>         
#     1 A/Hawaii/06/2019   EPI_ISL_342263
#   2 A/Hawaii/06/2019   EPI_ISL_353720 #keep; collection year matches isolate name, 2019

#   3 A/Michigan/82/2016 EPI_ISL_232046 #keep both; collection date differs by 6 days (3rd vs 9th), concatenate and use for uncertainty in collection date
#   4 A/Michigan/82/2016 EPI_ISL_241847

#   5 A/Ohio/27/2016     EPI_ISL_232044 #keep both; collection date differs by 1 day (30th vs 31st), concatenate and use for uncertainty in collection date
#   6 A/Ohio/27/2016     EPI_ISL_241846


set.seed(2024)
miso.alldupdseqs <- miso.alldupdseqs %>% 
  filter(!ID%in%c("EPI_ISL_342263")) %>%
  
  group_by(Isolate_Name)%>%
  mutate(Collection_Date = ifelse(ID%in%c("EPI_ISL_232046", "EPI_ISL_241847", "EPI_ISL_232044", "EPI_ISL_241846"), 
                                  paste0(min(Collection_Date[which(yearmatch)]), ":", max(Collection_Date[which(yearmatch)])), 
                                  as.character(Collection_Date))) %>%
  ungroup() %>% 
  
  group_by(Isolate_Name) %>%
  slice_sample(n=1) %>%
  ungroup()
  





miso <- miso %>% 
  filter(!alldupdseqs) %>%
  mutate(passage_type = case_when(grepl("original|or|clinical", tolower(Passage_History)) ~ "Original", 
                                  
                                  grepl("/|\\+|[a-z]{1}[1-9]{1}.?[a-z]{1}[1-9]{1}", tolower(Passage_History)) ~ "Multiple",
                                  !grepl(";", tolower(Passage_History)) & grepl(",", tolower(Passage_History)) ~ "Multiple",
                                  
                                  grepl("e[1-9]{1}|egg|embryonated", tolower(Passage_History)) ~ "EGG", 
                                  
                                  grepl("siat|s[0-9]{1}", tolower(Passage_History)) ~ "MDCK-SIAT", 
                                  
                                  grepl("mdck|mdck cells|m[1-9]{1}|mx", tolower(Passage_History)) ~ "MDCK", 
                                  
                                  grepl("rmk|rhmk|rii|pmk|prhmk|r[1-9]{1}", tolower(Passage_History)) ~ "RhMK", 
                                  
                                  grepl("c[:blank:]|c[1-9]{1}|cx|p[1-9]{1}|x[1-9]{1}", tolower(Passage_History)) ~ "Unknown Cell", 
                                  
                                  TRUE ~ tolower(Passage_History))) %>% 
  mutate(passage_type = factor(passage_type, 
                               levels = c("Original", "MDCK-SIAT", "MDCK", "EGG", "RhMK", "Unknown Cell", "Multiple", NA), 
                               labels = c("Original", "MDCK-SIAT", "MDCK", "EGG|RhMK|Unknown", "EGG|RhMK|Unknown", "EGG|RhMK|Unknown", "Multiple", "missing"), 
                               ordered = TRUE, 
                               exclude = NULL), 
         passage_numchars = gsub("[^0-9]", " ", gsub(";.+", "", gsub("\\(.+\\)", "", Passage_History)))) %>%
  (\(x){
    
    x$passage_naivedepth <- sapply(x$passage_numchars, 
                                   (\(y){
                                     
                                     y %>% strsplit(., split = " ") %>% unlist() %>% as.numeric() %>% sum(na.rm = T)
                                     
                                   }))
    
    
    x$ambigs <- sapply(x$seq, 
                       (\(y){
                         
                         gsub("A|T|C|G", "", y)
                         
                       }))
    
    x$ambigfreq <- nchar(x$ambigs)
    
    x$ambigprop <- x$ambigfreq / nchar(x$seq)
    
    x
                          
  })%>% 
  mutate(naivescore = as.numeric(passage_type) + ambigfreq + passage_naivedepth) %>%
  group_by(Isolate_Name) %>%
  mutate(best = naivescore==min(naivescore), 
         clearbest = sum(best)==1 | length(unique(whichones[best]))==1) %>% 
  ungroup() 


miso.clearbest <- miso %>% 
  filter(clearbest) %>%
  group_by(Isolate_Name)%>%
  mutate(Collection_Date = ifelse(length(unique(Collection_Date[which(yearmatch)]))>1, 
                                  paste0(min(Collection_Date[which(yearmatch)]), ":", max(Collection_Date[which(yearmatch)])), 
                                  as.character(Collection_Date))) %>%
  ungroup() %>% 
  filter(best) %>% 
  group_by(Isolate_Name) %>% 
  slice_sample(n=1) %>% 
  ungroup()



miso <- miso %>%
  filter(!clearbest) %>% 
  
  group_by(Isolate_Name) %>%
  mutate(betterseq = ambigfreq==min(ambigfreq), 
         singlecelltypepass = passage_type!="Multiple",
         betterchoice = best&betterseq&singlecelltypepass, 
         unclearchoice = sum(betterchoice)==1) %>%
  ungroup() %>%
  arrange(Isolate_Name, passage_type, naivescore, betterseq)


# > miso[which(!miso$unclearchoice),c(1,3)]
# # A tibble: 8 × 2
# isoid                ID            
# <chr>                <chr>         
#   1 A/Kentucky/09/2010   EPI_ISL_85572 
#   2 A/Kentucky/09/2010   EPI_ISL_85802 
#   3 A/Kentucky/09/2010   EPI_ISL_99105  #keep, less passage number and less ambiguous chars
#   4 A/New Mexico/09/2014 EPI_ISL_166301
#   5 A/New Mexico/09/2014 EPI_ISL_163480 #keep, better seq
#   6 B/Washington/01/2010 EPI_ISL_85667  #keep, other best is identical to seq that had higher number of passages, so maybe this one better
#   7 B/Washington/01/2010 EPI_ISL_86007 
#   8 B/Washington/01/2010 EPI_ISL_86040 


miso <- miso %>%
  mutate(betterchoice = ifelse(!unclearchoice, FALSE, betterchoice), 
         betterchoice = ifelse(ID%in%c("EPI_ISL_99105", "EPI_ISL_163480", "EPI_ISL_85667"), TRUE, betterchoice)) %>%
  group_by(Isolate_Name) %>% 
  mutate(unclearchoice = sum(betterchoice)==1) %>% 
  ungroup() %>%
  group_by(Isolate_Name)%>%
  mutate(Collection_Date = ifelse(length(unique(Collection_Date[which(yearmatch)]))>1, 
                                  paste0(min(Collection_Date[which(yearmatch)]), ":", max(Collection_Date[which(yearmatch)])), 
                                  as.character(Collection_Date))) %>%
  ungroup() %>% 
  filter(betterchoice)






seqs.df.clean <- bind_rows(uniqueiso%>%mutate(Collection_Date=as.character(Collection_Date)), 
                           miso.alldupdseqs, 
                           miso.clearbest, 
                           miso)


# > length(unique(seqs.df$Isolate_Name))
# [1] 36934
# > length(unique(seqs.df.clean$Isolate_Name))
# [1] 36934

# > table(duplicated(seqs.df$label)|duplicated(seqs.df$Isolate_Name), useNA = 'a')
# 
# FALSE  TRUE  <NA> 
#   36934  7068     0 
# > table(duplicated(seqs.df.clean$label)|duplicated(seqs.df.clean$Isolate_Name), useNA = 'a')
# 
# FALSE  <NA> 
#   36934     0 






## for outgroup taxon use oldest sequence for each subtype

# seqs.df.clean %>% group_by(Subtype) %>% mutate(oldest = Collection_Date==min(as.Date(Collection_Date))) %>% ungroup() %>% filter(oldest) %>% select(Subtype, ID)
# Subtype ID           
# <chr>   <chr>        
#   1 BVic    EPI_ISL_83921
#   2 H1      EPI_ISL_84194
#   3 H3      EPI_ISL_84067
#   4 BYam    EPI_ISL_85669



seqs.df <- seqs.df.clean







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


# set.seed(20230530)


for(ii in 1:nrow(combos)){
  
  
  if(!dir.exists(paste0("./01-Data/01-Processed-Data/Sequences/", combos$subtype[ii]))){
    dir.create(paste0("./01-Data/01-Processed-Data/Sequences/", combos$subtype[ii]))
  }

  this.outgroup <- seqs.df %>% 
    filter(Subtype%in%combos$subtype[ii] & ID%in%c("EPI_ISL_83921", "EPI_ISL_84194", "EPI_ISL_84067", "EPI_ISL_85669")) %>%
    mutate(Label = paste0(">", ID, "|", Collection_Date)) %>%
    select(Label, seq)
    
  these.seqs <- seqs.df %>% 
    filter(Subtype%in%combos$subtype[ii] & Collection_season%in%combos$season[ii] & Location3%in%combos$location[ii]) %>%
    mutate(Label = paste0(">", ID, "|", Collection_Date)) %>%
    select(Label, seq) %>%
    bind_rows(., 
              this.outgroup)
  
  if(nrow(these.seqs)>3){
    
    # if(nrow(these.seqs)>15){
    #   
    #   write.table(these.seqs, 
    #               file=paste0("./01-Data/01-Processed-Data/Sequences/", 
    #                           combos$subtype[ii], "/", 
    #                           combos$subtype[ii], "_", 
    #                           combos$season[ii], "_", 
    #                           combos$location[ii], 
    #                           "_n", nrow(these.seqs), "_rep0", ".fasta"), 
    #               sep="\n", 
    #               col.names = FALSE, 
    #               row.names = FALSE, 
    #               quote=FALSE)
    #   
    #   
    #   
    #   for(iii in 1:{ceiling(nrow(these.seqs)/15)*2}){
    #     
    #     this.subsample <- sample(1:nrow(these.seqs), sample(4:15, 1), replace = TRUE)
    #     
    #     these.seqs.resampled <- these.seqs[this.subsample,]
    #     
    #     for(j in 1:length(this.subsample)){
    #       these.seqs.resampled$Label[j] <- sub("\\|", paste0("0", j, "\\|"), these.seqs.resampled$Label[j])
    #     }
    #     
    #     
    #     write.table(these.seqs.resampled, 
    #                 file=paste0("./01-Data/01-Processed-Data/Sequences/", 
    #                             combos$subtype[ii], "/", 
    #                             combos$subtype[ii], "_", 
    #                             combos$season[ii], "_", 
    #                             combos$location[ii], 
    #                             "_n", length(this.subsample), "_rep", iii, ".fasta"), 
    #                 sep="\n", 
    #                 col.names = FALSE, 
    #                 row.names = FALSE, 
    #                 quote=FALSE)
    #     
    #     
    #   }
    #   
    #   
    # }else{
      write.table(these.seqs, 
                file=paste0("./01-Data/01-Processed-Data/Sequences/", 
                            combos$subtype[ii], "/", 
                            combos$subtype[ii], "_", 
                            combos$season[ii], "_", 
                            combos$location[ii], 
                            "_n", nrow(these.seqs)-1, "_rep0", ".fasta"), 
                sep="\n", 
                col.names = FALSE, 
                row.names = FALSE, 
                quote=FALSE)
      
    # }
    
    
  }
  
  
}





seqs.df <- seqs.df %>% 
  select(Subtype, Collection_season, Location3) %>% 
  rename(subtype = Subtype, season = Collection_season, location = Location3) %>% 
  group_by(subtype, season, location) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  full_join(., 
            combos, 
            by = c("subtype", "season", "location")) %>% 
  arrange(subtype, season, location)


save(seqs.df, file = "./01-Data/01-Processed-Data/sequences_freqs.rds")




rm(list = ls())
gc()






