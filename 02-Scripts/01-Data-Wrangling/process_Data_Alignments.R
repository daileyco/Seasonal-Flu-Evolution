# Script to write fasta files


## load data
load("./01-Data/01-Processed-Data/metadata.rds")


## packages
library(dplyr)
library(lubridate)


## helper functions







## read seqs into df
fastafiles <- list.files("./01-Data/00-Raw-Data/GISAID Sequences/subtypes", pattern = "[.]mafft$", full.names = T)



seqs.df.list <- list()


for(i in 1:length(fastafiles)){
  
  

  seqs <- readLines(fastafiles[i])
  
  seqs.df <- data.frame(label = seqs[which(substr(seqs, 1,1)==">")]) %>%
    mutate(ID = sub(">", "", label))
  
  start.indices <- which(substr(seqs, 1,1)==">")+1
  stop.indices <- c(which(substr(seqs, 1,1)==">")[-1]-1, length(seqs))
  
  seqs.df$seq <- NA
  
  for(k in seq_along(start.indices)){
    seqs.df$seq[k] <- paste0(seqs[start.indices[k]:stop.indices[k]], collapse = "")
  }
  
  
  seqs.df.list[[i]] <- seqs.df
  
}




seqs.df <- seqs.df.list %>% 
  bind_rows() 


seqs.df <- left_join(seqs.df, 
                     meta, 
                     by = c("ID"="Isolate_Id"))



## trim to coding regions

# starts <- regexpr("atg", seqs.df$seq) %>% 
#   c()
# 
# # # table(starts, seqs.df$subtype, useNA = 'a')
# # starts  BVic  BYam    H1    H3  <NA>
# # 34    7137  5315     0     0     0
# # 37       0     0 13673     0     0
# # 46       0     0     0 23463     0
# # 58       0     0     4     0     0
# # 63       0     0     1     0     0
# # 78       0     0     0    26     0
# # 83       0     0    10     0     0
# # 96       0     0    12     0     0
# # 110      0     0     1    82     0
# # 155      0     0     1     0     0
# # <NA>    30     1   190   247     0



seqs.df$seq <- substr(seqs.df$seq, 
                      case_when(seqs.df$subtype %in% c("BVic", "BYam") ~ 34, 
                                seqs.df$subtype %in% c("H1") ~ 37, 
                                seqs.df$subtype %in% c("H3") ~ 46, 
                                TRUE ~ 1), 
                      nchar(seqs.df$seq))




# codons <- bind_cols(start = seq(1, max(nchar(seqs.df$seq1), na.rm = T), by = 3), 
#                     stop = c(seq(1, max(nchar(seqs.df$seq1), na.rm = T), by = 3)[-1]-1, max(nchar(seqs.df$seq1))))
# 
# 
# vsubstr <- Vectorize(substr, vectorize.args = c("start", "stop")); 
# 
# stops <- lapply(seqs.df$seq, 
#                 (\(x){
#                   
#                   stopcodon <- which(vsubstr(x, codons$start, codons$stop)%in%c("tga", "tag", "taa"))
# 
#                   stopsite <- stopcodon*3
#                   
#                   ifelse(is.null(stopsite), NA, stopsite)
#                 })) %>% 
#   unlist()
# 
# 
# # # table(stops, seqs.df$subtype, useNA = 'a')
# # stops   BVic  BYam    H1    H3  <NA>
# # 1701     0     0     0 23486     0
# # 1710     0     0 13644     0     0
# # 1716     0     0     1     0     0
# # 1755     0  5315     0     0     0
# # 1758  7136     0     0     0     0
# # 1833     1     0     0     0     0
# # <NA>    30     1   247   332     0

seqs.df$seq <- substr(seqs.df$seq, 
                      1,
                      case_when(seqs.df$subtype %in% c("BVic") ~ 1758,
                                seqs.df$subtype %in% c("BYam") ~ 1755,
                                seqs.df$subtype %in% c("H1") ~ 1710, 
                                seqs.df$subtype %in% c("H3") ~ 1701, 
                                TRUE ~ nchar(seqs.df$seq)))







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
         yearname = sub("^.+/{1}([0-9]{4}).*$", "\\1", Isolate_Name), 
         yeardate = as.character(year(as.Date(Collection_Date, tryFormats = c("%Y-%m-%d", "%Y-%m", "%Y")))), 
         yearmatch = yearname==yeardate) %>%
  ungroup()



miso.alldupdseqs <- miso %>% 
  filter(alldupdseqs)


# > print(miso.alldupdseqs[which(!miso.alldupdseqs$samemeta), c(1,3)], n=50)
# # A tibble: 28 × 2

# isoid                        ID              
# <chr>                        <chr>          

# 11 A/Hawaii/06/2019             EPI_ISL_342263  
# 12 A/Hawaii/06/2019             EPI_ISL_353720  #keep; collection year matches isolate name, 2019

## all others dates off at most months, not years, concatenated
#   1 A/Baltimore/0244/2017        EPI_ISL_17027265
# 2 A/Baltimore/0244/2017        EPI_ISL_263443  

# 3 A/Baltimore/0284/2017        EPI_ISL_17027266
# 4 A/Baltimore/0284/2017        EPI_ISL_263406  

# 5 A/Baltimore/R0145/2017       EPI_ISL_18043281
# 6 A/Baltimore/R0145/2017       EPI_ISL_17034400

# 7 A/Baltimore/R0232/2018       EPI_ISL_18043290
# 8 A/Baltimore/R0232/2018       EPI_ISL_17034888

# 9 A/Baltimore/R0243/2018       EPI_ISL_18043296
# 10 A/Baltimore/R0243/2018       EPI_ISL_17034889


# 13 A/Michigan/82/2016           EPI_ISL_241847  
# 14 A/Michigan/82/2016           EPI_ISL_232046  

# 15 A/New York City/PV02895/2019 EPI_ISL_4072785 
# 16 A/New York City/PV02895/2019 EPI_ISL_15842298

# 17 A/New York/PV00528/2018      EPI_ISL_326929  
# 18 A/New York/PV00528/2018      EPI_ISL_17049059

# 19 A/New York/PV01148/2018      EPI_ISL_326910  
# 20 A/New York/PV01148/2018      EPI_ISL_17049060

# 21 A/New York/PV01516/2018      EPI_ISL_326895  
# 22 A/New York/PV01516/2018      EPI_ISL_17053722

# 23 A/New York/PV01536/2018      EPI_ISL_326885  
# 24 A/New York/PV01536/2018      EPI_ISL_17049058

# 25 A/Ohio/27/2016               EPI_ISL_241846  
# 26 A/Ohio/27/2016               EPI_ISL_232044  

# 27 B/Hawaii/01/2013             EPI_ISL_145969  
# 28 B/Hawaii/01/2013             EPI_ISL_143227




set.seed(2024)
miso.alldupdseqs <- miso.alldupdseqs %>% 
  filter(!ID%in%c("EPI_ISL_342263")) %>%
  
  group_by(Isolate_Name)%>%
  mutate(Collection_Date = case_when(length(unique(Collection_Date))==1 ~ Collection_Date, 
                                     length(unique(nchar(Collection_Date)))>1 ~ unique(Collection_Date[which(nchar(Collection_Date)==min(nchar(Collection_Date)))]), 
                                     TRUE ~ paste0(min(Collection_Date), ":", max(Collection_Date)))) %>%
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
                                  
                                  grepl("unknown|[.]", tolower(Passage_History)) ~ NA,
                                  
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
                         
                         gsub("a|A|t|T|c|C|g|G", "", y)
                         
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


# > miso[which(!miso$unclearchoice),c(1,3)]
# # A tibble: 18 × 2
# isoid                 ID            
# <chr>                 <chr>         
#  1 A/Indiana/15/2015     EPI_ISL_200778 # keep
#  2 A/Indiana/15/2015     EPI_ISL_195815
#  3 A/Indiana/15/2015     EPI_ISL_228175
#  4 A/Indiana/15/2015     EPI_ISL_228175
#  
#  5 A/Kentucky/09/2010    EPI_ISL_85572 
#  6 A/Kentucky/09/2010    EPI_ISL_85802 
#  7 A/Kentucky/09/2010    EPI_ISL_99105 #keep, less passage number and less ambiguous chars
#  
#  8 A/Maryland/63_16/2013 EPI_ISL_370324 #duplicats, random
#  9 A/Maryland/63_16/2013 EPI_ISL_370324
#  
# 10 A/Montana/01/2010     EPI_ISL_77693 
# 11 A/Montana/01/2010     EPI_ISL_77694 # keep
# 
# 12 A/New Mexico/09/2014  EPI_ISL_166301
# 13 A/New Mexico/09/2014  EPI_ISL_163480 #keep, better seq
# 
# 14 B/Washington/01/2010  EPI_ISL_85667  #keep
# 15 B/Washington/01/2010  EPI_ISL_86007 
# 16 B/Washington/01/2010  EPI_ISL_246626
# 17 B/Washington/01/2010  EPI_ISL_86040 
# 18 B/Washington/01/2010  EPI_ISL_246627



miso <- miso %>%
  filter(!duplicated(ID)) %>%
  mutate(betterchoice = ifelse(!unclearchoice, FALSE, betterchoice), 
         betterchoice = ifelse(ID%in%c("EPI_ISL_200778", 
                                       "EPI_ISL_99105", 
                                       "EPI_ISL_77694", 
                                       "EPI_ISL_163480", 
                                       "EPI_ISL_85667", 
                                       "EPI_ISL_370324"), TRUE, betterchoice)) %>%
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
# [1] 42113
# > length(unique(seqs.df.clean$Isolate_Name))
# [1] 42113

# > table(duplicated(seqs.df$label)|duplicated(seqs.df$Isolate_Name), useNA = 'a')
# 
# FALSE  TRUE  <NA> 
#   42113  7612     0 
# > table(duplicated(seqs.df.clean$label)|duplicated(seqs.df.clean$Isolate_Name), useNA = 'a')
# 
# FALSE  <NA> 
#   42113     0 






## for outgroup taxon use oldest 3 sequences for each subtype

seqs.df.clean %>%
  filter(nchar(Collection_Date)==10) %>%
  arrange(subtype, Collection_Date) %>%
  group_by(subtype) %>%
  mutate(rn = row_number()) %>%
  ungroup() %>%
  filter(rn <= 3) %>%
  select(subtype, ID)
# # # A tibble: 12 × 2
# # subtype ID              
# # <chr>   <chr>           
# # 1 BVic    EPI_ISL_83930   
# # 2 BVic    EPI_ISL_85659   
# # 3 BVic    EPI_ISL_71453 
# # c("EPI_ISL_71453", "EPI_ISL_71454", "EPI_ISL_71455")

# # 4 BYam    EPI_ISL_76940   
# # 5 BYam    EPI_ISL_77930   
# # 6 BYam    EPI_ISL_76942  
# # c("EPI_ISL_76940", "EPI_ISL_77930", "EPI_ISL_76942")

# # 7 H1      EPI_ISL_84161   
# # 8 H1      EPI_ISL_77727   
# # 9 H1      EPI_ISL_77728   
# # c("EPI_ISL_71398", "EPI_ISL_71406", "EPI_ISL_71431")

# # 10 H3      EPI_ISL_125870  
# # 11 H3      EPI_ISL_13531313
# # 12 H3      EPI_ISL_136605  
# # c("EPI_ISL_158599", "EPI_ISL_74088", "EPI_ISL_76705")


outgroupids <- c("EPI_ISL_71453", "EPI_ISL_71454", "EPI_ISL_71455", "EPI_ISL_76940", 
                 "EPI_ISL_77930", "EPI_ISL_76942", "EPI_ISL_71398", "EPI_ISL_71406", 
                 "EPI_ISL_71431", "EPI_ISL_158599", "EPI_ISL_74088", "EPI_ISL_76705")



seqs.df <- seqs.df.clean %>% 
  
  mutate(Collection_year = substr(Collection_Date, 1,4))




save(seqs.df, file = "./01-Data/01-Processed-Data/seqsdf_clean.rds")




rm(list = ls())
gc()






