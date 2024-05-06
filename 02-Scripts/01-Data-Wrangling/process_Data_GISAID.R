# script to process metadata

## load data

## packages
library(readxl)
library(dplyr)


## helper functions






## files

metafiles <- list.files("./01-Data/00-Raw-Data/GISAID Metadata", full.names = TRUE)



## aggregate in df
meta <- lapply(metafiles, 
               (\(x){
                 read_xls(x, col_types = "text")
               })) %>%
  bind_rows(.id = "metafile")




# N=52254 + 4 outgroups
# 
# N unique isolates = 44628 + 4 outgroups
# 
# N isolates with single sequence = 37694
# N isolates with multiple sequences = 6934 isos with 14560 seqs


## simple exclusions

###dates 
####okay 2010-2020, will keep early 2010 for outgroups

###subtype

meta <- meta %>% 
  mutate(subtype = case_when(Subtype%in%c("A / H1", "A / H1N1") ~ "H1", 
                             Subtype%in%c("A / H3", "A / H3N2") ~ "H3", 
                             Lineage%in%c("Victoria") ~ "BVic", 
                             Lineage%in%c("Yamagata") ~ "BYam", 
                             TRUE ~ NA)) %>%
  # mutate(subtype = case_when(Isolate_Id%in%c("EPI_ISL_6587") ~ "BYam",  #outgroups c("EPI_ISL_6587", "EPI_ISL_6726", "EPI_ISL_7047", "EPI_ISL_20973")
  #                            Isolate_Id%in%c("EPI_ISL_6726") ~ "H3", 
  #                            Isolate_Id%in%c("EPI_ISL_7047") ~ "H1", 
  #                            Isolate_Id%in%c("EPI_ISL_20973") ~ "BVic", 
  #                            TRUE ~ subtype)) %>%
  filter(!is.na(subtype))

# N = 50360 + 4

# meta %>% 
#   group_by(subtype, Isolate_Name) %>% 
#   summarise(nseqs = n()) %>% 
#   ungroup() %>% 
#   group_by(subtype) %>% 
#   summarise(nisos = n(), 
#             seqsperiso = mean(nseqs), 
#             nseqs = sum(nseqs)) %>% 
#   ungroup() %>% 
#   View()
# 
# structure(list(subtype = c("BVic", "BYam", "H1", "H3"), 
#                nisos = c(6403L, 4677L, 12550L, 19221L), 
#                seqsperiso = c(1.11994377635483, 1.13790891597178,1.11450199203187, 1.24239113469643), 
#                nseqs = c(7171L, 5322L, 13987L, 23880L)), 
#           class = c("tbl_df", "tbl", "data.frame"), 
#           row.names = c(NA,-4L))


meta <- meta %>%
  (\(x){
    locs <- strsplit(x$Location, "/") %>%
      lapply(., 
             (\(y){
               setNames(y, 
                        nm = paste0("Location", 1:length(y))) %>% 
                 trimws()
             })) %>% 
      bind_rows()
    
    bind_cols(x, locs)
  })



ustates <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", #"Central Province", 
             "Colorado", "Connecticut", "Delaware", "District of Columbia", 
             "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
             "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
             "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
             "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
             "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
             "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", #"Rhoded Island", 
             "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", #"USA",
             "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
             "Wyoming"#, NA
             )




meta <- meta %>% 
  mutate(Location3b = ifelse(is.na(Location3), 
                             gsub("^[A-Za-z]/{1}(.+)/.*/.*$", "\\1", Isolate_Name), 
                             Location3), 
         Location3 = ifelse(Location3b%in%ustates, 
                            Location3b, 
                            Location3))


meta <- meta %>%
  mutate(Location3 = case_when(!is.na(Location3) ~ Location3,
                               Location3b%in%c("A/Washington2958/2012", "Washington/02") ~ "Washington",
                               
                               Location3b%in%c("Baltimore", "EastBaltimore", "Bethesda", "MARYLAND") ~ "Maryland", 
                               Location3b%in%c("Boston") ~ "Massachusetts",
                               # Location3b%in%c("Cambridge") ~ "Massachusetts",
                               Location3b%in%c("Bronx", "Brooklyn", "Human/New York City", "New York City") ~ "New York",
                               
                               Location3b%in%c("CALIFORNIA", "San_Diego", "Santa Clara", "Los Angeles") ~ "California", 
                               
                               Location3b%in%c("Chicago") ~ "Illinois", 
                               
                               Location3b%in%c("Colorado/06") ~ "Colorado", 
                               
                               # Location3b%in%c("Columbia") ~ "South Carolina",
                               
                               Location3b%in%c("Dayton") ~ "Ohio", 
                               Location3b%in%c("DC", "Distric of Columbia", "District of Colombia", "District Of Columbia") ~ "District of Columbia",
                               
                               Location3b%in%c("DELAWARE", "Delware") ~ "Delaware",
                               
                               Location3b%in%c("Durham", "NC") ~ "North Carolina",
                               
                               Location3b%in%c("FLORIDA", "Gainesville") ~ "Florida", 
                               
                               Location3b%in%c("HAWAII") ~ "Hawaii", 
                               
                               Location3b%in%c("Houston") ~ "Texas", 
                               
                               Location3b%in%c("INDIANA", "Indiana/02") ~ "Indiana", 
                               
                               Location3b%in%c("IOWA") ~ "Iowa",
                               
                               Location3b%in%c("Memphis") ~ "Tennessee",
                               
                               Location3b%in%c("Michigan/173", "Michigan/45") ~ "Iowa",
                               
                               
                               Location3b%in%c("Minneapolis") ~ "Minnesota", 
                               
                               Location3b%in%c("Kansas/14") ~ "Kansas", 
                               Location3b%in%c("Okahoma") ~ "Oklahoma", 
                               
                               Location3b%in%c("PENNSYLVANIA", "Pennsylvania/1025") ~ "Pennsylvania", 
                               
                               Location3b%in%c("Rhoded Island") ~ "Rhode Island",
                               
                               Location3b%in%c("Rochester") ~ "New York", 
                               
                               Location3b%in%c("Santa Fe") ~ "New Mexico",
                               
                               Location3b%in%c("SouthCarolina") ~ "South Carolina", 
                               
                               Location3b%in%c("TEXAS", "Texas/50") ~ "Texas", 
                               Location3b%in%c("WISCONSIN") ~ "Wisconsin", 
                               Location3b%in%c("WYOMING") ~ "Wyoming", 
                               
                               
                               TRUE ~ Location3
  )) %>%
  mutate(Location3 = ifelse(Location3 %in% c("Rhoded Island"), "Rhode Island", Location3)) %>%
  filter(Location3%in%ustates)

# N = 50067 +4

# 
# length(unique(meta$Isolate_Name))
# N unique isolates = 42560 +4
# 
# length(unique(meta$Isolate_Name[which(!meta$Isolate_Name%in%meta$Isolate_Name[which(duplicated(meta$Isolate_Name))])]))
# N isolates with single sequence = 35741 +4
# length(unique(meta$Isolate_Name[which(meta$Isolate_Name%in%meta$Isolate_Name[which(duplicated(meta$Isolate_Name))])]))
# length(meta$Isolate_Name[which(meta$Isolate_Name%in%meta$Isolate_Name[which(duplicated(meta$Isolate_Name))])])
# N isolates with multiple sequences = 6819 isos with 14326 seqs





## save 
save(meta, file = "./01-Data/01-Processed-Data/metadata.rds")


## clean environment
rm(list = ls())
gc()

