# Influenza Sequence Census

# subtype.dirs <- c("B-vic", "B-yam", "H1", "H3")




##Process seqmeta files Lambo created
seqmetas.files <- c("./01-Data/00-Raw-Data/B-vic/B-Vic_seqmeta_USseason10-20.csv",
                    "./01-Data/00-Raw-Data/B-yam/B-yam_seqmeta_USseason10-20.csv",
                    "./01-Data/00-Raw-Data/H1/H1_seqmeta_USseason10-20.csv"#,  
                    #"./01-Data/00-Raw-Data/H3/H3_seqmeta_USseason10-20.csv"#something weird happened with this file, use a backup
                    )

seqmetas.list <- lapply(seqmetas.files, 
                        function(x){
                          read.csv(x)
                        })


library(dplyr)

seqmetas.df <- seqmetas.list %>% 
  bind_rows()


smh3 <- read.delim("./01-Data/00-Raw-Data/H3/H3-HA-seq+meta.tsv", sep = "\t") %>% 
  mutate(Host_Age = ifelse(Host_Age%in%c(999), NA, as.integer(Host_Age)), 
         id = Isolate_Id)

## checked above file with fasta file and they agree, except fasta has 41 more seqs, none from US
# seqh3 <- readLines("./01-Data/00-Raw-Data/H3/raw_data/HA-sequence/H3-HA.final.fasta")
# seqh3 <- data.frame(label = seqh3[which(substr(seqh3, 1,1)==">")], 
#                     seq = seqh3[-which(substr(seqh3, 1,1)==">")])
# 
# labelsh3 <- seqh3$label
# 
# labelsh3 <- sapply(labelsh3, 
#                    strsplit, split = "\\|")
# 
# library(purrr)
# 
# idsh3 <- lapply(labelsh3, pluck, 1)
# idsh3 <- sapply(unlist(idsh3), gsub, pattern = ">", replacement = "")



seqmetas.df <- bind_rows(seqmetas.df, 
                         smh3)



# no h3 seqs were indicated to be removed, checked directory for related file, read in, and remove ids present
h3remove <- readLines("./01-Data/00-Raw-Data/H3/raw_data/HA-sequence/removeseqs.txt")
h3remove <- h3remove[which(substr(h3remove, 1,3)%in%c("EPI"))]
h3remove <- sapply(h3remove, 
                   function(x){
                     gsub("^(.+)\\|[A-Z].+$", "\\1", x)
                   })

seqmetas.df <- seqmetas.df %>% filter(!id%in%h3remove)







## Process raw meta data files for comparison





raw.meta.files <- c("./01-Data/00-Raw-Data/B-vic/raw_data/B-Vic-post2012.csv", 
                    "./01-Data/00-Raw-Data/B-vic/raw_data/Bv-Vic-pre2012.xls",
                    
                    "./01-Data/00-Raw-Data/B-yam/raw_data/B-yam-post2008.xlsx", 
                    
                    "./01-Data/00-Raw-Data/H1/raw_data/meta/H1-2009-2016.csv", 
                    "./01-Data/00-Raw-Data/H1/raw_data/meta/H1-2016-2018.csv", 
                    "./01-Data/00-Raw-Data/H1/raw_data/meta/H1-2018-2019.csv", 
                    "./01-Data/00-Raw-Data/H1/raw_data/meta/H1-2019on.csv", 
                    
                    "./01-Data/00-Raw-Data/H3/raw_data/meta/old/H3-2015.xls",
                    "./01-Data/00-Raw-Data/H3/raw_data/meta/old/H3-2015-2017.xls", 
                    "./01-Data/00-Raw-Data/H3/raw_data/meta/old/H3-2017-2019.xls", 
                    "./01-Data/00-Raw-Data/H3/raw_data/meta/old/H3-2019on.xls"
                    )


library(readr)
library(readxl)

raw.meta.list <- lapply(raw.meta.files, 
                        function(x){
                          fext <- gsub("^[.].*?[.].*?", "\\1", x)
                          metadata <- do.call(paste0("read_", fext), list(x, col_types = ifelse(fext=="csv", list(.default=col_character()), "text")))
                          metadata <- metadata %>% mutate(across(everything(), ~as.character(.x)))
                          return(metadata)
                        })

raw.meta.df <- bind_rows(raw.meta.list)


raw.meta.df <- bind_cols(raw.meta.df, 
                         strsplit(raw.meta.df$Location, "/") %>% 
                           lapply(., 
                                  function(x){
                                    setNames(x,
                                             nm = paste0("Location", 
                                                         1:length(x)))
                                    }
                                  ) %>% 
                           bind_rows()
                         )

# N = 171716



library(lubridate)

raw.meta.df <- raw.meta.df %>% 
  mutate(across(matches("Location"), ~trimws(.x)), 
         Collection_Date = as.Date(Collection_Date), 
         Collection_epiweek = epiweek(Collection_Date), 
         Collection_year = year(Collection_Date), 
         Collection_season = ifelse(Collection_epiweek<40,
                                    paste0(Collection_year-1, "-", Collection_year), 
                                    paste0(Collection_year, "-", Collection_year+1)), 
         kept = Isolate_Id %in% seqmetas.df$id
  )





# raw.meta.df %>% 
#   group_by(Location1, Location2, Subtype, Lineage, Collection_season) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   arrange(desc(n))



raw.meta.df.us <- raw.meta.df %>%
  filter(grepl("United States", Location))

# N = 48749

raw.meta.df.us1020 <- raw.meta.df.us %>%
  filter(Collection_season %in% c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015",
                                  "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
         )

# N = 45305




# # compare raw metadata to seqmeta files
# table(raw.meta.df.us1020$Subtype, raw.meta.df.us1020$Isolate_Id%in%seqmetas.df$Isolate_Id)
# #          FALSE  TRUE
# # A / H1N1   541 11312
# # A / H1N2    20     5
# # A / H3N2     0 22237
# # B          475 10715


# # compare raw metadata to seqmeta files
# table(raw.meta.df.us1020$Subtype, raw.meta.df.us1020$Isolate_Id%in%seqmetas.df$Isolate_Id)
# #          FALSE  TRUE
# # A / H1N1   541 11312
# # A / H1N2    20     5
# # A / H3N2    51 22186
# # B          475 10715



raw.meta.df.us1020 <- raw.meta.df.us1020 %>% 
  filter(!Subtype%in%c("A / H1N2"))

# N = 45280










ustates <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", #"Central Province", 
             "Colorado", "Connecticut", "Delaware", "District of Columbia", 
             "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
             "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
             "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
             "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
             "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
             "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", #"Rhoded Island", 
             "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
             "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
             "Wyoming"#, NA
)


raw.meta.df.us1020 <- raw.meta.df.us1020 %>% 
  mutate(Location3b = ifelse(is.na(Location3), gsub("^[A-Za-z]/{1}(.+)/.*/.*$", "\\1", Isolate_Name), Location3), 
         Location3 = ifelse(Location3b%in%ustates, Location3b, Location3)) %>%
  mutate(Location3 = case_when(!is.na(Location3) ~ Location3,
                               Location3b%in%c("Colorado/06") ~ "Colorado", 
                               Location3b%in%c("DC", "Distric of Columbia", "District of Colombia", "District Of Columbia") ~ "District of Columbia",
                               Location3b%in%c("DELAWARE", "Delware") ~ "Delaware",
                               Location3b%in%c("FLORIDA") ~ "Florida", 
                               Location3b%in%c("INDIANA") ~ "Indiana", 
                               Location3b%in%c("Kansas/14") ~ "Kansas", 
                               Location3b%in%c("NC") ~ "North Carolina", 
                               Location3b%in%c("Okahoma") ~ "Oklahoma", 
                               Location3b%in%c("PENNSYLVANIA") ~ "Pennsylvania", 
                               Location3b%in%c("SouthCarolina") ~ "South Carolina", 
                               Location3b%in%c("TEXAS", "Texas/50") ~ "Texas", 
                               Location3b%in%c("WISCONSIN") ~ "Wisconsin", 
                               Location3b%in%c("WYOMING") ~ "Wyoming", 
                               
                               # need to double check cities, especially for pop names
                               Location3b%in%c("Baltimore", "EastBaltimore") ~ "Maryland", 
                               Location3b%in%c("Boston") ~ "Massachusetts", 
                               Location3b%in%c("Bronx", "Brooklyn") ~ "New York",
                               Location3b%in%c("Rochester") ~ "New York", #less confidence
                               Location3b%in%c("Chicago") ~ "Illinois", 
                               Location3b%in%c("Dayton") ~ "Ohio", 
                               Location3b%in%c("Durham") ~ "North Carolina", #less confidence
                               Location3b%in%c("Gainesville") ~ "Florida", #less confidence
                               Location3b%in%c("Houston") ~ "Texas", 
                               Location3b%in%c("Minneapolis") ~ "Minnesota", 
                               Location3b%in%c("San_Diego", "Santa Clara") ~ "California", 
                               TRUE ~ Location3
  )) %>%
  mutate(Location3 = ifelse(Location3 %in% c("Rhoded Island"), "Rhode Island", Location3)) %>%
  filter(Location3%in%ustates)

# N = 45061






# library(flextable)

census <- raw.meta.df.us1020 %>% 
  group_by(Location1, Location2, Subtype, Lineage, Collection_season) %>% 
  summarise(n = n(), 
            n_clean = sum(kept)) %>% 
  ungroup() %>% 
  arrange(Collection_season, Subtype, Lineage) #%>%
  # flextable() %>%
  # autofit() %>%
  # save_as_docx(path = "./03-Output/01-Tables/census.docx")
  

library(tidyr)

census.by.state <- raw.meta.df.us1020 %>% 
  group_by(Location3, Subtype, Lineage, Collection_season) %>% 
  summarise(n_clean = sum(kept)) %>% 
  ungroup() %>% 
  arrange(Collection_season, Subtype, Lineage, Location3) %>%
  pivot_wider(names_from = Collection_season, values_from = n_clean) %>%
  arrange(Subtype, Lineage, Location3) #%>%
  # flextable() %>%
  # autofit() %>%
  # save_as_docx(path = "./03-Output/01-Tables/census_by_state.docx")


save(census, census.by.state, file = "./03-Output/01-Tables/census.rdata")





meta.df.us1020 <- raw.meta.df.us1020 %>%
  filter(kept==TRUE)


# N = 44012



save(meta.df.us1020, file = "./01-Data/01-Processed-Data/metadata_us1020.rds")




rm(list = ls())
gc()




