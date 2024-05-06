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





# outgroupids <- c("EPI_ISL_71453", #"EPI_ISL_71454", "EPI_ISL_71455", 
#                  "EPI_ISL_76940", #"EPI_ISL_77930", "EPI_ISL_76942", 
#                  "EPI_ISL_71398", #"EPI_ISL_71406", "EPI_ISL_71431", 
#                  "EPI_ISL_158599" #,"EPI_ISL_74088", "EPI_ISL_76705"
#                  )
outgroupids <- c("EPI_ISL_6587", "EPI_ISL_6726", "EPI_ISL_7047", "EPI_ISL_20973")


# write in loop



for(ii in 1:nrow(combos)){
  
  
  if(!dir.exists(paste0("./01-Data/01-Processed-Data/Sequences/", combos$subtype[ii]))){
    dir.create(paste0("./01-Data/01-Processed-Data/Sequences/", combos$subtype[ii]))
  }

  this.outgroup <- seqs.df %>%
    filter(subtype%in%combos$subtype[ii] & ID%in%outgroupids) %>%
    mutate(Label = paste0(">", ID, "|", Collection_Date)) %>%
    select(Label, seq)
    
  these.seqs <- seqs.df %>% 
    filter(subtype%in%combos$subtype[ii] & Location3%in%combos$location[ii]) %>%
    filter(Collection_year %in% c(combos$season1[ii], combos$season2[ii])) %>%
    mutate(Label = paste0(">", ID, "|", Collection_Date)) %>%
    select(Label, seq) 
  
  nseqs <- nrow(these.seqs)
  
  # if(!combos$season[ii]%in%c("2010-2011", "2011-2012")){
  # 
  #   these.seqs <- these.seqs %>%
  #   bind_rows(.,
  #             this.outgroup)
  # }
  
  these.seqs <- these.seqs %>%
    bind_rows(.,
              this.outgroup)
  
  
  
  if(nseqs>2){
    
      write.table(these.seqs, 
                file=paste0("./01-Data/01-Processed-Data/Sequences/", 
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





# siphon off some summary stats


combos <- expand.grid(subtype = subtypes,
                      location = ustates, 
                      year = as.character(2010:2020))

seqs.df <- seqs.df %>% 
  select(subtype, Collection_Date, location = Location3, Isolate_Name) %>% 
  # filter(nchar(Collection_Date)==10) %>%
  mutate(year = sub("^.+/.+/.+/(20[0-9]{2}).*$", "\\1", Isolate_Name)
         # , 
         # Collection_Date = as.Date(Collection_Date), 
         # season = case_when(epiweek(Collection_Date)>=30 ~ paste0(year(Collection_Date), "-", year(Collection_Date)+1), 
         #                    TRUE ~ paste0(year(Collection_Date)-1, "-", year(Collection_Date)))
         ) %>% 
  mutate(year = case_when(Isolate_Name %in% c("A/New York/WC-LVD-14-102/0214") ~ "2014",
                          year %in% c(2009) ~ "2010",
                          TRUE ~ year)) %>% 
  # filter(season %in% unique(combos$season)) %>% 
  # rename(subtype = subtype, season = Collection_season, location = Location3) %>% 
  group_by(subtype, year, location) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  full_join(., 
            combos, 
            by = c("subtype", "year", "location")) %>% 
  arrange(subtype, year, location) %>%
  filter(year>2009)


## save
save(seqs.df, file = "./01-Data/01-Processed-Data/sequences_freqs.rds")



## clean environment
rm(list = ls())
gc()






