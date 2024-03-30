# script to tidy up correlations results and to make nice table


## load data
# load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")
load("./01-Data/02-Analytic-Data/correlations.rds")

# load("./01-Data/02-Analytic-Data/treesdf.rds")
# load("./01-Data/02-Analytic-Data/mycorrs.rds")

## helper functions



## packages
library(dplyr)
library(tidyr)



## unpack data from variable names
cors <- cordf %>%
  
  mutate(across(c(x,y), 
                list(var = ~sub("_.*", "", .x), 
                     subtype = ~sub(".*_", "", .x)), 
                .names = "{.fn}_{.col}"), 
         slagtype = case_when(grepl("nlag", y) ~ "Network", 
                          grepl("slag", y) ~ "Spatial", 
                          TRUE ~ "Auto"),
         slag = ifelse(!slagtype%in%c("Auto"), sub(".*[s|n]lag([0-9]+).*", "\\1", y), 0), 
         tlag = ifelse(grepl("tlag", y), sub(".*tlag([0-9]+).*", "\\1", y), 0)) %>%
  select(-x, -y) %>%
  relocate(matches("cor"), .after = tlag)


## make into neat table
table.cors <- cors %>%
  mutate(cor = ifelse(cor50==1, 
                       1, 
                       paste0(round(cor50,3), " (", round(cor2.5, 3), ", ", round(cor97.5, 3), ")"))) %>%
  select(-cor50, -cor2.5, -cor97.5) %>%
  pivot_wider(names_from = c(subtype_y, tlag), values_from = cor) %>%
  rename(Subtype = subtype_x, 
         `Spatial Lag Type` = slagtype, 
         `Spatial Lag` = slag) %>%
  select(-var_x, -var_y) %>%
  add_row(., 
          names(.) %>% 
            (\(x){
              case_when(grepl("_", x) ~ sub(".*_", "", x), 
                        TRUE ~ NA) %>% 
                setNames(., x) %>% 
                ifelse(.==0 & !duplicated(.), 
                       paste0("Tlag = ", .), 
                       .)
            }) %>% 
            t() %>% 
            as.data.frame(), 
          .before = 1) %>%
  select(1:3, order(names(.)[4:ncol(.)])+3) %>%
  setNames(., nm = sub("_.*", "", names(.)))
  
  




# table.corrs <- mycorrs %>% 
#   select(subtypex, subtypey, nbtype, tlag, corr.round) %>% 
#   mutate(nbtype = ifelse(is.na(nbtype), "Auto", nbtype), 
#          corr.round = ifelse(corr.round=="1***", "1", corr.round)) %>% 
#   pivot_wider(names_from = subtypey, values_from = corr.round) %>% 
#   pivot_wider(names_from = tlag, values_from = 4:7)
# 
# 
# 
# 
# table.autocorrs <- mycorrs %>% 
#   filter(cortype%in%c("Auto")) %>% 
#   select(subtypex, subtypey, nbtype, tlag, corr.round) %>% 
#   mutate(nbtype = ifelse(is.na(nbtype), "Auto", nbtype), 
#          corr.round = ifelse(corr.round=="1***", "1", corr.round)) %>% 
#   pivot_wider(names_from = nbtype, values_from = corr.round)
# 
# 
# 
# 
# table.crosscorrs <- mycorrs %>% 
#   select(subtypex, subtypey, cortype, nbtype, tlag, corr.round) %>% 
#   mutate(nbtype = ifelse(is.na(nbtype), "Auto", nbtype), 
#          corr.round = ifelse(corr.round=="1***", "1", corr.round)) %>% 
#   pivot_wider(names_from = subtypey, values_from = corr.round) %>% 
#   filter(cortype == "Cross") %>%
#   select(-cortype) %>%
#   # mutate(nbtype = factor(nbtype, levels = c("Auto", "Spatial", "Network"), ordered = TRUE)) %>% 
#   # arrange(subtypex, tlag, nbtype) %>%
#   pivot_wider(names_from = tlag, values_from = 4:7)
# 
# 
# 
# 
# save(table.corrs, 
#      table.autocorrs, 
#      table.crosscorrs, 
#      file = "./03-Output/01-Tables/tables_corrs.rdata")



## save
save(cors, 
     file = "./01-Data/02-Analytic-Data/correlations_clean.rds")

save(table.cors,
     file = "./03-Output/01-Tables/tables_cors.rds")


## clean environment
rm(list=ls())
gc()







