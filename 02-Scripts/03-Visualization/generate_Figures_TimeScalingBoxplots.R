


# load("./01-Data/02-Analytic-Data/mol_epi_reporting.rdata")
library(dplyr)


# molecular_epi_reporting %>% mutate(pdiff = p.ili-ifelse(is.nan(p.seqs), 0, p.seqs)) %>% group_by(season, location) %>% summarise(avgdiff = mean(pdiff, na.rm = T)) %>% ungroup() %>% mutate(across(c(season, location), ~factor(.x))) %>% View()








load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")




# boxplot(mean.rate~subtype+season, data = trees.full)



# png(filename = "./03-Output/02-Figures/mutation_rate_boxplots.png", width = 16, height = 9, res = 300, units = "in", pointsize = 10)
svg(filename = "./03-Output/02-Figures/mutation_rate_boxplots.svg", width = 8, height = 4.5, pointsize = 10)

par(mfrow = c(2,2))
for(i in 1:length(unique(trees.full$subtype))){
  boxplot(sqrt(as.numeric(rate))~season, 
          data = trees.full%>%filter(subtype%in%unique(trees.full$subtype)[i]), 
          main = unique(trees.full$subtype)[i], 
          ylim = c(0, sqrt(max(as.numeric(trees.full$rate), na.rm = T)))*c(0.95,1.05),
          xlab = "Season", 
          yaxt = 'n',
          ylab = "Estimated Mutation Rate (subs/site/year)")
  axis(2, at = seq(0, sqrt(max(as.numeric(trees.full$rate), na.rm = T))*1.05, by = 0.01), labels = round(seq(0, sqrt(max(as.numeric(trees.full$rate), na.rm = T))*1.05, by = 0.01)^2, 3))
  # abline(h = c(1e-4, 3e-3), lty = 5, cex = 1.5, col = "red")
}
dev.off()
















library(lubridate)


smalltrees.df <- smalltrees.df %>% 
  mutate(tmrca1 = date_decimal(tmrca), 
         ewtmrca = epiweek(tmrca1), 
         ew = ewtmrca-30, 
         # ew = ifelse(sign(ew)==-1, ifelse(season.num%in%c(2014,2015), 53+ewtmrca,52+ewtmrca),ew), 
         tmrca2 = tmrca - season.num - 1, 
         ew2 = factor(ifelse(ewtmrca==53,52,ewtmrca), levels = c(30:52,1:29), ordered = TRUE), 
         season = factor(season))

# data.frame(date = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by = "day"))%>%mutate(year = year(date), ew = epiweek(date)) %>% group_by(year) %>% summarise(ewmin = min(ew), ewmax = max(ew)) %>% ungroup() %>% View()

# png(filename = "./03-Output/02-Figures/tmrca_epiweek_boxplots.png", width = 16, height = 9, res = 300, units = "in", pointsize = 10)
svg(filename = "./03-Output/02-Figures/tmrca_epiweek_boxplots.svg", width = 8, height = 4.5, pointsize = 10)

par(mfrow = c(2,2))
for(i in 1:length(unique(trees.full$subtype))){
  boxplot(ew2~season, 
          data = smalltrees.df%>%filter(subtype%in%unique(trees.full$subtype)[i]), 
          main = unique(trees.full$subtype)[i], 
          xlab = "Epi/Calendar Week of Most Recent Common Ancestors",
          # ylim = c(1:52),
          # ylim = c(1:10),
          ylab = "Season", 
          horizontal = TRUE,
          xaxt = "n")
  axis(1, at = seq(1,52, by = 5), labels = c(30:52,1:29)[seq(1,52, by = 5)])
  # abline(h = c(1e-4, 3e-3), lty = 5, cex = 1.5, col = "red")
}
dev.off()



# 
# 
# sts <- smalltrees.df %>% group_by(subtype, season, location) %>% summarise(n = sum(!is.na(mpd))) %>% ungroup()



rm(list = ls())
gc()
                                                                           