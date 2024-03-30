# Figure heatmap showing proportion of cumulative seasonal ILI by week and state


## load data
load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")


## packages
library(dplyr)
library(tidyr)

## figure set up

sts <- smalltrees.df %>% 
  group_by(subtype, season, location) %>% 
  summarise(n = sum(!is.na(mpd))) %>% 
  ungroup() %>% 
  mutate(n = ifelse(n==0,NA,n))


x <- data.frame(season = unique(smalltrees.df$season)) %>%
  arrange(season) %>%
  mutate(number = as.numeric(substr(season,1,4)))


y <- data.frame(region = unique(smalltrees.df$location)) %>%
  arrange(desc(region)) %>%
  mutate(number = row_number())


z <- sts %>%
  select(region=location, season, subtype, n) %>%
  arrange(desc(region)) %>%
  pivot_wider(names_from = c(region), values_from = n) %>%
  arrange(season) %>%
  as.data.frame() %>% 
  split(~subtype) %>% 
  lapply(., 
         (\(x){
           rownames(x) <- x$season
           x %>% 
             select(-season,-subtype) %>% 
             as.matrix()
         }))





statecodes <- data.frame(region = c("Alabama", "Alaska", "Arizona", 
                                      "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                                      "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                                      "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                                      "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                                      "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                                      "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
                                      "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                                      "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                                      "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"), 
                         code = c("AL", 
                                  "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
                                  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
                                  "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
                                  "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
                                  "VA", "WA", "WV", "WI", "WY", "DC"))



y <- full_join(y, statecodes)






## figure
svg(filename = "./03-Output/02-Figures/tree_coverage_heatmap.svg", width = 8, height = 8, pointsize = 10)



par(mfrow = c(1,5), mar = c(5.1,2,2.5,0))

### heatmap


for(i in 1:4){
  image(x=x$number, 
        y=y$number, 
        z=z[[i]], 
        xlab = "", 
        ylab = "",
        axes = F, 
        col = viridis::viridis(22), 
        breaks = c(1:20,30,40,50))
  box()
  
  abline(h = y$number-0.5, col = rgb(0,0,0,0.25))
  abline(v = x$number+0.5, col = rgb(0,0,0,0.25))
  
  # if(i == 1){
    axis(2, 
         at = y$number, 
         labels = FALSE, 
         las = 2, 
         lwd = 0, 
         lwd.ticks = 0.5,
         padj = 0.5, 
         hadj = 1, 
         tcl = -0.2)
    
    axis(2, 
         at = y$number, 
         labels = y$code, 
         las = 2, 
         lwd = 0, 
         lwd.ticks = 0,
         padj = 0.5, 
         hadj = 0, 
         line = 0.5)
    

    
    
  # }
  
  axis(3, 
       at = c(2010,2019), 
       labels = F, 
       line = 0.5, 
       tcl = 0.2)
  axis(3, 
       at = mean(c(2010,2019)), 
       labels = c("BVic", "BYam", "H1", "H3")[i], 
       line = 0.5, 
       tcl = -0.2)
  
  axis(1, 
       at = 2010:2019, 
       labels = x$season,
       las = 2,
       hadj = 1, 
       lwd = 0,
       lwd.ticks = 0.5,
       line = 0)
  
  
}

par(mar = c(10.2,6,5,2))
image(x=1, 
      y=as.numeric(factor(c(0,1:20,30,40,50))), 
      z=as.matrix(t(c(NA,1:20,30,40,50))), 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = viridis::viridis(22), 
      breaks = c(1:20,30,40,50))
box()
axis(side = 2, 
     at = as.numeric(factor(c(0,1:20,30,40,50))), 
     labels = c(0,1:19, "20-29", "30-39", "40-49", "50+"), 
     tick = F,
     las = 1)
axis(side = 3, 
     at = 1, 
     labels = "Subtree\nFrequency", 
     tick = F)

dev.off()



## clean environment
rm(list = ls())
gc()





## old
# #least intense
# plot(ili$week_start[which(ili$year%in%c(2011, 2012) & ili$region%in%c("Louisiana"))], ili$ilitotal[which(ili$year%in%c(2011, 2012) & ili$region%in%c("Louisiana"))])
# #most intense
# plot(ili$week_start[which(ili$year%in%c(2014, 2015) & ili$region%in%c("Delaware"))], ili$ilitotal[which(ili$year%in%c(2014, 2015) & ili$region%in%c("Delaware"))])


# 
# 
# 
# 
# plot(unique(ili2$week_start), tapply(ili2$ilitotal, ili2$week_start, sum, na.rm = T), type = "l")
# plot(unique(ili2$week_start), tapply(ili2$total_patients, ili2$week_start, sum, na.rm = T), type = "l")
# 
# plot(unique(ili2$week_start), tapply(ili2$total_patients, ili2$week_start, sum, na.rm = T)/10, type = "l", ylim = c(0, max(tapply(ili2$total_patients, ili2$week_start, sum, na.rm = T))/10), xlab = "Week", ylab = "Count")
# lines(unique(ili2$week_start), tapply(ili2$ilitotal, ili2$week_start, sum, na.rm = T), lty = 2, col = "red")
# legend("topleft", lty = c(1,2), col = c("black", "red"), legend = c("Total Patients (unit=10s)", "Influenza-like Illness"))
# 
# 
# 














