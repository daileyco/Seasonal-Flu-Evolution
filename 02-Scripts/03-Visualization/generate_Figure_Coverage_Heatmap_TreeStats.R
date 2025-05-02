# Figure heatmap showing proportion of cumulative seasonal ILI by week and state


## load data
load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")


## packages
library(dplyr)
library(tidyr)
library(lubridate)

## figure set up

sts <- smalltrees.df %>% 
  group_by(subtype, season, location) %>% 
  summarise(n = mean(mpd, na.rm = TRUE)) %>% 
  ungroup() #%>% 
  # mutate(n = ifelse(n==0,NA,n)
  #        # ,
  #        # int = cut(n, mybreaks)
  #        )


mybreaks <- unique(quantile(sts$n, probs = 1:20/20, na.rm = TRUE))





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
# svg(filename = "./03-Output/02-Figures/tree_coverage_heatmap_mpdavg.svg", width = 8, height = 8, pointsize = 10)
# png(filename = "./03-Output/02-Figures/figure_heatmap_mpdavg.png", width = 6, height = 8, pointsize = 10, units = "in", res = 300)
svg(filename = "./03-Output/02-Figures/figure_heatmap_mpdavg.svg", width = 6, height = 7.8, pointsize = 10)



par(mfrow = c(1,5), mar = c(5.1,2,2.5,0))

### heatmap


for(i in 1:4){
  image(x=x$number, 
        y=y$number, 
        z=z[[i]], 
        xlab = "", 
        ylab = "",
        axes = F, 
        col = viridis::viridis(length(mybreaks)-1), 
        breaks = mybreaks)
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
      y=as.numeric(c(0, factor(mybreaks))), 
      z=as.matrix(t(c(NA,(mybreaks[-1]-c((diff(mybreaks)/2)))))), 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = viridis::viridis(length(mybreaks)-1), 
      breaks = mybreaks)
box()
axis(side = 2, 
     at = as.numeric(c(0, factor(mybreaks))), 
     labels = c(NA,format(mybreaks, digits = 3)), 
     tick = F,
     las = 1, 
     padj = 0.5, xpd = T)
axis(side = 3, 
     at = 1, 
     labels = "Average Diversity", 
     tick = F)

dev.off()




















# mutation

sts <- sts %>% 
  select(-n) %>%
  full_join(., 
            trees.full%>%
              select(location, season, subtype, rate)%>%
              mutate(rate = as.numeric(trimws(rate))), 
            by = c("location", "season", "subtype"))


mybreaks <- unique(quantile(sts$rate, probs = 1:20/20, na.rm = TRUE))



z <- sts %>%
  select(region=location, season, subtype, n=rate) %>%
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



## figure
# svg(filename = "./03-Output/02-Figures/tree_coverage_heatmap_mutrate.svg", width = 8, height = 8, pointsize = 10)
png(filename = "./03-Output/02-Figures/figure_heatmap_mutrate.png", width = 6, height = 8, pointsize = 10, units = "in", res = 300)



par(mfrow = c(1,5), mar = c(5.1,2,2.5,0))

### heatmap


for(i in 1:4){
  image(x=x$number, 
        y=y$number, 
        z=z[[i]], 
        xlab = "", 
        ylab = "",
        axes = F, 
        col = viridis::viridis(length(mybreaks)-1), 
        breaks = mybreaks)
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
      y=as.numeric(c(0, factor(mybreaks))), 
      z=as.matrix(t(c(NA,(mybreaks[-1]-c((diff(mybreaks)/2)))))), 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = viridis::viridis(length(mybreaks)-1), 
      breaks = mybreaks)
box()
axis(side = 2, 
     at = as.numeric(c(0, factor(mybreaks))), 
     labels = c(NA,format(mybreaks, scientific = TRUE, digits = 2)), 
     tick = F,
     las = 1, 
     padj = 0.5, xpd = T)
axis(side = 3, 
     at = 1, 
     labels = "Estimated Mutation Rate", 
     tick = F)

dev.off()










# tmrca

sts <- sts %>% 
  select(-rate) %>%
  full_join(., 
            trees.full%>%
              mutate(tMRCA = decimal_date(as.Date(trimws(tMRCA)))-season.num-1)%>%
              select(location, season, subtype, tMRCA), 
            by = c("location", "season", "subtype"))


mybreaks <- quantile(sts$tMRCA, probs = 1:20/20, na.rm = TRUE)
mybreaks <- c(mybreaks[1], -10, -5, mybreaks[7:length(mybreaks)])
mybreaks <- c(0, mybreaks)[order(c(0, mybreaks))]


z <- sts %>%
  select(region=location, season, subtype, n=tMRCA) %>%
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



## figure
# svg(filename = "./03-Output/02-Figures/tree_coverage_heatmap_tmrca.svg", width = 8, height = 8, pointsize = 10)
png(filename = "./03-Output/02-Figures/figure_heatmap_tmrca.png", width = 6, height = 8, pointsize = 10, units = "in", res = 300)



par(mfrow = c(1,5), mar = c(5.1,2,2.5,0))

### heatmap


for(i in 1:4){
  image(x=x$number, 
        y=y$number, 
        z=z[[i]], 
        xlab = "", 
        ylab = "",
        axes = F, 
        # col = viridis::viridis(length(mybreaks)-1),
        col = c("grey", "red", viridis::viridis(length(mybreaks)-4),"orange"),
        breaks = mybreaks)
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
      y=as.numeric(c(0, factor(mybreaks))), 
      z=as.matrix(t(c(NA,(mybreaks[-1]-c((diff(mybreaks)/2)))))), 
      xlab = "", 
      ylab = "",
      axes = F, 
      # col = viridis::viridis(length(mybreaks)-1), 
      col = c("red", viridis::viridis(length(mybreaks)-3),"orange"),
      breaks = mybreaks)
box()
axis(side = 2, 
     at = as.numeric(c(0, factor(mybreaks))), 
     labels = c(NA,format(mybreaks, scientific = F, digits = 2)), 
     tick = F,
     las = 1, 
     padj = 0.5, xpd = T)
axis(side = 3, 
     at = 1, 
     labels = "Estimated tMRCA\nrelative to Season", 
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














