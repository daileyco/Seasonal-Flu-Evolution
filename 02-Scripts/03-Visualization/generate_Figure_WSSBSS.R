# Figure heatmap


## load data
# load("./01-Data/01-Processed-Data/sequences_freqs.rds")
load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")


## packages
library(dplyr)
library(tidyr)

## figure set up

sts <- smalltrees.df %>%
  group_by(subtype, season, location) %>% 
  mutate(lmean = mean(mpd, na.rm = T), 
         l2mean = mean(mpd)) %>%
  ungroup() %>%
  group_by(subtype, season) %>%
  mutate(smean = mean(mpd, na.rm = T)) %>%
  ungroup()
  



stcors <- cors %>% 
  filter(subtype_x==subtype_y & slagtype!="Network") %>%
  split(., ~subtype_x) %>%
  lapply(., 
         (\(x){
           x %>% 
             select(subtype_x, slag, tlag, cor50) %>%
             arrange(tlag) %>%
             pivot_wider(names_from = slag, values_from = cor50) %>% 
             select(-1,-2) %>%
             as.matrix()
         }))



stcorssig <- cors %>% 
  mutate(sig = sign(cor50) == sign(cor2.5) & sign(cor50) == sign(cor97.5), 
         cor50 = ifelse(sig, round(cor50,3), NA)) %>% 
  filter(subtype_x==subtype_y & slagtype!="Network") %>%
  split(., ~subtype_x) %>%
  lapply(., 
         (\(x){
           x %>% 
             select(subtype_x, slag, tlag, cor50) %>%
             arrange(tlag) %>%
             pivot_wider(names_from = slag, values_from = cor50) %>% 
             select(-1,-2) %>%
             as.matrix()
         }))


# sts <- smalltrees.df %>% 
#   group_by(subtype, season, location) %>% 
#   summarise(n = sum(!is.na(mpd))) %>% 
#   ungroup() %>% 
#   mutate(n = ifelse(n==0,NA,n)
#          # ,
#          # int = cut(n, mybreaks)
#          )


mybreaks <- c(-1.1,seq(-1,1,length.out = 50)[-1])

mycols <- c(colorRampPalette(c("gainsboro", "red"))(ceiling(length(mybreaks-1)/2))[ceiling(length(mybreaks)/2):1], 
            colorRampPalette(c("gainsboro", "blue"))(ceiling(length(mybreaks-1)/2))[-1])




# x <- data.frame(season = unique(smalltrees.df$season)) %>%
#   arrange(season) %>%
#   mutate(number = as.numeric(substr(season,1,4)))
# 
# 
# y <- data.frame(region = unique(smalltrees.df$location)) %>%
#   arrange(desc(region)) %>%
#   mutate(number = row_number())
# 
# 
# z <- seqs %>%
#   select(region=location, season, subtype, n) %>%
#   arrange(desc(region)) %>%
#   pivot_wider(names_from = c(region), values_from = n) %>%
#   arrange(season) %>%
#   as.data.frame() %>% 
#   split(~subtype) %>% 
#   lapply(., 
#          (\(x){
#            rownames(x) <- x$season
#            x %>% 
#              select(-season,-subtype) %>% 
#              as.matrix()
#          }))
# 
# 
# z2 <- seqs %>%
#   mutate(havetree = ifelse(havetree, NA, ifelse(!maketree, NA, havetree))) %>%
#   select(region=location, season, subtype, havetree) %>%
#   arrange(desc(region)) %>%
#   pivot_wider(names_from = c(region), values_from = havetree) %>%
#   arrange(season) %>%
#   as.data.frame() %>% 
#   split(~subtype) %>% 
#   lapply(., 
#          (\(x){
#            rownames(x) <- x$season
#            x %>% 
#              select(-season,-subtype) %>% 
#              as.matrix()
#          }))
# 
# 
# 
# statecodes <- data.frame(region = c("Alabama", "Alaska", "Arizona", 
#                                       "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
#                                       "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
#                                       "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
#                                       "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
#                                       "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
#                                       "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
#                                       "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
#                                       "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
#                                       "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"), 
#                          code = c("AL", 
#                                   "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
#                                   "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
#                                   "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
#                                   "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
#                                   "VA", "WA", "WV", "WI", "WY", "DC"))
# 
# 
# 
# y <- full_join(y, statecodes)



## figure


for(i in 1:4){
  
  svg(filename = paste0("./03-Output/02-Figures/cors_heatmap_", c("BVic", "BYam", "H1", "H3")[i], ".svg"), width = 8, height = 4.5, pointsize = 10)
  
  layout(matrix(c(1,2),ncol = 2), widths = c(3,1), heights = c(1))
  # layout.show(n=2)
  
  par(mar = c(4.1,4.1,2,2))
  
  
  image(x=0:3, 
        y=0:3, 
        z=stcors[[i]], 
        xlab = "", 
        ylab = "",
        axes = F, 
        col = mycols, 
        breaks = mybreaks)
  
  text(x=rep(0:3,4),y=rep(0:3,each=4), labels = round(stcorssig[[i]],3))

  
  box()
  
  abline(h = 0:3-0.5, col = rgb(0,0,0,0.25))
  abline(v = 0:3+0.5, col = rgb(0,0,0,0.25))
  
  # if(i == 1){
    axis(1, 
         at = 0:3, 
         labels = FALSE, 
         las = 2, 
         lwd = 0, 
         lwd.ticks = 0.5,
         padj = 0.5, 
         hadj = 1, 
         tcl = -0.2)
    
    axis(1, 
         at = 0:3, 
         labels = 0:3, 
         las = 1, 
         lwd = 0, 
         lwd.ticks = 0,
         padj = 0.5, 
         hadj = 0.5, 
         line = -1)
    arrows(x0=0,y0=-1,x1=3,y1=-1,length=0.125, angle = 30, code = 2, lwd = 1.5, xpd = T)
    text(c(0, 3), rep(-1.1,2), labels = c("Current", "Past"), xpd = T, adj = c(0.5,1), font = 3, cex = 0.7)
    title(xlab = "Temporal Lag", line = 3)

    
    
    
    
    
    
    axis(2, 
         at = 0:3, 
         labels = FALSE, 
         las = 2, 
         lwd = 0, 
         lwd.ticks = 0.5,
         padj = 0.5, 
         hadj = 1, 
         tcl = -0.2)
    
    axis(2, 
         at = 0:3, 
         labels = 0:3, 
         las = 2, 
         lwd = 0, 
         lwd.ticks = 0,
         padj = 0.5, 
         hadj = 1, 
         line = -0.5)
    
    
    
    arrows(x0=-0.75,y0=0,x1=-0.75,y1=3,length=0.125, angle = 30, code = 2, lwd = 1.5, xpd = T)
    text(rep(-0.9,2), c(0, 3), labels = c("Local", "Distant"), xpd = T, adj = c(0.5,1), font = 3, srt = 90, cex = 0.7)
    title(ylab = "Spatial Lag", line = 3)
    
    
  # }
    
  title(main = c("BVic", "BYam", "H1", "H3")[i])
  
  # axis(3, 
  #      at = c(0,3), 
  #      labels = F, 
  #      line = 0.5, 
  #      tcl = 0.2)
  # axis(3, 
  #      at = mean(c(0,3)), 
  #      labels = c("BVic", "BYam", "H1", "H3")[i], 
  #      line = 0.5, 
  #      tcl = -0.2)
  
  


par(mar = c(5.1,6,5.1,4))
image(x=1, 
      y=c(as.numeric(factor(seq(-1,1,by=0.1)))), 
      z=as.matrix(t(seq(-1,1,by=0.1))), 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = mycols, 
      breaks = mybreaks)
box()
axis(side = 2, 
     at = c(as.numeric(factor(seq(-1,1,by=0.1))))[which(seq(-1,1,by=0.1)%in%seq(-1,1,by=0.2))]+c(rep(-0.5,5),0,rep(0.5,5)), 
     labels = seq(-1,1,by=0.2),
     tick = T,
     las = 1)
axis(side = 3, 
     at = 1, 
     labels = expression(paste("Correlation Coefficient, ", rho)), 
     tick = F)

dev.off()
}


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














