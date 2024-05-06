

# load("./01-Data/02-Analytic-Data/tree_summaries.rdata")

load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")


library(dplyr)


sts <- smalltrees.df %>%
  group_by(subtype, season.num, location) %>% 
  summarise(mpd = mean(mpd)) %>%
  ungroup() %>% 
  mutate(season.num = season.num + 1)



svg(filename = "./03-Output/02-Figures/mpd_by_tmrca_points.svg", width = 8, height = 4.5, pointsize = 10)
# png(filename = "./03-Output/02-Figures/mpd_by_tmrca_points.png", units = "in", res = 300, width = 16, height = 9, pointsize = 10)

par(mfrow=c(2,2), mar = c(4.1, 4.1, 2.1, 1.1))

for(ii in 1:length(unique(smalltrees.df$subtype))){
  
  plot(mpd~tmrca, 
       data = smalltrees.df%>%filter(subtype%in%unique(smalltrees.df$subtype)[ii]), 
       # type = "n",
       ylim = c(0, max(smalltrees.df$mpd, na.rm = T)), 
       xlim = range(c(sts$season.num, smalltrees.df$tmrca), na.rm = T), 
       ylab = "Mean Pairwise Distance", 
       xlab = "Time of Most Recent Common Ancestor", 
       main = unique(smalltrees.df$subtype)[ii], 
       axes = T, 
       xaxt = 'n')
  
  axis(1, at = 2011:2020)

  
  # points(mpd~tmrca, 
  #        data = smalltrees.df%>%filter(subtype%in%unique(smalltrees.df$subtype)[ii]))
  
  # for(i in 1:length(unique(smalltrees.df$location))){
  #   
  #   lines(mpd~season.num, 
  #         data = sts%>%
  #           filter(subtype==unique(smalltrees.df$subtype)[ii] & location==unique(smalltrees.df$location)[i]), 
  #         lty = i)
  #   
  # }
}
dev.off()







svg(filename = "./03-Output/02-Figures/mpd_timeseries.svg", width = 8, height = 4.5, pointsize = 10)
# png(filename = "./03-Output/02-Figures/mpd_timeseries.png", units = "in", res = 300, width = 16, height = 9, pointsize = 10)

par(mfrow=c(2,2), mar = c(4.1, 4.1, 2.1, 1.1))

for(ii in 1:length(unique(smalltrees.df$subtype))){
  
  plot(mpd~tmrca, 
       data = smalltrees.df%>%filter(subtype%in%unique(smalltrees.df$subtype)[ii]), 
       type = "n",
       ylim = c(0, max(smalltrees.df$mpd, na.rm = T)), 
       xlim = range(c(sts$season.num, smalltrees.df$tmrca), na.rm = T), 
       ylab = "Mean Pairwise Distance (subs/site)", 
       xlab = "Influenza Season", 
       main = unique(smalltrees.df$subtype)[ii], 
       axes = T, 
       xaxt = 'n')
  
  axis(1, at = 2011:2020)
  
  
  # points(mpd~tmrca, 
  #        data = smalltrees.df%>%filter(subtype%in%unique(smalltrees.df$subtype)[ii]))
  
  for(i in 1:length(unique(smalltrees.df$location))){
    
    lines(mpd~season.num, 
          data = sts%>%
            filter(subtype==unique(smalltrees.df$subtype)[ii] & location==unique(smalltrees.df$location)[i]), 
          lty = i)
    
  }
}
dev.off()








rm(list=ls())
gc()






# 
# # svg(filename = "./03-Output/02-Figures/mpd_timeseries.svg", width = 16, height = 9, pointsize = 10)
# png(filename = "./03-Output/02-Figures/mpd_timeseries.png", units = "in", res = 300, width = 16, height = 9, pointsize = 10)
# 
# par(mfrow=c(2,2), mar = c(4.1, 4.1, 2.1, 1.1))
# 
# for(ii in 1:length(unique(trees.full$subtype))){
#   
#   plot(mpd~season.num, 
#        data = trees.full%>%
#          filter(subtype==unique(trees.full$subtype)[ii] & location==unique(trees.full$location)[1]), 
#        type = "l", 
#        ylim = c(0, max(trees.full$mpd, na.rm = T)), 
#        xlim = range(trees.full$season.num), 
#        ylab = "Mean Pairwise Distance", 
#        xlab = "Influenza Season", 
#        main = unique(trees.full$subtype)[ii])
#   
#   for(i in 2:length(unique(trees.full$location))){
#     lines(mpd~season.num, 
#           data = trees.full%>%
#             filter(subtype==unique(trees.full$subtype)[ii] & location==unique(trees.full$location)[i]), 
#           lty = i)
#   }
# }
# dev.off()
# 


# for(ii in 1:length(unique(trees.df.imputed$subtype))){
#   
#   plot(mpd~season.num, 
#        data = trees.df.imputed%>%
#          filter(subtype==unique(trees.df.imputed$subtype)[ii] & location==unique(trees.df.imputed$location)[1]), 
#        type = "l", 
#        ylim = c(0, max(trees.df.imputed$mpd, na.rm = T)), 
#        xlim = range(trees.df.imputed$season.num), 
#        ylab = "Mean Pairwise Distance", 
#        xlab = "Influenza Season", 
#        main = unique(trees.df.imputed$subtype)[ii])
#   
#   for(i in 2:length(unique(trees.df.imputed$location))){
#     lines(mpd~season.num, 
#           data = trees.df.imputed%>%
#             filter(subtype==unique(trees.df.imputed$subtype)[ii] & location==unique(trees.df.imputed$location)[i]), 
#           lty = i)
#   }
# }

