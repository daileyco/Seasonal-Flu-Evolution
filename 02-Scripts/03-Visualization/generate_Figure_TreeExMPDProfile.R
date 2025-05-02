
## load data
load("./01-Data/02-Analytic-Data/smalltrees_summaries.rdata")
load("./01-Data/01-Processed-Data/spatial.rdata")

## packages
library(dplyr)
library(ape)
library(lubridate)
library(tidyr)
library(sf)
library(spdep)


## helper functions
vadjustcolor <- Vectorize(adjustcolor, vectorize.args = c("col", "alpha.f"))

vrep <- Vectorize(rep.int, vectorize.args = "times")




## data prep

### trees
gaex <- bind_rows(trees.full, smalltrees.df) %>% 
  arrange(subtype, season, location, rep) %>% 
  filter(location %in% c("Georgia")) %>%
  filter(subtype %in% c("H3") & 
           season %in% c("2016-2017"))


evotree <- read.tree(text = gaex$tree.newick2[which(gaex$rep==0)])
st1 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][1])
st2 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][2])
st3 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][3])
st4 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][4])
st5 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][5])


timetree <- read.nexus(file = gaex$timetreenexusfile[which(gaex$rep==0)])
tmrca <- decimal_date(as.Date(trimws(gaex$tMRCA[which(gaex$rep==0)])))




exdates <- data.frame(date = seq(as.Date("2009-01-01"), as.Date("2020-12-31"), by = "days")) %>%
  mutate(ew = epiweek(date)) %>% 
  filter(ew %in% c(30, 18) | (day(date)==1 & month(date)==1)) %>%
  mutate(ew = case_when(ew==30 ~ "start", 
                        ew==18 ~ "end", 
                        TRUE ~ "mid"), 
         year = year(date)) %>% 
  group_by(ew, year) %>%
  mutate(rn = row_number()) %>%
  mutate(rn = case_when(rn==max(rn) & ew == "end" ~ 1, 
                        rn==min(rn) & ew == "start" ~ 1, 
                        ew == "mid" ~ 1,
                        TRUE ~ 0)) %>%
  ungroup() %>%
  filter(rn == 1) %>% 
  select(-rn) %>%
  # pivot_wider(names_from = ew, values_from = date)
  mutate(dd = decimal_date(date), 
         x = dd - tmrca)

xlabs <- exdates %>% 
  filter(ew == "mid" & x > -0.5 & x < 6)


xwindows <- exdates %>% 
  filter(!ew %in% c("mid") & x < 6) %>% 
  mutate(x2 = lead(x, n = 1), 
         col = case_when(year(date) %in% c(2016) & ew %in% c("start") ~ adjustcolor("green", alpha.f = 0.2), 
                         ew %in% c("start") ~ NA, 
                         ew %in% c("end") ~ adjustcolor("grey", alpha.f = 0.1))) %>%
  filter(!is.na(x2))





### for map

state.neighbors.mat <- us.shape.state %>% 
  filter(NAME%in%unique(smalltrees.df$location)) %>%
  arrange(NAME)

state.neighbors <- state.neighbors.mat$NAME

state.nb.lags <- nblag(poly2nb(state.neighbors.mat), maxlag = 3)



ganbs <- lapply(state.nb.lags, 
                (\(x){
                  data.frame(nb = state.neighbors[x[[11]]])
                })) %>% 
  bind_rows(., 
            data.frame(nb = state.neighbors), 
            .id = "lag") %>% 
  mutate(lag = ifelse(nb%in%c("Georgia"), 0, lag)) %>% 
  group_by(nb) %>% 
  mutate(min = lag == min(lag, na.rm = T)) %>% 
  ungroup() %>% 
  filter(min) %>% 
  select(-min) %>% 
  arrange(nb) %>%
  mutate(lag = ifelse(lag==4, NA, lag))


usmap <- us.shape.state.contig %>% 
  left_join(., 
            ganbs, 
            by = c("NAME"="nb"))


centroids <- st_coordinates(sf::st_centroid(usmap))






## plot

### dendrogram
# plot(timetree, show.tip.label = FALSE) %>% print()

# png(filename = "./03-Output/02-Figures/figure_gaextree.png", width = 9, height = 5, units = "in", res = 300, pointsize = 10)
# svg(filename = "./03-Output/02-Figures/concept1a_timetree.svg", width = 8, height = 4.5, pointsize = 10)

svg(filename = "./03-Output/02-Figures/figure_gaextree.svg", width = 9, height = 4.6, pointsize = 10)


# layout(matrix(c(1,1,2,1,1,3,4,5,6),ncol=3,byrow=T), heights = c(1,1,1), widths = c(1,1,1))
layout(matrix(c(1,2,1,3,1,4,1,5,1,6),ncol=2,byrow=T), heights = c(1,1,1,1,1), widths = c(4,1))

# layout.show(n=6)
{
  par(mar = c(2,2,2,2))
  plot(timetree, show.tip.label = FALSE, x.lim = c(0, 6)) %>% 
    (\(x){
      
      rect(xleft = unlist(xwindows["x"]), 
           xright = unlist(xwindows["x2"]), 
           ybottom = x$y.lim[1], 
           ytop = x$y.lim[2], 
           xpd = T, 
           border = NA, 
           col = xwindows$col)
      
      # axis(1, at = x$x.lim)
      # axis(1, at = x$x.lim, labels = x$x.lim+decimal_date(as.Date(trimws(gaex$tMRCA[which(gaex$rep==0)]))), line = 2)
    })
  
  
  axis(1, at = xlabs$x, labels = format(xlabs$date, "%b\n%Y"), xpd = T, line = 0, tcl = -0.1)
  
  
  
  
  axis(3, at = unlist(xwindows[which(xwindows$ew%in%c("start") & !is.na(xwindows$col)), c("x", "x2")]), labels = F, tcl = 0.1, lwd = 2, line = -0.25)
  axis(3, at = mean(unlist(xwindows[which(xwindows$ew%in%c("start") & !is.na(xwindows$col)), c("x", "x2")])), labels = F, tcl = -0.1, lwd = 2, line = -0.25)
  axis(3, at = mean(unlist(xwindows[which(xwindows$ew%in%c("start") & !is.na(xwindows$col)), c("x", "x2")])), labels = "2016-2017\nFlu Season", tick = FALSE, lwd = 2, line = -1)
  
  # tiplabels(pch = 16, cex = 1)
  # tiplabels(pch = c(NA, 16)[as.numeric(timetree$tip.label%in%c(st1$tip.label, st2$tip.label, st3$tip.label))+1], cex = 1)
  
  tiplabels(pch = case_when(timetree$tip.label%in%c(st1$tip.label) ~ 22,#15, 
                            timetree$tip.label%in%c(st2$tip.label) ~ 21,#16, 
                            timetree$tip.label%in%c(st3$tip.label) ~ 24,#17, 
                            timetree$tip.label%in%c(st4$tip.label) ~ 23,#18, 
                            timetree$tip.label%in%c(st5$tip.label) ~ 25,#6, 
                            TRUE ~ NA), 
            cex = 1.25, 
            bg = case_when(timetree$tip.label%in%c(st1$tip.label) ~ RColorBrewer::brewer.pal(5, "Set2")[1], 
                           timetree$tip.label%in%c(st2$tip.label) ~ RColorBrewer::brewer.pal(5, "Set2")[2], 
                           timetree$tip.label%in%c(st3$tip.label) ~ RColorBrewer::brewer.pal(5, "Set2")[3],
                           
                           timetree$tip.label%in%c(st4$tip.label) ~ RColorBrewer::brewer.pal(5, "Set2")[4], 
                           timetree$tip.label%in%c(st5$tip.label) ~ RColorBrewer::brewer.pal(5, "Set2")[5],
                           TRUE ~ NA))
  
  
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "A", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
}





{
  
  
  plot(st5, show.tip.label = FALSE, x.lim = c(0,0.005), type = "p") %>% print()
  tiplabels(pch = 25, cex = 2, bg = RColorBrewer::brewer.pal(5, "Set2")[5], xpd = T)
  mpd <- mean(cophenetic(st5)[upper.tri(cophenetic(st5))])
  
  text(par('usr')[2], par('usr')[4], 
       labels = paste0("MPD = ", ifelse(round(mpd, 4)==0, "< 0.0001", round(mpd, 5))), 
       adj = c(1, -0.5), 
       cex = 1, 
       xpd = T)
  # box("figure")
  nodelabels(pch = 21, bg = "white")
  nodelabels(node = Ntip(st5)+1, pch = 10)
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "B", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
  
  plot(st4, show.tip.label = FALSE, x.lim = c(0,0.005), type = "c") %>% print()
  tiplabels(pch = 23, cex = 2, bg = RColorBrewer::brewer.pal(5, "Set2")[4], xpd = T)
  mpd <- mean(cophenetic(st4)[upper.tri(cophenetic(st4))])
  
  text(par('usr')[2], par('usr')[4], 
       labels = paste0("MPD = ", ifelse(round(mpd, 4)==0, "< 0.0001", round(mpd, 5))), 
       adj = c(1, 0), 
       cex = 1, 
       xpd = T)
  # box("figure")
  nodelabels(pch = 21, bg = "white")
  nodelabels(node = Ntip(st4)+1, pch = 10)
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "C", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
  
  # plot(st3, show.tip.label = FALSE, x.lim = c(0,0.0025), y.lim = c(0,0.0015), type = "unrooted") %>% print()
  plot(st3, show.tip.label = FALSE, x.lim = c(0,0.005), type = "p") %>% print()
  tiplabels(pch = 24, cex = 2, bg = RColorBrewer::brewer.pal(5, "Set2")[3], xpd = T)
  mpd <- mean(cophenetic(st3)[upper.tri(cophenetic(st3))])
  
  text(par('usr')[2], par('usr')[4], 
       labels = paste0("MPD = ", ifelse(round(mpd, 4)==0, "< 0.0001", round(mpd, 5))), 
       adj = c(1, 0), 
       cex = 1, 
       xpd = T)
  # box("figure")
  nodelabels(pch = 21, bg = "white")
  nodelabels(node = Ntip(st3)+1, pch = 10)
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "D", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
  # plot(st2, show.tip.label = FALSE, x.lim = c(0,0.0025), y.lim = c(0,0.0015), type = "unrooted") %>% print()
  plot(st2, show.tip.label = FALSE, x.lim = c(0,0.005), type = "p") %>% print()
  tiplabels(pch = 21, cex = 2, bg = RColorBrewer::brewer.pal(5, "Set2")[2], xpd = T)
  mpd <- mean(cophenetic(st2)[upper.tri(cophenetic(st2))])
  
  text(par('usr')[2], par('usr')[4], 
       labels = paste0("MPD = ", ifelse(round(mpd, 4)==0, "< 0.0001", round(mpd, 5))), 
       adj = c(1, 0), 
       cex = 1, 
       xpd = T)
  # box("figure")
  nodelabels(pch = 21, bg = "white")
  nodelabels(node = Ntip(st2)+1, pch = 10)
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "E", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
  
  # plot(st1, show.tip.label = FALSE, x.lim = c(0,0.0025), y.lim = c(0,0.0015), type = "unrooted") %>% print()
  plot(st1, show.tip.label = FALSE, x.lim = c(0,0.005), type = "p") %>% print()
  tiplabels(pch = 22, cex = 2, bg = RColorBrewer::brewer.pal(5, "Set2")[1], xpd = T)
  mpd <- mean(cophenetic(st1)[upper.tri(cophenetic(st1))])
  
  text(par('usr')[2], par('usr')[4], 
       labels = paste0("MPD = ", ifelse(round(mpd, 7)==0, "< 0.00001", format(round(mpd, 7), scientific = F))), 
       adj = c(1, 0), 
       cex = 1, 
       xpd = T)
  # box("figure")
  nodelabels(pch = 21, bg = "white")
  nodelabels(node = Ntip(st1)+1, pch = 10)
  
  axis(1, at = c(0,0.0005), labels = F, tcl = c(0.2), line = 1.5)
  mtext("0.0005 subs/site", side = 1, line = 1, at = c(0.0005), adj = -0.1, cex = 1)
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "F", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
}














dev.off()








sts <- smalltrees.df %>%
  group_by(subtype, season.num, location) %>% 
  summarise(mpd = mean(mpd)) %>%
  ungroup() %>% 
  mutate(season.num = season.num + 1)



# svg(filename = "./03-Output/02-Figures/mpd_by_tmrca_points.svg", width = 8, height = 4.5, pointsize = 10)
# png(filename = "./03-Output/02-Figures/mpd_profile.png", units = "in", res = 300, width = 9, height = 5, pointsize = 10)

svg(filename = "./03-Output/02-Figures/mpd_profile.svg", width = 9, height = 4.8, pointsize = 10)


par(mfrow=c(2,2), mar = c(4.1, 4.1, 2.1, 1.1))

for(ii in 1:length(unique(smalltrees.df$subtype))){
  
  plot(mpd~tmrca, 
       data = smalltrees.df%>%filter(subtype%in%unique(smalltrees.df$subtype)[c(4,3,1,2)][ii]), 
       # type = "n",
       ylim = c(0, max(smalltrees.df$mpd, na.rm = T)), 
       xlim = range(c(sts$season.num, smalltrees.df$tmrca), na.rm = T), 
       ylab = "Mean Pairwise Distance", 
       xlab = "Time of Most Recent Common Ancestor", 
       main = unique(smalltrees.df$subtype)[c(4,3,1,2)][ii], 
       axes = T, 
       xaxt = 'n')
  
  axis(1, at = 2011:2020)
  
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = LETTERS[ii], adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
  if(unique(smalltrees.df$subtype)[c(4,3,1,2)][ii]=="H3"){
    
    points(gaex$tmrca[6:2], 
           gaex$mpd[6:2], 
           pch = c(25,23,24,21,22), 
           bg = RColorBrewer::brewer.pal(5, "Set2")[5:1], 
           cex = 1.5)
    
  }
  
  
  
  
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










#map of avg ranks

stsranks <- sts %>% 
  filter(!is.na(mpd)) %>%
  group_by(subtype, season.num) %>% 
  # mutate(mpd = ifelse(is.na(mpd), 0, mpd)) %>% 
  arrange(desc(mpd)) %>% 
  mutate(rank = rank(mpd, ties.method = "min"), 
         ssmeanmpd = mean(mpd)) %>% 
  ungroup() %>% 
  mutate(meandiff = mpd-ssmeanmpd) %>%
  group_by(location) %>%
  summarise(avgrank = mean(rank), 
            avgmeandiff = mean(meandiff)) %>%
  ungroup() %>%
  mutate(avgmeandiff2 = case_when(avgmeandiff<0 ~ avgmeandiff/abs(min(avgmeandiff)), 
                                 avgmeandiff>0 ~ avgmeandiff/max(avgmeandiff), 
                                 TRUE ~ avgmeandiff))



us.shape.state2 <- stsranks %>%
  left_join(., 
            us.shape.state, 
            by = c("location"="NAME"))

md.breaks <- seq(-1,1,by=0.1)
# ei.breaks <- 0:40/40
md.col <- c(colorRampPalette(c("blue", "white"))(11), 
            colorRampPalette(c("white", "red"))(11)[-1])



# png(filename = "./03-Output/02-Figures/mpd_meandiffmap.png", units = "in", res = 300, width = 9, height = 5, pointsize = 10)
svg(filename = "./03-Output/02-Figures/mpd_meandiffmap.svg", width = 9, height = 4.6, pointsize = 10)



layout(matrix(c(1,1,1,2,3,4,5,2), ncol=4, byrow=T), heights = c(3,1), widths = c(1,1,1,1))
# layout.show(n=5)
par(mar=c(0,0,0,0))
plot(us.shape.state.contig$geometry, border = "gainsboro")

plot(us.shape.state2$geometry, 
     col = md.col[findInterval(us.shape.state2$avgmeandiff2, md.breaks, all.inside = TRUE)], 
     add = TRUE)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)

par(mar=c(0,2,2,0))
plot.new()
plot.window(xlim = c(-0.5,1), ylim = c(0,21))
rect(xleft=rep(0,21), xright=rep(1,21), ybottom = 0:20, ytop = 1:21, 
     col = md.col, 
     border = NA)
axis(2, 
     at = c(0.5,10.5,20.5), 
     labels = c("Less than Average", "Average", "More than Average"), 
     tick = F, 
     pos = 0, 
     las = 2, 
     hadj = 1)

axis(3, at = mean(par('usr')[1:2]), labels = c("Relative Differences in MPD"), tick = F, hadj = 0.5, font = 2)


par(mar=c(0,0,0,0))
plot(st_shift_longitude(us.shape.state2$geometry[which(us.shape.state2$location%in%c("Alaska"))]), 
     col = md.col[findInterval(us.shape.state2$avgmeandiff2[which(us.shape.state2$location%in%c("Alaska"))], md.breaks, all.inside = TRUE)], 
     add = F)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)


plot(us.shape.state2$geometry[which(us.shape.state2$location%in%c("Hawaii"))], 
     col = md.col[findInterval(us.shape.state2$avgmeandiff2[which(us.shape.state2$location%in%c("Hawaii"))], md.breaks, all.inside = TRUE)], 
     add = F)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "C", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)


plot(us.shape.state2$geometry[which(us.shape.state2$location%in%c("District of Columbia"))], 
     col = md.col[findInterval(us.shape.state2$avgmeandiff2[which(us.shape.state2$location%in%c("District of Columbia"))], md.breaks, all.inside = TRUE)], 
     add = F)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "D", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)

dev.off()




rm(list = ls())
gc()
