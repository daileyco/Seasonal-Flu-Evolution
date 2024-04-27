# script to make graphical abstract / methods conceptual figure


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




## data prep

### trees
gaex <- bind_rows(trees.full, smalltrees.df) %>% 
  arrange(subtype, season, location, rep) %>% 
  filter(location %in% c("Georgia")) %>%
  filter(subtype %in% c("H3") & 
           season %in% c("2015-2016"))


evotree <- read.tree(text = gaex$tree.newick2[which(gaex$rep==0)])
st1 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][1])
st2 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][2])
st3 <- read.tree(text = gaex$smalltree[which(!is.na(gaex$smalltree))][3])


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
  filter(ew == "mid" & x > -0.5 & x < 7)


xwindows <- exdates %>% 
  filter(!ew %in% c("mid")) %>% 
  mutate(x2 = lead(x, n = 1), 
         col = case_when(year(date) %in% c(2015) & ew %in% c("start") ~ adjustcolor("green", alpha.f = 0.2), 
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





















## plot

### dendrogram
# plot(timetree, show.tip.label = FALSE) %>% print()

png(filename = "./03-Output/02-Figures/concept_timetree.png", width = 16, height = 9, units = "in", res = 300, pointsize = 10)

{
  par(mar = c(2,2,2,2))
plot(timetree, show.tip.label = FALSE, x.lim = c(-0.5, 7)) %>% 
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
axis(3, at = mean(unlist(xwindows[which(xwindows$ew%in%c("start") & !is.na(xwindows$col)), c("x", "x2")])), labels = "2015-2016\nFlu Season", tick = FALSE, lwd = 2, line = -1)

# tiplabels(pch = 16, cex = 1)
# tiplabels(pch = c(NA, 16)[as.numeric(timetree$tip.label%in%c(st1$tip.label, st2$tip.label, st3$tip.label))+1], cex = 1)

tiplabels(pch = case_when(timetree$tip.label%in%c(st1$tip.label) ~ 22,#15, 
                          timetree$tip.label%in%c(st2$tip.label) ~ 21,#16, 
                          timetree$tip.label%in%c(st3$tip.label) ~ 24,#17, 
                          TRUE ~ NA), 
          cex = 1.25, 
          bg = case_when(timetree$tip.label%in%c(st1$tip.label) ~ RColorBrewer::brewer.pal(3, "Dark2")[1], 
                           timetree$tip.label%in%c(st2$tip.label) ~ RColorBrewer::brewer.pal(3, "Dark2")[2], 
                           timetree$tip.label%in%c(st3$tip.label) ~ RColorBrewer::brewer.pal(3, "Dark2")[3],
                           TRUE ~ NA))

}


dev.off()









### phylogram



png(filename = "./03-Output/02-Figures/concept_evotree.png", width = 16, height = 9, units = "in", res = 300, pointsize = 10)
{
layout(matrix(c(1,1,2,
                1,1,3,
                1,1,4), nrow = 3, byrow = T))
# layout.show(n=4)

par(mar = c(2,2,2,2))

plot(evotree, show.tip.label = FALSE, x.lim = c(0,0.02)) %>% print()
tiplabels(pch = case_when(evotree$tip.label%in%c(st1$tip.label) ~ 22, 
                          evotree$tip.label%in%c(st2$tip.label) ~ 21, 
                          evotree$tip.label%in%c(st3$tip.label) ~ 24, 
                          TRUE ~ NA), 
          cex = 2, 
          bg = case_when(evotree$tip.label%in%c(st1$tip.label) ~ RColorBrewer::brewer.pal(3, "Dark2")[1], 
                         evotree$tip.label%in%c(st2$tip.label) ~ RColorBrewer::brewer.pal(3, "Dark2")[2], 
                         evotree$tip.label%in%c(st3$tip.label) ~ RColorBrewer::brewer.pal(3, "Dark2")[3],
                         TRUE ~ NA))

axis(1, at = c(0,0.005), labels = F, tcl = c(0.2), line = 1)
mtext("0.005 subs/site", side = 1, line = 0.5, at = c(0.005), adj = -0.1, cex = 1)
box("figure")




# plot(st3, show.tip.label = FALSE, x.lim = c(0,0.0025), y.lim = c(0,0.0015), type = "unrooted") %>% print()
plot(st3, show.tip.label = FALSE, x.lim = c(0,0.00175), type = "c") %>% print()
tiplabels(pch = 24, cex = 2, bg = RColorBrewer::brewer.pal(3, "Dark2")[3], xpd = T)
mpd <- mean(cophenetic(st3)[upper.tri(cophenetic(st3))])

text(par('usr')[2], par('usr')[4], 
     labels = paste0("MPD = ", ifelse(round(mpd, 4)==0, "< 0.0001", round(mpd, 5))), 
     adj = c(1, 1), 
     cex = 1)
# box("figure")
nodelabels(pch = 21, bg = "white")
nodelabels(node = Ntip(st3)+1, pch = 10)


# plot(st2, show.tip.label = FALSE, x.lim = c(0,0.0025), y.lim = c(0,0.0015), type = "unrooted") %>% print()
plot(st2, show.tip.label = FALSE, x.lim = c(0,0.00175), type = "c") %>% print()
tiplabels(pch = 21, cex = 2, bg = RColorBrewer::brewer.pal(3, "Dark2")[2], xpd = T)
mpd <- mean(cophenetic(st2)[upper.tri(cophenetic(st2))])

text(par('usr')[2], par('usr')[4], 
     labels = paste0("MPD = ", ifelse(round(mpd, 4)==0, "< 0.0001", round(mpd, 5))), 
     adj = c(1, 1), 
     cex = 1)
# box("figure")
nodelabels(pch = 21, bg = "white")
nodelabels(node = Ntip(st2)+1, pch = 10)

# plot(st1, show.tip.label = FALSE, x.lim = c(0,0.0025), y.lim = c(0,0.0015), type = "unrooted") %>% print()
plot(st1, show.tip.label = FALSE, x.lim = c(0,0.00175), type = "c") %>% print()
tiplabels(pch = 22, cex = 2, bg = RColorBrewer::brewer.pal(3, "Dark2")[1], xpd = T)
mpd <- mean(cophenetic(st1)[upper.tri(cophenetic(st1))])

text(par('usr')[2], par('usr')[4], 
     labels = paste0("MPD = ", ifelse(round(mpd, 7)==0, "< 0.00001", format(round(mpd, 7), scientific = F))), 
     adj = c(1, 1), 
     cex = 1)
# box("figure")
nodelabels(pch = 21, bg = "white")
nodelabels(node = Ntip(st1)+1, pch = 10)

axis(1, at = c(0,0.0005), labels = F, tcl = c(0.2), line = 1)
mtext("0.0005 subs/site", side = 1, line = 0.5, at = c(0.0005), adj = -0.1, cex = 1)

}







dev.off()





### spatial lags map


png(filename = "./03-Output/02-Figures/concept_spatiallags.png", width = 16, height = 9, units = "in", res = 300, pointsize = 10)
layout(matrix(c(1,1,2,3,2,4,2,5,2,6,2,7), ncol = 2, byrow = T), heights = c(5,1,1,1,1,1), widths = c(3,1))
# layout.show(n=7)
par(mar = c(1,0,0,0))
plot(usmap$geometry, border = "grey")
plot(usmap[which(!is.na(usmap$lag)),]["lag"], add = T, border = "black", pal = RColorBrewer::brewer.pal(4, "Dark2"))




{
plot.new()
plot.window(xlim = c(-1,4.5), ylim = c(-1,4.5))
par(mar = c(0,0,0,0))

xlefts <- rep(c(0:3), each = 4)
xrights <- xlefts + 1

ybottoms <- rep(c(0:3), 4)
ytops <- ybottoms + 1

vadjustcolor <- Vectorize(adjustcolor, vectorize.args = c("col", "alpha.f"))

rect(xleft = xlefts, 
     ybottom = ybottoms, 
     xright = xrights, 
     ytop = ytops, 
     col = vadjustcolor(rep(RColorBrewer::brewer.pal(4, "Dark2"), 
                           4), 
                       alpha.f = rep(c(1,0.6,0.4,0.2), 
                                     each = 4)))


axis(1, 
     at = 0:3+0.5, 
     labels = FALSE, 
     las = 2, 
     lwd = 0, 
     lwd.ticks = 0.5,
     padj = 0.5, 
     hadj = 1, 
     tcl = -0.2, 
     pos = 0)

axis(1, 
     at = 0:3+0.5, 
     labels = 0:3, 
     las = 1, 
     lwd = 0, 
     lwd.ticks = 0,
     padj = 0, 
     hadj = 0.5, 
     pos = 0)



arrows(x0=0.5,y0=-1,x1=3.5,y1=-1,length=0.125, angle = 30, code = 2, lwd = 1.5, xpd = T)
text(c(0.5, 3.5), rep(-1.1,2), labels = c("Current", "Past"), xpd = T, adj = c(0.5,1), font = 3, cex = 1)
text(mean(c(0.5,3.5)), -0.75, labels = "Temporal Lag", xpd = T, adj = c(0.5,0.5), font = 2)


text(mean(c(0.5,3.5)), 4.25, labels = "Season", xpd = T, adj = c(0.5,0), font = 3)
text(c(0:3+0.5), 4.1, labels = c("2015-2016", "2014-2015", "2013-2014", "2012-2013"), xpd = T, adj = c(0.5,-0.1), cex = 0.8)



axis(2, 
     at = 0:3+0.5, 
     labels = FALSE, 
     las = 2, 
     lwd = 0, 
     lwd.ticks = 0.5,
     padj = 0.5, 
     hadj = 1, 
     tcl = -0.2, 
     pos = 0)

axis(2, 
     at = 0:3+0.5, 
     labels = 0:3, 
     las = 2, 
     lwd = 0, 
     lwd.ticks = 0,
     padj = 0.5, 
     hadj = 0, 
     pos = 0)



arrows(x0=-1,y0=0.5,x1=-1,y1=3.5,length=0.125, angle = 30, code = 2, lwd = 1.5, xpd = T)
text(rep(-0.9,2), c(0.5, 3.5), labels = c("Local", "Distant"), xpd = T, adj = c(0.5,1), font = 3, srt = 90, cex = 1)
text(-0.75, mean(c(0.5,3.5)), labels = c("Spatial Lag"), srt = 90, xpd = T, adj = c(0.5,0.5), font = 2)






}
{
par(mar = c(0,0,0,0))

plot(usmap$geometry[which(!is.na(usmap$lag))], border = "grey")
plot(usmap[which(usmap$lag==3),]["lag"], add = T, border = "black", col = RColorBrewer::brewer.pal(4, "Dark2")[4])

plot(usmap$geometry[which(!is.na(usmap$lag))], border = "grey")
plot(usmap[which(usmap$lag==2),]["lag"], add = T, border = "black", col = RColorBrewer::brewer.pal(4, "Dark2")[3])

plot(usmap$geometry[which(!is.na(usmap$lag))], border = "grey")
plot(usmap[which(usmap$lag==1),]["lag"], add = T, border = "black", col = RColorBrewer::brewer.pal(4, "Dark2")[2])

plot(usmap$geometry[which(!is.na(usmap$lag))], border = "grey")
plot(usmap[which(usmap$lag==0),]["lag"], add = T, border = "black", col = RColorBrewer::brewer.pal(4, "Dark2")[1])
}

dev.off()






## clean environment
rm(list = ls())
gc()