# script to make figure showing inclusion and exclusion of isolates & sequences, and frequencies of final data points





# png(filename = "./03-Output/02-Figures/data-flowchart.png", height = 8, width = 10, units = "in", res = 300, pointsize = 10)
svg(filename = "./03-Output/02-Figures/figure_data-flowchart.svg", height = 8, width = 10, pointsize = 10)


plot.new()
plot.window(xlim = c(-2,16), ylim = c(0,20))


par(mar = c(1,9.1,1,1))



xgridleft <- seq(0,20, by = 3)
xgridright <- xgridleft +2

ygridbottom <- seq(20,0, by = -2)
ygridtop <- ygridbottom +1

xgridleft;xgridright;ygridbottom;ygridtop

xlefts <- c(7.5,7.5,7.5,7.5)
ybottoms <- c(19,16,13,10)

xrights <- xlefts + 2
ytops <- ybottoms + 1



rect(xleft = xlefts,
     ybottom = ybottoms, 
     xright = xrights, 
     ytop = ytops, 
     col = c(rep(adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[1], alpha.f = 0.2), 2), 
             rep(adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[2], alpha.f = 0.2), 1), 
             rep(adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[3], alpha.f = 0.2), 1)))


arrows(x0 = (xrights+xlefts)/2, 
       x1 = (xrights+xlefts)/2, 
       y0 = ybottoms[-length(ybottoms)], 
       y1 = ytops[-1], 
       length = 0.1,
       angle = 45, 
       code = 2)

# text(x = xlefts+0.1, y = ytops-0.1, adj = c(0,1), labels = paste0("N = ", c("52 254", "50 067", "50 195", "42 113")))
text(x = xrights-0.1, y = (ybottoms+ytops)/2, adj = c(1,0.5), labels = paste0("N = ", c("52 254", "50 067", "50 195", "42 113")))


text(x = xlefts[1], y = ytops[1]+0.1, adj = c(1, 0), cex = 0.8, labels = c("Isolates in Downloaded Metadata"), font = 4)

text(x = xlefts[3], y = ytops[3]+0.1, adj = c(1, 0), cex = 0.8, labels = c("Sequences Downloaded"), font = 4)



# xgridleft;xgridright;ygridbottom;ygridtop
xlefts <- c(3,6,9,12, 
            3,6,9,12, 
            3,6,9,12)
ybottoms <- c(0,0,0,0,
              rep(3,4),
              rep(6,4))

xrights <- xlefts + 2
ytops <- ybottoms + 1



rect(xleft = xlefts,
     ybottom = ybottoms, 
     xright = xrights, 
     ytop = ytops, 
     col = c(rep(adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[5], alpha.f = 0.2), 4), 
             rep(adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[4], alpha.f = 0.2), 4), 
             rep(adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[3], alpha.f = 0.2), 4)))

segments(x0 = min((xlefts+xrights)/2), x1 = max((xlefts+xrights)/2), 
         y0 = 8.5, y1 = 8.5)
segments(x0 = 8.5, x1 = 8.5, 
         y0 = 10, y1 = 8.5)

arrows(x0 = (xrights+xlefts)/2, 
       x1 = (xrights+xlefts)/2, 
       y0 = rep(c(8.5, unique(ybottoms[-(1:4)])[order(unique(ybottoms[-(1:4)]), decreasing = T)]), each = 4), 
       y1 = ytops[order(ytops, decreasing = T)], 
       length = 0.1,
       angle = 45, 
       code = 2)



text(x = xrights-0.1, y = (ybottoms+ytops)/2, adj = c(1,0.5), 
     labels = paste0("N = ", c("838", "857", "1 384", "1 925", 
                               "409", "397", "490", "510", 
                               "6 371", "4 670", "12 243", "18 829")))

text(x = xrights, y = ybottoms-0.1, adj = c(1, 1), cex = 0.8, labels = c(rep("Subtrees", 4), rep("Trees", 4), c("BVic", "BYam", "H1", "H3")), font = 3)








# xgridleft;xgridright;ygridbottom;ygridtop
xlefts <- c(10.5,13.5,13.5)
ybottoms <- c(17.5, 18.5, 16.5)

xrights <- xlefts + 2
ytops <- ybottoms + 1



rect(xleft = xlefts,
     ybottom = ybottoms, 
     xright = xrights, 
     ytop = ytops,
     lty = 4, 
     border = rgb(1,0,0,1), 
     col = rgb(1,0,0,0.2))



segments(x0 = 13, x1 = 13, 
         y0 = 19, y1 = 17, 
         lty = 4, col = rgb(1,0,0,1))
segments(x0 = 12.5, x1 = 13, 
         y0 = 18, y1 = 18, 
         lty = 4, col = rgb(1,0,0,1))

arrows(x0 = c(8.5, 13, 13), 
       x1 = c(10.5, 13.5, 13.5), 
       y0 = c(18, 19, 17), 
       y1 = c(18, 19, 17), 
       length = 0.05,
       angle = 45, 
       code = 2, 
       lty = 4, col = rgb(1,0,0,1))


text(x = xrights-0.1, y = (ybottoms+ytops)/2, adj = c(1,0.5), 
     labels = paste0("N = ", c("2 187", "1 894", "293")))

text(x = xlefts[1], y = ytops[1]+0.1, adj = c(0, 0), cex = 0.8, labels = c("Excluded Isolates"), font = 4)

text(x = xrights[-1], y = ybottoms[-1]-0.1, adj = c(1, 1), cex = 0.8, labels = c("Not BVic,BYam,H1,H3", "No State-level Location"), font = 3)




# xgridleft;xgridright;ygridbottom;ygridtop
xlefts <- c(10.5,13.5,13.5,13.5)
ybottoms <- c(11.5, 13.5, 11.5, 9.5)

xrights <- xlefts + 2
ytops <- ybottoms + 1



rect(xleft = xlefts,
     ybottom = ybottoms, 
     xright = xrights, 
     ytop = ytops,
     lty = 4, 
     border = rgb(1,0,0,1), 
     col = rgb(1,0,0,0.2))




segments(x0 = 13, x1 = 13, 
         y0 = 14, y1 = 10, 
         lty = 4, col = rgb(1,0,0,1))
segments(x0 = 12.5, x1 = 13, 
         y0 = 12, y1 = 12, 
         lty = 4, col = rgb(1,0,0,1))

arrows(x0 = c(8.5, 13, 13, 13), 
       x1 = c(10.5, 13.5, 13.5, 13.5), 
       y0 = c(12, 14, 10), 
       y1 = c(12, 14, 10), 
       length = 0.05,
       angle = 45, 
       code = 2, 
       lty = 4, col = rgb(1,0,0,1))

text(x = xrights-0.1, y = (ybottoms+ytops)/2, adj = c(1,0.5), 
     labels = paste0("N = ", c("8 082", "454", "16", "7 612")))

text(x = xlefts[1], y = ytops[1]+0.1, adj = c(0, 0), cex = 0.8, labels = c("Excluded Sequences"), font = 4)

text(x = xrights[-1], y = ybottoms[-1]-0.1, adj = c(1, 1), cex = 0.8, labels = c("< 1 600 bps", "Poor Alignment", "Duplicates"), font = 3)





# xgridleft;xgridright;ygridbottom;ygridtop
xlefts <- c(0)
ybottoms <- c(1.5)

xrights <- xlefts + 2
ytops <- ybottoms + 1



rect(xleft = xlefts,
     ybottom = ybottoms, 
     xright = xrights, 
     ytop = ytops,
     lty = 4, 
     border = rgb(1,0,0,1), 
     col = rgb(1,0,0,0.2))




arrows(x0 = 4, 
       x1 = 2, 
       y0 = 2, 
       y1 = 2, 
       length = 0.05,
       angle = 45, 
       code = 2, 
       lty = 4, col = rgb(1,0,0,1))


text(x = xrights-0.1, y = (ybottoms+ytops)/2, adj = c(1,0.5), 
     labels = paste0("N = ", c("1")))

text(x = xlefts[1], y = ytops[1]+0.1, adj = c(0, 0), cex = 0.8, labels = c("Failed Tree Estimation"), font = 4)

text(x = xrights, y = ybottoms-0.1, adj = c(1, 1), cex = 0.8, labels = c("Sequences too Similar"), font = 3)





# 
# 
# axis(2, at = c(0:20), xpd = T)
# abline(h = 0:20)

axis(2, at = c(16,20), labels = F, tcl = 0.2)
axis(2, at = mean(c(16,20)), labels = "Metadata\nProcessing", las = 2, line = -0.5, tick = F, font = 2)
rect(-10, 16, -0.5, 20, 
     col = adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[1], alpha.f = 0.2), 
     xpd = T, 
     border = NA)

axis(2, at = c(11.5,15.5), labels = F, tcl = 0.2)
axis(2, at = mean(c(11.5,15.5)), labels = "Sequence\nProcessing", las = 2, line = -0.5, tick = F, font = 2)
rect(-10, 11.5, -0.5, 15.5, 
     col = adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[2], alpha.f = 0.2), 
     xpd = T, 
     border = NA)


axis(2, at = c(6,11), labels = F, tcl = 0.2)
axis(2, at = mean(c(6,11)), labels = "Included\nSequences / Isolates", las = 2, line = -0.5, tick = F, font = 2)
rect(-10, 6, -0.5, 11, 
     col = adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[3], alpha.f = 0.2), 
     xpd = T, 
     border = NA)


axis(2, at = c(1.5,5.5), labels = F, tcl = 0.2)
axis(2, at = mean(c(1.5,5.5)), labels = "Phylogenetic\nReconstructions", las = 2, line = -0.5, tick = F, font = 2)
rect(-10, 1.5, -0.5, 5.5, 
     col = adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[4], alpha.f = 0.2), 
     xpd = T, 
     border = NA)

axis(2, at = c(0,1), labels = F, tcl = 0.2)
axis(2, at = mean(c(0,1)), labels = "Tree\nPruning", las = 2, line = -0.5, tick = F, font = 2)
rect(-10, 0, -0.5, 1, 
     col = adjustcolor(RColorBrewer::brewer.pal(5, "Set2")[5], alpha.f = 0.2), 
     xpd = T, 
     border = NA)

dev.off()








rm(list = ls())
gc()