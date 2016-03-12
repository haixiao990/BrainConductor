setwd("C:/Users/UikosPC/Dropbox/Collaboration and People/Felix_Kevin/Felix_Kevin_Han-seniorthesis2015-16/data")
load("ABIDE_50002_matrix_2015-12-07.RData")
dat = dat$dat
idx = which(dat[1,] != 0)
dat = dat[,idx]
dat = dat[,c(1:50)]

dat2 = scale(dat, center = TRUE, scale = FALSE)
ylim = c(-5000, 5000)

setwd("C:/Users/UikosPC/Dropbox/figure")
png("fmri_timeseries.png", height = 8, width = 8, res = 300, units = "in")
par(mfrow = c(5, 5), mar = rep(1, 4))
for(i in 1:25){
  plot(dat2[,i], ylim = ylim, type = "l", lwd = 2)
}
dev.off()