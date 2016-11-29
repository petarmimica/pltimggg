library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(spatialfil)

source("~/Dropbox/WHIM_project/pltimggg.R")

nx <- 100
ny <- 100

xlim = c(0e0, 1e0)
ylim = c(0e0, 1e0)

x <- (0 : (nx - 1)) * (xlim[2] - xlim[1]) / (nx - 1) + xlim[1]
y <- (0 : (ny - 1)) * (ylim[2] - ylim[1]) / (ny - 1) + ylim[1]

img <- array(1e0, c(nx, ny))
img <- 10^(apply(img, c(1, 2), function(x) {rnorm(1, 0.5, 0.3) }) * 2e0) * 1e-15

#plt.nonconv <- pltimggg.plot2D(x, y, img, zlog=TRUE, xlim = c(0e0, 1e0), ylim = c(0e0, 1e0), zlim=c(1e-15, 1e-13), zlab = "intensity", xlab = "length", ylab = "width")

#plt.conv <- pltimggg.plot2D(x, y, img, zlog=TRUE, xlim = c(0e0, 1e0), ylim = c(0e0, 1e0), zlim=c(1e-15, 1e-13), zlab = "intensity", xlab = "length", ylab = "width", conv.kernel = 0.01e0, zlinformat = TRUE)

plt.vert <- pltimggg.plot2D(x, y, img, zlog=TRUE, xlim = c(0e0, 1e0), ylim = c(0e0, 1e0), zlim=c(1e-15, 1e-13), zlab = "intensity", xlab = "length", ylab = "width", legend.position = "right", legend.direction = "vertical", zbarwidth = 1, zbarheight = 15, ztitleposition = "right")

plt.conv.vert <- pltimggg.plot2D(x, y, img, zlog=TRUE, xlim = c(0e0, 1e0), ylim = c(0e0, 1e0), zlim=c(1e-15, 1e-13), zlab = "intensity", xlab = "length", ylab = "width", conv.kernel = 0.02e0, zlinformat = TRUE, , legend.position = "right", legend.direction = "vertical", zbarwidth = 1, zbarheight = 15, ztitleposition = "right")


grid.arrange(plt.vert, plt.conv.vert, ncol=1)
