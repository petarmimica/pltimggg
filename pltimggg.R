# This function plots an 2D array "z" given axes vectors "x" and "y".
pltimggg.plot2D <- function(x, y, z, xlim = c(0, 0), ylim = c(0, 0), zlim = c(0, 0), xlab = "x", ylab = "y", zlab = "z", colours = NULL, zbarwidth = 20, zbarheight = 1, ztitleposition = "top", ztitlehjust = 0.5, ztitlevjust = 0.5, legend.position = "bottom", legend.direction = "horizontal", zlog = FALSE, conv.kernel = NULL, zlinformat = FALSE, zdigits = 1, ztitleorientation = NULL, theme = NULL, igeom = 0, xlog = FALSE, ylog = FALSE, aspect = TRUE, xlinformat = FALSE, ylinformat = FALSE, xdigits = 1, ydigits = 1, clip= FALSE) {
    
    # my theme
    if (is.null(theme)) {
        mytheme <-  theme_bw()+theme(axis.text=element_text(size=18))+theme(axis.title=element_text(size=18))+theme(axis.line=element_line(size=2))+theme(legend.text=element_text(size=16))+theme(legend.title=element_text(size=18))+theme(panel.grid.major = element_line(size=0.5, colour="gray"), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    } else {
        mytheme <- theme
    }
    
    # initialize the colour bar if not present
    if (is.null(colours)) {
        colours <- c(colorRampPalette(c("black", "blue"))(100), colorRampPalette(c("blue", "red"))(100), colorRampPalette(c("red", "yellow"))(100), colorRampPalette(c("yellow", "white"))(100))
    }
    
    if (legend.direction == "vertical") {
        if (is.null(ztitleorientation))
            titleangle <- 90
        else
            titleangle <- ztitleorientation
        if (zbarheight == 1)
            zbarheight <- 20
        if (zbarwidth == 20)
            zbarwidth <- 1
        if (legend.position == "bottom")
            legend.position <- "right"
        if (ztitleposition == "top")
            ztitleposition <- "right"
    } else {
        if (is.null(ztitleorientation)) {
            titleangle <- 0
        } else {
            titleangle <- ztitleorientation
        }
    }
    
    
    # convolve if necessary
    if (!is.null(conv.kernel)) {
        kres <- conv.kernel / (x[2] - x[1])
        print(paste("Convolving with sigma = ", conv.kernel, " (pixels = ", kres, ")"))
        kernel <- convKernel(sigma = kres, k = 'gaussian')
        z <- applyFilter(x = z, kernel = kernel)
    }
    
    # expand the coordinate grids
    xm <- 5e-1 * (x[1:(length(x)-1)] + x[2:length(x)])
    xm <- c(x[1] - 5e-1 * (x[2] - x[1]), xm)
    xp <- 5e-1 * (x[1:(length(x)-1)] + x[2:length(x)])
    xp <- c(xp, x[length(x)] + 5e-1 * (x[length(x)] - x[length(x) - 1]))
    ym <- 5e-1 * (y[1:(length(y)-1)] + y[2:length(y)])
    ym <- c(y[1] - 5e-1 * (y[2] - y[1]), ym)
    yp <- 5e-1 * (y[1:(length(y)-1)] + y[2:length(y)])
    yp <- c(yp, y[length(y)] + 5e-1 * (y[length(y)] - y[length(y) - 1]))
    
    exp.coords <- expand.grid(x, y)
    exp.coords.xm <- expand.grid(xm, y)
    exp.coords.xp <- expand.grid(xp, y)
    exp.coords.ym <- expand.grid(x, ym)
    exp.coords.yp <- expand.grid(x, yp)
    
    # clip if necesary
    if (clip) {
        z[z<zlim[1]] <- zlim[1]
        z[z>zlim[2]] <- zlim[2]
    }
    
    # melt the 2D array
    z.melt <- melt(z)

    # geometry transformations
    if (igeom==2) { # spherical geometry
        
        # compute the spherical grid discretization intervals
        exp.coords$rm = exp.coords.xm$Var1
        exp.coords$rp = exp.coords.xp$Var1
        exp.coords$tm = exp.coords.yp$Var2
        exp.coords$tp = exp.coords.ym$Var2
        
        exp.coords$stm <- sin(exp.coords$tm)
        exp.coords$stp <- sin(exp.coords$tp)
        exp.coords$ctm <- cos(exp.coords$tm)
        exp.coords$ctp <- cos(exp.coords$tp)
        
        # generate the four corners of each element (inner-left, inner-right, outer-left, outer-right)
        exp.coords$xIL <- exp.coords$rm * exp.coords$stm
        exp.coords$xIR <- exp.coords$rm * exp.coords$stp
        exp.coords$xOL <- exp.coords$rp * exp.coords$stm
        exp.coords$xOR <- exp.coords$rp * exp.coords$stp
        exp.coords$yIL <- exp.coords$rm * exp.coords$ctm
        exp.coords$yIR <- exp.coords$rm * exp.coords$ctp
        exp.coords$yOL <- exp.coords$rp * exp.coords$ctm
        exp.coords$yOR <- exp.coords$rp * exp.coords$ctp
    
        # fake the limits
        exp.coords$Var1 = rep(min(exp.coords$xIL), length(exp.coords$Var1))
        exp.coords$Var1[1] = max(exp.coords$xOL)
        exp.coords$Var2 = rep(min(exp.coords$yIL), length(exp.coords$Var2))
        exp.coords$Var2[1] = max(exp.coords$yOL)
            
    }
    
    if (igeom == 0) { # Cartesian geometry
        
        # compute the grid discretization intervals
        dxm <- x[-1] - x[-length(x)]
        dxm <- c(dxm[1], dxm)
        dxp <- dxm
        dxp <- c(dxp, dxp[length(dxp)])
        dym <- y[-1] - y[-length(y)]
        dym <- c(dym[1], dym)
        dyp <- dym
        dyp <- c(dyp, dyp[length(dyp)])
        
        exp.coords$xm = exp.coords.xm$Var1
        exp.coords$xp = exp.coords.xp$Var1
        exp.coords$ym = exp.coords.yp$Var2
        exp.coords$yp = exp.coords.ym$Var2
        
        # generate the four corners of each element (inner-left, inner-right, outer-left, outer-right)
        exp.coords$xIL <- exp.coords$xm
        exp.coords$xIR <- exp.coords$xp
        exp.coords$xOL <- exp.coords$xm
        exp.coords$xOR <- exp.coords$xp
        exp.coords$yIL <- exp.coords$ym
        exp.coords$yIR <- exp.coords$ym
        exp.coords$yOL <- exp.coords$yp
        exp.coords$yOR <- exp.coords$yp
        
        # fake the limits
        exp.coords$Var1 = rep(min(exp.coords$xIL), length(exp.coords$Var1))
        exp.coords$Var1[1] = max(exp.coords$xOL)
        exp.coords$Var2 = rep(min(exp.coords$yIL), length(exp.coords$Var2))
        exp.coords$Var2[1] = max(exp.coords$yOL)
        
    }
    
    
    
    # compute the limits
    if (xlim[1] == 0 && xlim[2] == 0) {
        # default: use min/max of x
        xlim <- c(min(exp.coords$Var1), max(exp.coords$Var1))
    }
    
    if (ylim[1] == 0 && ylim[2] == 0) {
        # default: use min/max of y
        ylim <- c(min(exp.coords$Var2), max(exp.coords$Var2))
    }
    
    if (zlim[1] == 0 && zlim[2] == 0) {
        # default: use min/max of z
        zlim <- c(min(z.melt$value), max(z.melt$value))
    }
    
    # check if logarithm of z
    if (zlog) {
        z.melt$value <- log10(z.melt$value)
        zlim <- log10(zlim)
        if (!zlinformat) {
            labels <- function(breaks) {
                sapply(breaks, function(x) {
                    text <- as.character(x)
                    bquote("10"^~.(text))
                })
            }
        } else {
            labels <- function(breaks) {
                sapply(breaks, function(x) {
                    val <- as.numeric(10^x)
                    formatC(val, digits = zdigits, width = zdigits, format = "e")
                })
            }
        }
        
    } else {
        if (zlinformat) {
            labels <- function(breaks) {
                sapply(breaks, function(x) {
                    val <- as.numeric(x)
                    formatC(val, digits = zdigits, width = zdigits, format = "e")
                })
            }
        } else {
            labels <- function(breaks) {
                sapply(breaks, function(x) {
                    text <- as.character(x)
                    text
                })
            }  
        }
    }
    

    # check for logarithms of x and y
    if (xlog) {
        if (!xlinformat) {
            xlabels <- function(breaks) {
                sapply(breaks, function(x) {
                    text <- as.character(log10(x))
                    bquote("10"^~.(text))
                })
            }
        } else {
            xlabels <- function(breaks) {
                sapply(breaks, function(x) {
                    val <- as.numeric(x)
                    formatC(val, digits = xdigits, width = xdigits, format = "e")
                })
            }
        }
        xscl <- scale_x_log10(limits = xlim, expand = c(0, 0), labels = xlabels)
    } else { 
        if (xlinformat) {
            xlabels <- function(breaks) {
                sapply(breaks, function(x) {
                    val <- as.numeric(x)
                    formatC(val, digits = xdigits, width = xdigits, format = "e")
                })
            }
        } else {
            xlabels <- function(breaks) {
                sapply(breaks, function(x) {
                    text <- as.character(x)
                    text
                })
            }     
        }
        xscl <- scale_x_continuous(limits = xlim, expand = c(0, 0), labels = xlabels)
    }
    
    if (ylog) {
        if (!ylinformat) {
            ylabels <- function(breaks) {
                sapply(breaks, function(x) {
                    text <- as.character(log10(x))
                    bquote("10"^~.(text))
                })
            }
        } else {
            ylabels <- function(breaks) {
                sapply(breaks, function(x) {
                    val <- as.numeric(x)
                    formatC(val, digits = xdigits, width = ydigits, format = "e")
                })
            }
        }
        yscl <- scale_y_log10(limits = ylim, expand = c(0, 0), labels = ylabels)
    } else { 
        if (ylinformat) {
            ylabels <- function(breaks) {
                sapply(breaks, function(x) {
                    val <- as.numeric(x)
                    formatC(val, digits = xdigits, width = ydigits, format = "e")
                })
            }
        } else {
            ylabels <- function(breaks) {
                sapply(breaks, function(x) {
                    text <- as.character(x)
                    text
                })
            }     
        }
        yscl <- scale_y_continuous(limits = ylim, expand = c(0, 0), labels = ylabels)
    }
    
    
    # check for aspect ratio
    if (aspect)
        asp <- coord_equal()
    else
        asp <- NULL
    
    # general geometry plot using polygons
        
    # following the instructions given in the man page of geom_polygon()
    
    # first, generate a list of ids
    ids = 1:length(z.melt$value)
    
    # create a data frame with image only
    vals <- data.table(id = ids, z = z.melt$value)
    
    # for coordinates we will create a data frame that contains the four vertices of each pixel and  assign them the same id that corresponds to the image pixel
    xs <- unlist(lapply(1:length(z.melt$value), function(i) {
        c(exp.coords$xIL[i], exp.coords$xIR[i], exp.coords$xOR[i], exp.coords$xOL[i])  
    }))
    
    ys <- unlist(lapply(1:length(z.melt$value), function(i) {
        c(exp.coords$yIL[i], exp.coords$yIR[i], exp.coords$yOR[i], exp.coords$yOL[i])  
    }))
    pos <- data.table(id = rep(ids, each=4), x = xs, y = ys)
    
    df <- merge(vals, pos, by = c("id"))
    
    plt <- ggplot(data = df, aes(x = x, y = y, group=id)) + mytheme + geom_polygon(aes(fill = z, color=z))  + asp + xscl + yscl + scale_fill_gradientn(labels = labels, colours = colours, guide = guide_colourbar(title = zlab, title.position = ztitleposition, title.hjust = ztitlehjust, title.vjust = ztitlevjust, barwidth = zbarwidth, barheight = zbarheight, title.theme = element_text(angle=titleangle)), limits = zlim) + theme(legend.position = legend.position, legend.direction = legend.direction) + xlab(xlab) + ylab(ylab) + scale_colour_gradientn(labels = labels, colours = colours, guide = guide_colourbar(title = zlab, title.position = ztitleposition, title.hjust = ztitlehjust, title.vjust = ztitlevjust, barwidth = zbarwidth, barheight = zbarheight, title.theme = element_text(angle=titleangle)), limits = zlim)
    
    return(plt)
    
}


# This function "degrades" a 2D image "img" and the vectors "x" and "y" by returning every "skip"-th element.
degrade <- function(x, y, img, skip) {
    nx <- length(x)
    ny <- length(y)
    
    dx <- x[seq(1, nx, skip)]
    dy <- y[seq(1, ny, skip)]
    dimg <- img[seq(1, nx, skip), seq(1, ny, skip)]
    
    retval <- list(x = dx, y = dy, img = dimg)
    
    return(retval)
}
