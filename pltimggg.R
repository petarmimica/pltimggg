pltimggg.plot2D <- function(x, y, z, xlim = c(0, 0), ylim = c(0, 0), zlim = c(0, 0), xlab = "x", ylab = "y", zlab = "z", colours = NULL, zbarwidth = 20, zbarheight = 1, ztitleposition = "top", ztitlehjust = 0.5, ztitlevjust = 0.5, legend.position = "bottom", legend.direction = "horizontal", zlog = FALSE, conv.kernel = NULL, zlinformat = FALSE, zdigits = 1, ztitleorientation = NULL, theme = NULL, igeom = 0) {
    
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
    
    
    # expand the coordinate grid
    exp.coords <- expand.grid(x, y)
    
    # melt the 2D array
    z.melt <- melt(z)

    # geometry transformations
    if (igeom==2) { # spherical geometry
        
        # compute the spherical grid discretization intervals
        dx <- x[2] - x[1]
        dy <- y[2] - y[1]
        
        # generate four corners for each element (inner-left, inner-right, outer-left, outer-right)
        exp.coords$xIL <- (exp.coords$Var1 - dx / 2) * sin(exp.coords$Var2 - dy / 2)
        exp.coords$xIR <- (exp.coords$Var1 - dx / 2) * sin(exp.coords$Var2 + dy / 2)
        exp.coords$xOL <- (exp.coords$Var1 + dx / 2) * sin(exp.coords$Var2 - dy / 2)
        exp.coords$xOR <- (exp.coords$Var1 + dx / 2) * sin(exp.coords$Var2 + dy / 2)
        exp.coords$yIL <- (exp.coords$Var1 - dx / 2) * cos(exp.coords$Var2 - dy / 2)
        exp.coords$yIR <- (exp.coords$Var1 - dx / 2) * cos(exp.coords$Var2 + dy / 2)
        exp.coords$yOL <- (exp.coords$Var1 + dx / 2) * cos(exp.coords$Var2 - dy / 2)
        exp.coords$yOR <- (exp.coords$Var1 + dx / 2) * cos(exp.coords$Var2 + dy / 2)
        
        # fake the limits
        exp.coords$Var1 = c(min(exp.coords$xIL), max(exp.coords$xOR))
        exp.coords$Var2 = c(min(exp.coords$yIL), max(exp.coords$yOL))
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
    
    # check if logarithm
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
        
    } else
        labels <- function(breaks) {
            sapply(breaks, function(x) {
                val <- as.numeric(x)
                formatC(val, digits = zdigits, width = zdigits, format = "e")
            })
        }
    

    if (igeom == 0) {
        # create a data frame for plotting
        df <- data.frame(x = exp.coords$Var1, y = exp.coords$Var2, z = z.melt$value)
        
        
        # Cartesian plot using tiles
        plt <- ggplot(data = df, aes(x = x, y = y, fill = z)) + mytheme + geom_tile() + coord_equal() + scale_x_continuous(limits = xlim, expand = c(0, 0)) + scale_y_continuous(limits = ylim, expand = c(0, 0)) + scale_fill_gradientn(labels = labels, colours = colours, guide = guide_colourbar(title = zlab, title.position = ztitleposition, title.hjust = ztitlehjust, title.vjust = ztitlevjust, barwidth = zbarwidth, barheight = zbarheight, title.theme = element_text(angle=titleangle)), limits = zlim) + theme(legend.position = legend.position, legend.direction = legend.direction) + xlab(xlab) + ylab(ylab)
    } else {
        # general geometry plot using polygons
        
        # following the instructions given in the man page of geom_polygon()
        
        # first, generate a list of ids
        ids = 1:length(z.melt$value)
        
        # create a data frame with image only
        vals <- data.frame(id = ids, z = z.melt$value)
        
        # for coordinates we will create a data frame that contains the four vertices of each pixel and  assign them the same id that corresponds to the image pixel
        xs <- unlist(lapply(1:length(z.melt$value), function(i) {
          c(exp.coords$xIL[i], exp.coords$xIR[i], exp.coords$xOR[i], exp.coords$xOL[i])  
        }))
    
            ys <- unlist(lapply(1:length(z.melt$value), function(i) {
            c(exp.coords$yIL[i], exp.coords$yIR[i], exp.coords$yOR[i], exp.coords$yOL[i])  
        }))
        pos <- data.frame(id = rep(ids, each=4), x = xs, y = ys)
        
        df <- merge(vals, pos, by = c("id"))
        
        plt <- ggplot(data = df, aes(x = x, y = y)) + mytheme + geom_polygon(aes(fill = z, group=id)) + coord_equal() + scale_x_continuous(limits = xlim, expand = c(0, 0)) + scale_y_continuous(limits = ylim, expand = c(0, 0)) + scale_fill_gradientn(labels = labels, colours = colours, guide = guide_colourbar(title = zlab, title.position = ztitleposition, title.hjust = ztitlehjust, title.vjust = ztitlevjust, barwidth = zbarwidth, barheight = zbarheight, title.theme = element_text(angle=titleangle)), limits = zlim) + theme(legend.position = legend.position, legend.direction = legend.direction) + xlab(xlab) + ylab(ylab)
    }
    
    return(plt)
    
}


degrade <- function(x, y, img, skip) {
    nx <- length(x)
    ny <- length(y)
    
    dx <- x[seq(1, nx, skip)]
    dy <- y[seq(1, ny, skip)]
    dimg <- img[seq(1, nx, skip), seq(1, ny, skip)]
    
    retval <- list(x = dx, y = dy, img = dimg)
    
    return(retval)
}

