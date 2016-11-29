pltimggg.plot2D <- function(x, y, z, xlim = c(0, 0), ylim = c(0, 0), zlim = c(0, 0), xlab = "x", ylab = "y", zlab = "z", colours = NULL, zbarwidth = 20, zbarheight = 1, ztitleposition = "top", ztitlehjust = 0.5, ztitlevjust = 0.5, legend.position = "bottom", legend.direction = "horizontal", zlog = FALSE, conv.kernel = NULL, zlinformat = FALSE, zdigits = 3) {
    
    # my theme
    mytheme <-  theme_bw()+theme(axis.text=element_text(size=18))+theme(axis.title=element_text(size=18))+theme(axis.line=element_line(size=2))+theme(legend.text=element_text(size=16))+theme(legend.title=element_text(size=18))+theme(panel.grid.major = element_line(size=0.5, colour="gray"), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    
    # compute the limits
    if (xlim[1] == 0 && xlim[2] == 0) {
        # default: use min/max of x
        xlim <- c(min(x), max(x))
    }
    
    if (ylim[1] == 0 && ylim[2] == 0) {
        # default: use min/max of y
        ylim <- c(min(y), max(y))
    }
    
    if (zlim[1] == 0 && zlim[2] == 0) {
        # default: use min/max of z
        zlim <- c(min(z), max(z))
    }
    
    # check if logarithm
    if (zlog) {
        z <- log10(z)
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
    
    # create a data frame for plotting
    df <- data.frame(x = exp.coords$Var1, y = exp.coords$Var2, z = z.melt$value)
    
    # initialize colour bar if not present
    if (is.null(colours)) {
        colours <- c(colorRampPalette(c("black", "blue"))(100), colorRampPalette(c("blue", "red"))(100), colorRampPalette(c("red", "yellow"))(100), colorRampPalette(c("yellow", "white"))(100))
    }
    
    if (legend.direction == "vertical") {
        titleangle <- 90
        if (zbarheight == 1)
            zbarheight <- 20
        if (zbarwidth == 20)
            zbarwidth <- 1
        if (legend.position == "bottom")
            legend.position <- "right"
        if (ztitleposition == "top")
            ztitleposition <- "right"
    } else
        titleangle <- 0
    
    plt <- ggplot(data = df, aes(x = x, y = y, fill = z)) + mytheme + geom_tile() + coord_equal() + scale_x_continuous(limits = xlim, expand = c(0, 0)) + scale_y_continuous(limits = ylim, expand = c(0, 0)) + scale_fill_gradientn(labels = labels, colours = colours, guide = guide_colourbar(title = zlab, title.position = ztitleposition, title.hjust = ztitlehjust, title.vjust = ztitlevjust, barwidth = zbarwidth, barheight = zbarheight, title.theme = element_text(angle=titleangle)), limits = zlim) + theme(legend.position = legend.position, legend.direction = legend.direction) + xlab(xlab) + ylab(ylab)
    
    return(plt)
    
}
