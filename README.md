Prerequisites
-------------

You need to install the following packages: ggplot2, reshape2 and
RColorBrewer. If you plan to use plotimggg to convolve the arrays, you
also need to install spatialfil.

A reminder: to install these packages, you can run the following code:

    install.packages("ggplot2")
    install.packages("reshape2")
    install.packages("RColorBrewer")
    install.packages("spatialfil")

Including the plotting routine
------------------------------

For the time being the plotting routine is not an R package, but rather
it should be included using the source function. The file can be found
here: [plotimggg.R](http://www.uv.es/mimica/Rblog/pltimggg.R). Put the
file somewhere (in your working directory, for example) and include it:

    source("pltimggg.R")

2D plots
--------

### Basic usage

For it to make any sense to use pltimggg, you need to have a 2D array
and the corresponding vectors corresponding to each axis. In this
example we will generate a 100x100 array with axes spanning the interval
\[1, 11\].

    # define array dimensions
    nx <- 100
    ny <- 100

    # axes limits
    xlim = c(1e0, 11e0)
    ylim = c(1e0, 11e0)

    # compute axes
    x <- (0 : (nx - 1)) * (xlim[2] - xlim[1]) / (nx - 1) + xlim[1]
    y <- (0 : (ny - 1)) * (ylim[2] - ylim[1]) / (ny - 1) + ylim[1]

    # declare array
    img <- array(1e0, c(nx, ny))

    # fill array with random numbers in a certain interval
    img <- 10^(apply(img, c(1, 2), function(x) {rnorm(1, 0.5, 0.3) }) * 2e0) * 1e-15

Now we can try to plot the image using the default settings:

    pltimggg.plot2D(x, y, img)

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### Parameters

Now we review the parameters that can be passed to pltimggg.plot2D.

#### zlog: logarithmic scale for the values, default zlog = FALSE

To plot the image in logarithmic scale set zlog to TRUE:

    pltimggg.plot2D(x, y, img, zlog=TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

#### legend.direction: colourbar direction, default legend.direction = "horizontal"

To plot the colourbar on the right set legend.direction to "vertical":

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical")

![](README_files/figure-markdown_strict/unnamed-chunk-6-1.png)

#### xlab, ylab and zlab: axes and colourbar labels, default: "x", "y" and "z", respectively

The labels of the axes and the colourbar can be changed by giving the
appropriate values to xlab, ylab and zlab:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", xlab="x (degrees)", ylab="y (degrees)", zlab=expression(paste("intensity ["~"erg"~"s"^"-1"~"cm"^"-2"~"arcmin"^"-2"~"]")))

![](README_files/figure-markdown_strict/unnamed-chunk-7-1.png)

#### xlog, ylog: logarithmic scaling of the coordinate axes, default: FALSE

We can also plot in the logarithmic scale by setting xlog and/or ylog to
TRUE.

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", xlog = TRUE, ylog = TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png)

#### zlinformat: colourscale values format, default zlinformat = FALSE

When the image is plotted in logarithmic scale, by default the colourbar
values are printed as powers of ten. If we want them to be printed in
the scientific notation we set zlinformat to TRUE:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", zlinformat=TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

#### xlinformat, ylinformat: axes values format, default FALSE

Similar to zlinformat, but for axes. By default nothing special is done
with the axes, but it can be changed:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", xlinformat=TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.png)

#### xdigits, ydigits and zdigits: number of digits in the scientific notation for the colourbar, default 1

By default only one digit is printed in the scientific notation. To use
2 set xdigits, ydigits or zdigits to 2 (note that this only has an
effect when xlinformat, ylinformat or zlinformat = TRUE or when plotting
in a linear scale \[xlog, ylog or zlog = FALSE\]):

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", zlinformat=TRUE, zdigits = 2)

![](README_files/figure-markdown_strict/unnamed-chunk-11-1.png)

The parameter has no effect when the powers of ten are used:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", zdigits = 2)

![](README_files/figure-markdown_strict/unnamed-chunk-12-1.png)

An example with the x-axis:

    pltimggg.plot2D(x, y, img, xdigits = 2, xlinformat = TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-13-1.png)

#### aspect: preserve aspect ratio, default aspect = TRUE

By default, the coordinates have an equal length scale in x and y
directions. This can cause problems in semilogarithmic plots:

    pltimggg.plot2D(x, y, img, xlog = TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-14-1.png)

A remedy is to set aspect to FALSE:

    pltimggg.plot2D(x, y, img, xlog = TRUE, aspect = FALSE)

![](README_files/figure-markdown_strict/unnamed-chunk-15-1.png)

#### zbarwidth and zbarheight: colourbar width and height, default: zbarwidth = 20, zbarheight = 1

By default the colourbar is horizontal, its length is 20 (zbarwidth =
20) and its thickness is 1 (zbarheight = 1). If legend.direction is set
to "vertical" these values are interchanged by the routine itself.

To make the horizontal colourbar longer and thicker set zbarwidth to 25
and zbarheight to 2:

    pltimggg.plot2D(x, y, img, zlog=TRUE, zbarwidth=25, zbarheight = 2)

![](README_files/figure-markdown_strict/unnamed-chunk-16-1.png)

For the vertical bar remember that zbarwidth and zbarheight have the
opposite meaning than in the horizontal case. To reduce the length and
thickness set zbarheight to 18 and zbarwidth to 0.5:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", zbarheight = 18, zbarwidth = 0.5)

![](README_files/figure-markdown_strict/unnamed-chunk-17-1.png)

#### ztitleposition: colourbar title position, default ztitleposition = "top"

By default, the position of the colourbar title is above the colourbar
in the horizontal case and to the right in the vertical case. To plot
the title below the vertical colourbar set ztitleposition to "bottom":

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", ztitleposition = "bottom", zlab="values")

![](README_files/figure-markdown_strict/unnamed-chunk-18-1.png)

Note that the title is rotated by 90 degrees, which is not something we
may necessarily want here. The next parameter can help.

#### zitleorientation: colourbar title orientation angle, default ztitleorientation = 0

By default, the title is printed horizonally (ztitleorientation = 0)
when the colourbar is horizontal, and vertically (ztitleorientation =
90) when it is vertical (this is automatically changed by the routine
when legend.direction is set to "vertical"). In the previous example we
may want to print the title horizontally and set ztitleorientation to 0:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", ztitleposition = "bottom", zlab="values", ztitleorientation = 0)

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

#### ztitlehjust and ztitlevjust: title horizontal and vertical justification, default: 0.5 both

By default, the title is centered both horizontally and vertically
(ztitlehjust = 0.5 and ztitlevjust = 0.5). To justify left (bottom), set
ztitlehjust the value to 0, and for the right (right and top)
justification set it to 1.

Move the title to the bottom:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", zlab="values", ztitlehjust = 0)

![](README_files/figure-markdown_strict/unnamed-chunk-20-1.png)

Move the title to the right:

    pltimggg.plot2D(x, y, img, zlog=TRUE, zlab="values", ztitlehjust = 1)

![](README_files/figure-markdown_strict/unnamed-chunk-21-1.png)

#### colours: colourscale to be used, default: internally defined rainbow

There is a rainbow colourscale defined internally. You can replace it by
your own colourscale. For example, to use grayscale you first generate
the colours useing e.g. colorRampPalette and then pass the result to
plotimggg.plot2D:

    myPalette <- colorRampPalette(c("black", "white"))(100)
    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", colours = myPalette)

![](README_files/figure-markdown_strict/unnamed-chunk-22-1.png)

You can also combine the palettes. Let's add another two palettes,
usefull when particular ranges of values should be emphasized:

    myPalette <- c(colorRampPalette(c("black", "white"))(100), colorRampPalette(c("red", "blue"))(100), colorRampPalette(c("magenta", "orange"))(100))
    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", colours = myPalette)

![](README_files/figure-markdown_strict/unnamed-chunk-23-1.png)

"Interesting" plots can be generated by shuffling the colours (not the
use of rbind() instead of c(), also white has been replaced by green):

    myPalette <- rbind(colorRampPalette(c("black", "green"))(100), colorRampPalette(c("green", "red"))(100), colorRampPalette(c("red", "blue"))(100))
    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", colours = myPalette)

![](README_files/figure-markdown_strict/unnamed-chunk-24-1.png)

#### theme: ggplot theme to be used, default: internally defined theme

plotimggg.plot2D defines a reasonable black and white theme with some
adjustments for fonts. You can supply your own theme instead. For
exmple, to use the minimal theme set theme to theme\_minimal():

    pltimggg.plot2D(x, y, img, zlog=TRUE, theme = theme_minimal())

![](README_files/figure-markdown_strict/unnamed-chunk-25-1.png)

More information can be obtained by reading the ggplot documentation. Or
you can copy the definition of "mytheme" from the plotimggg.R and create
your own custom theme.

#### conv.kernel: image convolution

If we want to perform a simple convolution of the image with a Gaussian
kernel we have to provide the kernel width (FWHM of the Gaussian) in the
units in which axes are measured. In our examples the axes go from 1 to
11. The following command convolves the image shown in the previous
example with a Gaussin kernel 0.6 units wide:

    pltimggg.plot2D(x, y, img, zlog=TRUE, legend.direction = "vertical", conv.kernel = 0.6)

    ## [1] "Convolving with sigma =  0.6  (pixels =  5.94 )"

![](README_files/figure-markdown_strict/unnamed-chunk-26-1.png)

Note that the convolution is currently performed on the original 2D
image array, and thus only makes sense when applied to Cartesian
geometry. Hopefully in the future a more general convolution will be
implemented.

### Spherical coordinates

We can also provide the data in spherical coordinates. The routine
creates an internal representation in the form of polygons and calls the
ggplot geom\_polygon() routines. This is slowe, but it provides
reasonable results.

First, we create an array with spherical coordinates:

    # define array dimensions
    snx <- 100
    sny <- 400

    # axes limits
    sxlim = c(1e0, 2e0)
    sylim = c(0e0, 2 * pi)

    # compute axes
    sx <- (0 : (snx - 1)) * (sxlim[2] - sxlim[1]) / snx + sxlim[1]
    sy <- (0 : (sny - 1)) * (sylim[2] - sylim[1]) / sny + sylim[1]

    # declare array
    simg <- array(1e0, c(snx, sny))

    # fill array with random numbers in a certain interval
    for (i in 1:snx) {
        for (j in 1:sny) {
            simg[i, j] = (rnorm(1, 1e1, 0.3))^2 * 1e10 * (cos(sy[j])^2 + 1e0)/ (sx[i]^2)
        }
    }

Now we call the routine with the parameter igeom set to 2:

    pltimggg.plot2D(sx, sy, simg, legend.direction = "vertical", igeom = 2, zlog=TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-28-1.png)

We can also plot just a small wedge:

    pltimggg.plot2D(sx, sy[250:270], simg[,250:270], legend.direction = "vertical", igeom = 2, zlog=TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-29-1.png) Or we
can also plot everything but that small wedge:

    pltimggg.plot2D(sx, sy[-(250:270)], simg[,-(250:270)], legend.direction = "vertical", igeom = 2, zlog=TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-30-1.png)
