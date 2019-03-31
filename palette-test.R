library(colorspace)
library(purrr)

setup <- 'swatch'
#setup <- 'light'
#setup <- 'dark'

# export xrdb configuration
export.xrdb <- TRUE

# export light-style configuration
light.export <- FALSE
light.export <- TRUE

# set 'bright' colors to high contrast (i.e. darker) in light configuration
light.export.contrast.swap <- FALSE
light.export.contrast.swap <- TRUE

############################################################
##### Good light theme
# set up grays
fg.bg <- hcl(0, 0, c(100, 5))
grays <- hcl(0, 0, c(10, 30, 70, 90))

term.order=c('r', 'g', 'y', 'b', 'm', 'c')
hue1s <- c(r=000, y=030, g=120, c=180, b=240, m=300)    #
hue2s <- c(r=000, y=030, g=120, c=180, b=240, m=300)    #

n <- length(hue1s)

l1 <- rep(35, n)
l2 <- rep(50, n)

#        rd  gn  yl  bu  mn  cn
c1 <- c( 90, 45, 40, 45, 60, 25)
c2 <- c( 25, 20, 15, 20, 25, 10) + c1
c1 <- c( 90, 45, 60, 45, 60, 25)
c2 <- c(-10,-10, 10,-10,-10,-10) + c1

############################################################
##### Good dark theme
if(!light.export) {
    # set up grays
    fg.bg <- hcl(0, 0, c(100, 10))
    grays <- hcl(0, 0, c(20, 40, 70, 90))

    term.order=c('r', 'g', 'y', 'b', 'm', 'c')
    hue1s <- c(r=000, y=060, g=120, c=180, b=240, m=300)    #
    hue2s <- c(r=000, y=060, g=120, c=180, b=240, m=300)    #

    n <- length(hue1s)

    l1 <- rep(60, n)
    l2 <- rep(75, n)

    #        rd  gn  yl  bu  mn  cn
    c1 <- c( 50, 45, 50, 45, 50, 35)
    c2 <- c( 10, 20, 20, 20, 25, 10) + c1
}

hues <- c(hue1s[term.order], hue2s[term.order])
chrom <- c(c1, c2)
lum <- c(l1, l2)

cols <- hcl(hues, chrom, lum, fixup=FALSE)
cols[is.na(cols)] <- '#777777'

# max chroma calculations
max.chrom <- max_chroma(hues, lum)
max.chrom.str <- paste0(chrom, ifelse(chrom>max.chrom, '>', '<'), max.chrom)
mcs.colors <- ifelse(lum < 65, fg.bg[1], fg.bg[2])

# set up coordinates
xs <- rep(c(0,1), each=n)
ys <- 0:(n-1)

# set up plot
plot.new()
par(mar=c(0,0,0,0))
plot.window(c(0,2),
            c(0,n),
            xaxs = "i", yaxs = "i")

if(light.export) {
    grays <- rev(grays)
    fg.bg <- rev(fg.bg)
}

if(setup=='swatch') {
    par(bg='#777777')

    # draw rects
    rect(xs, ys, xs+1, ys+1,
         col=cols,
         border=FALSE)

    # color labels
    texts <- transpose(list(y=seq(0.1, by=0.10, length.out=4),
                            col=grays))
    for(spec in texts) {
        text(xs+0.1, y1s+spec$y,
             labels=cols, col=spec$col,
             adj=c(0,0))
    }

    # print max chroma
    text(xs+0.1, ys+0.8, adj=c(0,0),
         labels=max.chrom.str,
         col=mcs.colors)

    # dark bg

} else if(setup=='light') {
    par(bg='#ffffff')
    #text(xs+0.2, y1s+0.45,
    #     labels=cols, col=cols,
    #     adj=c(0,0))
} else if(setup=='dark') {
    par(bg='#000000')
    #text(xs+0.2, y1s+0.45,
    #     labels=cols, col=cols,
    #     adj=c(0,0))
}

if(export.xrdb) {
    #export.fn <- '~/.local/src/iTerm2-Color-Schemes/xrdb/export.xrdb'
    export.fn <- 'export.xrdb'

    if(light.export && light.export.contrast.swap) {
        cols <- cols[c(n+(1:n), 1:n)]
    }
    xrdb.ansi.colors <- sprintf('Ansi_%d_Color %s', c(1:6, 9:14), cols)
    xrdb.grayscales <- sprintf('Ansi_%d_Color %s', c(0,8,7,15), grays)
    xrdb.fg.bg <- paste(c('Foreground_Color', 'Background_Color'), fg.bg)
    xrdb <- paste('#define', c(xrdb.ansi.colors, xrdb.grayscales, xrdb.fg.bg))
    write(xrdb, export.fn)
}
