
### image scale
image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}



### filled contour plot

filled.contour2 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    par(las = las)
    mar <- mar.orig
    plot.new()
    par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 4)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }



### wind arrow plot
quiver<- function(u,v,x.val,y.val,scale=1.5,length=leng)
{
  xx <- matrix(ncol=length(x.val),rep((x.val),length(y.val)),nrow=length  (y.val),byrow=TRUE)
  yy <- matrix(ncol=length(x.val),rep((y.val),length(x.val)),nrow=length    (y.val),byrow=FALSE)
  speed <- sqrt(u*u+v*v)
  maxspeed <- max(speed)
  u <- u*scale/maxspeed
  v <- v*scale/maxspeed
  arrows(xx,yy[,(length(yy[1,])):1],xx+t(u),yy[,(length(yy[1,])):1]+t(v),length=length*min(par.uin()),lwd=0.8)
}
par.uin <- function()# determine scale of inches/userunits in x and y
{
  u <- par("usr")
  p <- par("pin")
  c(p[1]/(u[2] - u[1]), p[2]/(u[4] - u[3]))
}
quiver.leg <- function(u,v,x.val,y.val,scale=1.5,length=leng)
{
  xx <- matrix(ncol=length(x.val),rep((x.val),length(y.val)),nrow=length	(y.val),byrow=TRUE)
  yy <- matrix(ncol=length(x.val),rep((y.val),length(x.val)),nrow=length		(y.val),byrow=FALSE)
  speed <- sqrt(u*u+v*v)
  maxspeed <- max(speed)
  u <- u*scale/maxspeed
  v <- v*scale/maxspeed
  arrows(xx,yy[,length(yy[,1]):1],xx+u,yy[,length(yy[,1]):1]+v,length=leng*min(par.uin()),lwd=0.8)
}

####
monthi <- function(i) {ifelse(i %% 12 == 0,12,i %% 12)}