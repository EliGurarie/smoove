#' Scan Track (enhanced plotting with time)
#' 
#' Plotting x-y, time-x, time-y
#' 
#' @param {x,y} coordinates; can be two separate vectors OR a two-column matrix/date-frame.
#' @param z optional complex location vector 
#' @param time time vector
#' @param {set.layout} whether or not to provide the (default) layout of x-y on the left, and t-x, t-y to the right. 
#' @param ... options to be passed to plot functions
#' 
#' @examples 
#' data(Bowhead)
#' par(bty="l", mar = c(0,2,0,2), oma=c(4,0,4,0), xpd=NA) 
#'  with(Bowhead, scan.track(time = DateTime, x = x.km, y = y.km))


scan.track <- function(track=NULL, time=NULL, x = NULL, y=NULL, z = NULL, set.layout = TRUE, ...)
{
  if("track" %in% is(track)){
    x <- track$X
    y <- track$Y
    time <- track$Time
  }
  
  if(is.null(z)){ 
    if(!is.null(y)) z <- x + 1i*y else
      z <- x[,1] + 1i*x[,2]
    }
  
  if(is.null(time)) time <- 1:length(z)
  
  if(set.layout) layout(rbind(c(1,2), c(1,3)))
  
  plot(z,asp=1, type="o", pch=19, col=rgb(0,0,0,.5), cex=0.5, ...)
  points(z[c(1,length(z))], pch=c(15,16), col=c("green", "red"), cex=1)
  plot(time,Re(z), type="o", pch=19, col=rgb(0,0,0,.5), xaxt="n", xlab="", cex=0.5, ...)
  plot(time,Im(z), type="o", pch=19, col=rgb(0,0,0,.5), cex=0.5, ...)
}
