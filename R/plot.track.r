#' Plot Track
#' 
#' Default method for plotting tracks
#' @param track a two-column matrix matrix or a complex location vector. 
#' @param pch default point style (filled circle)
#' @param col default color (transluscent grey)
#' @param ... other arguments to pass to \code{plot}
#' @method plot track
#' @examples
#' data(Bowhead)
#' bh <- data.frame(X = Bowhead$x.km, Y = Bowhead$y.km)
#' plot.track(bh)

plot.track <- function(track, pch=19, col =rgb(0,0,0,.2), cex = NULL, type = NULL, ...)
{
  if(is.complex(track)){x <- Re(track); y <- Im(track)} else
    if(ncol(track) == 2){x <- track[,1]; y <- track[,2]} else 
      stop("Please provide this function either a two-column matrix or data frame or a complex vector of locations.")
  
  if(is.null(cex)) cex <- 0.5
  if(is.null(type)) type <- "o"
  
  
  plot(x, y, asp=1, type=type, pch=pch, cex=cex, col=col, ...)
  points(x[1], y[1], bg="green", pch=21)
  points(x[length(x)], y[length(x)], bg="red", pch=23)  
}
