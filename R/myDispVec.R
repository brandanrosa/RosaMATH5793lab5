#' myDispVec
#'
#' Plots the displacement (residual) vectors
#'
#' @param x a list inherited from `sampleG()`
#' @param ... passes additional arguments throughout the function
#'
#' @return a 3D plot of the displacement (residual) vectors
#' @export
#'
#' @importFrom graphics segments
#' @importFrom scatterplot3d scatterplot3d
#'
#' @examples \dontrun{myDispVec(x = first)}
myDispVec <- function(x, ...) {
  q <- as.data.frame(head(x, 3))
  q <- rbind(0, q)
  z <- t(q)

  x <- as.numeric(z[1,])
  y <- as.numeric(z[2,])
  z <- as.numeric(z[3,])

  s <- scatterplot3d(x, y, z,
                     pch = 17,
                     bg = "hotpink",
                     grid = FALSE,
                     box = FALSE,
                     xlab = "1",
                     ylab = "2",
                     zlab = "3",
                     main = "Displacement Vectors")
  # Convert
  p1 <- s$xyz.convert(x[1], y[1], z[1])
  p2 <- s$xyz.convert(x[2], y[2], z[2])
  p3 <- s$xyz.convert(x[3], y[3], z[3])
  p4 <- s$xyz.convert(x[4], y[4], z[4])

  # Draw Segments
  segments(p1$x,p1$y,p3$x,p3$y,lwd=2,col= "darkgreen")
  segments(p1$x,p1$y,p4$x,p4$y,lwd=2,col="blue")
  segments(p1$x,p1$y,p2$x,p2$y,lwd=2,col= "red")

  # List
  listy <- list(x=x, y=y, z=z)
  invisible(listy)
}
