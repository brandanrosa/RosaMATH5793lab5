#' print.samg
#'
#' An S3 Method for `sampleG()` which plots the first three components of the displacement vectors and returns a named list
#'
#' @param x a list inherited from `sampleG()`
#' @param ... passes additional arguments throughout the function
#'
#' @return a plot of the `p` vectors
#' @export
#'
#' @importFrom stats cor
#' @importFrom utils head tail
#'
#' @examples \dontrun{print(x = l)}
print.samg <- function(x = list(), ...) {

  # First & Last
  first <- as.matrix(head(x$Xc, 3))
  last <- as.matrix(tail(x$Xc, 3))

  # Theta
  theta <- as.matrix(acos(cor(x$X)))

  # Plot
  myDispVec(x = first)

  # List
  list(first=first, last=last, theta=theta)
}
