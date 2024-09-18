#' sampleG
#'
#' A function which uses the quantitative data frame to produce xbar the sample mean vector and the centered matrix
#'
#' @param x a data frame
#'
#' @return a named list
#' @export
#'
#' @examples \dontrun{sampleG(x = ddt[1:10, c("LENGTH", "WEIGHT", "DDT")])}
sampleG <- function(x = data.frame()) {
  # Data matrix
  X <- as.matrix(x)

  # Sample Size
  n <- length(X[,1])

  # Number of Variables
  p <- length(X[1,])

  # Xbar
  xBar <- as.matrix(colMeans(X))

  # Centered matrix
  one <- as.matrix(rep(1,n))
  Xc <- X - one %*% t(xBar)

  # List
  x <- list(Xc=Xc, xBar=xBar, X=X, n=n, p=p)
  class(x) <- "samg"
  invisible(x)
}
