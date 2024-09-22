#' summary.samg
#'
#' An S3 Method for `sampleG()` which returns a named list containing the generalized variance, total variance, sample cov matrix S, and the eigenvalues and eigenvectors of S
#'
#' @param object a list inherited from `sampleG()`
#' @param ... passes additional arguments throughout the function
#'
#' @return a named list
#' @export
#'
#' @importFrom stats cov
#'
#' @examples \dontrun{summary(x=l)}
summary.samg <- function(object = list(), ...) {

  #x <- object

  # Variance-Covariance Matrix
  S <- as.matrix(cov(object$X))

  # Generalized Variance
  gv <- det(S)

  # Total Variance
  tv <- sum(diag(S))

  # Eigenvalues and Eigenvectors of S
  eig <- eigen(S)

  # List
  list(GenVariance=gv, TotVariance=tv, S=S, EIGEN=eig)
}
