#' myMatdec.plot2d
#'
#' An S3 method for `myMatdec` which plots the data matrix and mean in 2D
#'
#' @param X a 3x2 matrix
#'
#' @return a plot of the data matrix and the mean
#' @export
#'
#' @examples \dontrun{myMatdec.plot2d(X=X)}
myMatdec.plot2d <- function(X) {

  # y1 & y2
  y1 <- as.matrix(X[,1])
  y2 <- as.matrix(X[,2])

  # xBar
  xBar <- as.matrix(colMeans(X))
  xbar1 <- as.matrix(xBar[1])
  xbar2 <- as.matrix(xBar[2])

  # Plot
  p <- plot(y1, y2, cex = 1.5, pch = 21, bg = "hotpink",
            main = "p = 2 Dim Plot")
  graphics::points(xbar1, xbar2, cex = 1.5, pch = 21, bg = "darkgreen")
  graphics::text(xbar1 + 0.3, xbar2, bquote(bar(x)), cex = 1.5)

  # List
  ll <- list(X=X, y1=y1, y2=y2, xBar=xBar)
  invisible(ll)
}
