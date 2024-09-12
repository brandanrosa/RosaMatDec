#' myMatdec
#'
#' A function that breaks down a 3x2 matrix into important parts for OU course MATH5793
#'
#' @param X a 3x2 matrix
#'
#' @return a named list with various items of not-so-nefarious natures
#' @export
#'
#' @examples \dontrun{myMatdec(X=X)}
myMatdec <- function(X) {

  # ONE matrix
  one <- matrix(c(1,1,1), ncol = 1)

  # xbar
  xBar <- as.matrix(colMeans(X))

  # xbar1 & xbar2
  xbar1 <- xBar[1] * one
  xbar2 <- xBar[2] * one

  # x1, x2, & x3
  x1 <- as.matrix(X[1,])
  x2 <- as.matrix(X[2,])
  x3 <- as.matrix(X[3,])

  # y1 & y2
  y1 <- as.matrix(X[,1])
  y2 <- as.matrix(X[,2])

  # d1 & d2
  d1 <- y1 - xbar1
  d2 <- y2 - xbar2

  # e11, e22, & e12
  e11 <- t(d1) %*% d1
  e22 <- t(d2) %*% d2
  e12 <- t(d1) %*% d2

  # Length of Deviation Vectors
  Ld1 <- sqrt(e11)
  Ld2 <- sqrt(e22)

  # s11, s22, & s12
  s11 <- e11/3
  s22 <- e22/3
  s12 <- e12/3

  # r12
  r12 <- s12/(sqrt(s11) * sqrt(s22))

  # Sn & R Matrices
  Sn <- matrix(c(s11, s12, s12, s22), nrow = 2, ncol = 2, byrow = TRUE)
  R <- matrix(c(1, r12, r12, 1), nrow = 2, ncol = 2, byrow = TRUE)

  # Theta
  theta <- acos(r12)

  # List
  l <- list(X=X,
            xBar = xBar,
            x1=x1,
            x2=x2,
            x3=x3,
            y1=y1,
            y2=y2,
            d1=d1,
            d2=d2,
            Ld1=Ld1,
            Ld2=Ld2,
            s11=s11,
            s22=s22,
            s12=s12,
            r12=r12,
            Sn=Sn,
            R=R,
            Theta=theta)

  invisible(l)
}
