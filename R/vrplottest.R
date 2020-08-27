#' Testing, should prolly use testhat
#'
#'
testit <- function() {
  x <- seq(-1, 1, length.out = 100)
  y <- seq(-1, 1, length.out = 100)
  z <- sin(x)
  colInd = rep(1:10, length.out = 100)
  vrplot("Plot3", x, y, z, colInd, type="cube")
  vrlabels("Plot3", "X stuff", "Y stuff", "Z stuff")
  vrclear("Plot3")
}
