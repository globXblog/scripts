# projectDatasetOnCircle function ----
#'
#' Let given points in a dataframe form a line called AB.
#' Bend the AB line given a bending angle (how far the line is bent), a pivot (where the line is bent),
#' and a bending option (how the line is bent).
#' @param data: dataframe used as an argument in getDataframeForHclust function.
#' Contains x and y coordinates of the points to place on a circle with given circleCenter and circleRad.
#' @param bendingAngle: numeric higher than 0 and lower than 2pi, how bent the line should be (in radian), 0 corresponds to a line and 2*pi to a full circle.
#' @param circleCenter, vector of 2, circle center coordinates with a given bendingAngle.
#' @param circleRad, numeric, circle radius with a given bendingAngle.
#' @param ptA, vector of 2, line start point coordinates.
#' @param ptB, vector of 2, line end point coordinates.
#' @param aCoord, vector of 2, coordinates of ptA on the bent circle with given bendingAngle, circleCenter and circleRad.
#' @param bCoord, vector of 2, coordinates of ptB on the bend circle with given bendingAngle, circleCenter and circleRad.
#' @param bendingOption: integer between 1 and 4, how the line bends:
## 1. the bent line has the same size as the original AB line length (default option)
## 2. the circle radius depends on the circle arc length
## 3. the circle radius depends on the circle string length
## 4. the circle radius depends on the bendingAngle
#' @return A data frame similar to data with the following added columns:
#' \enumerate{
#'   \item xbent, x-axis coordinates of the given point on the bent AB line
#'   \item ybent, y-axis coordinates of the given point on the bent AB line
#---------------

projectDatasetOnCircle <- function (data, bendingAngle, ptA, ptB, aCoord,bCoord, circleCenter, circleRad, bendingOption){
  dat <- data
  n <- NROW(data)
  
  # Find circleRad for bendingAngle==2*pi
  CR <- getCircleProperties(data=data,
                            bendingAngle=2*pi, 
                            bendingOption=bendingOption)
  
  # Find y min and max to rescale data
  mnm <- min(dat$y)
  denom <- max(dat$y)-min(dat$y)

  # Get bent coordinates of all points in data
  for (i in 1:n){
    # Rescale y so highest y is circleCenter y-coordinate
    ui1 <- (dat$y[i]-mnm)/denom   # Rescale between 0 and 1
    ui2 <- 0+(CR$circleRad-0)*ui1   # Rescale between 0 and circleRad (in CR object)
    # Find ptM
    ptM <- c(dat$x[i],ui2)
    radius <- abs(circleRad*((circleCenter[2]-ptM[2])/(circleCenter[2]-ptA[2])))
    beta <-(ptM[1]-ptA[1])/(ptB[1]-ptA[1])*bendingAngle
    # Get ptM coordinates on the guven circle
    Mbent <- turnAndScale(A=aCoord, C=circleCenter, theta=beta, d=radius)
    dat$xbent[i] <- Mbent[1]
    dat$ybent[i] <- Mbent[2]
  }
  dat$bendingAngle <- bendingAngle
  return(dat)
}