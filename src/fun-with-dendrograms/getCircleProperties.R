# turnAndScale function ----
#'
#' Find coordinates of a point ptA rotated around C with an angle equals to theta
#' @param A: vector of 2, coordinates of the point to rotate
#' @param C: vector of 2, coordinates of the rotation point
#' @param theta: numeric, rotation angle
#' @param d: numeric, distance from C to rotated A
#' @return A vector of 2 which are the coordinates of ptA rotated with a given theta angle and a d distance.
#---------------

turnAndScale <- function(A,theta,C=c(0,0),d){
  A2 <- A-C  # Coordinates of A in system with C as origin instead of O(0,0) 
  CAlength <- sqrt(sum(A2^2))   # Length of line CA
  if(A2[1]>0){   # calculate angle beta from x horizontal axis to CA
    beta <- asin(A2[2]/CAlength) # if xA2 > 0 (between 0 and pi)
  } else {
    beta <- pi-asin(A2[2]/CAlength) # if xA2 < 0 (between pi and 2*pi)
  }
  xa <- cos(theta+beta)   # calculate xa (xA2 rotated with theta+beta) when length Ca=1
  if(sin(theta+beta)>0){   # calculate ya (yA2 rotated with theta+beta). Since aC=1, xa^2+ya^2=1
    ya <- sqrt(1-xa^2)  # if sin(theta+beta)>0, yb>0
  } else {
    ya <- -sqrt(1-xa^2)  # if sin(theta+beta)<0, yb<0
  }
  aCoordAtO <- c(xa,ya)  # a coordinates at O origin
  aCoordAtOAndd <- aCoordAtO*d   # a coordinates at O(0,0) origin AND lengthOa=d
  aCoord <- aCoordAtOAndd+C  # a coordinates at C origin and lengthCa=d
  return(aCoord)
}

#---------------
#---------------

# getBentCoordinates function ----
#'
#' Let AB a line from ptA to ptB through ptM.
#' Bend the AB line given a bending angle (how far the line is bent), a pivot (where the line is bent),
#' and a bending option (how the line is bent).
#' 
#' @param ptA : Vector of 2, line start point coordinates.
#' @param ptB : Vector of 2, line end point coordinates.
#' @param ptM: Vector of 2, position on the line of the point to be moved.
#' @param bendingAngle: numeric higher than 0 and lower than 2pi, how bent the line should be (in radian), 0 corresponds to a line and 2*pi to a full circle.
#' @param pivot: numeric, between 0 and 1, position on the line of the bending point, 0 the line bend at ptA and 1 the line bend at ptB.
#' @param bendingOption: integer between 1 and 4, how the line bends:
## 1. the bent line has the same size as the original AB line length (default option)
## 2. the circle radius depends on the circle arc length
## 3. the circle radius depends on the circle string length
## 4. the circle radius depends on the bendingAngle
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item xbent, x-axis coordinates of ptM on the bent AB line
#'   \item ybent, y-axis coordinates of ptM on the bent AB line
#'   \item Axbent, x-axis coordinates of ptA on the bent AB line
#'   \item Aybent, y-axis coordinates of ptA on the bent AB line
#'   \item Bxbent, x-axis coordinates of ptB on the bent AB line
#'   \item Bybent, y-axis coordinates of ptB on the bent AB line
#'   \item pivotx, x-axis coordinates of pivot on the bent AB line
#'   \item pivoty, y-axis coordinates of pivot on the bent AB line
#'   \item bendingAngle, bending angle value
#'   \item ABlength, AB line length
#'   \item centerx,  x-axis coordinates of the circle center
#'   \item centery, x-axis coordinates of the circle center
#'   \item circleRad, circle radius length
#---------------

getBentCoordinates <- function (ptA, ptB, ptM, bendingAngle, pivot, bendingOption=1){
  # Coordinates of vector AB (coordinates of the line in coordinate system with origin (0,0))
  ABvector <- ptB - ptA
  # Pivot coordinates
  pivotCoord <- ptA +  ABvector * pivot
  # Find AB distance
  ABlength <- sqrt(ABvector[1]^2+ABvector[2]^2)   # Length of line AB
  # Find bendingFactor
  bendFactor <- bendingAngle/(2*pi)
  # Find circle radius
  if (bendingOption==1){
    # Radius when the line is a perfect circle (bendingAngle==2*pi)
    finalRad <- ABlength/(2*pi)
    # Radius of the circle with a given bendingAngle
    circleRad <- finalRad/bendFactor
  }
  if (bendingOption==2){
    # Find circle arc length:
    # When bendingAngle=0, arcLength=ABlength
    # When bendingAngle=2*pi, ABlength becomes the circle diameter, arcLength=2*pi*(ABlength/2)=pi*ABlength
    # In between these 2 states, the arcLength is given by the following formula:
    arcLength <- (1-bendFactor)*ABlength+(bendFactor*pi)*ABlength
    # Radius of the circle with a given bendingAngle
    circleRad <- arcLength/(bendingAngle)
  }
  if (bendingOption==3){
    # Find circle string length
    stringLength <- ABlength
    # Radius of the circle with a given bendingAngle
    if (bendFactor<0.5){   # the angle is smaller than pi so circleRad is:
      circleRad <- (stringLength/2)/sin(bendingAngle/2)
    } else {   # the angle is greater than pi, the circle diameter equals ABlength, so CircleRad is:
      circleRad <- ABlength/2
    }
  }
  if (bendingOption==4){
    # Find the radius of the circle with a given bendingAngle directly
    # When bendingAngle=0, the circle circumference equals ABlength, so CircleRad=ABlength/2*pi
    # When bendingAngle=2*pi, the circle diameter equals ABlength, so CircleRad=ABlength/2
    # In between these 2 states, circleRad is given by the following formula:
    circleRad <- (ABlength/(2*pi))*(1-bendFactor) + (ABlength/2)*bendFactor
  }
  # Find the coordinates of circle's center: rotate PivotB segment of pi/2 angle with circleRad length
  circleCenter <- turnAndScale(A=ptB, C=pivotCoord, theta=pi/2, d=circleRad)
  # Coordinates of points A and B on the new circle (named points a and b)
  theta <- pivot*bendingAngle
  # aCoord <- turnAndScale(A=pivotCoord, C=circleCenter, theta=theta-bendingAngle, d=circleRad)
  # bCoord <- turnAndScale(A=pivotCoord, C=circleCenter, theta=theta, d=circleRad)
  aCoord <- turnAndScale(A=pivotCoord, C=circleCenter, theta=-theta, d=circleRad)
  bCoord <- turnAndScale(A=pivotCoord, C=circleCenter, theta=bendingAngle*(1-pivot), d=circleRad)
  # Find coordinates of the point M on the bent line
  M <-  (ptM - ptA)/ABvector 
  mAngle <- M[1]*bendingAngle
  mCoord <- turnAndScale(A=aCoord, C=circleCenter, theta=mAngle, d=circleRad)
  return(data.frame(xbent=mCoord[1],
                    ybent=mCoord[2],
                    Axbent=aCoord[1],
                    Aybent=aCoord[2],
                    Bxbent=bCoord[1],
                    Bybent=bCoord[2],
                    pivotx=pivotCoord[1],
                    pivoty=pivotCoord[2],
                    bendingAngle=bendingAngle,
                    ABlength=ABlength,
                    centerx=circleCenter[1],
                    centery=circleCenter[2],
                    circleRad=circleRad))
}

#---------------
#---------------

# getCircleProperties function ----
#'
#' Let data be a dataset with values on the same line.
#' Bend this line given a bending angle and a bending option
#' and fing the circle properties (center, radius).
#' 
#' @param data, dataframe containing data used as an argument in getDataframeForHclust function.
#' @param bendingAngle, numeric higher than 0 and lower than 2pi, how far the line should be bent.
#' 0 corresponds to 'not bent' (line) and 2pi to 'fully bent' (circle).
#' @param bendingOption, integer between 1 and 4, how the line bends:
## 1. the bent line has the same size as the original AB line length (default option)
## 2. the circle radius depends on the circle arc length
## 3. the circle radius depends on the circle string length
## 4. the circle radius depends on the bendingAngle
#' @return A list with the following:
#' \enumerate{
#'   \item ptA, line start point coordinates
#'   \item ptB, line end point coordinates 
#'   \item aCoord, ptA coordinates on the bent circle with given bendingAngle and bendingOption 
#'   \item bCoord, ptB coordinates on the bent circle with given bendingAngle and bendingOption 
#'   \item circleCenter, circle center coordinates with given bendingAngle and bendingOption 
#'   \item pivot, coordinates of the point where the line bends
#'   \item circleRad, circle radius length given a bendingAngle and a bendingOption 
#---------------

getCircleProperties <- function (data,bendingAngle, bendingOption){
  
  # Get start and end point coordinates of the line containing data
  ptA <- c(min(data$x)-0.5,0)
  ptB <- c(max(data$x)+0.5,0)
  
  # Get any ptM coordinates
  ptM <- 0.1*ptB
  
  # Get pivot position on AB line from highest coordinates (x pivot=x highest point)
  ypivot <- max(data$y)   # Find point with highest y-axis coordinate
  xpivot <- data$x[which(data$y==ypivot)[1]]   # Find x-axis coordinate of the highest y-axis coordinate point
  pivot <- (xpivot-ptA[1])/(ptB[1]-ptA[1])  # Where the AB line bends
  
  # Get bent coordinates of ptA, ptB and center points,
  # and angle and circleRad with given bendingAngle
  d <- getBentCoordinates(ptA=ptA, ptB=ptB, ptM=ptM, bendingAngle=bendingAngle, pivot=pivot, bendingOption=bendingOption)
  aCoord <- c(d$Axbent,d$Aybent)
  bCoord <- c(d$Bxbent,d$Bybent)
  circleCenter <- c(d$centerx,d$centery)
  bendingAngle <- d$bendingAngle
  circleRad <- d$circleRad

  return(list(ptA=ptA,
              ptB=ptB,
              a=aCoord,
              b=bCoord,
              circleCenter=circleCenter,
              pivot=pivot,
              circleRad=circleRad))
}

