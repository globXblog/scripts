# Utilities ----

#' Distance
#' 
#' Compute distance between A and B
#' 
#' @param A vector of 2, coordinates of point A
#' @param B vector of 2, coordinates of point B
#' @return the distance (numeric).
#' @examples
#' A=c(3,3);B=c(2,2);distance(A,B)

distance <- function(A,B){
  return(sqrt(sum((A-B)^2)))
}

#' Orthogonal projection
#' 
#' Compute coordinates of point M projected on AB
#' 
#' @param M vector of 2, coordinates of point M
#' @param A vector of 2, coordinates of point A
#' @param B vector of 2, coordinates of point B
#' @return vector of 2, coordinates of projected point M'
#' @examples
#' A=c(3,3);B=c(2,8);M=c(1,6)
#' P=project(M,A,B)
#' plot(rbind(M,A,B,P),pch=19,asp=1)
#' text(rbind(M,A,B,P),labels=c('M','A','B','P'),adj=2)

project <- function(M,A,B){
  AB=B-A
  AM=M-A
  alpha=(AB[1]*AM[1]+AB[2]*AM[2])/(distance(A,B)^2)
  return(A+alpha*(B-A))
}

#' Rescale
#' 
#' Rescale numbers between given minimum and maximum values
#' 
#' @param x numeric, value to be rescaled.
#' @param from numeric, rescale range minimum. Default 0.
#' @param to numeric, rescale range maximum. Default 1.
#' @return rescaled value (numeric).
#' @examples
#' x=seq(1,6);rescale(x)

rescale <- function(x,from=0,to=1){
  u <- (x-min(x))/(max(x)-min(x)) # between 0 and 1
  from+(to-from)*u # between from and to
}

# Basic transformations ----

#' Rotation
#' 
#' Find coordinates of a point A rotated with a given angle around C 
#' 
#' @param A vector of 2, coordinates of the point to rotate
#' @param C vector of 2, coordinates of the center of rotation
#' @param angle numeric, rotation angle in radians
#' @param finalSize numeric, size of segment C - rotated A, default CA.
#' @return A vector of 2 which are the coordinates of rotated A.
#' @examples
#' rotate(A=c(3,3),C=c(2,2),angle=pi/4)

rotate <- function(A,C=c(0,0),angle=pi/2,finalSize=distance(A,C)){
  # if(all(A==C)){return(A)}
  if(all(A==C)){  # A and C are the same point
    A[1] <- A[1]+0.00001   # Add arbitrary value to A x-coordinate
    }
  A2 <- A-C  # Coordinates of A in system with C as origin instead of O(0,0) 
  d <- distance(A,C)
  if(A2[1]>0){   # calculate angle beta from x horizontal axis to CA
    beta <- asin(A2[2]/d) # if xA2 > 0 (between 0 and pi)
  } else {
    beta <- pi-asin(A2[2]/d) # if xA2 < 0 (between pi and 2*pi)
  }
  xa2 <- cos(angle+beta)   # calculate xa2 (xA2 rotated with angle+beta). Note that distance(C,a2)=1
  if(sin(angle+beta)>0){   # calculate ya2 (yA2 rotated with angle+beta) using xa2^2+ya2^2=1
    ya2 <- sqrt(1-xa2^2)  # if sin(angle+beta)>0, yb>0
  } else {
    ya2 <- -sqrt(1-xa2^2)  # if sin(angle+beta)<0, yb<0
  }
  a2 <- c(xa2,ya2)  # a2 coordinates
  a <- a2*finalSize+C   # scale a2 and go back to original center
  return(a)
}

#' Folding
#' 
#' Fold a segment AB given a bending angle (2pi -> full circle) and 
#' a pivot on the segment AB (where the line is bent).
#' 
#' @param A vector of 2, segment start coordinates.
#' @param B vector of 2, segment end coordinates.
#' @param pivot numeric, between 0 and 1, position of the pivot on AB (0: the line folds at A; 1: the line folds at B).
#' @param angle numeric between 0 and 2pi, bending angle (in radian). 0 corresponds to a line and 2*pi to a full circle.
#' @param M numeric vector, values between 0 and 1 denoting the position of the points on segment AB to be bent.
#' @param finalSize numeric, length of circle arc corresponding to bent segment AB (default distance(A,B)).
#' @return a dataframe with as many rows as M and 3 columns named M,x,y (original vector M and coordinates of bent points)
#' @examples
#' A=c(1,1);B=c(4,2);pivot=0.25;angle=5*pi/4
#' res=fold(A,B,pivot,angle)
#' plot(res$x,res$y,type='b',pch=19,asp=1,xlim=c(0.5,4.5),ylim=c(0.5,3))
#' lines(c(A[1],B[1]),c(A[2],B[2]),type='b',pch=19,col='blue')
#' pc=A+pivot*(B-A);points(pc[1],pc[2],pch=19,col='red')

fold <- function (A,B,pivot=0.5,angle=pi,M=seq(0,1,0.1),finalSize=distance(A,B)){
  if(angle==0){ # trivial case, transformation is identity
    # out=data.frame(M=M,x=(1-M)*A[1]+M*B[1],y=(1-M)*A[2]+M*B[2])
    # Coordinates of vector AB (coordinates of the segment in coordinate system with origin (0,0))
    ABvector <- B - A
    # Pivot coordinates
    pivotCoord <- A + ABvector * pivot
    out=data.frame(M=M,x=pivotCoord[1],y=A[2])
  } else {
    # Coordinates of vector AB (coordinates of the segment in coordinate system with origin (0,0))
    ABvector <- B - A
    # Pivot coordinates
    pivotCoord <- A + ABvector * pivot
    radius=finalSize/angle
    # Find the coordinates of circle's center: rotate B with angle=pi/2 and center=pivot and length finalSize
    if(pivot!=1){
      circleCenter <- rotate(B,pivotCoord,pi/2,radius)
    } else {
      circleCenter <- rotate(A,pivotCoord,-pi/2,radius)
    }
    # Coordinates of points A and B on the new circle (named points a and b)
    aCoord <- rotate(pivotCoord,circleCenter,-pivot*angle,radius)
    bCoord <- rotate(pivotCoord,circleCenter,(1-pivot)*angle,radius)
    # Find coordinates of points M on the bent line
    out=data.frame(M=M,x=NA,y=NA)
    for(i in 1:length(M)){
      mCoord <- rotate(aCoord,circleCenter,M[i]*angle,radius)
      out$M[i]=M[i]
      out$x[i]=mCoord[1]
      out$y[i]=mCoord[2]
    }
  }
  return(out)
}

#' Folding in an alternative parameterization
#' 
#' Fold a segment AB given a bending angle (2pi -> full circle) and a center.
#' 
#' @param A vector of 2, segment start coordinates.
#' @param B vector of 2, segment end coordinates.
#' @param C vector of 2, center coordinates.
#' @param angle numeric between 0 and 2pi, bending angle (in radian). 0 corresponds to a line and 2*pi to a full circle.
#' @param M numeric vector, values between 0 and 1 denoting the position of the points on segment AB to be bent.
#' @return a dataframe with as many rows as M and 3 columns named M,x,y (original vector M and coordinates of bent points)
#' @examples
#' A=c(1,1);B=c(4,2);C=c(2,2.5);angle=5*pi/4
#' res=fold2(A,B,C,angle)
#' plot(res$x,res$y,type='b',pch=19,asp=1,xlim=c(1,4),ylim=c(0,3))
#' lines(c(A[1],B[1]),c(A[2],B[2]),type='b',pch=19,col='blue')
#' points(C[1],C[2],pch=19,col='red')
#' 
fold2 <- function (A,B,C,angle=pi,M=seq(0,1,0.1)){
  P=project(C,A,B)
  pivot=((P-A)/(B-A))[1]
  radius=distance(P,C)
  out=fold(A,B,pivot,angle,M,finalSize=radius*angle)
  #out=fold(A,B,pivot,angle,M,finalSize=distance(A,B))
  return(out)
}

#' Folding a scatterplot
#' 
#' Fold a scatterplot given a bending angle (2pi -> full circle) and a center.
#' 
#' @param x vector, x coordinates.
#' @param y vector, y coordinates.
#' @param C vector of 2, center coordinates.
#' @param angle numeric between 0 and 2pi, bending angle (in radian). 0 corresponds to a line and 2*pi to a full circle.
#' @param borders vector of 2, x coordinates of segment’s start and end. Default c(min(x),max(x))
#' @return a dataframe with as many rows as M and 3 columns named M,x,y (original vector M and coordinates of bent points)
#' @examples
#' n=100;x=(1:n)/n;y=cos(4*pi*x)+0.1*rnorm(n);y=y/sd(y);C=c(0.5,1.5*max(y));angle=pi/2
#' foo=foldScatterplot(x,y,C,angle)
#' plot(foo$x,foo$y,pch=19,xlim=c(min(foo$tx),max(foo$tx)),ylim=c(min(foo$ty),max(foo$ty)),type='b',asp=1)
#' lines(foo$tx,foo$ty,pch=19,col='blue',type='b')
#' points(C[1],C[2],pch=19,col='red')
#' 
foldScatterplot <- function (x,y,C,angle=pi,borders=c(min(x),max(x))){
  out=data.frame(x=x,y=y,tx=NA,ty=NA)
  for(i in 1:length(x)){
    A=c(borders[1],y[i])
    B=c(borders[2],y[i])
    M=(x[i]-borders[1])/(borders[2]-borders[1])
    foo=fold2(A,B,C,angle,M)
    out$tx[i]=foo$x
    out$ty[i]=foo$y
  }
  return(out)
}


#' Bending a scatterplot
#' 
#' Bend a scatterplot given a bending angle (2pi -> full circle) and a pivot.
#' 
#' @param data dataframe, containing x and y columns with coordinates of points to be bent.
#' @param pivot numeric, between 0 and 1, position of the pivot on AB (0: the line folds at A; 1: the line folds at B).
#' @param angle numeric between 0 and 2pi, bending angle (in radian). 0 corresponds to a line and 2*pi to a full circle.
#' @return a dataframe with 4 columns named x,y, tx and ty (original x and y coordinates, and coordinates of bent points)
#' @examples
#' data=data.frame(x=rep(seq(1,10), 10),y=rep(seq(1,10), each=10))
#' foo=bendScatterplot(data)
#' plot(foo$x,foo$y,pch=19,xlim=c(min(foo$tx),max(foo$tx)),ylim=c(min(foo$ty),max(foo$ty)),asp=1, col="grey80")
#' points(foo$tx,foo$ty,pch=19,col='red')


bendScatterplot <- function(data, pivot=0.5, angle=pi){
  
  # If angle is 0, transformation is identity
  if (angle==0){
    out <- cbind(data,data)
    colnames(out) <- c("x","y","tx","ty")
  } else {
    # Rescale data bewteen 0 and 1
    out <- data.frame(x=rescale(data$x),y=rescale(data$y))
    
    # FIND CIRCLE CENTRE, CIRCLE RADIUS AND COORDINATES OF BENT "A" 
    # BASED ON DATA, PIVOT AND BENDING ANGLE
    
    # Get start and end point coordinates of the line containing data
    A <- c(min(out$x),max(out$y))
    B <- c(max(out$x),max(out$y))
    
    # Coordinates of vector AB (coordinates of the line in coordinate system with origin (0,0))
    ABvector <- B - A
    # Pivot coordinates
    pivotCoord <- A +  ABvector * pivot
    # Find circle radius
    ABlength <- distance(A,B)   # Distance between A and B
    circleRad <- ABlength/angle
    # Find the coordinates of circle's center: rotate PivotB segment of pi/2 angle with circleRad length
    circleCenter <- rotate(A=B,C=pivotCoord,angle=pi/2,finalSize=circleRad)
    
    # Coordinates of point A on the circle  with given with circleCentre and radius
    # (bent "A" named aCoord)
    theta <- pivot*angle
    aCoord <- rotate(A=pivotCoord,C=circleCenter,angle=-theta,finalSize=circleRad)
    
    # GET BENT COORDINATES FOR ALL POINTS IN DATA
    n <- NROW(out)
    # 
    for (i in 1:n){
      # New coordinates of point to be bent (named "M")
      M <- c(out$x[i],out$y[i])
      radius <- abs(circleRad*((circleCenter[2]-M[2])/(circleCenter[2]-A[2])))
      beta <-(M[1]-A[1])/(B[1]-A[1])*angle
      # Coordinates of point M on the circle with given with circleCentre and radius (bent "M")
      Mbent <- rotate(A=aCoord, C=circleCenter, angle=beta, finalSize=radius)
      out$tx[i] <- Mbent[1]
      out$ty[i] <- Mbent[2]
    }
    
    # Re-rescale bent coordinates
    out$x <- min(data$x)+out$x*(max(data$x)-min(data$x))
    out$tx <- min(data$x)+out$tx*(max(data$x)-min(data$x))
    out$y <- min(data$y)+out$y*(max(data$y)-min(data$y))
    out$ty <- min(data$y)+out$ty*(max(data$y)-min(data$y))
  }
  return(out)
}

# Illustrations ----

#' Illustrate rotation
#' 
#' Illustration of a rotation
#' 
#' @examples
#' illustrateRotation()
#' 
illustrateRotation <- function(){
  A=c(3,3)
  C=c(2,2)
  theta <- pi/2
  d=distance(A,C)
  
  A2 <- A-C
  beta <- asin(A2[1]/d)
  beta2 <- seq(0, beta, length.out = 9)
  Rbeta <- 0.5
  xbeta <-  C[1] + Rbeta*cos(beta2)
  ybeta = C[2] + Rbeta*sin(beta2)
  theta2 <- seq(0.78,0.78+(pi/2),length.out=9)
  Rtheta <- 0.7
  xtheta = C[1] + Rtheta*cos(theta2)
  ytheta = C[2] + Rtheta*sin(theta2)
  xa <- cos(theta+beta)
  ya <- sqrt(1-xa^2)
  
  # original coordinate system
  plot(x=0,y=0,pch=3,
       xlim=c(-1.2,A[1]*1.35),
       ylim = c(-1.2,A[2]*1.35))
  arrows(x0=-1, y0=0, x1=4, y1=0, length = 0.1, angle = 20)
  arrows(x0=0, y0=-1, x1=0, y1=4, length = 0.1, angle = 20)
  text(x=-0.3, y=-0.3, labels="O(0,0)")
  Sys.sleep(1)
  # A and C
  points(x=c(C[1],A[1]), y=c(C[2],A[2]), col="red", pch=19)
  segments(x0=C[1], y0=C[2], x1=A[1], y1=A[2], col="red")
  text(x=c(0.9*C[1],1.1*A[1]), y=c(0.9*C[2],1.1*A[2]), labels = c("C(2,2)","A(3,3)"))
  text(x=1.1*A[1],y=1.3*A[2],labels=expression(paste("d=", sqrt(1^2 + 1^2))), col="red")
  Sys.sleep(1)
  # New coordinates when C is the center
  arrows(x0=C[1], y0=C[2], x1=C[1]+1.5*d, y1=C[2], lty="dotted", col="darkblue", length = 0.1, angle = 20)
  arrows(x0=C[1], y0=C[2], x1=C[1], y1=C[2]+1.5*d, lty="dotted", col="darkblue", length = 0.1, angle = 20)
  text(x=c(0.9*C[1],1.1*A[1]), y=c(0.7*C[2],0.9*A[2]), labels = c("C2(0,0)","A2(1,1)"), col="darkblue")
  Sys.sleep(1)
  # Rotate
  lines(x=xbeta,y=ybeta, col="grey50")
  text(x=3.2, y=2.3, labels=expression(paste(beta, "=asin", (x[A2]), "/d")), col="grey50")
  Sys.sleep(1)
  lines(x=xtheta, y=ytheta, col="darkgreen")
  text(x=2, y=3, labels=expression(theta),col="darkgreen")
  Sys.sleep(1)
  points(x=C[1]+xa, y=C[2]+ya, pch=4)  # a coordinates at O origin
  segments(x0=C[1],y0=C[2], x1=C[1]+xa, y1=C[2]+ya, col="orange")
  text(x=1.1*(C[1]+xa), y=1.1*(C[2]+ya), labels="a2")
  Sys.sleep(1)
  text(x=0.9*(C[1]+xa), y=0.8*(C[2]+ya), labels=expression(paste(x[a2],"=cos", (theta+beta))), col="darkgreen")
  Sys.sleep(1)
  # Scale
  segments(x0=0+C[1], y0=0+C[2], x1=(xa*d)+C[1], y1=(ya*d)+C[2], col="orange")
  points(x=C[1]+xa*d, y=C[2]+ya*d,pch=19, col="orange")   # a coordinates at O origin AND lengthOa=d
  text(x=0.9*(C[1]+xa*d), y=1.1*(C[2]+ya*d), labels="a",col="orange")
}

