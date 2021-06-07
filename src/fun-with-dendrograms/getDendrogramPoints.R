
# getDendrogramPoints function ----
#' Create data frame with all dendrogram points
#'
#' Transform result from getClusteringStepsAndGroups function
#' into a dataframe, containing dendrogram point properties (size, coordinates and group).
#'
#' @param StepsAndGroupsDF Dataframe resulting from getClusteringStepsAndGroups function which describes
#' the merging steps of hierarchical clustering
#' @param data, dataframe used as an argument in getDataframeForHclust function
#' @param pointSizeCol, integer corresponding to the index of the column where the point size is stored
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item id, point name
#'   \item type, point type (leaf, node - merging of 2 leaves- or shoulder)
#'   \item val, point size
#'   \item x, point coordinate on x-axis
#'   \item y, point coordinate on y-axis
#'   \item gp, group the point belongs to
#---------------

getDendrogramPoints <- function(StepsAndGroupsDF, data, pointSizeCol=1) {

  # Number of lines in StepsAndGroupsDF
  m <- NROW(StepsAndGroupsDF)

  # Create points to draw "shoulders" between 2 merged points
  m1 <- m*2
  shoulderProperties <- data.frame(id=paste("shoulder", seq(1:m1), sep=""),
                                   val=rep(0,(m1)),
                                   x=numeric(m1),
                                   y=numeric(m1),
                                   gp=numeric(m1),
                                   stringsAsFactors = FALSE)
  for (i in 1:m){
    l1 <- (i*2)-1
    l2 <- (i*2)
    shoulderProperties$x[l1] <- StepsAndGroupsDF$x_pt1[i]
    shoulderProperties$x[l2] <- StepsAndGroupsDF$x_pt2[i]
    shoulderProperties$y[l1] <- StepsAndGroupsDF$height_pts[i]
    shoulderProperties$y[l2] <- StepsAndGroupsDF$height_pts[i]
    shoulderProperties$gp[l1] <- StepsAndGroupsDF$group_pt1[i]
    shoulderProperties$gp[l2] <- StepsAndGroupsDF$group_pt2[i]
  }
  
  # Create a data frame containing leaf properties (coordinates, color, size)
  n <- length(levels(as.factor(data$siteid)))
  leafProperties <-data.frame(id_pt = character(n),   # Point name
                              val_pt = numeric(n),   # Point size
                              x_pt = numeric(n),   # Point x coordinate
                              height_pt = numeric(n),   # Point x coordinate
                              group_pt = numeric(n),      # Point group (result of cutree() function)
                              stringsAsFactors = FALSE)
  
  leafid <- levels(as.factor(data$siteid))
  
  for (i in 1:n){
    id <- leafid[i]
    leafProperties$id_pt[i] <- id
    lval <- data[which(data$siteid==id),pointSizeCol][1]
    leafProperties$val_pt[i] <- lval

    ididx <- which(StepsAndGroupsDF$id_pt1 == id | StepsAndGroupsDF$id_pt2 == id)
    leafProperties$height_pt[i] <- StepsAndGroupsDF$height_pts[ididx]
    if (id %in% StepsAndGroupsDF$id_pt1){
      idx <- which(id == StepsAndGroupsDF$id_pt1)
      leafProperties$x_pt[i] <- StepsAndGroupsDF$x_pt1[idx]
      leafProperties$group_pt[i] <- StepsAndGroupsDF$group_pt1[idx]
    }
    if (id %in% StepsAndGroupsDF$id_pt2){
      idx <- which(id == StepsAndGroupsDF$id_pt2)
      leafProperties$x_pt[i] <- StepsAndGroupsDF$x_pt2[idx]
      leafProperties$group_pt[i] <- StepsAndGroupsDF$group_pt2[idx]
    }
  }
  
  # Create data frame with all dendrogram points
  DF_points <- data.frame(id=c(leafProperties$id_pt, StepsAndGroupsDF$id_node, shoulderProperties$id),
                          type=c(rep("leaf", n), rep("node", m1-n+1), rep("shoulder", m1)),
                          val=c(leafProperties$val_pt, rep(0, m), shoulderProperties$val),
                          x=c(leafProperties$x_pt, StepsAndGroupsDF$x_node, shoulderProperties$x),
                          y=c(rep(0,n), StepsAndGroupsDF$height_pts, shoulderProperties$y),
                          gp=c(leafProperties$group_pt, StepsAndGroupsDF$group_node, shoulderProperties$gp),
                          stringsAsFactors = FALSE)
 
  return(DF_points)
}