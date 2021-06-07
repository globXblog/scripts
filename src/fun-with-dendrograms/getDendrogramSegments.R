
# getDendrogramSegments function ----
#' Create data frame with all dendrogram segments (links between points)
#'
#' Transform result from getClusteringStepsAndGroups and getDendrogramPoints functions
#' into a dataframe, containing dendrogram segment coordinates to link points.
#'
#' @param StepsAndGroupsDF, dataframe resulting from getClusteringStepsAndGroups function which
#' describes the merging steps of hierarchocal clustering.
#' @param dataDendro_pts, dataframe resulting from getDendrogramPoints function which contains
#' dendrogram point properties (size, coordinates and group).
#' @xCol index of the column with x-axis coordinates in dataDendro_pts dataframe
#' @yCol index of the column with y-axis coordinates in dataDendro_pts dataframe
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item from_id, point name of the segment start 
#'   \item to_id, point name of the segment end 
#'   \item from_x, coordinate on x-axis of the segment start
#'   \item to_x, coordinate on x-axis of the segment end
#'   \item from_y, coordinate on y-axis of the segment start
#'   \item to_y, point coordinate on y-axis of the segment end
#'   \item gp_sgt, group the segment belongs to
#---------------

getDendrogramSegments <- function(StepsAndGroupsDF, dataDendro_pts, xCol, yCol) {

  # Number of lines in StepsAndGroupsDF
  m <- NROW(StepsAndGroupsDF)
  
  # Initialize DF_segments dataframe
  m1 <- m*4
  DF_segments <- data.frame(from_id=character(m1),
                            to_id=character(m1),
                            from_x=integer(m1),
                            to_x=integer(m1),
                            from_y=integer(m1),
                            to_y=integer(m1),
                            gp_sgt=numeric(m1),
                            stringsAsFactors=FALSE)
  
  
  
  # Fill from_id and to_id in DF_segments from StepsAndGroupsDF (nodes and points)
  # and shoulderProperties (shoulder points)
  for (i in 1:m) {
    l1 <- (i*4)-3
    l2 <- (i*4)-2
    l3 <- (i*4)-1
    l4 <- (i*4)
    shoulderA <- paste("shoulder", (i*2)-1, sep="")
    shoulderB <- paste("shoulder", (i*2), sep="")
    DF_segments$from_id[l1] <- StepsAndGroupsDF$id_pt1[i]
    DF_segments$to_id[l1] <- shoulderA
    DF_segments$from_id[l2] <- shoulderA
    DF_segments$to_id[l2] <- StepsAndGroupsDF$id_node[i]
    DF_segments$from_id[l3] <- StepsAndGroupsDF$id_pt2[i]
    DF_segments$to_id[l3] <- shoulderB
    DF_segments$from_id[l4] <- shoulderB
    DF_segments$to_id[l4] <- StepsAndGroupsDF$id_node[i]
  }
  
  # Fill from_x, to_x, from_y and to_y in DF_segments from dataDendro_pts
  for (i in 1:m1){
    fromid <- DF_segments$from_id[i]
    toid <- DF_segments$to_id[i]
    DF_segments$from_x[i] <- dataDendro_pts[which(dataDendro_pts$id==fromid), xCol]
    DF_segments$from_y[i] <- dataDendro_pts[which(dataDendro_pts$id==fromid), yCol]
    DF_segments$to_x[i] <- dataDendro_pts[which(dataDendro_pts$id==toid), xCol]
    DF_segments$to_y[i] <- dataDendro_pts[which(dataDendro_pts$id==toid), yCol]
    DF_segments$gp_sgt[i] <- dataDendro_pts$gp[which(dataDendro_pts$id==fromid)]
  }
  
  return(DF_segments)
}