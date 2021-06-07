
# getClusteringStepsAndGroups function ----
#' hclust to dataframe translator.
#'
#' Transform an hclust object (result from hclust function) into a dataframe, 
#' containing the merging of clusters at each step (node) of the hierachical clustering
#' and height of each node and points and their groups (cutree function)
#'
#' @param hclust_data Object of class hclust which describes the tree produced by the clustering process.
#' @param nb_groups Integer, number of groups data should be divided into
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item node, each step of the hierarchical clustering
#'   \item id_pt1, name of the first element merged at the i-est step of the clustering
#'   \item id_pt2, name of the second element merged at the i-est step of the clustering
#'   \item height_pts, height of the 2 points merged at the i-est step of the clustering (as produced in hclust$height)
#'   \item height_node, height of the merging node at the i-est step of the clustering (as produced in hclust$height)
#'   \item x_pt1, x coordinate of the first point to be merged at the i-est step of the clustering
#'   \item x_pt2, x coordinate of the second point to be merged at the i-est step of the clustering
#'   \item x_node, x coordinate of the merging node at the i-est step of the clustering
#'   \item group_pt1, group of the first point to be merged at the i-est step of the clustering
#'   \item group_pt2, group of the second point to be merged at the i-est step of the clustering
#'   \item group_node, group of the merging node at the i-est step of the clustering
#'   
#' }
#' @examples
#' 
#' @export
#'
#---------------
getClusteringStepsAndGroups <- function(hclust_data, nb_groups){
  
  # Cut hclust dendrogram into groups using cutree
  pt_group <- cutree(tree=hclust_data,k=nb_groups)
  
  # Number of hclust merging steps
  n_merge <- NROW(hclust_data$merge)  
  
  # Create merging_data dataframe to store the merging steps of hclust object
  
  merging_data <- data.frame(id_node=paste("node", seq(1:n_merge), sep=""),
                             id_pt1=hclust_data$merge[,1],
                             id_pt2=hclust_data$merge[,2],
                             height_pts=hclust_data$height,  # y axis value for points
                             height_node=numeric(n_merge),   # y axis value for node
                             x_pt1=numeric(n_merge),  # x axis value for pt1
                             x_pt2=numeric(n_merge), # x axis value for pt2
                             x_node=numeric(n_merge),   # x axis value for node
                             group_pt1=numeric(n_merge),  # pt1 group
                             group_pt2=numeric(n_merge), # pt2 group
                             group_node=numeric(n_merge),   # node group
                             stringsAsFactors = FALSE)
  
  # Replace id_pt1 and id_pt2 with names intead of indices
  merging_data$id_pt1 <- ifelse(merging_data$id_pt1 < 0, hclust_data$labels[abs(merging_data$id_pt1)], paste("node", merging_data$id_pt1, sep=""))
  merging_data$id_pt2 <- ifelse(merging_data$id_pt2 < 0, hclust_data$labels[abs(merging_data$id_pt2)], paste("node", merging_data$id_pt2, sep=""))
  
  # Add x coordinate and group for each point and node in merging_data
  for (i in 1:n_merge) {
    id1 <- merging_data$id_pt1[i]   # pt 1 id in ith row
    id2 <- merging_data$id_pt2[i]   # pt 2 id in ith row
    if (id1 %in% hclust_data$labels) {   # if id1 is not a node
      idx1 <- which(hclust_data$labels == id1)   # find index of id1
      merging_data$x_pt1[i] <- which(hclust_data$order == idx1) # fill x_pt1 col
      merging_data$group_pt1[i] <- as.numeric(pt_group[id1])  # fill group_pt1 col
    } else {      # id1 IS a node
      row1 <- which(merging_data$id_node==id1)   # find row of the step named id1
      merging_data$x_pt1[i] <- merging_data$x_node[row1] # fill x_pt1 with mean of x_pt1 and x_pt2 values at step row1
      merging_data$group_pt1[i] <- merging_data$group_node[row1]  # fill group_pt1 with group_node value at step row1
    }
    if (id2 %in% hclust_data$labels) {     # if id2 is not a node
      idx2 <- which(hclust_data$labels == id2)     # find index of id2
      merging_data$x_pt2[i] <- which(hclust_data$order == idx2)   # fill x_pt2 col
      merging_data$group_pt2[i] <- as.numeric(pt_group[id2])    # fill group_pt2 col
    } else {      # id2 IS a node
      row2 <- which(merging_data$id_node==id2)     # find row of the step named id2
      merging_data$x_pt2[i] <- merging_data$x_node[row2]   # fill x_pt2 with mean of x_pt1 and x_pt2 values at step row2
      merging_data$group_pt2[i] <- merging_data$group_node[row2]    # fill group_pt2 with group_node value at step row2
    }
    # Find group of each node:if the 2 merging pts are in the same group,
    # node group is also the same group, else group_node stays at 0
    if (merging_data$group_pt1[i] == merging_data$group_pt2[i]){  
      merging_data$group_node[i] <- merging_data$group_pt1[i]
    }
    # For each row, node x-coordinates is the mean of the coordinates of the 2 merging pts
    merging_data$x_node[i] <- mean(c(merging_data$x_pt1[i], merging_data$x_pt2[i]))
  }
  
  # Add y coordinate (height) for each node in merging_data
  for (i in 1:n_merge){
    # Find y coordinate of each node if node_id is in id_pt1 or id_pt2 cols
    id <- merging_data$id_node[i]
    if (id %in% merging_data$id_pt1 | id %in% merging_data$id_pt2 ) {
      id_idx <- which(merging_data$id_pt1 == id | merging_data$id_pt2 == id)
      merging_data$height_node[i] <- merging_data$height_pts[id_idx]
    }
  } 
  # Add y coordinate (height) for last node in merging_data
  merging_data$height_node[n_merge] <- max(merging_data$height_node, na.rm = TRUE)
  
  # Replace 0 in group_pt1, group_pt2 and group_node cols by NA value
  merging_data$group_pt1[which(merging_data$group_pt1==0)] <- NA
  merging_data$group_pt2[which(merging_data$group_pt2==0)] <- NA
  merging_data$group_node[which(merging_data$group_node==0)] <- NA
  
  return(data.frame(merging_data))
  
}

