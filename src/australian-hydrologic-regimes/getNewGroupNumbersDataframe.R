
# getNewGroupNumbersDataframe function ----

#' Define new groups in a dataset when divided into several groups in a row
#' so unchanged groups have the same index whichever "nb group" is.
#'
#' Find the divided groups for greater nb of groups
#' Rename groups when dataset is split into fewer number of groups if needed
#'
#' @param dataGroups dataframe, Dataframe containing group properties
#' @param colGp integer, which column to get group index
#' @param colNbGroups integer, which column to get info about how many groups the dataset has been split into
#' @param colId integer, which column to get individual id
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item whenNbgpIs, if colNbGroups is equal to this value
#'   \item switchGp, this group should be switch
#'   \item withGp, with this group
#' @examples
#' @export
#'
#'#---------------
# load("testSegments.RData")
# load("textTest.RData")
# 
# dataGroups=text
# colGp=7
# colNbGroups=14
# colId=4
# 
# dataGroups[,colGp] <- as.numeric(dataGroups[,colGp])
# dataGroups[,colNbGroups] <- as.numeric(dataGroups[,colNbGroups])
# dataGroups[,colId] <- as.numeric(dataGroups[,colId])

getNewGroupNumbersDataframe <- function(dataGroups,colId,colGp,colNbGroups){
  # Find groups to switch
  # Duplicate dataGroups
  dataGroups2 <- dataGroups
  dataGroups2[,colGp] <- as.vector(dataGroups2[,colGp])
  dataGroups2[,colNbGroups] <- as.vector(dataGroups2[,colNbGroups])
  dataGroups2[,colId] <- as.vector(dataGroups2[,colId])
  # Create a new col group in dataGroup to store new group numbers
  dataGroups2$newGroup <- dataGroups2[,colGp]
  
  # Find unique values in colNbGroups
  un <- unique(dataGroups2[,colNbGroups])
  nbOfGps <- length(un)
  unmax <- max(un)
  unmin <- min(un)
  
  # Create stages dataframe to follow gp when nbgp changes
  stages <- as.data.frame(matrix(ncol=(nbOfGps+1), nrow=nbOfGps), stringsAsFactors=FALSE)
  colnames(stages) <- c(un,"id")
  stages[,nbOfGps] <- un  # Fill nbOfGps-th col in stages  
  # Fill "id" col in stages by finding one invividual id per group when nbgp==unmax 
  dfmax <- dataGroups2[dataGroups2[,colNbGroups]==unmax,]  # nbgp==unmax only
  # Remove rows filled with na in dfmax
  naRows <- which(is.na(dfmax[,colGp]))
  if (length(naRows)!=0){
    dfmax <- dfmax[-naRows,]
  }
  for (i in 1:nbOfGps){  # Fill id col in stages
    gp <- un[i]
    stages$id[i] <- as.character(dfmax[dfmax[,colGp]==gp, colId][1])
  }
  
  # Fill stages
  for (i in 1:nbOfGps){  # rows in stages
    idToFind <- stages$id[i]
    for(j in 1:(nbOfGps-1)){   # columns in stages
      gp <-  un[j]
      whichGroup <- dataGroups2[dataGroups2[,colNbGroups]==gp & dataGroups2[,colId]==idToFind,]
      stages[i,j] <- whichGroup[,colGp][1]    
    }
  }
  
  # Duplicate stages
  stages0 <- stages
  
  # Find what groups are merged between (i-1) and i number of groups
  for (i in 2:nbOfGps){   # For each nb of groups
    # Find what group appears at least twice in (i-1) nb of groups
    whatGpIsDuplicated <- unique(stages[duplicated(stages[,(i-1)]),(i-1)])
    # If more than one group is duplicated, find what groups is duplicated in (i-1)th col
    # and split in i-th col
    if(length(whatGpIsDuplicated)>1){
      temp <- stages[,c((i-1),i)]
      unik <- 1  # Initisalise unik
      k <- 1  # Initialise k
      while (length(unik)<2){   # Find which group in whatGpIsDuplicated splits at i-th stage
        groupToTest <- whatGpIsDuplicated[k]   # Test j-th group in whatGpIsDuplicated
        unik <- unique(temp[which(temp[,1]==groupToTest),2]) # Number of groups
        k <- k+1
      }
      whatGpIsDuplicated <- groupToTest
    }
    
    # What groups in i nb of groups are merged in (i-1) nb of groups
    whatGpsMerge <- unique(stages[stages[,(i-1)]==whatGpIsDuplicated,i])
    
    # What group does not merged group
    whatGpsDoNotMerge <- unique(stages[stages[,(i-1)]!=whatGpIsDuplicated,i])
    if (length(whatGpsDoNotMerge)==0){
      whatGpsDoNotMerge <- -9999
    }
    # What is the name of whatGpsDoNotMerge in (i-1) nb of groups
    nonMergingGroupNames <- unique(stages[stages[,(i-1)]!=whatGpIsDuplicated,(i-1)])
    if (length(nonMergingGroupNames)==0){
      nonMergingGroupNames <- -9999
    }
  
    # Change gp names in stages if whatGpsDoNotMerge is not nonMergingGroupNames
    if(paste(whatGpsDoNotMerge, collapse="")!=paste(nonMergingGroupNames, collapse="")){
      # Number of groups to be switched
      l <- length(whatGpsDoNotMerge)
      # Loop for each number to switch
      for (j in 1:l){
        # in (i-1), nonMergingGroupNames should be whatGpsDoNotMerge
        switchGp <- whatGpsDoNotMerge[j]
        withGp <- nonMergingGroupNames[j]
        # Switch group numbers in stages
        for (k in i:nbOfGps){
         gpVec <- stages[,k]
         stages[,k] <- ifelse(gpVec==switchGp,withGp,ifelse(gpVec==withGp,switchGp,gpVec))
        }
      }
    }
  }
  
  # Compare stages0 and stages and store differences in df
  m <- sum(stages0!=stages)
  df <- data.frame("whenNbgpIs"=numeric(m),
                   "switchGp"=numeric(m),
                   "withGp"=numeric(m),
                   stringsAsFactors=FALSE)
  
  k=1
  for (i in 1:nbOfGps){
    whatRows <- which(stages0[,i]!=stages[,i])
    if (length(whatRows)!=0){
      l <- length(whatRows)
      for (j in 1:l){
        df$whenNbgpIs[k] <- i
        df$switchGp[k] <- stages0[whatRows,i][j]
        df$withGp[k] <- stages[whatRows,i][j]
        k <- k+1
      }
    }
  }
  
  # Swith groups in dataGroups2
  dataGroups2$newGroup <- dataGroups2[,colGp]
  
  for (i in 1:m){
    whenNbGroupsIs <- df$whenNbgpIs[i]
    switchGroup <- df$switchGp[i]
    withGroup <- df$withGp[i]
    dataRows <- which(dataGroups[,colNbGroups]==whenNbGroupsIs & dataGroups[,colGp]==switchGroup)
    dataGroups2$newGroup[dataRows] <- withGroup
  }
  
  return(dataGroups2)
}
