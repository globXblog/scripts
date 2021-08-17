#---------------
# Transform data to be easily read in ggplot2
#---------------
# Load data
D <- read.table('data.csv',header = T, sep = ';')
Q <- read.table('Qnetwork.csv',header = T, sep = ';')
P <- read.table('Pnetwork.csv',header = T, sep = ';')
Temp <- read.table('Tnetwork.csv',header = T, sep = ';')
years <- read.table('time.csv',header = T, sep = ';')

#---------------
# Create a list of ivar
# if iVar==1, ispaceP  // if iVar==2, ispaceQ // if iVar==3, ispaceT // if iVar==4, ispaceT
ispaces <- list(P,Q,Temp,Temp)

# Add which row to look at in a given iVar dataframe in D
for(i in 1:NROW(D)){
  D$ispacerow[i] <- D[i,(which(D[i,c(4,5,6)]!=0)+3)]
}

# Add years and lon/lat in D
D$year <- NA
D$lon <- NA
D$lat <- NA

for (i in 1:NROW(D)){
  # Transform the itime column into years
  time_index <- D$itime[i]
  D$year[i] <- years$year[time_index]
  # Transform ispaces col into long/lat coordinates (find ispace-th row in iVar)
  var_index <- D$iVar[i]  
  space_index<- D$ispacerow[i]
  D$lon[i] <- ispaces[[var_index]][space_index,1]
  D$lat[i] <- ispaces[[var_index]][space_index,2]
}

# Number of years available
y <- NROW(years)

#---------------
# P dataframe
n <- NROW(P)*y
dfP <- data.frame(site_index=as.numeric(rep(seq(1,NROW(P)),y)),
                  lon=as.numeric(rep(P$lon,y)),
                  lat=as.numeric(rep(P$lat,y)),
                  year=as.numeric(rep(years$year,each=NROW(P))),
                  value=NA,
                  normVal=NA,
                  stringsAsFactors=FALSE)

for (i in 1:n) {
  rw <- which(D$lon==dfP$lon[i] & D$lat==dfP$lat[i] & D$year==dfP$year[i] & D$iVar==1)
  if (length(rw)!=0){
    dfP$value[i] <- D$value[rw]
  }
}

# # Check that all the values in D are in dfP
# NROW(D[D$iVar==1,])
# n-sum(is.na(dfP$value))

# Fill normVal col by station
for (i in 1:NROW(P)) {
  mask <- which(dfP$site_index==i)
  if (length(mask)!=0 & all(is.na(dfP$value[mask]))==FALSE){
    mx <- max(dfP$value[mask], na.rm=TRUE)
    mn <- min(dfP$value[mask], na.rm=TRUE)
    dfP$normVal[mask] <- (dfP$value[mask]-mn)/(mx-mn)
  }
}

dfP$ivar <- factor("dfP")

#---------------
# Q dataframe
m <- NROW(Q)*y
dfQ <- data.frame(site_index=as.numeric(rep(seq(1,NROW(Q)),y)),
                  lon=as.numeric(rep(Q$lon,y)),
                  lat=as.numeric(rep(Q$lat,y)),
                  year=as.numeric(rep(years$year,each=NROW(Q))),
                  value=NA,
                  normVal=NA,
                  stringsAsFactors=FALSE)

for (i in 1:m) {
  rw <- which(D$lon==dfQ$lon[i] & D$lat==dfQ$lat[i] & D$year==dfQ$year[i] & D$iVar==2)
  if (length(rw)!=0){
    dfQ$value[i] <- D$value[rw]
  }
}

# # Check that all the values in D are in dfQ
# NROW(D[D$iVar==2,])
# m-sum(is.na(dfQ$value))

# Fill normVal col by station
for (i in 1:NROW(Q)) {
  mask <- which(dfQ$site_index==i)
  if (length(mask)!=0 & all(is.na(dfQ$value[mask]))==FALSE){
    mx <- max(dfQ$value[mask], na.rm=TRUE)
    mn <- min(dfQ$value[mask], na.rm=TRUE)
    dfQ$normVal[mask] <- (dfQ$value[mask]-mn)/(mx-mn)
  }
}

dfQ$ivar <- factor("dfQ")

#---------------
# Temp dataframe
o <- NROW(Temp)*y
dfT <- data.frame(site_index=as.numeric(rep(seq(1,NROW(Temp)),y)),
                  lon=as.numeric(rep(Temp$lon,y)),
                  lat=as.numeric(rep(Temp$lat,y)),
                  year=as.numeric(rep(years$year,each=NROW(Temp))),
                  value=NA,
                  normVal=NA,
                  nbValues=NA,
                  normNbVal=NA,
                  stringsAsFactors=FALSE)

for (i in 1:o) {
  rw1 <- which(D$lon==dfT$lon[i] & D$lat==dfT$lat[i] & D$year==dfT$year[i] & D$iVar==4)
  if (length(rw1)!=0){
    dfT$value[i] <- max(D$value[rw1])
  }
  rw2 <- which(D$lon==dfT$lon[i] & D$lat==dfT$lat[i] & D$year==dfT$year[i] & D$iVar==3)
  if (length(rw2)!=0){
    dfT$nbValues[i] <- D$value[rw2]
    if(D$value[rw2]==0){
      dfT$value[i] <- 0
    }
  }
}

# # Check that all the values in Temp are in dfT
# NROW(unique(D[(D$iVar)==4,][c(8,9,10)]))
# o-sum(is.na(dfT$value))-sum(dfT$value==0, na.rm=TRUE)
# NROW(D[(D$iVar)==3,])
# o-sum(is.na(dfT$nbValues))

# Fill normVal col by station
for (i in 1:NROW(Temp)) {
  mask <- which(dfT$site_index==i)
  if (length(mask)!=0 & all(is.na(dfT$value[mask]))==FALSE){
    mx <- max(dfT$value[mask], na.rm=TRUE)
    mn <- min(dfT$value[mask], na.rm=TRUE)
    # if (mx==-Inf){
    #   print(i)
    #   break
    # }
    dfT$normVal[mask] <- (dfT$value[mask]-mn)/(mx-mn)
    mxnb <- max(dfT$nbValues[mask], na.rm=TRUE)
    mnnb <- min(dfT$nbValue[mask], na.rm=TRUE)
    dfT$normNbVal[mask] <- (dfT$nbValues[mask]-mnnb)/(mxnb-mnnb)
  }
}

dfTval <- dfT[,seq(1,6)]
dfTnb <- dfT[,c(1,2,3,4,7,8)]
colnames(dfTnb) <- colnames(dfTval)

dfTval$ivar <- factor("dfTval")
dfTnb$ivar <- factor("dfTnb")

#---------------
# Combine all 4 dataframes
bigDF <- rbind(dfQ,dfP,dfTval,dfTnb)
save(bigDF, file="bigDF.RData")
#write.table(bigDF, file="bigDF.csv",row.names = FALSE)
