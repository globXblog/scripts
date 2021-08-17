#---------------
# Load required packages and data
library(ggplot2);library(gganimate);library(patchwork);library(av)

load("bigDF.RData")

# Load new font
# myCustomGlyphes.ttf MUST be installed on computer. Will only works on Windows.
try(windowsFonts(CustomGlyphes=windowsFont("font24")))

#---------------
# Set plot properties
fill <- 'grey62'

#---------------
# Video properties
fps <- 1  # Frame per sec

#---------------
# Create background map
world <- map_data("world")
g0 <- ggplot()+
  geom_polygon(data=world,aes(long,lat,group=group),fill=fill,color=NA,size=0.1)+
  coord_fixed(ratio=1,ylim=c(-44,-29),xlim=c(133.5,153.5),expand=FALSE)+
  theme_void()+
  theme(panel.border=element_rect(fill=NA))

#---------------
# Create map images
# Count number of years (j loop)
years <- unique(bigDF$year)

# Create map with points (i loop)
icons <- data.frame(ivar=c("dfQ","dfP","dfTval","dfTnb"),
                    lowCol=c("\U0063","\U0066","\U0067","\U006A"),  # legend icon for lower normVal 
                    medCol=c("\U0062","\U0065","\U0068","\U006B"),  # legend icon for mid normVal
                    highCol=c("\U0061","\U0064","\U0069","\U006C"),  # legend icon for higher normVal
                    stringsAsFactors=FALSE)
ivars <- NROW(icons)

# Create a function to get map for each ivar
getIndividualMap <- function(data, g0, lowCol, medCol, highCol){
  g1 <- g0+geom_point(data=data, aes(x=lon,y=lat,color=normVal),size=8)+
           scale_colour_distiller(palette='Spectral', na.value="grey42",
                                   limits=c(0,1),breaks=c(0,0.5,1),name=NULL,
                                   labels=c(lowCol,medCol,highCol))+
           theme_bw()+
           theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid=element_blank(),
                 legend.position=c(0.08,0.18),
                 legend.key.size=unit(2.5,"lines"),
                 legend.text=element_text(family="CustomGlyphes", size=40, vjust=c(0,0.5,1)))
  return(g1)
}

# Arrange 4 maps for Q, P, Tnb and Tval and annotate the result
assembleFourMaps <- function(allMaps,plotTitle){
  bigMap <- (allMaps$dfQ|allMaps$dfP)/(allMaps$dfTval|allMaps$dfTnb)+
  plot_annotation(title=plotTitle,
                  subtitle=expression(paste("Top-left: ",bold("river drought duration  "),
                                            "Top-right: ",bold("number of dry days  "),
                                            "Bottom-left: ",bold("heatwave intensity  "),
                                            "Bottom-right: ",bold("number of heatwaves"))),
                  caption = "Source: Bureau of Meteorology",
                  theme=theme(plot.title=element_text(size=58, hjust=0.5),
                              plot.subtitle=element_text(size=28, hjust=0.5),
                              plot.caption=element_text(size=28, hjust=0.81)))
  return(bigMap)
}

# Create "images" folder to store images
unlink('images',recursive=TRUE);dir.create('images')

# Create empty list to store each "facet" of one given j year
allMaps <- list(dfQ=NULL,dfP=NULL, dfTval=NULL, dfTnb=NULL)

# Start with a map showing station location only (intro)
for (i in 1:ivars){  # individual map for each of the 4 variables
  ivar <- icons$ivar[i]
  foo=bigDF[bigDF$ivar==ivar,c('lon','lat','normVal')]
  # NA everywhere to show station location only 
  foo$normVal=as.numeric(NA)
  foo=unique(foo)
  allMaps[[i]]  <- getIndividualMap(data=foo,g0=g0,
                          lowCol=icons$lowCol[i],medCol=icons$medCol[i],highCol=icons$highCol[i])
}
bigMap <- assembleFourMaps(allMaps,"Droughts and heatwaves in South-East Australia during summer ........")
png(filename=file.path("images",paste0(sprintf("im_%03d", 0), ".png")),
    width=(1280*2), height=(720*2), type="cairo")
print(bigMap)
dev.off()

# Assemble map for each year
for (j in 1:length(years)){  # Loop on years
  yr <- years[j]
  maskYear <- bigDF[bigDF$year==yr,]
  for (i in 1:ivars){  # Loop on ivars
    ivar <- icons$ivar[i]
    mask <- maskYear[maskYear$ivar==ivar,]
    map <- getIndividualMap(data=mask,g0=g0,
                            lowCol=icons$lowCol[i],medCol=icons$medCol[i],highCol=icons$highCol[i])
    allMaps[[i]] <- map
  }
  plotTitle <- paste("Droughts and heatwaves in South-East Australia during summer", yr)
  bigMap <- assembleFourMaps(allMaps,plotTitle)
  png(filename=file.path("images",paste0(sprintf("im_%03d", j), ".png")),
      width=(1280*2), height=(720*2), type="cairo")
  print(bigMap)
  dev.off()
}

#---------------
# Create animation

# List all the images created with gganimate 
lf <- list.files("images", full.names=TRUE)  # List all images (one per year + initial maps with stations only)
# First and last files name should be repeated 4 and 8 times to stick with audio
lf <- c(rep(lf[1],4),lf[2:length(lf)], rep(lf[length(lf)],8))

# Write animation
av_encode_video(input=lf,output="HCI.mp4",framerate=fps,audio="HCI.mp3")
