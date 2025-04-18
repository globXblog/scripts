# ----------
# Plot required packages
library(tidyr);library(dplyr);library(ggplot2);library(patchwork);library(av)

# ----------
# Read data file
Antarctica <- read.table("AntarcticaAnimation.txt", header=TRUE)
Antarctica$date <- as.Date(Antarctica$date)

# ----------
# Arrange data
n <- NROW(Antarctica)
d0 <- Antarctica$date[1]  # Starting date
dn <- Antarctica$date[n]    # Ending date

wd <- getwd()
m <- ((NCOL(Antarctica)-1)/2)  # Number of variables

# Create function to get day duration (plot background colour)
getDayLightDuration <- function(d,lat){   # d=date, lat=latitude of given station
  # Number of days since last spring equinox
  dm <- format(d, format="%m-%d")
  y <- format(d,format="%Y")
  if(d>=as.Date(paste(y,"03-01", sep="-"))){
    nbDays <- as.numeric(d-as.Date(paste(y,"03-20", sep="-")))
  } else {
    nbDays <- as.numeric(d-as.Date(paste((as.numeric(y)-1),"03-20", sep="-")))
  }
  # Angle travelled by the Earth since last spring equinox
  travelledAngle <- nbDays*(360*pi/180)/365
  # Compute day duration
  f1 <- tan(lat*pi/180)*tan(asin(0.3979*sin(travelledAngle)))   # sin(23.5*pi/180), 23.5° is the Earth's inclination
  if(f1> 1){f1=1}
  if(f1< -1){f1=-1}
  dayDuration <- 24-((24/pi)*acos(f1))
  return(dayDuration)
}

# Add dayLight to Antarctica
Antarctica$dayDuration <- NA
for (i in 1:n){
  d <- Antarctica$date[i]
  Antarctica$dayDuration[i] <- getDayLightDuration(d=d,lat=-77.52343)
}

# Split Antarctica into 2 dataframe (one for visualisation, one for sonification)
dataVisu <- Antarctica[,c(1,2,4,6,8,10,12)]
dataSoni <- Antarctica[,c(1,3,5,7,9,11)]

# ----------
# Plot

# Create function to get plot for one variable
getPlot <- function(dataViz, dataSon, 
                    ptColour="black", bgColour="grey",lineColour="black",textColour="black",
                    yAxisText=0,
                    d0=min(dataViz[,1], na.rm=TRUE), dn=min(dataViz[,1], na.rm=TRUE),
                    yLimMin=min(dataViz[,2], na.rm=TRUE),yLimMax=max(dataViz[,2]), na.rm=TRUE,
                    circleSizeMin=min(dataViz[,2], na.rm=TRUE), circleSizeMax=max(dataViz[,2], na.rm=TRUE),
                    circleSizeRange=c(0,60),
                    plotTitle="title",
                    yAxistitle="value", ...){
  # Combine visu and soni
  data2 <- cbind(dataViz, dataSon[,2])
  colnames(data2) <- c("date", "value", "volume")
  
  # Plot
  p <- ggplot()+
    annotate("text", label=plotTitle, x=(d0-15), y=yLimMax, size=30, hjust=0, colour=textColour)+
    geom_line(data=data2,aes(x=date,y=value), colour=lineColour, size=2)+
    geom_point(data=data2[NROW(data2),],aes(x=date, y=value, size=volume, stroke=volume*10), 
               shape=21, fill=ptColour, colour=scales::alpha(colour=ptColour, alpha=0.5))+
    coord_cartesian(xlim=c(d0,dn))+
    scale_y_continuous(limits=c(yLimMin,yLimMax), expand=c(0.15,0.15),position = "right", ...)+
    labs(y=yAxistitle)+
    scale_size(range=circleSizeRange,limits=c(circleSizeMin, circleSizeMax))+
    theme(legend.position="none",
          panel.grid=element_blank(),
          axis.text=element_text(size=70),
          axis.text.x=element_text(margin=margin(t=30, b=20)),
          axis.title=element_text(size=90),
          panel.background=element_rect(fill=bgColour),
          plot.margin=margin(t=0, b=0,unit="cm"),
          axis.line = element_line(color=bgColour,size=2),
          panel.border=element_rect(color="white", fill="NA", size=2),
          axis.text.y.right=element_text(colour=textColour, margin=margin(l=-200)),
          axis.title.y.right = element_blank())
  return(p)
}

# Set Y axis manual breaks for each variable
varParam <- list("Radiation"=c(0,250,500,750,1000),
                 "Relative Humidity"=c(20,40,60,80,100),
                 "Streamflow"=c(0,4000,8000,12000),
                 "Temperature"=c(-50,-35,-20,-5,10),
                 "Windspeed"=c(0,5,10,15))

varName <- list("Radiation"=expression(paste("Radiation [",mu,"M/s/",m^2,"]")),
                 "Relative Humidity"="Relative Humidity [%]",
                 "Streamflow"="Streamflow [L/s]",
                 "Temperature"="Temperature [°C]",
                 "Windspeed"="Windspeed [m/s]")
# Size of largest circle for each variable
largestPoint=list(60,20,60,60,60)

# Set title colour
titleColour <- "#AEBACE"
# textColour <- "#44EE77"
textColour <- "#6EF5A5"
lineColour <- "#1E5786"

# Initialise list to store plot for each variable
varPlot <- vector(length=m, mode="list")
unlink('anim_im',recursive=TRUE);dir.create('anim_im')

for (i in 1:n) {   # For each row in Antarctica (one specific date)
  # Substring data
  dataViz <- dataVisu[seq(1,i),]
  dataSon <- dataSoni[seq(1,i),]
  currentDate <- dataViz$date[i]
  
  # Find panel background colour based on radiation value
  data_colour <- dataVisu[i,]
  p0 <- ggplot()+ geom_point(data=data_colour,aes(x=date, y=dayDuration, color=dayDuration))+
      scale_color_gradient(low="#103257", high="#81BEF2", na.value="white", 
                           limits=c(min(Antarctica$dayDuration, na.rm=TRUE),max(Antarctica$dayDuration, na.rm=TRUE)))

  col0 <- ggplot_build(p0)
  bgColour <- col0$data[[1]]$colour
  
  # Find point colour based on radiation value
  p1 <- ggplot()+ geom_point(data=data_colour,aes(x=date, y=dayDuration, color=dayDuration))+
      scale_color_gradient(low="#F1ECE4", high="#F4E577",na.value="white", 
                           limits=c(min(Antarctica$dayDuration, na.rm=TRUE),max(Antarctica$dayDuration, na.rm=TRUE)))
  
  col1 <- ggplot_build(p1)
  ptColour <- col1$data[[1]]$colour
  textColour <- ptColour
  
  # Get plot for each variable
  for (j in 1:m){ 
    # Get plot for j-th variable
    temp <- getPlot(dataViz=dataViz[,c(1,j+1)],dataSon=dataSon[,c(1,j+1)],
                    bgColour=bgColour,ptColour=ptColour,lineColour=lineColour,textColour=textColour,
                    d0=d0,dn=dn,
                    yLimMin=min(dataVisu[,j+1], na.rm=TRUE),yLimMax=max(varParam[[j]]),
                    circleSizeMin=min(dataSoni[,j+1], na.rm=TRUE), circleSizeMax=max(dataSoni[,j+1], na.rm=TRUE),
                    circleSizeRange=c(0,largestPoint[[j]]),
                    yAxistitle=varName[[j]],
                    plotTitle=varName[[j]],
                    breaks=varParam[[j]])
    # add current date and data source on Q panel
    vname=names(varParam)[j]
    if(vname=='Streamflow'){
      temp <- temp+annotate(geom='text',label=format(currentDate,format="%B %Y"),
                            x=Antarctica$date[round(n/2)],
                            y=0.5*max(varParam[[vname]]),
                            color=rgb(0,0,0,0.05),size=200,fontface='bold')
      temp <- temp+annotate(geom='text',label='Data: McMurdo Dry Valleys LTER',
                            x=Antarctica$date[1],y=min(Antarctica[[vname]],na.rm=TRUE),
                            hjust=0.2,vjust=1,
                            color=rgb(0,0,0,0.6),size=28)
    }
    # add plot title on R panel
    if(vname=='Radiation'){
      temp <- temp+annotate(geom='text',label=c("Weather in the Dry Valleys of Antarctica"),
                            x=Antarctica$date[round(n/2)],
                            y=0.97*max(varParam[[vname]]),
                            color=rgb(0,0,0,0.4),size=60)
    }
    # Add x axis
    temp <- temp+theme(axis.title.x=element_blank(),
                       axis.text.x=element_blank())
    # Store plot in varPlot
    varPlot[[j]] <- temp
  }
  
  # Assemble plots of all variable for i-th date
  allPlots <- varPlot[[1]]/varPlot[[2]]/varPlot[[5]]/varPlot[[4]]/varPlot[[3]]

  # Save
  png(filename=paste0(wd, "/anim_im/",sprintf("im_%04d", i),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(allPlots)
  dev.off()
  print(paste0(i,"/",n))
}

# ----------
# Animate
bpm=120
audio="finalMix.mp3"
output="Antarctica.mp4"
# Get image files and audio duration
lf <- list.files("anim_im", full.names=TRUE)  # List all images
k <- length(lf)
spt=(60/bpm)/4 # second per time step
d=k*spt
# Make sure lf is sorted numerically rather than alphabetically
foo=unlist(strsplit(lf,'_'))
i.png=foo[seq(3,length(foo),3)]
foo=unlist(strsplit(i.png,'\\.'))
number=as.integer(foo[seq(1,length(foo),2)])
w=sort.int(number,index.return=T)
lf=lf[w$ix]
# Set animation fade in and fade out
nFadeIn <- 3   # In seconds
nFadeOut <- 8   # In seconds
fadeFilter <- paste0("fade=t=in:st=0:d=", nFadeIn, ":alpha=0", ", fade=t=out:st=", d-nFadeOut, ":d=", nFadeOut, ":alpha=0")

av_encode_video(input=lf,audio=audio,output=output,framerate=k/d,
                vfilter=fadeFilter)   # Fade in and out
  