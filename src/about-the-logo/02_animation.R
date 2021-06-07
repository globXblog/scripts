# Create logo animation
#---------------

# Load required data and packages
library(ggplot2);library(gganimate);library(av)

# Read data
dfplot <- read.csv("logoData.csv", sep=";")

# Set colours
colpal <- c("#DEEBF7","#9ECAE1","#4292C6", "#08519C",  # winter
            "#E5F5E0", "#A1D99B", "#41AB5D", "#006D2C",   # spring
            "#FEE6CE", "#FDAE6B", "#F16913", "#A63603",   # summer
            "#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F")   # fall

#---------------
# Animate growing plot by year and month while turning

# Create dfplot2 and fill out with 1st year and 1st month
dfplot2 <- dfplot[1:4,]
dfplot2$anim_state <- 1

# Fill first year in dfplot2
yrs <- unique(dfplot$year)   # Years available
n <- length(yrs)   # Number of years

yr <- min(dfplot$year)
for (i in 2:12){
  mask <- dfplot[dfplot$year==yr & dfplot$month<=i,]
  mask$anim_state <- i
  dfplot2 <- rbind(dfplot2,mask)
}

# Fill all other years in dfplot2 except lastest year
temp <- dfplot2[dfplot2$anim_state==12,]
animState <- 13

for (i in 2:(n-1)){      # Loop on n years
  yr <- yrs[i]
  for (j in 1:12){   # Loop on 12 months
  whichRows <- which(temp$month==j)
  for (k in whichRows){
    targetClf <- temp$classif[k]
    temp$year <- yr
    temp$nb[k] <- dfplot$nb[dfplot$year==yr & dfplot$month==j & dfplot$classif==targetClf]
  }
  temp$anim_state <- animState
  dfplot2 <- rbind(dfplot2,temp)
  animState <- animState+1
  }
}

# Fill lastest year in dfplot2
latestMonth <- dfplot$month[NROW(dfplot)]
latestYear <- dfplot$year[NROW(dfplot)]

for (j in 1:latestMonth){   # Loop on months
  whichRows <- which(temp$month==j)
  for (k in whichRows){
    targetClf <- temp$classif[k]
    temp$year <- latestYear
    temp$nb[k] <- dfplot$nb[dfplot$year==latestYear & dfplot$month==j & dfplot$classif==targetClf]
  }
  temp$anim_state <- animState
  dfplot2 <- rbind(dfplot2,temp)
  animState <- animState+1
}

# Calculate plot rotation angle
dfplot2$changingMonth <- NA
dfplot2$angle <- NA
for (i in 1:NROW(dfplot2)){
  j <- dfplot2$anim_state[i]%%12
  if (j==0){
    dfplot2$changingMonth[i] <- 12
  } else {
    dfplot2$changingMonth[i] <- j
  }
  dfplot2$angle[i] <- -(pi/6)*(j-1)
}

# Plot each anim_state and save plot
wd <- getwd()
unlink('anim_im',recursive=TRUE);dir.create('anim_im')
p <- (12*(n-1))+latestMonth   # Number of data, latest year (2020) is not complete, the latest month is June

for (i in 1:p){
  mask <- dfplot2[dfplot2$anim_state==i,]
  theta <- mask$angle[1]
  g <- ggplot()+
    geom_col(aes(x=mask$changingMonth[1],y=625),fill="grey92")+
    geom_col(data=mask, aes(x=month, y=nb, fill=colr))+
    geom_hline(yintercept=-5, size=1.6)+
    scale_fill_gradientn(colours=colpal, limits=c(1,16))+
    coord_polar(start=theta)+
    xlim(0.5,12.5)+
    ylim(-200,680)+
    theme_void()+
    theme(legend.position="none")
  png(filename=paste0(wd, "/anim_im/",sprintf("im_%03d", i), ".png"),
      width=(1280*3), height=(720*3), res=36)
  print(g)
  dev.off()
  print(paste0(i,"/",p))
}

# Create last image one without grey column
mask <- dfplot2[dfplot2$anim_state==p,]
theta <- mask$angle[1]
g <- ggplot()+
  geom_col(data=mask, aes(x=month, y=nb, fill=colr))+
  geom_hline(yintercept=-5, size=1.6)+
  scale_fill_gradientn(colours=colpal, limits=c(1,16))+
  coord_polar(start=theta)+
  xlim(0.5,12.5)+
  ylim(-200,680)+
  theme_void()+
  theme(legend.position="none")
png(filename=paste0(wd, "/","lastImage.png"),
    width=(1280*3), height=(720*3), res=36)
print(g)
dev.off()

# List all images
lf <- list.files("anim_im", full.names=TRUE)

# Write animation between 2 last images and logo so transition is smooth
av_encode_video(input=c(lf[p],paste0(wd, "/lastImage.png"),paste0(wd, "/blogLogo.png")),
                output="lastImagesAnimation.mp4",
                vfilter = 'framerate=fps=480',
                framerate=24)

# Extract images from "lastImagesAnimation"
av_video_convert(video="lastImagesAnimation.mp4",
                 output=file.path(paste0(wd, "/anim_im/","im_8%02d.png")))

# Write full animation
lf <- list.files("anim_im", full.names=TRUE)  # List all images
av_encode_video(input=lf,
                audio="logo.wav",
                output="about-the-logo.mp4",
                framerate=18)

