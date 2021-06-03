library(ggplot2)

# read data
floods=read.table(file='floodObservatory_06June2020.csv',header=TRUE,sep=';',quote=NULL)
# order by area and rescale
floods=floods[order(floods$Area,decreasing=TRUE),]
floods$Area=floods$Area/10000
# restrict to target year
targetYear=2019
years=as.numeric(format(as.Date(floods$Began,format='%d/%m/%Y'),'%Y'))
floods=floods[years==targetYear,]

# Create background map
world = map_data("world")
g=ggplot()
g=g+geom_polygon(data=world,aes(long,lat,group=group),
                 fill='#fff4caff',color=NA,size=0.1)
g=g+coord_fixed(ratio=1,ylim=c(-58,83.6),expand=FALSE)
g=g+theme_void()
g=g+theme(panel.border=element_rect(fill=NA))
# Add points
g=g+geom_point(data=floods,aes(x=long,y=lat,colour=Duration,size=Area),alpha=0.6)
lab=expression(paste('Area [',10^4,' ',km^2,']'))
g=g+scale_size(lab,breaks=c(1,10,50,100),range=c(0,10))
g=g+scale_colour_distiller('Duration [day]',palette='Blues',trans='log10',direction=1)
g=g+guides(size=guide_legend(order=1),colour=guide_colourbar(order=2)) # order of legends
# titles
g=g+labs(title=paste('Major flood events in',targetYear),
         subtitle='Taken from the Dartmouth Flood Observatory',
         caption='G.R.Brakenridge, "Global Active Archive of Large Flood Events", Dartmouth Flood Observatory, University of Colorado.\n http://floodobservatory.colorado.edu/Archives/index.html, accessed 06 June 2020')

# save
png(filename=paste0('floods',targetYear,'.png'),width=250,height=100,units='mm',res=300)
print(g)
dev.off()
