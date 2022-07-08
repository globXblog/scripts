getPafPolygons <- function(pafData) {
  # Duplicate data
  foo <- pafData
  n <- nrow(foo)
  # Get polygon coordinates to draw monthly paf
  paf_polygons <- data.frame(matrix(ncol=4,nrow=5*n))  # Each polygon has 5 points
  colnames(paf_polygons) <- c("x","y","id","fill")
  for (i in 1:n){
    irows <- c((((i-1)*5)+1):(i*5))
    if(i==1){
      a2 <- (foo$paf_regime[i+1]-foo$paf_regime[i])/(foo$idx_monthly[i+1]-foo$idx_monthly[i])
      b2 <- foo$paf_regime[i+1]-(a2*foo$idx_monthly[i+1])
      ymid2 <- (a2*(foo$idx_monthly[i]+0.45))+b2
      paf_polygons$x[irows] <- c(foo$idx_monthly[i]-0.45,foo$idx_monthly[i],foo$idx_monthly[i]+0.45,foo$idx_monthly[i]+0.45,foo$idx_monthly[i]-0.45)
      paf_polygons$y[irows] <- c(foo$paf_regime[i],foo$paf_regime[i],ymid2,foo$paf_monthly[i],foo$paf_monthly[i])
      paf_polygons$id[irows] <- i
      paf_polygons$fill[irows] <- ifelse(foo$paf_monthly[i]>foo$paf_regime[i],"wet","dry")
    }
    else if(i==n){
      a1 <- (foo$paf_regime[i]-foo$paf_regime[i-1])/(foo$idx_monthly[i]-foo$idx_monthly[i-1])
      b1 <- foo$paf_regime[i]-(a1*foo$idx_monthly[i])
      ymid1 <- (a1*(foo$idx_monthly[i]-0.45))+b1
      paf_polygons$x[irows] <- c(foo$idx_monthly[i]-0.45,foo$idx_monthly[i],foo$idx_monthly[i]+0.45,foo$idx_monthly[i]+0.45,foo$idx_monthly[i]-0.45)
      paf_polygons$y[irows] <- c(ymid1,foo$paf_regime[i],foo$paf_regime[i],foo$paf_monthly[i],foo$paf_monthly[i])
      paf_polygons$id[irows] <- i
      paf_polygons$fill[irows] <- ifelse(foo$paf_monthly[i]>foo$paf_regime[i],"wet","dry")
    }
    else {
      # Slope before i-th point
      a1 <- (foo$paf_regime[i]-foo$paf_regime[i-1])/(foo$idx_monthly[i]-foo$idx_monthly[i-1])
      b1 <- foo$paf_regime[i]-(a1*foo$idx_monthly[i])
      ymid1 <- (a1*(foo$idx_monthly[i]-0.45))+b1
      # Slope after i-th point
      a2 <- (foo$paf_regime[i+1]-foo$paf_regime[i])/(foo$idx_monthly[i+1]-foo$idx_monthly[i])
      b2 <- foo$paf_regime[i+1]-(a2*foo$idx_monthly[i+1])
      ymid2 <- (a2*(foo$idx_monthly[i]+0.45))+b2
      # Create dataframe
      paf_polygons$x[irows] <- c(foo$idx_monthly[i]-0.45,foo$idx_monthly[i],foo$idx_monthly[i]+0.45,foo$idx_monthly[i]+0.45,foo$idx_monthly[i]-0.45)
      paf_polygons$y[irows] <- c(ymid1,foo$paf_regime[i],ymid2,foo$paf_monthly[i],foo$paf_monthly[i])
      paf_polygons$id[irows] <- i
      paf_polygons$fill[irows] <- ifelse(foo$paf_monthly[i]>foo$paf_regime[i],"wet","dry")
    }
  }
  return(paf_polygons)
}

getAnnualPolygons <- function(annualData) {
  # Duplicate data
  foo <- annualData
  # Get polygon coordinares to draw annual values
  annual_polygons <- data.frame(matrix(ncol=4,nrow=4*n))  # Each polygon has 4 points
  colnames(annual_polygons) <- c("x","y","id","fill")
  for (i in 1:n){
    irows <- c((((i-1)*4)+1):(i*4))
    # x axis starts at 0.5 (not 0.55 like getPafPolygons, so there is no gap between 2 bars in a row)
    annual_polygons$x[irows] <- c(foo$idx_monthly[i]-0.5,foo$idx_monthly[i]+0.5,foo$idx_monthly[i]+0.5,foo$idx_monthly[i]-0.5)
    annual_polygons$y[irows] <- c(0,0,foo$annual[i],foo$annual[i])
    annual_polygons$id[irows] <- i
  }
  return(annual_polygons)
}

getPafPlot0 <- function(annualData, pafPolygons,regimeColour="black") {
  # Duplicate data
  foo_a <- annualData
  foo_p <- pafPolygons
  n <- NROW(foo_a)
  # Plot regime paf (black line)
  pafPlot_t0 <- ggplot()+theme_void()+
    geom_line(data=foo_a,aes(x=idx_monthly,y=paf_regime),color=regimeColour)
  # Add y lims for a given site (get max ylims with ggplot_build by drawing the whole series)
  pafPlot_ylims <- pafPlot_t0+geom_polygon(data=foo_p, aes(x=x,y=y,group=id))
  paf_ylims <- ggplot_build(pafPlot_ylims)$layout$panel_scales_y[[1]]$range$range
  pafPlot_t0 <- pafPlot_t0+
    scale_x_continuous(limits=c(0.5,(n+1)), expand=c(0,0))+
    scale_y_continuous(limits=paf_ylims)
  return(pafPlot_t0)
}

getAnnualDailyPlot0 <- function(dailyData, annualPolygons, annualColour="red", dailyFullColour="grey96") {
  # Duplicate data
  foo_d <- dailyData
  foo_ap <- annualPolygons
  n <- max(foo_ap$id)
  # Get y lims for a given site (get max ylims with ggplot_build by drawing the whole series)
  # annualPlot_ylims <- ggplot2::ggplot()+ggplot2::geom_polygon(data=foo_ap, aes(x=x,y=y,group=id),fill=annualColour)
  # annual_ylims <- ggplot2::ggplot_build(annualPlot_ylims)$layout$panel_scales_y[[1]]$range$range
  # dailyPlot_ylims <- ggplot2::ggplot()+ggplot2::geom_line(data=foo_d, aes(x=new_x,y=value_daily))
  # daily_ylims <- ggplot2::ggplot_build(dailyPlot_ylims)$layout$panel_scales_y[[1]]$range$range
  ylimmax <- max(foo_d$value_daily,na.rm=TRUE)
  # Plot annual values, month by month (grey bars)
  annualDailyPlot_t0 <- ggplot2::ggplot()+
    ggplot2::geom_line(data=foo_d, aes(x=new_x,y=value_daily), colour=dailyFullColour)+
    ggplot2::theme_void()+
    ggplot2::scale_x_continuous(limits=c(0.5,(n+1)), expand=c(0,0))+
    ggplot2::scale_y_continuous(limits=c(0,ylimmax))+
    ggplot2::geom_text(aes(x=0.5,y=ylimmax*0.97, 
                           label=min(foo_d$year, na.rm=TRUE)), hjust=0, colour=dailyFullColour, size=14)+
    ggplot2::geom_text(aes(x=foo_d$new_x[foo_d$year==max(foo_d$year, na.rm=TRUE)][1],y=ylimmax*0.97, 
                           label=max(foo_d$year, na.rm=TRUE)), hjust=0, colour=dailyFullColour, size=14)
  return(annualDailyPlot_t0)
}

# getAnnualPlot0 <- function(data, dataPolygons, annualColour="grey92") {
#   # Duplicate data
#   foo <- data
#   annual_polygons <- dataPolygons
#   n <- NROW(foo)
#   # Get y lims for a given site (get max ylims with ggplot_build by drawing the whole series)
#   annualPlot_ylims <- ggplot()+geom_polygon(data=annual_polygons, aes(x=x,y=y,group=id),fill=annualColour)
#   annual_ylims <- ggplot_build(annualPlot_ylims)$layout$panel_scales_y[[1]]$range$range
#   # Plot annual values, month by month (grey bars)
#   annualPlot_t0 <- ggplot()+theme_void()+xlim(c(0.55,(n+0.45)))+ylim(annual_ylims)
#   return(annualPlot_t0)
# }
# 
# getDailyPlot0 <- function(data) {
#   # Duplicate data
#   foo <- data
#   # Get number of months
#   n <- max(foo$id)
#   # Get y lims (get max ylims with ggplot_build by drawing the whole series)
#   dailyPlot_ylims <- ggplot()+geom_line(data=foo, aes(x=new_x,y=value_daily))
#   daily_ylims <- ggplot_build(dailyPlot_ylims)$layout$panel_scales_y[[1]]$range$range
#   # Plot daily values, month by month (black line)
#   dailyPlot_t0 <- ggplot()+theme_void()+xlim(c(0.55,(n+0.45)))+ylim(daily_ylims)
#   return(dailyPlot_t0)
# }
