plot_section = function(ctd,var,zlim,cex.all=2){
  
  # switch for variable
  if(var == 'temperature'){
    pal = oce.colorsTemperature()
    lab = 'Temperature [deg C]'
    # zlim = c(-1,25)
  } else if(var == 'salinity'){
    pal = oce.colorsSalinity()
    lab = 'Salinity'
    # zlim = c(22,35)
  } else if(var == 'density'){
    pal = oce.colorsDensity()
    lab = 'Density [kg/m3]'
    # zlim = c(1017,1028)
  } else {
    stop('Unknown variable! Please choose from: temperature, salinity, or density')
  }
  
  # define colormap
  c = colormap(unlist(ctd[which(colnames(ctd) == var)]), breaks=100, zlim = zlim, zclip = T, col = pal) # map values to colors
  
  # setup for plotting
  m = rbind(c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2))
  
  layout(m)
  
  # plot section
  plot(ctd$time, ctd$depth, ylim = c(175,0), type = 'l', col = 'grey', 
       ylab = 'Depth [m]', xlab = '', cex.lab = cex.all, cex.axis = cex.all)
  mtext(paste0(lab),side = 3, line = 1,adj = 0)
  points(ctd$time, ctd$depth, pch = 21, bg = c$zcol, cex = cex.all, col = NULL)
  
  # add palette
  drawPalette(c$zlim, col=c$col, breaks=c$breaks, zlab = '', fullpage = T, 
              cex.axis = cex.all, cex.lab = cex.all)
  
}