# add points to section plot ----------------------------------------------

add_points = function(wd, pos, pt_col, pt_pch = 21, cex = 2){
  
  x = as.POSIXct(wd$time)
  y = rep(pos, length(x))
  
  points(x, y, pch = pt_pch, bg = pt_col, cex = cex)
}

# plot section ------------------------------------------------------------

plot_section = function(ctd,var,zlim,cex.all=1.5){
  
  # setup
  rw_pos = 150
  fw_pos = 160
  sw_pos = 170
  hw_pos = 180
  lab_pos = c(rw_pos,fw_pos,sw_pos,hw_pos)
  spp = c('right', 'fin', 'sei', 'humpback')
  
  # setup layout for plotting
  m = rbind(c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,2),
            c(1,1,1,1,1,1,1,1,1,1,1,3))
  
  # switch for variable
  if(var == 'temperature'){
    pal = oce.colorsTemperature()
    lab = 'Temperature [deg C]'
    c = colormap(ctd$temperature, breaks=100, zclip = T, col = pal, zlim = zlim)
  } else if(var == 'salinity'){
    pal = oce.colorsSalinity()
    lab = 'Salinity'
    c = colormap(ctd$salinity, breaks=100, zclip = T, col = pal, zlim = zlim)
  } else if(var == 'density'){
    pal = oce.colorsDensity()
    lab = 'Density [kg/m3]'
    c = colormap(ctd$density, breaks=100, zclip = T, col = pal, zlim = zlim)
  } else {
    stop('Unknown variable! Please choose from: temperature, salinity, or density')
  }
  
  layout(m)
  
  # plot section
  plot(ctd$time, ctd$depth, ylim = c(175,0), type = 'l', col = 'grey', 
       ylab = 'Depth [m]', xlab = '', cex.lab = cex.all, cex.axis = cex.all)
  mtext(paste0(lab),side = 3, line = 1,adj = 0)
  points(ctd$time, ctd$depth, pch = 21, bg = c$zcol, cex = 1.75, col = NULL)
  
  # overlay detections
  abline(h = lab_pos, col = 'grey')
  add_points(rw_m, rw_pos, 'yellow')
  add_points(rw_p, rw_pos, 'red')
  add_points(fw_m, fw_pos, 'yellow')
  add_points(fw_p, fw_pos, 'red')
  add_points(sw_m, sw_pos, 'yellow')
  add_points(sw_p, sw_pos, 'red')
  add_points(hw_m, hw_pos, 'yellow')
  add_points(hw_p, hw_pos, 'red')
  
  # add axis
  axis(side = 4, at = lab_pos, tick = T, labels = spp, las = 2)
  
  # add palette
  drawPalette(c$zlim, col=c$col, breaks=c$breaks, zlab = '', fullpage = T, 
              cex.axis = cex.all, cex.lab = cex.all)
  
}