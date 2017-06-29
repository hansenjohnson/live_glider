# Set up environment ------------------------------------------------------

rm(list = ls())
# load in various required packages and data
library(data.table, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(plotly, quietly = T, warn.conflicts = F)
library(oce, quietly = T, warn.conflicts = F)
library(ocedata, quietly = T, warn.conflicts = F)
library(cowplot, quietly = T, warn.conflicts = F)
library(plyr, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(marmap, quietly = T)
library(gridExtra, quietly = T)
data(coastlineWorldFine)
load('bathy.rda')

# Get data from server ----------------------------------------------------

ctd_file = "http://gliders.oceantrack.org/data/live/dal556_sci_water_live.csv"
ctd = fread(ctd_file)

# Manipulate data frame ---------------------------------------------------

# make timestamps intelligable (convert from unixtime)
ctd$time <- as.POSIXct(ctd$unixtime, tz ="America/Halifax", origin = "1970-01-01 00:00:00")

# convert from dbar to bar
ctd$sci_water_pressure = ctd$sci_water_pressure*10

# calculate salinity
ctd$salinity = swSCTp(conductivity = ctd$sci_water_cond, 
                      temperature = ctd$sci_water_temp,
                      pressure = ctd$sci_water_pressure,
                      conductivityUnit = "S/m")

# calculate density
ctd$density = swRho(salinity = ctd$salinity, 
                    temperature = ctd$sci_water_temp, 
                    pressure = ctd$sci_water_pressure)



# ggplot ------------------------------------------------------------------

plot_text = paste0('Plotting all ', nrow(ctd), ' data points...')

# Only plot a subset of data 
if(nrow(ctd)>10000){
  norig = nrow(ctd) # original number of points
  nsub = round(nrow(ctd)/10000,0) # every n number of samples to plot
  ctd = ctd[seq(1, nrow(ctd), nsub),] # subset to plot every other data point
  
  plot_text = paste0('Plotting all ', norig, ' points will take too long. Plotting every ', nsub, ' points instead...')
}

# remove spurious values recorded at the surface
ctd$salinity[which(ctd$sci_water_pressure < 1 & ctd$salinity < 15)] = NA
ctd$sci_water_temp[which(ctd$sci_water_pressure < 1 & ctd$sci_water_temp <= 0)] = NA

temp = ggplot()+
  geom_line(data = ctd, aes(x = time, y = sci_water_pressure), colour = 'grey', na.rm = TRUE)+
  geom_point(data = ctd, aes(x = time, y = sci_water_pressure, colour = sci_water_temp), na.rm = TRUE)+
  scale_colour_gradientn(colours = oce.colorsTemperature(200)) +
  scale_y_reverse() +
  labs(x = '', y = 'Pressure [dbar]', title = 'Temperature [deg C]', colour = '_______')+
  theme_bw()

sal = ggplot()+
  geom_line(data = ctd, aes(x = time, y = sci_water_pressure), colour = 'grey', na.rm = TRUE)+
  geom_point(data = ctd, aes(x = time, y = sci_water_pressure, colour = salinity), na.rm = TRUE)+
  scale_colour_gradientn(colours = oce.colorsSalinity(200)) +
  labs(x = '', y = 'Pressure [dbar]', title = 'Salinity', colour = '_______')+
  scale_y_reverse() +
  theme_bw()

dens = ggplot()+
  geom_line(data = ctd, aes(x = time, y = sci_water_pressure), colour = 'grey', na.rm = TRUE)+
  geom_point(data = ctd, aes(x = time, y = sci_water_pressure, colour = density), na.rm = TRUE)+
  scale_colour_gradientn(colours = oce.colorsDensity(200)) +
  labs(x = '', y = 'Pressure [dbar]', title = 'Density [kg/m3]', colour = '_______')+
  scale_y_reverse() +
  theme_bw()


# map ---------------------------------------------------------------------

# vars for plotting dimensions
# plotting dimensions
mlon = median(ctd$lon)
mlat = median(ctd$lat)
span = ceiling(geodDist(latitude1 = min(ctd$lat), longitude1 = min(ctd$lon),
                        latitude2 = max(ctd$lat), longitude2 = max(ctd$lon)))*2

# plot coastline
plot(coastlineWorldFine, clon = mlon, clat = mlat, span = span)

# plot bathymetry
contour(bathyLon,bathyLat,bathyZ,levels = c(-50, -100, -150, -200, -250),lwd = c(1, 1, 2, 2, 3),lty = c(3, 1, 3, 1, 3),drawlabels = TRUE,add = TRUE,col = 'darkgray')

# add depth legend
legend("bottomright",lwd = c(1, 1, 2, 2, 3),lty = c(3, 1, 3, 1, 3),col = 'darkgray',seg.len = 3,cex = 0.7,title = "Depth [m]",legend = c("50", "100", "150", "200", "250"),bg= "white")


# glider track
lines(ctd$lon, ctd$lat, col = 'blue')

# # plot timestamp each day
# date = seq(trunc(min(ctd$time), 'day'), max(ctd$time), by = 'day')
# 
# # determine day labels
# labels = data.frame()
# for(i in seq_along(date)){
#   d = ctd[which.min(abs(ctd$time - date[i])),]
#   labels = rbind(d, labels)
# }
# 
# # day labels
# daylab = labels[1:(nrow(labels)-1),]
# points(daylab$lon, daylab$lat, pch = 4)
# text(daylab$lon, daylab$lat, labels = format(round(daylab$time, 'days'), '%d-%b'), pos = 4)

# begin and end
begin = ctd[1,]
points(begin$lon, begin$lat, pch = 17, col = 'black')
# text(begin$lon, begin$lat, labels = begin$time, pos = 4, col = 'black')

latest = ctd[nrow(ctd),]
points(latest$lon, latest$lat, pch = 20, col = 'red')
# text(latest$lon, latest$lat, labels = latest$time, pos = 4, col = 'red')

# add glider legend
legend("bottomleft", lty=c(1,NA,NA), pch = c(NA, 17, 20), col = c('blue', 'black', 'red'), c('Track', paste0('Start: ', begin$time), paste0('Latest: ', latest$time)), cex = 0.7, title = 'Glider:')

# # stack static ggplots
# grid.arrange(temp, sal, dens, ncol = 1)

# interactive sections
ggplotly(temp, dynamicTicks = T)
ggplotly(sal,dynamicTicks = T)
ggplotly(dens,dynamicTicks = T)