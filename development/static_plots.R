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

live_file = "http://gliders.oceantrack.org/data/live/dal556_sci_water_live.csv"
d = fread(live_file)

# Manipulate data frame ---------------------------------------------------

time <- as.POSIXct(d$unixtime, origin=as.POSIXct("1970-01-01 00:00:00", tz="UTC"))
longitude <- d$lon
latitude <- d$lat
depth <- d$i_depth
mlat <- median(latitude)
mlon <- median(longitude)
temperature <- d$sci_water_temp
pressure <- 10 * d$sci_water_pressure # DLH says pressure in bar, not dbar
conductivity <- d$sci_water_cond
salinity <- swSCTp(conductivity, temperature, pressure, conductivityUnit="S/m")

Tlim <- range(temperature, na.rm=TRUE)
Slim <- range(salinity, na.rm=TRUE)
plim <- rev(range(pressure, na.rm=TRUE))
ctd <-
  as.ctd(
    salinity,
    temperature,
    pressure,
    latitude = latitude,
    longitude = longitude,
    time = time
  )

# isolate profiles --------------------------------------------------------

# params
hpressure = 200
hcond = 5
lcond = 3
smoother = T
fwindow = 11
pressurediff = 5
direction = 'descending'

if(smoother){
  # find profiles using a smoother
  message('Isolating profiles using a smoother...')
  movingAverage <- function(x, n = fwindow, ...)
  {
    f <- rep(1/n, n)
    stats::filter(x, f, ...)
  }
  profiles <- ctdFindProfiles(ctd, smoother = movingAverage, minLength = 10, direction = direction)
} else {
  message('Isolating profiles using pressure difference')
  breaks <- which(pressurediff < diff(pressure))
  profiles <- ctdFindProfiles(ctd, breaks=breaks, direction = direction)
  
  # trim profiles to remove spurious values from beginning of scan
  profiles.trimmed <- profiles
  for (iprofile in seq_along(profiles)) {
    profile <- profiles[[iprofile]]
    profiles.trimmed[[iprofile]] <- ctdTrim(profile, "range",
                                            parameters = list(item = "scan", from =
                                                                min(profile[['scan']]) + 5))
  }
  profiles <- profiles.trimmed
}

# plot basic sections -----------------------------------------------------

message('Gridding profiles and extracting data for section plots...')

fullsection = as.section(profiles)

plot(fullsection)

sg <- sectionGrid(fullsection, p=seq(0, ceiling(max(pressure)), 0.5)) # grid profiles into 0.5 m depth bins from 0 to 200 m

nstation <- length(sg[['station']])
p <- unique(sg[['pressure']])
np <- length(p)
dL <- array(NA, dim = c(nstation, 2)) # pull out lat lon of each profile
Temp <- array(NA, dim=c(nstation, np))
Sal <- array(NA, dim=c(nstation, np))
St <- array(NA, dim=c(nstation, np))
time <- rep(NA, nstation)

for (i in 1:nstation) {
  temperature = sg[['station']][[i]][['temperature']]
  salinity = sg[['station']][[i]][['salinity']]
  pressure = sg[['station']][[i]][['pressure']]
  latitude = sg[['station']][[i]][['latitude']]
  longitude = sg[['station']][[i]][['longitude']]
  rho = swRho(salinity, temperature, pressure, latitude, longitude)
  
  Temp[i, ] <- temperature
  Sal[i, ] <- salinity
  St[i, ] <- rho
  dL[i, 1] <- min(longitude, na.rm = T)
  dL[i, 2] <- min(latitude, na.rm = T)
  time[i] <- mean(sg[['station']][[i]][['time']], na.rm = T)
}

time = as.POSIXct(time, origin = '1970-01-01 00:00:00', tz = 'UTC')

message('Plotting sections separately...')

imagep(x = time, y = p, z = Temp, decimate = TRUE, col = oce.colorsTemperature, flipy = T)
imagep(x = time, y = p, z = Sal, decimate = FALSE, col = oce.colorsSalinity, flipy = T)
imagep(x = time, y = p, z = St, decimate = FALSE, col = oce.colorsDensity, flipy = T)


# calculate colour levels for contoured sections
n.col = 8 # number of levels to break colour bar into
Tcm <- colormap(Temp, breaks=seq(round(min(Temp, na.rm = T),0), round(max(Temp, na.rm = T),0), length.out = n.col), col=oceColorsTemperature)
Scm <- colormap(Sal, breaks=seq(round(min(Sal, na.rm = T),0), round(max(Sal, na.rm = T),0), length.out = n.col), col=oceColorsSalinity)
Dcm <- colormap(St, breaks=seq(round(min(St, na.rm = T),0), round(max(St, na.rm = T),0), length.out = n.col), col=oceColorsDensity)


# plot contoured sections separately
imagep(x = time, y = p, z = Temp, colormap=Tcm, flipy=TRUE, decimate = F, 
       ylab='Pressure [dbar]', filledContour=TRUE,
       zlab='Temperature [deg C]', missingColor = NULL)

imagep(x = time, y = p, z = Sal, colormap=Scm, flipy=TRUE, decimate = F, 
       ylab='Pressure [dbar]', filledContour=TRUE,
       zlab='Salinity', missingColor = NULL)

imagep(x = time, y = p, z = St, colormap=Dcm, flipy=TRUE, decimate = F, 
       xlab='Time', ylab='Pressure [dbar]', filledContour=TRUE,
       zlab='Density [kg/m3]', missingColor = NULL)

message('Plotting stacked sections...')

par(mfrow = c(3,1))

imagep(x = time, y = p, z = Temp, decimate = FALSE, col = oce.colorsTemperature, flipy = T, zlab = 'Temperature [deg C]', ylab = 'Pressure [dbar]')
imagep(x = time, y = p, z = Sal, decimate = FALSE, col = oce.colorsSalinity, flipy = T, zlab = 'Salinity [PSU]', ylab = 'Pressure [dbar]')
imagep(x = time, y = p, z = St, decimate = FALSE, col = oce.colorsDensity, flipy = T, zlab = 'Density [kg m-3]', ylab = 'Pressure [dbar]')


par(mfrow=c(3, 1))
imagep(x = time, y = p, z = Temp, colormap=Tcm, flipy=TRUE, decimate = F, 
       ylab='Pressure [dbar]', filledContour=TRUE,
       zlab='Temperature [deg C]', missingColor = NULL)
imagep(x = time, y = p, z = Sal, colormap=Scm, flipy=TRUE, decimate = F, 
       ylab='Pressure [dbar]', filledContour=TRUE,
       zlab='Salinity', missingColor = NULL)
imagep(x = time, y = p, z = St, colormap=Dcm, flipy=TRUE, decimate = F, 
       xlab='Time', ylab='Pressure [dbar]', filledContour=TRUE,
       zlab='Density [kg/m3]', missingColor = NULL)


# # Full TS diagram ---------------------------------------------------------
# # plain diagram
# # plotTS(fullsection)
# message('Plotting full TS section...')
# 
# png(paste0(subdir3,'/full_ts.png'), width=7, height=5, unit="in", res=100,pointsize=10)
# 
# plotTS(fullsection, pch = 1, mar=par("mar")+c(0, 0, 0, 1))
# 
# dev.off()

# Save data for further section plotting ----------------------------------




# map ---------------------------------------------------------------------

# vars for plotting dimensions
# plotting dimensions
mlon = median(ctd$lon)
mlat = median(ctd$lat)
span = ceiling(geodDist(latitude1 = min(ctd$lat), longitude1 = min(ctd$lon),
                        latitude2 = max(ctd$lat), longitude2 = max(ctd$lon)))*2.5

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
points(begin$lon, begin$lat, pch = 17, col = 'darkgreen')
# text(begin$lon, begin$lat, labels = begin$time, pos = 4, col = 'black')

latest = ctd[nrow(ctd),]
points(latest$lon, latest$lat, pch = 20, col = 'darkred')
# text(latest$lon, latest$lat, labels = latest$time, pos = 4, col = 'red')

# add glider legend
legend("bottomleft", lty=c(1,NA,NA), pch = c(NA, 17, 20), col = c('blue', 'black', 'red'), c('Track', paste0('Start: ', begin$time), paste0('Latest: ', latest$time)), cex = 0.7, title = 'Glider:')

