
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
library(ggplot2)
library(marmap)
library(plotly)
library(devtools)
library(ggmap)
devtools::install_github('ropensci/plotly')
devtools::install_github("hadley/ggplot2")

data(coastlineWorldFine)
load('bathy.rda')

# Get data from server ----------------------------------------------------

pres_file = "http://gliders.oceantrack.org/data/live/bond_sci_water_pressure_live.csv"
pres = fread(pres_file)

temp_file = "http://gliders.oceantrack.org/data/live/bond_sci_water_temp_live.csv"
temp = fread(temp_file)

cond_file = "http://gliders.oceantrack.org/data/live/bond_sci_water_cond_live.csv"
cond = fread(cond_file)

# Manipulate data frame ---------------------------------------------------

# merge to create a single CTD data frame
ctd = join_all(list(pres, temp, cond), type = 'full')

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

# vars for plotting dimensions
lat_offset = 0.5
lon_offset = 0.5
ymax = max(ctd$lat+lat_offset)
ymin = min(ctd$lat-lat_offset)
xmax = max(ctd$lon+lon_offset)
xmin = min(ctd$lon-lon_offset)

# begin and end
begin = ctd[1,]
points(begin$lon, begin$lat, pch = 17, col = 'darkgreen')
text(begin$lon, begin$lat, labels = format(begin$time, '%d-%b %H:%M:%S'), pos = 4, col = 'darkgreen')

latest = ctd[nrow(ctd),]
points(latest$lon, latest$lat, pch = 20, col = 'darkred')
text(latest$lon, latest$lat, labels = format(latest$time, '%d-%b %H:%M:%S'), pos = 4, col = 'darkred')

# convert bathy to form that ggplot recognizes
b.df = fortify(b)

gg = 
  ggplot()+
  geom_contour(data=b.df, aes(x=x, y=y,z=z), breaks=c(-50,-100,-150,-200,-500),colour="darkgrey", size=0.1)+
  geom_contour(data=b.df, aes(x=x, y=y,z=z), breaks=c(0),colour="black", size=0.5)+
  xlim(xmin,xmax)+
  ylim(ymin,ymax)+
  labs(x = 'Longitude', y = 'Latitude')+
  geom_point(data = ctd, aes(x=lon,y=lat, z=time), colour = 'blue', size = 0.1)+
  geom_point(data = begin, aes(x=lon,y=lat, z=time), colour = 'green', size = 1)+
  geom_point(data = latest, aes(x=lon,y=lat, z=time), colour = 'red', size = 1)+
  theme_bw()+
  coord_quickmap()

ggplotly(gg)


# with ggmaps -------------------------------------------------------------

mp = get_map(location = c(lon = mean(c(xmin,xmax)), lat = mean(c(ymin,ymax))), source="stamen", color="bw", maptype="toner", zoom = 7)

ggmap(mp)

# construct ggplot
g = ggmap(mp)+
  geom_contour(data=b.df, aes(x=x, y=y,z=z), breaks=c(-50,-100,-150,-200,-500),colour="darkgrey", size=0.1)+
  xlim(xmin,xmax)+
  ylim(ymin,ymax)+
  labs(x = 'Longitude', y = 'Latitude')+
  geom_point(data = ctd, aes(x=lon,y=lat, z=time), colour = 'blue', size = 0.1)+
  geom_point(data = begin, aes(x=lon,y=lat, z=time), colour = 'green', size = 1)+
  geom_point(data = latest, aes(x=lon,y=lat, z=time), colour = 'red', size = 1)+
  theme_bw()
g
# make interactive plot
ggplotly(g)



# basic map ---------------------------------------------------------------

# plotting dimensions
mlon = median(ctd$lon)
mlat = median(ctd$lat)
span = 4 * 111 * diff(range(ctd$lat))

# plot coastline
plot(coastlineWorldFine, clon = mlon, clat = mlat, span = span)

# plot bathymetry
contour(bathyLon,bathyLat,bathyZ,levels = c(-100, -200), lwd = c(2, 2), lty = c(3, 1), drawlabels = TRUE,add = TRUE,col = 'darkgray')

# add bathy legend
legend("bottomright", lwd=c(2,2), lty=c(3,1), col='darkgray', seg.len=3,
       title="Depth [m]", legend=c("100", "200"), bg="white")

# glider track
lines(ctd$lon, ctd$lat, col = 'blue')

# plot timestamp each day
date = seq(trunc(min(ctd$time), 'day'), max(ctd$time), by = 'day')

# determine day labels
labels = data.frame()
for(i in seq_along(date)){
  d = ctd[which.min(abs(ctd$time - date[i])),]
  labels = rbind(d, labels)
}

# day labels
daylab = labels[1:(nrow(labels)-1),]
points(daylab$lon, daylab$lat, pch = 4)
text(daylab$lon, daylab$lat, labels = format(round(daylab$time, 'days'), '%d-%b'), pos = 4)

# begin and end
begin = ctd[1,]
points(begin$lon, begin$lat, pch = 17, col = 'darkgreen')
text(begin$lon, begin$lat, labels = begin$time, pos = 4, col = 'darkgreen')

latest = ctd[nrow(ctd),]
points(latest$lon, latest$lat, pch = 20, col = 'darkred')
text(latest$lon, latest$lat, labels = latest$time, pos = 4, col = 'darkred')

# add glider legend
legend("bottomleft", lty=c(1,NA,NA), pch = c(NA, 17, 20), col = c('blue', 'darkgreen', 'darkred'), c('Track', 'Start', 'Latest'), title = 'Glider')




