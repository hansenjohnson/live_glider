#########################
# Noise from glider CTD #
#########################
# Hansen Johnson
# 04 July 2016

# Purpose: plot up interactive live data CTD plots and compare to noise in real-time pitch tracks
# to determine if spurious tracks were caused by the CTD pump


# Set up environment ------------------------------------------------------

rm(list = ls())
# load in various required packages
library(data.table, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(plotly, quietly = T, warn.conflicts = F)
library(oce, quietly = T, warn.conflicts = F)
library(cowplot, quietly = T, warn.conflicts = F)
library(plyr, quietly = T, warn.conflicts = F)
library(zoo, quietly = T, warn.conflicts = F)

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

##### make pretty plots with ggplot #####

temp = ggplot()+
  geom_point(data = ctd, aes(x = time, y = sci_water_pressure, colour = sci_water_temp), na.rm = TRUE)+
  scale_colour_gradientn(colours = oce.colorsTemperature(200)) +
  scale_y_reverse() +
  theme_bw()

con = ggplot()+
  geom_point(data = ctd, aes(x = time, y = sci_water_pressure, colour = sci_water_cond), na.rm = TRUE)+
  scale_colour_gradientn(colours = oce.colorsSalinity(200)) +
  scale_y_reverse() +
  theme_bw()

sal = ggplot()+
  geom_point(data = ctd, aes(x = time, y = sci_water_pressure, colour = salinity), na.rm = TRUE)+
  scale_colour_gradientn(colours = oce.colorsSalinity(200)) +
  scale_y_reverse() +
  theme_bw()

dens = ggplot()+
  geom_point(data = ctd, aes(x = time, y = sci_water_pressure, colour = density), na.rm = TRUE)+
  scale_colour_gradientn(colours = oce.colorsDensity(200)) +
  scale_y_reverse() +
  theme_bw()

all = plot_grid(temp, sal, dens, ncol = 1, nrow = 3, align="v")
all

# interactive plot
ggplotly(temp)
ggplotly(con)
ggplotly(sal)
ggplotly(dens)
