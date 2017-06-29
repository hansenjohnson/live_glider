#########################
# Noise from glider CTD #
#########################
# Hansen Johnson
# 04 July 2016

# Purpose: plot up interactive live data CTD plots and compare to noise in real-time pitch tracks
# to determine if spurious tracks were caused by the CTD pump

rm(list = ls())
# load in various required packages
library(data.table)
library(ggplot2)
library(plotly)
library(oce)
library(cowplot)
  
depth_file = "http://gliders.oceantrack.org/data/live/bond_sci_water_live.csv"
ctd = fread(depth)
  
# make timestamps intelligable (convert from unixtime)
ctd$time <- as.POSIXct(ctd$unixtime, 
                       tz ="America/Halifax", # note this is now converted to local time
                       origin = "1970-01-01 00:00:00")
  
# subset to remove erronious data
ctd = subset(ctd, sci_water_temp>0 & sci_water_temp<30)
  
# change direction of depth vector ("positive up" convention)
ctd$depth = -ctd$depth
  
# rename temp
ctd$temp = ctd$sci_water_temp
  
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
    geom_point(data = ctd, aes(x = time, y = depth, colour = temp), na.rm = TRUE)+
    scale_colour_gradientn(colours = oce.colorsJet(200)) +
    theme_bw()
  
sal = ggplot()+
    geom_point(data = ctd, aes(x = time, y = depth, colour = salinity), na.rm = TRUE)+
    scale_colour_gradientn(colours = oce.colorsJet(200)) +
    theme_bw()
  
dens = ggplot()+
    geom_point(data = ctd, aes(x = time, y = depth, colour = density), na.rm = TRUE)+
    scale_colour_gradientn(colours = oce.colorsJet(200)) +
    theme_bw()

all = plot_grid(temp, sal, dens, ncol = 1, nrow = 3, align="v")
all

# interactive plot
ggplotly(dens)
