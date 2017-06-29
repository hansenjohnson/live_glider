# plot static section and map (both long term and every 3 days of a deployment)

ctd.bk = ctd

# Set up environment ------------------------------------------------------

# rm(list = ls())
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

# basic section -----------------------------------------------------------



