# process all glider data in a given directory

# define useful functions -------------------------------------------------

proc_glider_ctd = function(glider_dir){
  # process glider ctd data
  
  # required libraries
  library(oce, quietly = T, warn.conflicts = F)
  
  # find ctd file
  ctd_file = list.files(path = glider_dir, pattern = '*_sci_water_live.csv', full.names = T)
  
  # read ctd file
  ctd = read.csv(ctd_file)
  
  # make timestamps intelligable (convert from unixtime)
  ctd$time = as.POSIXct(ctd$unixtime, tz ="America/Halifax", origin = "1970-01-01 00:00:00")
  
  # make date timestamp
  ctd$date = as.Date(ctd$time)
  
  # convert pressure from dbar to bar
  ctd$pressure = ctd$sci_water_pressure*10
  
  # calculate depth
  ctd$depth = swDepth(ctd$pressure, latitude = ctd$lat)
  
  # rename temperature
  ctd$temperature = ctd$sci_water_temp
  
  # calculate salinity
  ctd$salinity = swSCTp(conductivity = ctd$sci_water_cond, 
                        temperature = ctd$sci_water_temp,
                        pressure = ctd$sci_water_pressure,
                        conductivityUnit = "S/m")
  
  # calculate density
  ctd$density = swRho(salinity = ctd$salinity, 
                      temperature = ctd$sci_water_temp, 
                      pressure = ctd$sci_water_pressure)
  
  saveRDS(ctd, file = paste0(glider_dir,'/ctd.rds'))
}

proc_glider_kml = function(glider_dir){
  # quietly load library
  suppressMessages(library(rgdal))
  
  # find gps file
  kml_file = list.files(path = glider_dir, pattern = '*.kml', full.names = T)
  
  # list layers of kml
  lyrs = ogrListLayers(kml_file)
  
  # surfacings --------------------------------------------------------------
  
  # extract glider surfacings
  surf = readOGR(kml_file, layer = lyrs[grep('Surfacings', lyrs)], verbose = F)
  
  # re-arrange surfacings in data frame
  surf = cbind.data.frame(surf@coords[,c(2,1)], as.character(surf$Name))
  colnames(surf) = c('lat', 'lon', 'time')
  surf$time = as.character(surf$time)
  
  # convert timestamp of latest surfacing
  surf$time[nrow(surf)] = strsplit(x = as.character(surf$time[nrow(surf)]), split = '[()]')[[1]][2]
  
  # fix date
  surf$time = as.POSIXct(surf$time, format = '%m-%d %H:%M')
  surf$date = as.Date(surf$time)
  
  # save
  saveRDS(surf, file = paste0(glider_dir,'/surfacings.rds'))
  
  # waypoints ---------------------------------------------------------------
  
  # extract glider waypoints
  wpts = readOGR(kml_file, layer = lyrs[grep('Waypoints', lyrs)], verbose = F)
  
  # re-arrange surfacings in data frame
  wpts = cbind.data.frame(wpts@coords[,c(2,1)], as.character(wpts$Name))
  colnames(wpts) = c('lat', 'lon', 'name')
  
  # isolate waypoint IDs
  wpts$name = grep(as.character(wpts$name), pattern = '(*)')
  
  # save
  saveRDS(wpts, file = paste0(glider_dir,'/waypoints.rds'))
}

proc_glider_detections = function(glider_dir){
  # process glider detection data
  
  # read in detection data
  manual_analysis_file = list.files(path = glider_dir, pattern = 'manual_analysis.csv', full.names = T)
  
  # read in detection data
  detections = read.csv(manual_analysis_file)
  
  # remove unneeded columns
  detections$analyst = NULL
  detections$notes = NULL
  
  # fix time
  detections$datetime_utc = as.character(detections$datetime_utc)
  detections$time = as.POSIXct(detections$datetime_utc,format = '%Y%m%d%H%M%S',tz = 'UTC')
  
  # convert to local timezone
  detections$time = format(detections$time, tz="America/Halifax",usetz=TRUE)
  detections$date = as.Date(detections$time)
  
  saveRDS(detections, file = paste0(glider_dir,'/detections.rds'))
}

proc_glider_data = function(glider_dir){
  # run processing functions
  
  # process data 
  proc_glider_ctd(glider_dir)
  proc_glider_kml(glider_dir)
  proc_glider_detections(glider_dir)
  
}

# process glider data -----------------------------------------------------

# find glider directories
glider_dir_list = dir(pattern = 'data-', recursive = F)

# process data
for(i in seq_along(glider_dir_list)){
  proc_glider_data(glider_dir_list[i])
}