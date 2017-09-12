get_mapInfo = function(species, reactive = T){
  # extract all necessary info for plotting a given species 
  
  # setup -------------------------------------------------------------------
  
  # find index of target species
  ind = which(colnames(detections) == species)
  
  # initiate list
  spp = list()
  
  # presence subsets --------------------------------------------------------
  
  # subset of present
  spp[['present']] = subset(detections, detections[,ind] == 'present')
  
  # subset of maybe
  spp[['maybe']] = subset(detections, detections[,ind] == 'maybe')
  
  # make plotting group labels ----------------------------------------------
  
  # present group label
  spp[['present_grp']] = paste0('Definite ', species, ' whale detections [latest: ', format(max(spp[['present']]$date, na.rm = T), '%d-%b'), '; n = ', length(spp[['present']]$date),']')
  
  # maybe group label
  spp[['maybe_grp']] = paste0('Possible ', species, ' whale detections [latest: ', format(max(spp[['maybe']]$date, na.rm = T), '%d-%b'), '; n = ', length(spp[['maybe']]$date),']')
  
  return(spp)
}

# # testing
# rw = get_mapInfo('right', F)


