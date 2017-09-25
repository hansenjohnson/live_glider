# server.R

# simple app for testing reactive data input
library(leaflet)
library(shiny)
library(htmltools)
library(htmlwidgets)
library(oce)
library(shinydashboard)

# setup -------------------------------------------------------------------

# make list of glider deployments
glider_list = dir(path = './', pattern = 'data-')

# isolate glider name
glider_list = gsub(glider_list, pattern = 'data-', replacement = '')

# species choices
spp_choices = c('right', 'fin', 'sei', 'humpback')

# color choices
col_choices = c('red', 'lightgrey', 'darkgrey', 'tan', 'white', 'brown', 'yellow', 'lightblue', 'cyan', 'lightgreen', 'darkred', 'purple','green', 'black')

# glider icon
gliderIcon = makeIcon("icons/slocum.png", iconWidth = 50, iconHeight = 50)


# section setup -----------------------------------------------------------

# setup
lab_top = 180
rw_pos = 190
fw_pos = 200
sw_pos = 210
hw_pos = 220
lab_pos = c(rw_pos,fw_pos,sw_pos,hw_pos)
spp = c('right', 'fin', 'sei', 'humpback')

# setup layout for plotting
m = rbind(c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
          c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
          c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
          c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
          c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
          c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2),
          c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3),
          c(4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3))

# plot text size
cex.all=1.5

# server ------------------------------------------------------------------

function(input, output, session) {
  
  # read help fxns-----------------------------------------------------------
  
  source('helper.R')
  
  # choose glider -----------------------------------------------------------
  
  output$glider1 = renderText({ 
    paste("Glider: ", input$glider)
  })
  
  output$glider2 = renderText({ 
    paste("Glider: ", input$glider)
  })
  
  # deployment data -----------------------------------------------------------
  
  # surfacings
  Surf = reactive({
    readRDS(paste0('data-', input$glider,'/surfacings.rds'))
  })
  
  # waypoints
  Wpts = reactive({
    readRDS(paste0('data-', input$glider,'/waypoints.rds'))
  })
  
  # detections
  Det = reactive({
    readRDS(paste0('data-', input$glider,'/detections.rds'))
  })
  
  # ctd
  Ctd = reactive({
    readRDS(paste0('data-', input$glider,'/ctd.rds'))
  })
  
  # date slider -----------------------------------------------------------
  
  # build glider-specific date slider
  output$sliderDate <- renderUI({
    
    # define start and end dates
    begin = min(Surf()$date)
    end = max(Surf()$date)
    
    # define slider bar
    sliderInput("range", "Choose time range:", begin, end,
                value = c(begin, end), animate = T)
  })
  
  # change date -------------------------------------------------------
  
  # observeEvent(input$forward,{
  #   val <- input$range
  #   updateSliderInput(session, "range", value = c(val[1], val[2]+1))
  # })
  # 
  # observeEvent(input$back,{
  #   val <- input$range
  #   updateSliderInput(session, "range", value = c(val[1], val[2]-1))
  # })
  
  # reactive data -----------------------------------------------------------
  
  # surfacings
  SURF <- reactive({
    subset(Surf(), Surf()$date >= input$range[1] & Surf()$date <= input$range[2])
  })
  
  # latest surfacing
  NOW <- reactive({
    SURF()[nrow(SURF()),]
  })
  
  # detections
  DET <- reactive({
    Det()[Det()$date >= input$range[1] & Det()$date <= input$range[2],]
  })
  
  # RIGHT #
  
  rw_present <- reactive({
    DET()[DET()$right == 'present',]
  })
  
  rw_maybe <- reactive({
    DET()[DET()$right == 'maybe',]
  })
  
  # FIN #
  
  fw_present <- reactive({
    DET()[DET()$fin == 'present',]
  })
  
  fw_maybe <- reactive({
    DET()[DET()$fin == 'maybe',]
  })
  
  # SEI #
  
  sw_present <- reactive({
    DET()[DET()$sei == 'present',]
  })
  
  sw_maybe <- reactive({
    DET()[DET()$sei == 'maybe',]
  })
  
  # HUMPBACK #
  
  hw_present <- reactive({
    DET()[DET()$humpback == 'present',]
  })
  
  hw_maybe <- reactive({
    DET()[DET()$humpback == 'maybe',]
  })
  
  # map groups -------------------------------------------------------
  
  # glider tracks
  track_grp = 'Glider track'
  
  # glider surfacings
  now_grp = 'Latest surfacing'
  
  # glider surfacings
  surf_grp = 'All surfacings'
  
  # glider wpts
  wpt_grp = "Glider waypoints"
  
  # basemap -----------------------------------------------------------
  
  output$gliderMap <- renderLeaflet({
    
    leaflet(Det()) %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      fitBounds(~max(lon, na.rm = T), ~min(lat, na.rm = T), ~min(lon, na.rm = T), ~max(lat, na.rm = T)) %>%
      
      # use NOAA graticules
      addWMSTiles(
        "https://maps.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
        layers = c("1-degree grid", "5-degree grid"),
        options = WMSTileOptions(format = "image/png8", transparent = TRUE),
        attribution = "NOAA") %>%
      
      # add extra map features
      addScaleBar(position = 'topright')%>%
      addMeasure(primaryLengthUnit = "kilometers",secondaryLengthUnit = 'miles', primaryAreaUnit = "hectares",secondaryAreaUnit="acres", position = 'bottomleft') %>%
      
      # add layer control panel
      addLayersControl(
        overlayGroups = c(track_grp,
                          now_grp,
                          surf_grp,
                          wpt_grp),
        options = layersControlOptions(collapsed = FALSE), position = 'bottomright') %>%
      
      # hide some groups by default
      hideGroup(c(surf_grp, wpt_grp))
    
  })
  
  # glider observer -----------------------------------------------------------
  observe(priority = 5,{
    
    proxy = leafletProxy("gliderMap") 
    proxy %>%
      clearMarkers() %>%
      clearShapes() %>%
      
      # add glider track
      addPolylines(data = SURF(), ~lon, ~lat, weight = 2, group = track_grp, smoothFactor = 2) %>%
      
      # add latest surfacing
      addMarkers(data = NOW(), ~lon, ~lat, icon = gliderIcon,
                 popup = ~paste(sep = "<br/>",
                                paste0('Latest surfacing: ', input$glider),
                                paste0('Time: ', as.character(time)),
                                paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                 label = ~paste0('Latest surfacing: ', as.character(time)), group = now_grp) %>%
      
      # add all surfacings
      addCircleMarkers(data = SURF(), ~lon, ~lat, radius = 6, fillOpacity = .2, stroke = F,
                       popup = ~paste(sep = "<br/>",
                                      "Glider surfacing",
                                      as.character(time),
                                      paste0(as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Glider surfacing: ', as.character(time)), group = surf_grp) %>%
      
      # add glider waypoints
      addCircleMarkers(data = Wpts(), ~lon, ~lat, radius = 6, stroke = T, weight = 2,
                       fillOpacity = 0.6, color = 'white', fillColor = 'orange', opacity = 1,
                       popup = ~paste(sep = "<br/>",
                                      paste0("Waypoint: ", as.character(name)),
                                      paste0(as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Waypoint: ', as.character(name)), group = wpt_grp)
    
  })
  
  # fin whale observers -----------------------------------------------------------
  
  # present
  observe(priority = 4,{
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = fw_present(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black',weight = 1, fill = T, fillColor = input$fw_present_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: fin whale",
                                      "Score: definite",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Definite fin whale: ', as.character(date)), group = 'fw_present')
    
    ifelse(input$fw_present, showGroup(proxy, 'fw_present'),hideGroup(proxy, 'fw_present'))
  })
  
  # maybe
  observe({
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = fw_maybe(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black',weight = 1, fill = T, fillColor = input$fw_maybe_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: fin whale",
                                      "Score: possible",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Possible fin whale: ', as.character(date)), group = 'fw_maybe')
    
    ifelse(input$fw_maybe, showGroup(proxy, 'fw_maybe'),hideGroup(proxy, 'fw_maybe'))
  })
  
  # humpback whale observers -----------------------------------------------------------
  
  # present
  observe(priority = 3,{
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = hw_present(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black',weight = 1, fill = T, fillColor = input$hw_present_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: humpback whale",
                                      "Score: definite",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Definite humpback whale: ', as.character(date)), group = 'hw_present')
    
    ifelse(input$hw_present, showGroup(proxy, 'hw_present'),hideGroup(proxy, 'hw_present'))
  })
  
  # maybe
  observe({
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = hw_maybe(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black',weight = 1, fill = T, fillColor = input$hw_maybe_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: humpback whale",
                                      "Score: possible",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Possible humpback whale: ', as.character(date)), group = 'hw_maybe')
    
    ifelse(input$hw_maybe, showGroup(proxy, 'hw_maybe'),hideGroup(proxy, 'hw_maybe'))
  })
  
  # sei whale observers -----------------------------------------------------------
  
  # present
  observe(priority = 2,{
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = sw_present(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black',weight = 1, fill = T, fillColor = input$sw_present_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: sei whale",
                                      "Score: definite",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Definite sei whale: ', as.character(date)), group = 'sw_present')
    
    ifelse(input$sw_present, showGroup(proxy, 'sw_present'),hideGroup(proxy, 'sw_present'))
  })
  
  # maybe
  observe({
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = sw_maybe(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black',weight = 1, fill = T, fillColor = input$sw_maybe_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: sei whale",
                                      "Score: possible",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Possible sei whale: ', as.character(date)), group = 'sw_maybe')
    
    ifelse(input$sw_maybe, showGroup(proxy, 'sw_maybe'),hideGroup(proxy, 'sw_maybe'))
  })
  
  # right whale observer -----------------------------------------------------------
  
  # present
  observe(priority = 1,{
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = rw_present(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black', weight = 1, fill = T, fillColor = input$rw_present_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: right whale",
                                      "Score: definite",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Definite right whale: ', as.character(date)), group = 'rw_present')
    
    ifelse(input$rw_present, showGroup(proxy, 'rw_present'),hideGroup(proxy, 'rw_present'))
  })
  
  # maybe
  observe({
    proxy = leafletProxy("gliderMap")
    proxy %>%
      addCircleMarkers(data = rw_maybe(), ~lon, ~lat, radius = 6,
                       fillOpacity = 1, stroke = T, color = 'black',weight = 1, fill = T, fillColor = input$rw_maybe_col,
                       popup = ~paste(sep = "<br/>" ,
                                      "Glider Detection",
                                      "Species: right whale",
                                      "Score: possible",
                                      paste0('Time: ', as.character(time)),
                                      paste0('Position: ', as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Possible right whale: ', as.character(date)), group = 'rw_maybe')
    
    ifelse(input$rw_maybe, showGroup(proxy, 'rw_maybe'),hideGroup(proxy, 'rw_maybe'))
  })
  
  # ctd date ----------------------------------------------------------------
  
  CTD <- reactive({
    # subset of CTD data by date
    ctd = subset(Ctd(), Ctd()$date >= input$range[1] & Ctd()$date <= input$range[2])
  })
  
  # ctd subsample ----------------------------------------------------------
  
  CTD_sub <- reactive({
    # number of points to subset
    npoints = input$npoints
    # plot a subset of data if there are too many points 
    if(nrow(CTD())>npoints){
      norig = nrow(CTD())
      nsub = round(norig/npoints,0)
      CTD()[seq(1, nrow(CTD()), nsub),] # subset to plot every other data point
    } else {
      CTD()
    }
  })
  
  # ctd points -----------------------------------------------------------
  
  output$npoints_text <- renderUI({
    str1 = paste0('Plotting every ', round(nrow(CTD())/nrow(CTD_sub()),0), ' point(s)')
    str2 = paste0('(', nrow(CTD_sub()), ' out of ', nrow(CTD()), ')')
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  # ctd limits -----------------------------------------------------------
  
  # build variable slider
  output$sliderVar <- renderUI({
    if(input$autoscale){return()}
    
    # switch for variable
    if(input$section_var == 'temperature'){
      rng = c(-5,30)
      value = c(floor(min(CTD_sub()$temperature, na.rm = T)),
                ceiling(max(CTD_sub()$temperature, na.rm = T)))
    } else if(input$section_var == 'salinity'){
      rng = c(15,45)
      
      value = c(floor(min(CTD_sub()$salinity, na.rm = T)),
                ceiling(max(CTD_sub()$salinity, na.rm = T)))
    } else if(input$section_var == 'density'){
      rng = c(1010,1035)
      
      value = c(floor(min(CTD_sub()$density, na.rm = T)),
                ceiling(max(CTD_sub()$density, na.rm = T)))
    }
    
    # define slider bar
    sliderInput("section_limits", "Choose colorbar limits:", min = rng[1], max = rng[2], value = value, animate = F)
  }) # CTD VAR SLIDER
  
  # ctd section -----------------------------------------------------------
  
  # plot
  output$ctdPlot = renderPlot({
    
    # rename subset
    ctd = CTD_sub()
    
    # variable-specific definitions
    if(input$section_var == 'temperature'){
      if(input$autoscale){
        zlim = c(floor(min(ctd$temperature, na.rm = T)), ceiling(max(ctd$temperature, na.rm = T)))
      } else {
        zlim = input$section_limits
      }
      
      pal = oce.colorsTemperature()
      lab = 'Temperature [deg C]'
      c = colormap(ctd$temperature, breaks=100, zclip = T, col = pal, zlim = zlim)
      
    } else if(input$section_var == 'salinity'){
      if(input$autoscale){
        zlim = c(floor(min(ctd$salinity, na.rm = T)), ceiling(max(ctd$salinity, na.rm = T)))
      } else {
        zlim = input$section_limits
      }
      
      pal = oce.colorsSalinity()
      lab = 'Salinity'
      c = colormap(ctd$salinity, breaks=100, zclip = T, col = pal, zlim = zlim)
      
    } else if(input$section_var == 'density'){
      if(input$autoscale){
        zlim = c(floor(min(ctd$density, na.rm = T)), ceiling(max(ctd$density, na.rm = T)))
      } else {
        zlim = input$section_limits
      }
      
      pal = oce.colorsDensity()
      lab = 'Density [kg/m3]'
      c = colormap(ctd$density, breaks=100, zclip = T, col = pal, zlim = zlim)
      
    }
    
    # start plot device
    layout(m)
    
    # plot section
    plot(ctd$time, ctd$depth, ylim = c(220,0), type = 'l', col = 'grey', 
         ylab = 'Depth [m]', xlab = '', cex.lab = cex.all, cex.axis = cex.all, yaxt = 'n')
    mtext(paste0(lab),side = 3, line = 1,adj = 0)
    points(ctd$time, ctd$depth, pch = 21, bg = c$zcol, cex = 1.75, col = NULL)
    
    # add axis
    axis(side = 2, at = seq(0, 150, 50), labels = T, cex.axis = cex.all)
    
    # overlay detections
    abline(h = lab_top, col = 'black')
    abline(h = lab_pos, col = 'grey')
    if(input$rw_maybe){add_points(rw_maybe(), rw_pos, input$rw_maybe_col)}
    if(input$rw_present){add_points(rw_present(), rw_pos, input$rw_present_col)}
    if(input$fw_maybe){add_points(fw_maybe(), fw_pos, input$fw_maybe_col)}
    if(input$fw_present){add_points(fw_present(), fw_pos, input$fw_present_col)}
    if(input$sw_maybe){add_points(sw_maybe(), sw_pos, input$sw_maybe_col)}
    if(input$sw_present){add_points(sw_present(), sw_pos, input$sw_present_col)}
    if(input$hw_maybe){add_points(hw_maybe(), hw_pos, input$hw_maybe_col)}
    if(input$hw_present){add_points(hw_present(), hw_pos, input$hw_present_col)}
    
    # add species labels on both sides
    axis(side = 2, at = lab_pos, tick = T, labels = spp, las = 2, cex.axis = cex.all)
    axis(side = 4, at = lab_pos, tick = T, labels = spp, las = 2, cex.axis = cex.all)
    
    # add palette
    drawPalette(c$zlim, col=c$col, breaks=c$breaks, zlab = '', fullpage = T, 
                cex.axis = cex.all, cex.lab = cex.all)
  
  })
  
}