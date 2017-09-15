# simple app for testing reactive data input
library(leaflet)
library(shiny)
library(htmltools)
library(htmlwidgets)
library(oce)

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

# ui ----------------------------------------------------------------------

ui <- fluidPage(
  
  # import styles from index
  # includeCSS("../server_index/css/style.css"),

  titlePanel("Live Glider Data"),
  
  # sidebar -----------------------------------------------------------------
  
  sidebarLayout(
    sidebarPanel(
      
      # glider -----------------------------------------------------------------
      
      # choose glider
      selectInput("glider", "Choose glider:", choices = glider_list, 
                  selected = glider_list[2], multiple = FALSE),
      
      # choose time (reacts to glider choice)
      uiOutput("sliderDate"),
      
      # ctd -----------------------------------------------------------------
      
      # choose plotting variable
      selectInput("section_var", "Choose section to plot:", 
                  choices = c('temperature', 'salinity', 'density'), 
                  selected = 'temperature', multiple = FALSE),
      
      # # choose number of points to plot
      numericInput("npoints", label = 'Choose subsample to plot:', value = 10000, min = 1, max = 100000000, step = 100),
      
      uiOutput("sliderNpoints"),
      
      # show how many points are plotted
      helpText(textOutput("npoints_text")),
      
      # choose if plots autoscale or not
      checkboxInput("autoscale", "Autoscale?", value = T),
      
      # choose limits
      uiOutput("sliderVar"),
      
      # whale -----------------------------------------------------------------
      
      # right
      tags$h5(strong("Right whales:")),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("rw_present", 'present', value = T)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("rw_present_col", NULL, choices = col_choices, 
                      selected = col_choices[1], multiple = F)),br(),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("rw_maybe", 'maybe', value = F)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("rw_maybe_col", NULL, choices = col_choices, 
                      selected = col_choices[2], multiple = F)),br(),
      
      # sei
      tags$h5(strong("Sei whales:")),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("sw_present", 'present', value = T)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("sw_present_col", NULL, choices = col_choices, 
                      selected = col_choices[5], multiple = F)),br(),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("sw_maybe", 'maybe', value = F)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("sw_maybe_col", NULL, choices = col_choices, 
                      selected = col_choices[6], multiple = F)),br(),
      
      # fin
      tags$h5(strong("Fin whales:")),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("fw_present", 'present', value = T)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("fw_present_col", NULL, choices = col_choices, 
                      selected = col_choices[3], multiple = F)),br(),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("fw_maybe", 'maybe', value = F)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("fw_maybe_col", NULL, choices = col_choices, 
                      selected = col_choices[4], multiple = F)),br(),
      
      # humpback
      tags$h5(strong("Humpback whales:")),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("hw_present", 'present', value = T)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("hw_present_col", NULL, choices = col_choices, 
                      selected = col_choices[7], multiple = F)),br(),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          checkboxInput("hw_maybe", 'maybe', value = F)),
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("hw_maybe_col", NULL, choices = col_choices, 
                      selected = col_choices[8], multiple = F)),br(),
      
      # set panel width
      width = 3),
    
    # main panel ---------------------------------------------------------------
    
    mainPanel(
      textOutput("glider"),
      leafletOutput("gliderMap", width = "100%", height = 400),
      plotOutput("ctdPlot", height = 300)
      )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output) {
  
  # read help fxns-----------------------------------------------------------
  
  source('helper.R')
  
  # choose glider -----------------------------------------------------------
  
  output$glider = renderText({ 
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
    
    leaflet(Surf()) %>% 
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
      addPolylines(data = SURF(), ~lon, ~lat, weight = 2, group = track_grp) %>%
      
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
  
  output$npoints_text = renderText({ 
    paste0('Plotting every ', round(nrow(CTD())/nrow(CTD_sub()),0), ' point(s) (', nrow(CTD_sub()), ' out of ', nrow(CTD()), ')')
  })
  
  # ctd limits -----------------------------------------------------------
  
  # build variable slider
  output$sliderVar <- renderUI({
    
    # switch for variable
    if(input$section_var == 'temperature'){
      rng = c(-5,30)
      if(input$autoscale){
        value = c(floor(min(CTD_sub()$temperature, na.rm = T)),
                  ceiling(max(CTD_sub()$temperature, na.rm = T)))
      } else {
        value = c(-1, 20)
      }
    } else if(input$section_var == 'salinity'){
      rng = c(15,45)
      if(input$autoscale){
        value = c(floor(min(CTD_sub()$salinity, na.rm = T)),
                  ceiling(max(CTD_sub()$salinity, na.rm = T)))
      } else {
        value = c(22, 35)
      }
    } else if(input$section_var == 'density'){
      rng = c(1010,1035)
      if(input$autoscale){
        value = c(floor(min(CTD_sub()$density, na.rm = T)),
                  ceiling(max(CTD_sub()$density, na.rm = T)))
      } else {
        value = c(1017, 1028)
      }
    }
    
    # define slider bar
    sliderInput("section_limits", "Choose colorbar limits:", min = rng[1], max = rng[2], value = value, animate = F)
  }) # CTD VAR SLIDER
  
  # ctd section -----------------------------------------------------------
  
  # plot
  output$ctdPlot = renderPlot({
    plot_section(ctd = CTD_sub(), var = input$section_var, zlim = input$section_limits)
  })

} # SERVER

# run app -----------------------------------------------------------
shinyApp(ui, server)
