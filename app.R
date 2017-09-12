# simple app for testing reactive data input
library(leaflet)
library(shiny)
library(htmltools)
library(htmlwidgets)
library(oce)
source('helper.R')

# setup -------------------------------------------------------------------

# make list of glider deployments
glider_list = dir(path = './', pattern = 'data-')

# isolate glider name
glider_list = gsub(glider_list, pattern = 'data-', replacement = '')

# choices
# spp_choices = c('present', 'maybe')
spp_choices = c('right', 'fin', 'sei', 'humpback')

col_choices = c('red', 'lightgrey', 'darkgrey', 'tan', 'white', 'brown', 'yellow', 'lightblue', 'cyan', 'lightgreen', 'darkred', 'purple','green', 'black')

# ui ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Live Glider Data"),
  
  sidebarLayout(
    sidebarPanel(
      
      # choose glider
      selectInput("glider", "Choose glider:", choices = glider_list, 
                  selected = glider_list[2], multiple = FALSE),
      
      # choose time (reacts to glider choice)
      uiOutput("sliderDate"),
      
      # choose plotting variable
      selectInput("section_var", "Choose section to plot:", 
                  choices = c('temperature', 'salinity', 'density'), 
                  selected = 'temperature', multiple = FALSE),
      
      # choose colorbar limits
      uiOutput("sliderVar"),
      
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
                      selected = col_choices[8], multiple = F)),br()
    ),
    
    mainPanel(
      textOutput("glider"),
      leafletOutput("gliderMap"),
      plotOutput("ctdPlot")
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output) {
  
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
  surf_grp = 'Glider surfacings'
  
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
      
      # add glider surfacings
      addCircleMarkers(data = SURF(), ~lon, ~lat, radius = 6, fillOpacity = .2, stroke = F,
                       popup = ~paste(sep = "<br/>",
                                      "Glider surfacing",
                                      as.character(time),
                                      paste0(as.character(lat), ', ', as.character(lon))),
                       label = ~paste0('Glider surfacing: ', as.character(time)), group = surf_grp) %>%
      
      # add glider waypoints
      addCircleMarkers(data = Wpts(), ~lon, ~lat, radius = 6, fillOpacity = .2, stroke = F, color = 'orange',
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
  
  # CTD data -----------------------------------------------------------
  
  # build variable slider
  output$sliderVar <- renderUI({
    
    # switch for variable
    if(input$section_var == 'temperature'){
      rng = c(-5,30)
      value = c(-1,25)
    } else if(input$section_var == 'salinity'){
      rng = c(15,45)
      value = c(22,35)
    } else if(input$section_var == 'density'){
      rng = c(1010,1035)
      value = c(1017,1028)
    }
    
    # define slider bar
    sliderInput("section_limits", "Choose colorbar limits:", rng[1], rng[2],
                value = value, animate = F)
  })
  
  output$ctdPlot = renderPlot({
    
    CTD <- reactive({
      
      # subset of CTD data by date
      ctd = subset(Ctd(), Ctd()$date >= input$range[1] & Ctd()$date <= input$range[2])
      
      # threshold number of points to being subsetting
      npoints = 10000
      
      # plot a subset of data if there are too many points 
      if(nrow(ctd)>npoints){
        norig = nrow(ctd) # original number of points
        nsub = round(nrow(ctd)/npoints,0) # every n number of samples to plot
        ctd[seq(1, nrow(ctd), nsub),] # subset to plot every other data point
      } else {
        ctd
      }

    })  
    
    # plot data
    plot_section(CTD(), input$section_var, input$section_limits)
  })
  
}

# run app -----------------------------------------------------------
shinyApp(ui, server)
