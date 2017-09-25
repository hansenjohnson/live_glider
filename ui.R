# ui.R

# simple app for testing reactive data input
library(leaflet)
library(shiny)
library(htmltools)
library(htmlwidgets)
library(oce)
library(shinydashboard)
library(shinyjqui)

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

header <- dashboardHeader(
  title = "Live Glider"
)

# body --------------------------------------------------------------------

body <- dashboardBody(
  fluidRow(
    
    # sidebar --------------------------------------------------------------------
    jqui_sortabled(column(width = 3,
                          
                          # deployment ---------------------------------------------------------
                          box(width = NULL, status = "warning", 
                              title = 'Deployment input', collapsible = T, collapsed = F,
                              
                              # choose glider
                              selectInput("glider", "Choose glider:", choices = glider_list, 
                                          selected = glider_list[1], multiple = FALSE),
                              
                              # slide bar
                              uiOutput("sliderDate")
                              
                              # # add button to zoom
                              # div(style="display: inline-block;vertical-align:top; width: 120px;",
                              #     actionButton("back", "Back"), actionButton("forward", "Forward"))
                              
                          ),
                          
                          # section controls ------------------------------------------------------
                          box(width = NULL, status = "warning", 
                              title = 'Section input', collapsible = T, collapsed = T,
                              
                              # choose plotting variable
                              selectInput("section_var", "Choose section to plot:", 
                                          choices = c('temperature', 'salinity', 'density'), 
                                          selected = 'temperature', multiple = FALSE),
                              
                              # # choose number of points to plot
                              numericInput("npoints", label = 'Choose number of points to plot:', 
                                           value = 20000, min = 1, max = 100000000, step = 100),
                              
                              uiOutput("sliderNpoints"),
                              
                              # show how many points are plotted
                              helpText(htmlOutput("npoints_text")),
                              
                              # choose if plots autoscale or not
                              h5(strong('Choose color scale:')),
                              checkboxInput("autoscale", "Autoscale?", value = T),
                              
                              # choose limits
                              uiOutput("sliderVar")
                              
                          ),
                          
                          # whale -----------------------------------------------------------------
                          box(width = NULL, status = "warning",
                              title = 'Species input', collapsible = T, collapsed = T,
                              
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
                                              selected = col_choices[8], multiple = F))
                          )
    )),
    
    # main display --------------------------------------------------------------------
    jqui_sortabled(column(width = 9,
                          
                          
                          # # time----------------------------------------------------------------
                          # box(width = NULL, solidHeader = TRUE, status = 'primary',
                          #     title = 'Time', collapsible = T, collapsed = F,
                          #     
                          #     # slide bar
                          #     uiOutput("sliderDate")
                          # ),
                          
                          
                          # map -----------------------------------------------------------------
                          box(width = NULL, solidHeader = TRUE, status = 'primary',
                              title = 'Map', collapsible = T, collapsed = F,
                              
                              leafletOutput("gliderMap", width = "100%", height = 500)
                          ),
                          
                          # section -----------------------------------------------------------------
                          box(width = NULL, solidHeader = TRUE, status = 'primary',
                              title = 'Section', collapsible = T, collapsed = F,
                              
                              plotOutput("ctdPlot", height = 300)
                          )
    ))
  )
)


# construct ui ----------------------------------------------------------

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)