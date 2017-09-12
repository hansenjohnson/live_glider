# ui ----------------------------------------------------------------------

verbatimTextOutput("Click_text", placeholder = T)

# click observer -----------------------------------------------------------
observe({
  click<-input$gliderMap_click
  text<-paste0(click$lat, click$lng)
  output$Click_text<-renderText({
    text
  })
})