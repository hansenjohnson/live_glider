live_glider
================
### Overview

The aim of this work is to create a tool to visualize sensor output (ocean hydrography and baleen whale acoustic detections) from Slocum gliders in the field.

### Description of Contents

`app.R` - Shiny application  
`helper.R` - helper script containing functions called/required by the app 
`get_glider_data.sh` - bash script for downloading and processing (with `proc_glider_data.R`) glider data (detections, tracks, and ctd). This must be run before the app will operate.  
`proc_glider_data.R` - script to process all downloaded glider data  

### Demonstration

Currently live (but still in testing) [here](http://leviathan.ocean.dal.ca/live_glider/)

### To Do

* Convert UI to shinydashboard (prettier, more adjustable, and more adaptable to different viewing platforms)  
* Overlay selected isobaths  
* Explore possibility of overlaying satellite layers (e.g. Chl a, SST, etc)
* Add whale detections to section
* Add some ability to select point on map and see exact point on section. My first thought here is to add a slider bar to pick a time which moves 1) a marker on the map (e.g. glider) and 2) a vertical line on the section. To do this I suspect I will need to re-arrange the code to create a reactive CTD product before mapping. I think it's possible to do this because the CTD data also have lat/lon position. Doing it this way allows the section and map 'locator' to share the same timesteps.
* Adjust playback speed for the date slider animation (it's currently set at 1 day, and very slow)  
* Add option for interactive plotting of the CTD section (i.e. plotly). I'm curious to see how slow this will be compared to base graphics...  
* Add links to the deployment page on Mark's website and OTN  
* Look into adding links to specific pitch track records for each detection  
* Add some diagnostics (i.e. latest ctd data, latest kml data, latest detections, ...)  
* Play around with clustering options for species detections  
