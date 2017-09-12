live_glider
================
### Overview

The aim of this work is to create a tool to visualize sensor output (ocean hydrography and baleen whale acoustic detections) from Slocum gliders in the field.

### Description of Contents

`app.R` - Shiny application  
`get_glider_data.sh` - bash script for downloading and processing (with `proc_glider_data.R`) glider data (detections, tracks, and ctd). This must be run before the app will operate.  
`proc_glider_data.R` - script to process all downloaded glider data  
`helper.R` - helpful functions for the app  

### Demonstration

Currently live (but still in testing) [here](http://leviathan.ocean.dal.ca/live_glider/)
