library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
#library(fresh)
library(stringr)
library(plotly)
library(dplyr)
library(DT)
library(reactable)
library(stringr)
library(lubridate)
library(scales)

# Setup extended js code to allow dynamic collapsing of boxes
collapsejs = "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

scrollbarjs = '
$(document).ready(function(){
  $("[id^=sw-content-]").on("shown", function(){
    $(".sidebar").css({"overflow-y": "visible"});
  }).on("hidden", function(){
    $(".sidebar").css({"overflow-y": "auto"});
  });
});
'

# Function to check if box is expanded
collapseInput = function(inputId, boxId) {
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.setInputValue('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.setInputValue('%s', false);})",
      boxId, inputId
    )
  )
}

# Custom theme for styling
# mytheme <- create_theme(
#   bs4dash_vars(
#     navbar_light_color = "#bec5cb",
#     navbar_light_active_color = "#FFF",
#     navbar_light_hover_color = "#FFF"
#   ),
#   bs4dash_yiq(
#     contrasted_threshold = 10,
#     text_dark = "#FFF", 
#     text_light = "#272c30"
#   ),
#   bs4dash_layout(
#     main_bg = "#353c42"
#   ),
#   bs4dash_sidebar_light(
#     bg = "#272c30", 
#     color = "#bec5cb",
#     hover_color = "#FFF",
#     submenu_bg = "#272c30", 
#     submenu_color = "#FFF", 
#     submenu_hover_color = "#FFF"
#   ),
#   bs4dash_status(
#     primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
#   ),
#   bs4dash_color(
#     gray_900 = "#FFF"
#   )
# )



# Launch App
runApp(launch.browser = T)