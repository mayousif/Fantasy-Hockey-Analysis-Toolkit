library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(stringr)
library(plotly)
library(dplyr)
library(DT)
library(reactable)
library(stringr)
library(lubridate)
library(scales)
library(data.table)

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

# Launch App
runApp(launch.browser = T)