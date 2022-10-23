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
library(tools)

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

# Read in all players, current lines, current fantasy teams
playernames <<- read.csv("Data/PlayerNames.csv")
colnames(playernames)[2] = 'HR.Position'
playernames$Name = toTitleCase(tolower(playernames$Name))

playernamesyahoo <<- read.csv("Data/PlayerNamesYahoo.csv")
colnames(playernamesyahoo) = c("Name","Y.Position1","Y.Position2","Y.Position3")
playernamesyahoo$Name = make.unique(playernamesyahoo$Name)
playernamesyahoo$Name = gsub('.1', ' (2)', playernamesyahoo$Name)
playernamesyahoo$Name = toTitleCase(tolower(playernamesyahoo$Name))
playernames = left_join(playernames,playernamesyahoo,by='Name')
playernames$Position = playernames$Y.Position1
playernames$Position[is.na(playernames$Position)] = playernames$HR.Position[is.na(playernames$Position)]

playerlines <<- read.csv("Data/PlayerLines.csv")
playerlines$Player = toTitleCase(tolower(playerlines$Player))
playerlines$Linemate1 = toTitleCase(tolower(playerlines$Linemate1))
playerlines$Linemate2 = toTitleCase(tolower(playerlines$Linemate2))

leaguerosters <<- read.csv("Data/TeamsYahoo.csv")
leaguerosters <<- leaguerosters[leaguerosters$selected_position_position != 'IR',]
leaguerosters$player_name_full = toTitleCase(tolower(leaguerosters$player_name_full))


# Get current season
currentSeason <<- as.integer(lubridate::quarter(Sys.Date(), with_year = TRUE, fiscal_start = 10))

# Launch App
runApp(launch.browser = T)