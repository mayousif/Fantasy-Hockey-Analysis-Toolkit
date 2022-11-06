library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyauthr)
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
library(sodium)
library(purrr)
library(httr)
library(YFAR)
library(rjson)
library(reactlog)

enableBookmarking(store="server")

# Current directory of the app
currDir <<- paste0('C:/Users/Meguel/Desktop/nhl/Fantasy-Hockey-Analyzer')
setwd(currDir)

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

# Read in all players, current lines, and nhl team names/abvs
playernames <<- read.csv(paste0(currDir,"/Data/PlayerNames.csv"))
playerlines <<- read.csv(paste0(currDir,"/Data/PlayerLines.csv"))
nhlteams <<- read.csv(paste0(currDir,"/Data/TeamNames.csv"))


# Get current season
currentSeason <<- as.integer(lubridate::quarter(Sys.Date(), with_year = TRUE, fiscal_start = 10))


# Get current season data for skaters/goalies
skaterData <<- read.csv(paste0(currDir,"/Data/allSkaters/",currentSeason,".csv"))
skaterData <<- left_join(skaterData,playernames,"ID")
goalieData <<- read.csv(paste0(currDir,"/Data/allGoalies/",currentSeason,".csv"))
goalieData <<- left_join(goalieData,playernames,"ID")
skaterDataLS <<- read.csv(paste0(currDir,"/Data/allSkaters/",currentSeason-1,".csv"))
skaterDataLS <<- left_join(skaterDataLS,playernames,"ID")
goalieDataLS <<- read.csv(paste0(currDir,"/Data/allGoalies/",currentSeason-1,".csv"))
goalieDataLS <<- left_join(goalieDataLS,playernames,"ID")

allFantasySkaters <<- unique(rbind(skaterData[,c('ID','Name')],skaterDataLS[,c('ID','Name')]))
allFantasyGoalies <<- unique(rbind(goalieData[,c('ID','Name')],goalieDataLS[,c('ID','Name')]))

# Team logos
logos <<- read.csv(paste0(currDir,"/Data/Logos.csv"))

## Sidebar =================================
sidebar = dashboardSidebar(
  width = "15%",
  sidebarMenu(id = "tabs",
              menuItem(
                selected = TRUE,
                "Fantasy Metrics", 
                tabName = "teamstats", 
                icon = icon("people-group")
              ),    
              menuItem(text = "Player Stats",
                       startExpanded = TRUE,
                       icon = icon("person-skating"),
                       menuSubItem(selectizeInput("playerInput", 
                                                  "Select Player",
                                                  choices = NULL,
                                                  selected = NULL),tabName = "playerstats",icon=NULL)
              )
  )
)
## Main Body =================================
body <- dashboardBody(
  #use_theme(mytheme),
  useShinyjs(),
  extendShinyjs(text = collapsejs,functions = c("collapse")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "test.css"),
    tags$style(HTML('.box-header .box-title {display: block;}'),
               HTML('.box-header {padding-top: 0px;
                                  padding-bottom: 0px;}'),
               HTML('.Reactable {padding-top: 10px;}'),
               HTML('.box {background: #f6f8fc}'),
               HTML('.form-group {margin-bottom: 0px}'),
               HTML('.main-sidebar {font-size: 20px;
                                   font-weight: 700;}'),
               HTML("
                input[type=number] {
                      -moz-appearance:textfield;
                }
                input[type=number]::{
                      -moz-appearance:textfield;
                }
                input[type=number]::-webkit-outer-spin-button,
                input[type=number]::-webkit-inner-spin-button {
                      -webkit-appearance: none;
                      margin: 0;}
               "),
               HTML('#login-button {margin-top: 10px}'),
               HTML('#loginopen {margin-top: 10px}'),
               HTML('#createacc {margin-top: 10px;
                                 margin-right: 20px}'),
               HTML('.modal-content {border: 2px solid #000000;
                                     background: #f6f8fc}'),
               HTML('#logout-button {display: block !important}')
               ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    tags$script(HTML(scrollbarjs)),
    tags$script("
      Shiny.addCustomMessageHandler('gear_click', function(value) {
      Shiny.setInputValue('gear_click', value);
      });
    ")
  ),
  tabItems(
    # Player stats tab
    tabItem(tabName = "playerstats",
            style = "overflow-x: hidden;overflow-y: auto;", 
            fluidRow(
              box(
                id = "seasonrankingbox",
                width = 4,
                title = h4("Percentile Ranking"),
                solidHeader=T,
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                fluidRow(
                  column(width = 3, align = "center",
                         uiOutput("playerName"),
                         uiOutput("playerImage")
                  ),
                  column(width=9,align = "center",
                         fluidRow(
                           column(width = 4, align = "center",
                                  selectInput("playerRankSeason",h2("Season"),
                                              choices = NA)
                           ),
                           column(width = 8, align = "center",
                                  radioGroupButtons("playerRankType",
                                                    label = h2("Stat Type"),
                                                    choices = c("Total" = "tot",
                                                                "Per Game" = "pg",
                                                                "Per 60" = "p60"),
                                                    selected = character(0),
                                                    status = "primary")
                           )
                         ),
                         fluidRow(
                           column(width = 4, align = "center",
                                  selectInput("playerRankGPFilter",h2("Min. GP"),
                                              choices = c("1 GP" = 1, "5 GP"=5,"10 GP" = 10, "20 GP" = 20, "40 GP" = 40),
                                              selected = "1 GP") 
                           ),
                           column(width = 8,align = "center",
                                  pickerInput("playerRankPositionFilter",
                                              label = h2("Positions"),
                                              choices = NULL,
                                              multiple=T,
                                              width = '75%')
                           )
                         )
                  )
                ),
                fluidRow(
                  column(width = 4,align="center", style = "padding-right:0px;padding-left:0px;",
                         uiOutput("text1_1"),
                         uiOutput("valueBox1_1"),
                         uiOutput("text2_1"),
                         uiOutput("valueBox2_1"),
                         uiOutput("text3_1"),
                         uiOutput("valueBox3_1")
                  ),
                  column(width = 4,align="center", style = "padding-right:0px;padding-left:0px;",
                         uiOutput("text1_2"),
                         uiOutput("valueBox1_2"),
                         uiOutput("text2_2"),
                         uiOutput("valueBox2_2"),
                         uiOutput("text3_2"),
                         uiOutput("valueBox3_2")
                  ),
                  column(width = 4,align="center", style = "padding-right:0px;padding-left:0px;",
                         uiOutput("text1_3"),
                         uiOutput("valueBox1_3"),
                         uiOutput("text2_3"),
                         uiOutput("valueBox2_3"),
                         uiOutput("text3_3"),
                         uiOutput("valueBox3_3")
                  )
                  
                )
              ),
              box(
                id = "seasonstatsbox",
                width = 8,
                title = h4("Seasonal Summary"),
                solidHeader=T,
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                fluidRow(
                  column(width = 3,align="center",
                         radioGroupButtons("seasonStatsType",
                                           label = h2("Stat Type"),
                                           choices = c("Total" = "tot",
                                                       "Per Game" = "pg",
                                                       "Per 60" = "p60"),
                                           selected = character(0),
                                           status = "primary")
                         
                  )
                ),
                reactableOutput("playerStats")
              ),
              collapseInput(inputId = "iscolseasonrankingbox", boxId = "seasonrankingbox"),
              collapseInput(inputId = "iscolseasonstatsbox", boxId = "seasonstatsbox")
            )
    ),
    
    # Fantasy team stats tab
    tabItem(tabName = "teamstats",
            style = "overflow-x: hidden;overflow-y: auto;",
            column(width = 9,
                   box(
                     id = "teamloadingbox",
                     width = 12,
                     title = h4("Create/Upload Team"),
                     solidHeader=T,
                     status = "primary",
                     collapsible = TRUE,
                     fluidRow(
                       column(width = 4,align="center",
                              radioGroupButtons("datasource",h1("Data Source"),choices = c("Local" = "loc"),status = "primary"),
                              uiOutput("teamloadchoices"),
                              uiOutput("fantasyteam")
                              
                       ),
                       column(width=6,align="center",offset = 1,
                              fluidRow(
                                h1("Team Composition")
                              ),
                              fluidRow(
                                column(width = 4,
                                       selectInput("numLW",h2("LW"),1:9,selected=4)
                                ),
                                column(width = 4, 
                                       selectInput("numC",h2("C"),1:9,selected=4)
                                ),
                                column(width = 4,
                                       selectInput("numRW",h2("RW"),1:9,selected=4)
                                )
                              ),
                              fluidRow(
                                column(width = 4,
                                       selectInput("numD",h2("D"),1:9,selected=4)
                                ),
                                column(width = 4,
                                       selectInput("numMisc",h2("Misc (Util, IR, IR+)"),1:9,selected=4)
                                ),
                                column(width = 4,
                                       selectInput("numG",h2("G"),1:9,selected=4)
                                )
                                
                              )
                       )
                     ),
                     fluidRow(
                       column(width=2,align="center",
                              h2("Left Wingers"),
                              uiOutput("leftwings")
                       ),
                       column(width=2,align="center",
                              h2("Centers"),
                              uiOutput("centers")
                       ),
                       column(width=2,align="center",
                              h2("Right Wingers"),
                              uiOutput("rightwings")
                       ),
                       column(width=2,align="center",
                              h2("Defensemen"),
                              uiOutput("defensemen")
                       ),
                       column(width=2,align="center",
                              h2("Misc"),
                             uiOutput("misc")
                       ),
                       column(width=2,align="center",
                              h2("Goalies"),
                              uiOutput("goalies")
                       )
                     )
                   )
            ),
            column(width = 3,
                   box(
                     id = "leaguesettingsbox",
                     width = 12,
                     title = h4("League Settings"),
                     solidHeader=T,
                     status = "primary",
                     collapsible = TRUE,
                     collapsed = FALSE,
                     column(width = 12, align ="center",
                            h1("Fantasy Points Scoring")
                     ),
                     column(width =6, align = "center",
                            h1("Skaters",style ="font-size:18px"), 
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Goals:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("goalsFP",character(0),value = 6)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Assists:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("assistsFP",character(0),value = 4)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Points:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("pointsFP",character(0),value = 0)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("PPP:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("pppFP",character(0),value = 2)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("SHP:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("shFP",character(0),value = 3)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Shots:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("shotsFP",character(0),value = 0.6)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Hits:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("hitsFP",character(0),value = 0.4)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Blocks:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("blocksFP",character(0),value = 0.4)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("FOW:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("fowFP",character(0),value = 0)
                              )
                            )
                     ),
                     column(width =6, align = "center",
                            h1("Goalies",style ="font-size:18px"), 
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("GS:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("gsFP",character(0),value = 1)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Wins:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("winsFP",character(0),value = 5)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("GA:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("gaFP",character(0),value = -3)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("Saves:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("savesFP",character(0),value = 0.6)
                              )
                            ),
                            fluidRow(
                              column(width = 5,align = "right",
                                     br(),
                                     h2("SO:")
                              ),
                              column(width = 7,align = "left",
                                     numericInput("soFP",character(0),value = 5)
                              )
                            )
                     )
                   )
            ),
            column(width = 12,
                   box(
                     id = "teamstatsbox",
                     width = 12,
                     title = h4("Team Stats"),
                     solidHeader=T,
                     status = "primary",
                     collapsible = TRUE,
                     collapsed = FALSE,
                     fluidRow(
                       column(width = 3, align = "center",
                              radioGroupButtons("teamStatType",
                                                label = h2("Stat Type"),
                                                choices = c("Total" = "tot",
                                                            "Per Game" = "pg"),
                                                selected = "tot",
                                                status = "primary")
                       ),
                       column(width = 3, align = "center",
                              selectInput("teamStatRange",
                                          label = h2("Filter Period"),
                                          choices = c("Last Season" = "ls",
                                                      "Current Season" = "s",
                                                      "Last 30 Days" = 30,
                                                      "Last 14 Days" = 14,
                                                      "Last 7 Days" = 7),
                                          selected = "s")
                       )
                     ),
                     reactableOutput("teamSkaterStats"),
                     reactableOutput("teamGoalieStats")
                   ),
                   box(
                     id = "testbox",
                     width = 12,
                     title = h1("Test"),
                     solidHeader=T,
                     status = "primary",
                     collapsible = TRUE,
                     collapsed = FALSE
                   )
            )
    )
  ),
  div(style = "width: 100%; height: 90vh")
  
)

## Create Page =================================
shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(title = tagList(h5(class = "logo-lg","FHA")),
                  titleWidth =  "15%",
                  tags$li(
                    class = "dropdown",
                    fluidRow(
                      actionButton("loginopen",strong("Login")),
                      actionButton("createacc",strong("Create Account"))
                    )
                  ),
                  userOutput("user")),
  sidebar,
  body
))

