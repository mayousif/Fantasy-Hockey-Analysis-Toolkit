## Sidebar =================================
sidebar = dashboardSidebar(
  width = "15%",
  title = h4("Fantasy Hockey Analysis Toolkit"),
  sidebarMenu(id = "tabs",
    menuItem(
      selected = TRUE,
      strong("Player Stats"),
      icon = icon("person-skating"),
      menuSubItem(
        selectizeInput("playerInput", 
                       "Select Player",
                       choices = NULL,
                       selected = NULL),
        icon = NULL
      ),
      menuSubItem(
        actionButton("loadPlayerStats","Load Data"),
        icon = NULL,
        tabName = "playerstats"
      )
    ),
    menuItem(
      strong("Fantasy Metrics"), 
      tabName = "teamstats", 
      icon = icon("people-group")
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
               HTML('.skin-black .sidebar a {font-size: 18px;}'),
               HTML('.Reactable {padding-top: 10px;}'),
               HTML('.box {background: #f6f8fc}'),
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
               HTML('.left-side, .main-sidebar {padding-top: 0px;}')),
    tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }"),
    tags$script(HTML(scrollbarjs)),
  ),
  tabItems(
    # Player stats tab
    tabItem(tabName = "playerstats",
      style = "overflow-x: hidden;overflow-y: auto;", 
      fluidRow(
        box(
          id = "seasonrankingbox",
          width = 4,
          title = h1("Percentile Ranking"),
          solidHeader=T,
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          fluidRow(
            column(width = 3, align = "center",
              uiOutput("playerName"),
              uiOutput("playerImage")
            ),
            column(width=9,align = "center",
              fluidRow(
                column(width = 4, align = "left",
                  selectInput("playerRankSeason",h2("Season"),
                              choices = NA)
                ),
                column(width = 8, align = "left",
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
                column(width = 4, align = "left",
                  selectInput("playerRankGPFilter",h2("Min. GP"),
                          choices = c("1 GP" = 1, "5 GP"=5,"10 GP" = 10, "20 GP" = 20, "40 GP" = 40),
                          selected = "1 GP") 
                ),
                column(width = 4,align = "left",
                  pickerInput("playerRankPositionFilter",
                              label = h2("Positions"),
                              choices = NULL,
                              multiple=T)
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
          title = h1("Seasonal Summary"),
          solidHeader=T,
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          fluidRow(
            column(width = 5,align="left",
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
      column(width = 8,
        box(
          id = "teamloadingbox",
          width = 12,
          title = h1("Create/Upload Team"),
          solidHeader=T,
          status = "primary",
          collapsible = TRUE,
          collapsed = FALSE,
          fluidRow(
            column(width = 3,align="center",
              fileInput(
                "teamFileLoad",
                label = h2("Load Team From .csv File"),
                accept = ".csv"
              )
            ),
            column(width=6,align="center",
              fluidRow(
                h2("Team Composition")
              ),
              fluidRow(
                column(width = 4,
                  numericInput("numLW","LW",4,min=1)
                ),
                column(width = 4, 
                  numericInput("numC"," C",4,min=1)
                ),
                column(width = 4,
                  numericInput("numRW","RW",4,min=1)
                )
              ),
              fluidRow(
                column(width = 4, offset = 2,
                  numericInput("numD","D",4,min=1)
                ),
                column(width = 4,
                  numericInput("numG","G",4,min=1)
                )
              )
            ),
            column(width=3,align="center",
              h2("Save Team to .csv File"),
              downloadButton(
                "teamFileSave",
                label = "Save",
                icon = icon(lib="glyphicon", "download-alt")
              )
            )
          ),
          fluidRow(
            column(width=12,align="center",
             h3("Left Wingers")
            ),
            uiOutput("leftwings")
          ),
          fluidRow(
            column(width=12,align="center",
             h3("Centers")
            ),
            uiOutput("centers")
          ),
          
          fluidRow(
            column(width=12,align="center",
             h3("Right Wingers")
            ),
            uiOutput("rightwings")
          ),
          fluidRow(
            column(width=12,align="center",
              h3("Defensemen")
            ),
            uiOutput("defensemen")
          ),
          fluidRow(
            column(width=12,align="center",
              h3("Goalies")
            ),
            uiOutput("goalies")
          )
        
          
        )
      ),
      column(width = 4,
        box(
          id = "leaguesettingsbox",
          width = 12,
          title = h1("League Settings"),
          solidHeader=T,
          status = "primary",
          collapsible = TRUE,
          collapsed = FALSE,
          column(width = 12, align ="center",
            h1("Fantasy Stat Values")
          ),
          column(width =6, align = "center",
            h1("Skaters"), 
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
            )
          ),
          column(width =6, align = "center",
            h1("Goalies"), 
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
                h2("SHO:")
              ),
              column(width = 7,align = "left",
                numericInput("shoFP",character(0),value = 5)
              )
            )
          )
        )
      ),
      box(
        
        
        
        
      )
    )
  ),
  div(style = "width: 100%; height: 90vh")
  
)

## Create Page =================================
dashboardPage(
  title = "Fantasy Hockey Analyser",
  skin = "black",
  dashboardHeader(disable = TRUE),
  sidebar,
  body
)