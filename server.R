function(input, output, session) {
  ## Initial data and function setup =================================
  # Read in all players
  playernames <<- read.csv("Data/PlayerNames.csv")
  
  # Get current season
  currentSeason <<- as.integer(quarter(Sys.Date(), with_year = TRUE, fiscal_start = 11))

  # Colour map for conditional formatting
  palette <- function(x){
    if (!is.na(x)){
      rgb(colorRamp(c("#FF0000","#FFFFFF", "#00FF00"))(x),alpha = 100, maxColorValue = 255)
    } else {
      "#e9e9e9" #grey
    }
  }  
  
  ## Set default attributes for UI elements =================================
  updateSelectizeInput(session, 
                       'playerInput', 
                       choices = playernames$Name,
                       selected = character(0), 
                       server = T)
  
  hideElement("seasonStatsType")
  hideElement("playerRankSeason")
  hideElement("playerRankType")
  hideElement("playerRankGPFilter")
  hideElement("playerRankPositionFilter")


  ## Player Metrics - Loading =================================
  observeEvent(input$loadPlayerStats, ignoreInit = T,priority = 10,{
    withProgress(message = "Loading Data...",value = 0.5, {
      
      # Get player ID
      playerID <<- playernames$ID[playernames$Name == input$playerInput]
      
      # Merge game logs
      files = list.files(paste0("Data/Players/",playerID,"/"), full.names=T,pattern = "csv")
      
      playerdata = data.frame()
      for (i in 1:length(files)) {
        tempdata = read.csv(files[i])
        tempdata['Season'] = as.integer(substr(files[i], nchar(files[i])-7, nchar(files[i])-4))
        playerdata = rbind(playerdata,tempdata)
      }
      
      # Skaters
      if (playernames$Position[playernames$ID==playerID] != "G"){
        
        # Create season summary tables
        tableData <<- playerdata[,c("G","Age","Tm",
                                  "Scoring_G","Scoring_A","Scoring_PTS",
                                  "X...","Goals_EV","Goals_PP","Goals_SH",
                                  "Goals_GW","Assists_EV","Assists_PP",
                                  "Assists_SH","S","S.","TOI","HIT", "PIM",
                                  "BLK","FOW","FOL","FO.","Season")]
        
        
        
        totalTable <<- tableData %>%
          group_by(Season,Tm) %>%
          summarize(
            Age = max(Age,na.rm=T),
            GP = n(),
            `Avg TOI (mins)` = paste0(floor(mean(TOI,na.rm=T)),
                                      ":",
                                      str_pad(round((mean(TOI,na.rm=T)%%1)*60,digits = 0),2,pad = "0")),
            Goals = sum(Scoring_G,na.rm=T),
            Assists = sum(Scoring_A,na.rm=T),
            Points = sum(Scoring_PTS,na.rm=T),
            `+/-` = sum(X...,na.rm=T),
            `PP Goals` = sum(Goals_PP,na.rm=T),
            `PP Assists` = sum(Assists_PP,na.rm=T),
            Shots = sum(S,na.rm=T),
            FOW = sum(FOW,na.rm=T),
            PIM = sum(PIM,na.rm=T),
            Hits = sum(HIT,na.rm=T),
            Blocks = sum(BLK,na.rm=T),
            .groups = "keep"
          ) %>%
          arrange(desc(Season))
        
        averageTable <<- tableData %>%
          group_by(Season,Tm) %>%
          summarize(
            Age = max(Age),
            GP = n(),
            `Avg TOI (mins)` = paste0(floor(mean(TOI,na.rm=T)),
                                      ":",
                                      str_pad(round((mean(TOI,na.rm=T)%%1)*60,digits = 0),2,pad = "0")),
            Goals = sum(Scoring_G,na.rm=T)/n(),
            Assists = sum(Scoring_A,na.rm=T)/n(),
            Points = sum(Scoring_PTS,na.rm=T)/n(),
            `PP Goals` = sum(Goals_PP,na.rm=T)/n(),
            `PP Assists` = sum(Assists_PP,na.rm=T)/n(),
            Shots = sum(S,na.rm=T)/n(),
            `Shot %` = 100*(sum(Scoring_G,na.rm=T)/sum(S,na.rm=T)),
            `Avg FO %` = 100*sum(FOW,na.rm=T)/(sum(FOW,na.rm=T)+sum(FOL,na.rm=T)),
            FOW = sum(FOW,na.rm=T)/n(),
            PIM = sum(PIM,na.rm=T)/n(),
            Hits = sum(HIT,na.rm=T)/n(),
            Blocks = sum(BLK,na.rm=T)/n(),
            .groups = "keep"
          ) %>%
          arrange(desc(Season))
      
        per60Table <<- tableData %>%
          group_by(Season,Tm) %>%
          summarize(
            Age = max(Age,na.rm=T),
            GP = n(),
            `Avg TOI (mins)` = paste0(floor(mean(TOI,na.rm=T)),
                                      ":",
                                      str_pad(round((mean(TOI,na.rm=T)%%1)*60,digits = 0),2,pad = "0")),
            Goals = 60*sum(Scoring_G,na.rm=T)/sum(TOI,na.rm=T),
            Assists = 60*sum(Scoring_A,na.rm=T)/sum(TOI,na.rm=T),
            Points = 60*sum(Scoring_PTS,na.rm=T)/sum(TOI,na.rm=T),
            `PP Goals` = 60*sum(Goals_PP,na.rm=T)/sum(TOI,na.rm=T),
            `PP Assists` = 60*sum(Assists_PP,na.rm=T)/sum(TOI,na.rm=T),
            Shots = 60*sum(S,na.rm=T)/sum(TOI,na.rm=T),
            FOW = 60*sum(FOW,na.rm=T)/sum(TOI,na.rm=T),
            PIM = sum(PIM,na.rm=T)/sum(TOI,na.rm=T),
            Hits = 60*sum(HIT,na.rm=T)/sum(TOI,na.rm=T),
            Blocks = 60*sum(BLK,na.rm=T)/sum(TOI,na.rm=T),
            .groups = "keep"
          ) %>%
          arrange(desc(Season))
        
        # Round averages to two digits
        averageTable[,6:17] <<- round(averageTable[,6:17],digits=2)
        per60Table[,6:15] <<- round(per60Table[,6:15],digits=2)
        
        # Update UI buttons for skaters
        showElement("seasonStatsType")
        updateRadioGroupButtons(session,"seasonStatsType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg",
                                            "Per 60" = "p60"),
                                selected = character(0),
                                status = "primary")
        updateRadioGroupButtons(session,"seasonStatsType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg",
                                            "Per 60" = "p60"),
                                selected = "tot",
                                status = "primary")        
        updateRadioGroupButtons(session,"playerRankType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg",
                                            "Per 60" = "p60"),
                                selected = character(0),
                                status = "primary")
        updateRadioGroupButtons(session,"playerRankType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg",
                                            "Per 60" = "p60"),
                                selected = "tot",
                                status = "primary")
        showElement("playerRankSeason")
        showElement("playerRankType")
        showElement("playerRankGPFilter")
        showElement("playerRankPositionFilter")
        updatePickerInput(session,"playerRankPositionFilter",
                          choices = c("C","LW","RW","D"),
                          selected = character(0),
                          options = list(
                                selectedTextFormat = "count",
                                countSelectedText = "{0} selected",
                                noneSelectedText = "No filter"
                          ))
        
        
        
      # Goalies
      } else {
        
        tableData = playerdata[,c("G","Age","Tm","Result","DEC",
                                  "Goalie.Stats_GA","Goalie.Stats_SA",
                                  "Goalie.Stats_SV","Goalie.Stats_SV.",
                                  "Goalie.Stats_SO","Season"
                                  )]
        
        totalTable <<- tableData %>%
          group_by(Season,Tm) %>%
          summarize(
            Age = max(Age,na.rm=T),
            GP = n(),
            Wins = sum(DEC=="W",na.rm=T),
            Saves = sum(Goalie.Stats_SV,na.rm=T),
            `Goals Against` = sum(Goalie.Stats_GA,na.rm=T),
            `Shots Against` = sum(Goalie.Stats_SA,na.rm=T),
            Shutouts = sum(Goalie.Stats_SO,na.rm=T),
            .groups = "keep"
          ) %>%
          arrange(desc(Season))
        
        averageTable <<- tableData %>%
          group_by(Season,Tm) %>%
          summarize(
            Age = max(Age),
            GP = n(),
            `Save %` = sum(Goalie.Stats_SV,na.rm=T)/sum(Goalie.Stats_SA,na.rm=T),
            `GAA` = sum(Goalie.Stats_GA,na.rm=T)/n(),
            `SAA` = sum(Goalie.Stats_SA,na.rm=T)/n(),
            .groups = "keep"
          ) %>%
          arrange(desc(Season))
        
        averageTable[,5:7] <<- round(averageTable[,5:7],digits=2)
        
        # Update UI buttons for goalies
        showElement("seasonStatsType")
        updateRadioGroupButtons(session,"seasonStatsType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg"),
                                selected = character(0),
                                status = "primary")
        updateRadioGroupButtons(session,"seasonStatsType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg"),
                                selected = "tot",
                                status = "primary")
        updateRadioGroupButtons(session,"playerRankType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg"),
                                selected = character(0),
                                status = "primary")
        updateRadioGroupButtons(session,"playerRankType",
                                choices = c("Total" = "tot",
                                            "Per Game" = "pg"),
                                selected = "tot",
                                status = "primary")
        showElement("playerRankSeason")
        showElement("playerRankType")
        showElement("playerRankGPFilter")
        hideElement("playerRankPositionFilter")

      }
      
      # Add player name and image to UI
      output$playerName = renderUI({
        
        htmlcode = paste0("<h2>",playernames$Name[playernames$ID==playerID],"</h2>",
                          "<h2>",paste0("(",playernames$Position[playernames$ID==playerID],")"),"</h2>")
        HTML(htmlcode)
        
      })      
      output$playerImage = renderUI({
        tags$img(src = paste0("playerimages/",playerID,".jpg"), height="150px")
      })
      
      # Update UI inputs
      updateRadioGroupButtons(session,"seasonStatsType",
                  selected = "tot")
      updateSelectInput(session,"playerRankSeason",
                  choices = c(0,1),
                  selected = NULL)
      updateSelectInput(session,"playerRankSeason",
                  choices = unique(tableData$Season),
                  selected = max(tableData$Season))

    })
  })
  
  # Expand UI boxes
  observeEvent(input$loadPlayerStats,once=T, ignoreInit = T, {
      if (!isFALSE(input$iscolseasonstatsbox)) {
        js$collapse("seasonstatsbox")
      }
      if (!isFALSE(input$iscolseasonrankingbox)) {
        js$collapse("seasonrankingbox")
      }

  })
  
  
  
  
  ## Player Metrics - Season Stats =================================
  # Cell styling for reactables
  totalStyle <- function(value, index, name) {
    normalized <- (value - min(totalTable[name], na.rm = T)) /
      (max(totalTable[name], na.rm = T) - min(totalTable[name], na.rm = T))
    color <- palette(normalized)
    list(background = color)
  }      
  averageStyle <- function(value, index, name) {
    normalized <- (value - min(averageTable[name], na.rm = T)) /
      (max(averageTable[name], na.rm = T) - min(averageTable[name], na.rm = T))
    color <- palette(normalized)
    list(background = color)
  }      
  per60Style <- function(value, index, name) {
    normalized <- (value - min(per60Table[name], na.rm = T)) /
      (max(per60Table[name], na.rm = T) - min(per60Table[name], na.rm = T))
    color <- palette(normalized)
    list(background = color)
  }      
  

  # Display tables on button clicks
  observeEvent(input$seasonStatsType, ignoreInit = T, {
    if (input$seasonStatsType == "tot") {
      output$playerStats <- renderReactable({
        reactable(
          totalTable,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 50,
            headerStyle = list(background = "#f7f7f8"),
            style = totalStyle
          ),
          columns = list(
            Season = colDef(style = list()),
            Tm = colDef(style = list()),
            Age = colDef(style = list()),
            GP = colDef(style = list()),
            `Avg TOI (mins)` = colDef(style = list())
          ),
          bordered = TRUE,
          highlight = TRUE,
          defaultPageSize = 20
        )
      })
    } else if (input$seasonStatsType == "pg") {
      output$playerStats <- renderReactable({
        reactable(
          averageTable,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 50,
            headerStyle = list(background = "#f7f7f8"),
            style = averageStyle
          ),
          columns = list(
            Season = colDef(style = list()),
            Tm = colDef(style = list()),
            Age = colDef(style = list()),
            GP = colDef(style = list()),
            `Avg TOI (mins)` = colDef(style = list()),
            Goals = colDef(format = colFormat(digits = 2))
          ),
          bordered = TRUE,
          highlight = TRUE,
          defaultPageSize = 20
        )
      })  
    } else {
      output$playerStats <- renderReactable({
        reactable(
          per60Table,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 50,
            headerStyle = list(background = "#f7f7f8"),
            style = per60Style
          ),
          columns = list(
            Season = colDef(style = list()),
            Tm = colDef(style = list()),
            Age = colDef(style = list()),
            GP = colDef(style = list()),
            `Avg TOI (mins)` = colDef(style = list())
          ),
          bordered = TRUE,
          highlight = TRUE,
          defaultPageSize = 20
        )
      })
    }
    

    
  })

  
  ## Player Metrics - Season Ranking =================================
  
  # Update min GP filter based on season choice
  observeEvent(c(input$playerRankSeason),ignoreInit = TRUE, priority = 9, {
    if (!is.na(as.numeric(input$playerRankSeason))) {
      if (totalTable$GP[totalTable$Season==as.numeric(input$playerRankSeason)] < 5) {
        updateSelectInput(session, "playerRankGPFilter",
            choices = c("1 GP" = 1))
      } else if (totalTable$GP[totalTable$Season==as.numeric(input$playerRankSeason)] < 10) {
        updateSelectInput(session, "playerRankGPFilter",
            choices = c("1 GP" = 1, "5 GP"=5))
      } else if (totalTable$GP[totalTable$Season==as.numeric(input$playerRankSeason)] < 20) {
        updateSelectInput(session, "playerRankGPFilter",
            choices = c("1 GP" = 1, "5 GP"=5, "10 GP"= 10))
      } else if (totalTable$GP[totalTable$Season==as.numeric(input$playerRankSeason)] < 40) {
        updateSelectInput(session, "playerRankGPFilter",
            choices = c("1 GP" = 1, "5 GP"=5, "10 GP"= 10,"20 GP"= 20))
      } else {
        updateSelectInput(session, "playerRankGPFilter",
            choices = c("1 GP" = 1, "5 GP"=5, "10 GP"= 10,"20 GP"= 20,"40 GP"= 40))
      }
    }

    

    
  })
  
  
  # Update rankings based on chosen filters
  observeEvent(c(input$playerRankSeason,
                 input$playerRankType,
                 input$playerRankGPFilter,
                 input$playerRankPositionFilter), ignoreInit = TRUE, priority = 5, {
    
    # Load and filter data
    if (playernames$Position[playernames$ID==playerID] != "G") {
      allPlayerData <<- read.csv(paste0("Data/allSkaters/",input$playerRankSeason,".csv"))
      allPlayerData <<- left_join(allPlayerData,playernames,"ID")
      
      # Filter positions if needed
      if (length(input$playerRankPositionFilter) >=1 &
          length(input$playerRankPositionFilter) <4) {
        
        allPlayerData <<- allPlayerData[allPlayerData$Position %in% input$playerRankPositionFilter |
                                        allPlayerData$ID == playerID,]
        
      }
      
    } else {
      allPlayerData <<- read.csv(paste0("Data/allGoalies/",input$playerRankSeason,".csv"))
    }
    allPlayerData <<- allPlayerData[allPlayerData$GP>=as.integer(input$playerRankGPFilter),]
    
    # Calculate ranks for skaters
    if (playernames$Position[playernames$ID==playerID] != "G") {
      if (input$playerRankType == "tot") {
          goalsPerc = (rank(allPlayerData$Goals,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          goalsRank = rank(-allPlayerData$Goals,ties.method = "min")[allPlayerData$ID==playerID]
          assistsPerc = (rank(allPlayerData$Assists,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          assistsRank = rank(-allPlayerData$Assists,ties.method = "min")[allPlayerData$ID==playerID]
          pointsPerc = (rank(allPlayerData$Points,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          pointsRank = rank(-allPlayerData$Points,ties.method = "min")[allPlayerData$ID==playerID]        
          shotsPerc = (rank(allPlayerData$Shots,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          shotsRank = rank(-allPlayerData$Shots,ties.method = "min")[allPlayerData$ID==playerID]       
          PPGPerc = (rank(allPlayerData$PP.Goals,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          PPGRank = rank(-allPlayerData$PP.Goals,ties.method = "min")[allPlayerData$ID==playerID] 
          PPAPerc = (rank(allPlayerData$PP.Assists,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          PPARank = rank(-allPlayerData$PP.Assists,ties.method = "min")[allPlayerData$ID==playerID]
          HitsPerc = (rank(allPlayerData$Hits,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          HitsRank = rank(-allPlayerData$Hits,ties.method = "min")[allPlayerData$ID==playerID]
          BlocksPerc = (rank(allPlayerData$Blocks,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          BlocksRank = rank(-allPlayerData$Blocks,ties.method = "min")[allPlayerData$ID==playerID]
          FOWPerc = (rank(allPlayerData$FOW,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          FOWRank = rank(-allPlayerData$FOW,ties.method = "min")[allPlayerData$ID==playerID]
      } else if (input$playerRankType == "pg") {
          goalsPerc = (rank(allPlayerData$Goals/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          goalsRank = rank(-allPlayerData$Goals/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]
          assistsPerc = (rank(allPlayerData$Assists/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          assistsRank = rank(-allPlayerData$Assists/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]
          pointsPerc = (rank(allPlayerData$Points/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          pointsRank = rank(-allPlayerData$Points/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]        
          shotsPerc = (rank(allPlayerData$Shots/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          shotsRank = rank(-allPlayerData$Shots/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]       
          PPGPerc = (rank(allPlayerData$PP.Goals/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          PPGRank = rank(-allPlayerData$PP.Goals/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID] 
          PPAPerc = (rank(allPlayerData$PP.Assists/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          PPARank = rank(-allPlayerData$PP.Assists/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]
          HitsPerc = (rank(allPlayerData$Hits/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          HitsRank = rank(-allPlayerData$Hits/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]
          BlocksPerc = (rank(allPlayerData$Blocks/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          BlocksRank = rank(-allPlayerData$Blocks/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]
          FOWPerc = (rank(allPlayerData$FOW/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          FOWRank = rank(-allPlayerData$FOW/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]
      } else {
          goalsPerc = (rank(allPlayerData$Goals/allPlayerData$TOI.Total,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          goalsRank = rank(-allPlayerData$Goals/allPlayerData$TOI.Total,ties.method = "min")[allPlayerData$ID==playerID]
          assistsPerc = (rank(allPlayerData$Assists/allPlayerData$TOI.Total,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          assistsRank = rank(-allPlayerData$Assists/allPlayerData$TOI.Total,ties.method = "min")[allPlayerData$ID==playerID]
          pointsPerc = (rank(allPlayerData$Points/allPlayerData$TOI.Total,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          pointsRank = rank(-allPlayerData$Points/allPlayerData$TOI.Total,ties.method = "min")[allPlayerData$ID==playerID]        
          shotsPerc = (rank(allPlayerData$Shots/allPlayerData$TOI.Total,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          shotsRank = rank(-allPlayerData$Shots/allPlayerData$TOI.Total,ties.method = "min")[allPlayerData$ID==playerID]       
          PPGPerc = (rank(allPlayerData$PP.Goals/allPlayerData$TOI.Total,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          PPGRank = rank(-allPlayerData$PP.Goals/allPlayerData$TOI.Total,ties.method = "min")[allPlayerData$ID==playerID] 
          PPAPerc = (rank(allPlayerData$PP.Assists/allPlayerData$TOI.Total,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          PPARank = rank(-allPlayerData$PP.Assists/allPlayerData$TOI.Total,ties.method = "min")[allPlayerData$ID==playerID]
          HitsPerc = (rank(allPlayerData$Hits/allPlayerData$TOI,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          HitsRank = rank(-allPlayerData$Hits/allPlayerData$TOI,ties.method = "min")[allPlayerData$ID==playerID]
          BlocksPerc = (rank(allPlayerData$Blocks/allPlayerData$TOI,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          BlocksRank = rank(-allPlayerData$Blocks/allPlayerData$TOI,ties.method = "min")[allPlayerData$ID==playerID]
          FOWPerc = (rank(allPlayerData$FOW/allPlayerData$TOI,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          FOWRank = rank(-allPlayerData$FOW/allPlayerData$TOI,ties.method = "min")[allPlayerData$ID==playerID]
      }
    

      # Output to UI
      output$text1_1 = renderUI({h3("Goals")})
      output$valueBox1_1 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(goalsPerc))),paste0("Rank: ",goalsRank,"/",nrow(allPlayerData)),
                   color="red",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-red { background-color: ",palette(goalsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text1_2 = renderUI({h3("Assists")})
      output$valueBox1_2 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(assistsPerc))),paste0("Rank: ",round(assistsRank),"/",nrow(allPlayerData)),
                   color="yellow",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-yellow { background-color: ",palette(assistsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text1_3 = renderUI({h3("Points")})
      output$valueBox1_3 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(pointsPerc))),paste0("Rank: ",round(pointsRank),"/",nrow(allPlayerData)),
                   color="aqua",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-aqua { background-color: ",palette(pointsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text2_1 = renderUI({h3("Shots")})
      output$valueBox2_1 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(shotsPerc))),paste0("Rank: ",round(shotsRank),"/",nrow(allPlayerData)),
                   color="blue",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-blue { background-color: ",palette(shotsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text2_2 = renderUI({h3("PPG")})
      output$valueBox2_2 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(PPGPerc))),paste0("Rank: ",round(PPGRank),"/",nrow(allPlayerData)),
                   color="light-blue",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-light-blue { background-color: ",palette(PPGPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text2_3 = renderUI({h3("PPA")})
      output$valueBox2_3 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(PPAPerc))),paste0("Rank: ",round(PPARank),"/",nrow(allPlayerData)),
                   color="green",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-green { background-color: ",palette(PPAPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text3_1 = renderUI({h3("Hits")})
      output$valueBox3_1 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(HitsPerc))),paste0("Rank: ",round(HitsRank),"/",nrow(allPlayerData)),
                   color="navy",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-navy { background-color: ",palette(HitsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text3_2 = renderUI({h3("Blocks")})
      output$valueBox3_2 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(BlocksPerc))),paste0("Rank: ",round(BlocksRank),"/",nrow(allPlayerData)),
                   color="teal",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-teal { background-color: ",palette(BlocksPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text3_3 = renderUI({h3("FOW")})
      output$valueBox3_3 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(FOWPerc))),paste0("Rank: ",round(FOWRank),"/",nrow(allPlayerData)),
                   color="olive",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-olive { background-color: ",palette(FOWPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
    
    # Calculate rankings for goalies
    } else {
      
      
      if (input$playerRankType == "tot") {
          winsPerc = (rank(allPlayerData$Wins,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          winsRank = rank(-allPlayerData$Wins,ties.method = "min")[allPlayerData$ID==playerID]
          SOPerc = (rank(allPlayerData$SO,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          SORank = rank(-allPlayerData$SO,ties.method = "min")[allPlayerData$ID==playerID]
          SAPerc = (rank(allPlayerData$SA,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          SARank = rank(-allPlayerData$SA,ties.method = "min")[allPlayerData$ID==playerID]
          GAPerc = (rank(-allPlayerData$GA,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          GARank = rank(allPlayerData$GA,ties.method = "min")[allPlayerData$ID==playerID]
          
          # Output to UI
          output$text1_1 = renderUI({h3("Wins")})
          output$valueBox1_1 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(winsPerc))),paste0("Rank: ",winsRank,"/",nrow(allPlayerData)),
                       color="red",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-red { background-color: ",palette(winsPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_2 = renderUI({h3("SO")})
          output$valueBox1_2 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(SOPerc))),paste0("Rank: ",round(SORank),"/",nrow(allPlayerData)),
                       color="yellow",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-yellow { background-color: ",palette(SOPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_3 = renderUI({h3("GA")})
          output$valueBox1_3 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(GAPerc))),paste0("Rank: ",round(GARank),"/",nrow(allPlayerData)),
                       color="blue",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-blue { background-color: ",palette(GAPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text2_1 = renderUI({h3("SA")})
          output$valueBox2_1 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(SAPerc))),paste0("Rank: ",round(SARank),"/",nrow(allPlayerData)),
                       color="aqua",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-aqua { background-color: ",palette(SAPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text2_2 = renderUI({})
          output$valueBox2_2 = renderUI({})
          output$text2_3 = renderUI({})
          output$valueBox2_3 = renderUI({})
          output$text3_1 = renderUI({})
          output$valueBox3_1 = renderUI({})
          output$text3_2 = renderUI({})
          output$valueBox3_2 = renderUI({})
          output$text3_3 = renderUI({})
          output$valueBox3_3 = renderUI({})
          
      } else if (input$playerRankType == "pg") {
          savefracPerc = (rank(allPlayerData$SV..,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          savefracRank = rank(-allPlayerData$SV..,ties.method = "min")[allPlayerData$ID==playerID]
          SAPerc = (rank(allPlayerData$SA/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          SARank = rank(-allPlayerData$SA/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]
          GAPerc = (rank(-allPlayerData$GA/allPlayerData$GP,ties.method = "max")/nrow(allPlayerData)*100)[allPlayerData$ID==playerID]
          GARank = rank(allPlayerData$GA/allPlayerData$GP,ties.method = "min")[allPlayerData$ID==playerID]      
          
          # Output to UI
          output$text1_1 = renderUI({h3("Save %")})
          output$valueBox1_1 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(savefracPerc))),paste0("Rank: ",savefracRank,"/",nrow(allPlayerData)),
                       color="red",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-red { background-color: ",palette(savefracPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_2 = renderUI({h3("GAA")})
          output$valueBox1_2 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(GAPerc))),paste0("Rank: ",round(GARank),"/",nrow(allPlayerData)),
                       color="yellow",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-yellow { background-color: ",palette(GAPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_3 = renderUI({h3("SAA")})
          output$valueBox1_3 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(SAPerc))),paste0("Rank: ",round(SARank),"/",nrow(allPlayerData)),
                       color="aqua",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-aqua { background-color: ",palette(SAPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text2_1 = renderUI({})
          output$valueBox2_1 = renderUI({})
          output$text2_2 = renderUI({})
          output$valueBox2_2 = renderUI({})
          output$text2_3 = renderUI({})
          output$valueBox2_3 = renderUI({})
          output$text3_1 = renderUI({})
          output$valueBox3_1 = renderUI({})
          output$text3_2 = renderUI({})
          output$valueBox3_2 = renderUI({})
          output$text3_3 = renderUI({})
          output$valueBox3_3 = renderUI({})
          
      }
      
    }
      
      
    
    
  })
  
  
  
  
  
  

  
  
  
  
  ## Fantasy Team - Create/Upload ===============================
  # Get current season data for skaters/goalies
  skaterData <<- read.csv(paste0("Data/allSkaters/",currentSeason,".csv"))
  skaterData <<- left_join(skaterData,playernames,"ID")
  goalieData <<- read.csv(paste0("Data/allGoalies/",currentSeason,".csv"))
  goalieData <<- left_join(goalieData,playernames,"ID")
  
  #Initialize tracking variables
  numC_UI <- 0
  numLW_UI <- 0
  numRW_UI <- 0
  numD_UI <- 0
  numG_UI <- 0
  numC <- reactive({input$numC})
  numLW <- reactive({input$numLW})
  numRW <- reactive({input$numRW})
  numD <- reactive({input$numD})
  numG <- reactive({input$numG})
  numC_d <- debounce(numC,100)
  numLW_d <- debounce(numLW,100)
  numRW_d <- debounce(numRW,100)
  numD_d <- debounce(numD,100)
  numG_d <- debounce(numG,100)
  
  observeEvent(input$teamFileLoad,ignoreNULL = FALSE, priority=2,{
    output$centers = renderUI({
      if (numC_d() != numC_UI) {
        numC_UI <<- numC_d()
      }
      return(lapply(1:numC_d(), function(i) {
        column(width = floor(12/numC_d()),
          selectizeInput(
            paste0("C",i),
            label = NULL,
            choices = c("",skaterData$Name),
            selected = ""
          )
        )
      }))
    })
  })
  
  observeEvent(input$teamFileLoad,ignoreNULL = FALSE, priority=2,{
    output$leftwings = renderUI({
      if (numLW_d() != numLW_UI) {
        numLW_UI <<- numLW_d()
      }
      return(lapply(1:numLW_d(), function(i) {
        column(width = floor(12/numLW_d()),
          selectizeInput(
            paste0("LW",i),
            label = NULL,
            choices = c("",skaterData$Name),
            selected = ""
          )
        )
      }))
    })
  })
  
  observeEvent(input$teamFileLoad,ignoreNULL = FALSE, priority=2,{
    output$rightwings = renderUI({
      if (numRW_d() != numRW_UI) {
        numRW_UI <<- numRW_d()
      }
      return(lapply(1:numRW_d(), function(i) {
        column(width = floor(12/numRW_d()),
          selectizeInput(
            paste0("RW",i),
            label = NULL,
            choices = c("",skaterData$Name),
            selected = ""
          )
        )
      }))
    })
  })
  
  observeEvent(input$teamFileLoad,ignoreNULL = FALSE, priority=2,{
    output$defensemen = renderUI({
      if (numD_d() != numD_UI) {
        numD_UI <<- numD_d()
      }
      return(lapply(1:numD_d(), function(i) {
        column(width = floor(12/numD_d()),
          selectizeInput(
            paste0("D",i),
            label = NULL,
            choices = c("",skaterData$Name),
            selected = ""
          )
        )
      }))
    })
  })
  
  observeEvent(input$teamFileLoad,ignoreNULL = FALSE, priority=2,{
    output$goalies = renderUI({
      if (numG_d() != numG_UI) {
        numG_UI <<- numG_d()
      }
      return(lapply(1:numG_d(), function(i) {
        column(width = floor(12/numG_d()),
          selectizeInput(
            paste0("G",i),
            label = NULL,
            choices = c("",goalieData$Name),
            selected = ""
          )
        )
      }))
    })
  })
  
  # Dynamically update global team dataframe
  observe({
    # compile team members into one dataframe
    team = as.data.frame(matrix(ncol=3))
    colnames(team) = c("ID","Name","Position")
    

    # append centers
    for (i in 1:numC_UI) {
      playerName = input[[paste0("C",i)]]
      playerID = skaterData$ID[skaterData$Name == playerName]
      if (!is.null(playerName)) {
        if (!playerName=="") {
          team = rbind(team,c(playerID,playerName,"C"))
        }
      }
    }
    
    # append left wingers
    for (i in 1:numLW_UI) {
      
      playerName = input[[paste0("LW",i)]]
      playerID = skaterData$ID[skaterData$Name == playerName]
      if (!is.null(playerName)) {
        if (!playerName=="") {
          team = rbind(team,c(playerID,playerName,"LW"))
        }
      }
    }    
    
    # append right wingers
    for (i in 1:numRW_UI) {
      
      playerName = input[[paste0("RW",i)]]
      playerID = skaterData$ID[skaterData$Name == playerName]
      if (!is.null(playerName)) {
        if (!playerName=="") {
          team = rbind(team,c(playerID,playerName,"RW"))
        }
      }
    }        
    
        
    # append defensemen
    for (i in 1:numD_UI) {
      
      playerName = input[[paste0("D",i)]]
      playerID = skaterData$ID[skaterData$Name == playerName]
      if (!is.null(playerName)) {
        if (!playerName=="") {
          team = rbind(team,c(playerID,playerName,"D"))
        }
      }
    }    
    
    # append goalies
    for (i in 1:numG_UI) {
      
      playerName = input[[paste0("G",i)]]
      playerID = goalieData$ID[goalieData$Name == playerName]
      if (!is.null(playerName)) {
        if (!playerName=="") {
          team = rbind(team,c(playerID,playerName,"G"))
        }
      }
    }
    
    team = team[-1,]
    teamGLOB <<- team
    
    
  })
  
  # Save selected team to csv file
  output$teamFileSave = downloadHandler(
    filename = function () {"fantasyTeam.csv"},
    
    content = function(file) {
      # save to csv
      write.csv(teamGLOB,file,row.names = F)
    }
  )
  
  
  
  # Load team from csv file
  observeEvent(input$teamFileLoad,priority = 3, {
    filePath = input$teamFileLoad
    teamGLOB <<- read.csv(filePath$datapath)
    
    
    # Update team composition filters
        updateNumericInput(session,"numLW",value =0)
    updateNumericInput(session,"numC",value =0)
    updateNumericInput(session,"numRW",value =0)
    updateNumericInput(session,"numD",value =0)
    updateNumericInput(session,"numG",value =0)
    updateNumericInput(session,"numLW",value =sum(teamGLOB$Position=="LW"))
    updateNumericInput(session,"numC",value =sum(teamGLOB$Position=="C"))
    updateNumericInput(session,"numRW",value =sum(teamGLOB$Position=="RW"))
    updateNumericInput(session,"numD",value =sum(teamGLOB$Position=="D"))
    updateNumericInput(session,"numG",value =sum(teamGLOB$Position=="G"))
  })
  
  # fill in player names
  observe(priority = 1, {
    # add these to trigger the observer
    numLW_d()
    numC_d()
    numRW_d()
    numD_d()
    numG_d()
    
    if (exists("teamGLOB")) {
    
      for (i in 1:numLW_d()) {
        updateSelectizeInput(session,
                             paste0("LW",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="LW"][i])
      }
      for (i in 1:numC_d()) {
        updateSelectizeInput(session,
                             paste0("C",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="C"][i])
      }
      for (i in 1:numRW_d()) {
        updateSelectizeInput(session,
                             paste0("RW",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="RW"][i])
      }
      for (i in 1:numD_d()) {
        updateSelectizeInput(session,
                             paste0("D",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="D"][i])
      }
      for (i in 1:numG_d()) {
        updateSelectizeInput(session,
                             paste0("G",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="G"][i])
      }
    }
  })
  
  
  
}