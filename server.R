shinyServer(function(input, output, session) {
  ## Initial setup =================================

  # Colour map for conditional formatting
  GrnRedPalette <- function(x){
    if (!is.na(x)){
      if (x>1) {
        x = 1
      }
      rgb(colorRamp(c("#FF0000","#FFFFFF", "#00FF00"))(x),alpha = 75, maxColorValue = 255)
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
  observeEvent(input$playerInput, ignoreInit = T,priority = 10,{
    if (input$playerInput %in% playernames$Name) {
      
      # Switch tabs
      updateTabItems(session, 'tabs', selected = 'playerstats')

      
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
      colnames(playerdata)[4] = "Team"
      
      # Skaters
      if (playernames$Position[playernames$ID==playerID] != "G"){
        
        # Create season summary tables
        tableData <<- playerdata[,c("G","Age","Team",
                                  "Scoring_G","Scoring_A","Scoring_PTS",
                                  "X...","Goals_EV","Goals_PP","Goals_SH",
                                  "Goals_GW","Assists_EV","Assists_PP",
                                  "Assists_SH","S","S.","TOI","HIT", "PIM",
                                  "BLK","FOW","FOL","FO.","Season")]
        
        
        
        totalTable <<- tableData %>%
          group_by(Season,Team) %>%
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
          group_by(Season,Team) %>%
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
            `FO %` = 100*sum(FOW,na.rm=T)/(sum(FOW,na.rm=T)+sum(FOL,na.rm=T)),
            FOW = sum(FOW,na.rm=T)/n(),
            PIM = sum(PIM,na.rm=T)/n(),
            Hits = sum(HIT,na.rm=T)/n(),
            Blocks = sum(BLK,na.rm=T)/n(),
            .groups = "keep"
          ) %>%
          arrange(desc(Season))
      
        per60Table <<- tableData %>%
          group_by(Season,Team) %>%
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
        
        tableData = playerdata[,c("G","Age","Team","Result","DEC",
                                  "Goalie.Stats_GA","Goalie.Stats_SA",
                                  "Goalie.Stats_SV","Goalie.Stats_SV.",
                                  "Goalie.Stats_SO","Season"
                                  )]
        
        totalTable <<- tableData %>%
          group_by(Season,Team) %>%
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
          group_by(Season,Team) %>%
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
    }
  })
  
  # Expand UI boxes
  # observeEvent(input$loadPlayerStats,once=T, ignoreInit = T, {
  #     if (!isFALSE(input$iscolseasonstatsbox)) {
  #       js$collapse("seasonstatsbox")
  #     }
  #     if (!isFALSE(input$iscolseasonrankingbox)) {
  #       js$collapse("seasonrankingbox")
  #     }
  # 
  # })
  
  
  
  
  ## Player Metrics - Season Stats =================================
  # Cell styling for reactables
  totalStyle <- function(value, index, name) {
    normalized <- (value - min(totalTable[name], na.rm = T)) /
      (max(totalTable[name], na.rm = T) - min(totalTable[name], na.rm = T))
    color <- GrnRedPalette(normalized)
    list(background = color,fontWeight = 600,fontSize=14,minWidth = 80,maxWidth = 80)
  }      
  averageStyle <- function(value, index, name) {
    normalized <- (value - min(averageTable[name], na.rm = T)) /
      (max(averageTable[name], na.rm = T) - min(averageTable[name], na.rm = T))
    color <- GrnRedPalette(normalized)
    list(background = color,fontWeight = 600,fontSize=14,minWidth = 80,maxWidth = 80)
  }      
  per60Style <- function(value, index, name) {
    normalized <- (value - min(per60Table[name], na.rm = T)) /
      (max(per60Table[name], na.rm = T) - min(per60Table[name], na.rm = T))
    color <- GrnRedPalette(normalized)
    list(background = color,fontWeight = 600,fontSize=14,minWidth = 80,maxWidth = 80)
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
            width = 0,
            headerStyle = list(background = "#deedf7",minWidth = 80,maxWidth = 80),
            style = totalStyle,
            vAlign ="center",
            html = T
          ),
          columns = list(
            Season = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=100,maxWidth=100),
                            headerStyle = list(background = "#deedf7",minWidth=100,maxWidth=100),
                            sticky = "left",vAlign ="center"),
            Team = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=75,maxWidth=75),
                        headerStyle = list(background = "#deedf7",minWidth=75,maxWidth=75),
                        sticky = "left",vAlign ="center"),
            Age = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=50,maxWidth=50),
                         headerStyle = list(background = "#deedf7",minWidth=50,maxWidth=50),
                         vAlign ="center"),
            GP = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=50,maxWidth=50),
                        headerStyle = list(background = "#deedf7",minWidth=50,maxWidth=50),
                        vAlign ="center"),
            `Avg TOI (mins)` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=75,maxWidth=75),
                                      headerStyle = list(background = "#deedf7",minWidth=75,maxWidth=75),
                                      vAlign ="center")
          ),
          borderless = TRUE,
          outline =TRUE,
          highlight = TRUE,
          striped =TRUE,
          defaultPageSize = 100,
          theme = reactableTheme(
            style = list(".rt-tr-striped-sticky" = list(backgroundColor = "#ffffff"),
                         ".rt-tr-highlight-sticky:hover" = list(backgroundColor = "#D7E4EC"),
                         ".rt-tr-striped-sticky:hover" = list(backgroundColor = "#D7E4EC")),
            backgroundColor = "#f6f8fc"
          )
        )
      })
    } else if (input$seasonStatsType == "pg") {
      output$playerStats <- renderReactable({
        reactable(
          averageTable,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            width = 0,
            headerStyle = list(background = "#deedf7",minWidth = 80,maxWidth = 80),
            style = averageStyle,
            vAlign ="center"
          ),
          columns = list(
            Season = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=100,maxWidth=100),
                            headerStyle = list(background = "#deedf7",minWidth=100,maxWidth=100),
                            sticky = "left",vAlign ="center"),
            Team = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=75,maxWidth=75),
                          headerStyle = list(background = "#deedf7",minWidth=75,maxWidth=75),
                          sticky = "left",vAlign ="center"),
            Age = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=50,maxWidth=50),
                         headerStyle = list(background = "#deedf7",minWidth=50,maxWidth=50),
                         vAlign ="center"),
            GP = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=50,maxWidth=50),
                        headerStyle = list(background = "#deedf7",minWidth=50,maxWidth=50),
                        vAlign ="center"),
            `Avg TOI (mins)` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=75,maxWidth=75),
                                      headerStyle = list(background = "#deedf7",minWidth=75,maxWidth=75),
                                      vAlign ="center"),
            Goals = colDef(format = colFormat(digits = 2))
          ),
          borderless = TRUE,
          outline =TRUE,
          highlight = TRUE,
          striped =TRUE,
          defaultPageSize = 100,
          fullWidth = TRUE,
          theme = reactableTheme(
            style = list(".rt-tr-striped-sticky" = list(backgroundColor = "#ffffff"),
                         ".rt-tr-highlight-sticky:hover" = list(backgroundColor = "#D7E4EC"),
                         ".rt-tr-striped-sticky:hover" = list(backgroundColor = "#D7E4EC")),
            backgroundColor = "#f6f8fc"
          )
        )
      })  
    } else {
      output$playerStats <- renderReactable({
        reactable(
          per60Table,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            width = 0,
            headerStyle = list(background = "#deedf7",minWidth = 80,maxWidth = 80),
            style = per60Style,
            vAlign ="center"
          ),
          columns = list(
            Season = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=100,maxWidth=100),
                            headerStyle = list(background = "#deedf7",minWidth=100,maxWidth=100),
                            sticky = "left",vAlign ="center"),
            Team = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=75,maxWidth=75),
                          headerStyle = list(background = "#deedf7",minWidth=75,maxWidth=75),
                          sticky = "left",vAlign ="center"),
            Age = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=50,maxWidth=50),
                         headerStyle = list(background = "#deedf7",minWidth=50,maxWidth=50),
                         vAlign ="center"),
            GP = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=50,maxWidth=50),
                        headerStyle = list(background = "#deedf7",minWidth=50,maxWidth=50),
                        vAlign ="center"),
            `Avg TOI (mins)` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=75,maxWidth=75),
                                      headerStyle = list(background = "#deedf7",minWidth=75,maxWidth=75),
                                      vAlign ="center")
          ),
          borderless = TRUE,
          outline =TRUE,
          highlight = TRUE,
          striped =TRUE,
          defaultPageSize = 100,
          fullWidth = TRUE,
          theme = reactableTheme(
            style = list(".rt-tr-striped-sticky" = list(backgroundColor = "#ffffff"),
                         ".rt-tr-highlight-sticky:hover" = list(backgroundColor = "#D7E4EC"),
                         ".rt-tr-striped-sticky:hover" = list(backgroundColor = "#D7E4EC")),
            backgroundColor = "#f6f8fc"
          )
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
      output$text1_1 = renderUI({h2("Goals")})
      output$valueBox1_1 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(goalsPerc))),paste0("Rank: ",goalsRank,"/",nrow(allPlayerData)),
                   color="red",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-red { background-color: ",GrnRedPalette(goalsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text1_2 = renderUI({h2("Assists")})
      output$valueBox1_2 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(assistsPerc))),paste0("Rank: ",round(assistsRank),"/",nrow(allPlayerData)),
                   color="yellow",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-yellow { background-color: ",GrnRedPalette(assistsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text1_3 = renderUI({h2("Points")})
      output$valueBox1_3 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(pointsPerc))),paste0("Rank: ",round(pointsRank),"/",nrow(allPlayerData)),
                   color="aqua",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-aqua { background-color: ",GrnRedPalette(pointsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text2_1 = renderUI({h2("Shots")})
      output$valueBox2_1 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(shotsPerc))),paste0("Rank: ",round(shotsRank),"/",nrow(allPlayerData)),
                   color="blue",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-blue { background-color: ",GrnRedPalette(shotsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text2_2 = renderUI({h2("PPG")})
      output$valueBox2_2 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(PPGPerc))),paste0("Rank: ",round(PPGRank),"/",nrow(allPlayerData)),
                   color="light-blue",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-light-blue { background-color: ",GrnRedPalette(PPGPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text2_3 = renderUI({h2("PPA")})
      output$valueBox2_3 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(PPAPerc))),paste0("Rank: ",round(PPARank),"/",nrow(allPlayerData)),
                   color="green",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-green { background-color: ",GrnRedPalette(PPAPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text3_1 = renderUI({h2("Hits")})
      output$valueBox3_1 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(HitsPerc))),paste0("Rank: ",round(HitsRank),"/",nrow(allPlayerData)),
                   color="navy",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-navy { background-color: ",GrnRedPalette(HitsPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text3_2 = renderUI({h2("Blocks")})
      output$valueBox3_2 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(BlocksPerc))),paste0("Rank: ",round(BlocksRank),"/",nrow(allPlayerData)),
                   color="teal",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-teal { background-color: ",GrnRedPalette(BlocksPerc/100)," !important; color: #000000 !important; }")))
        )
      })
      
      output$text3_3 = renderUI({h2("FOW")})
      output$valueBox3_3 = renderUI({
        tagList(
          valueBox(paste0(ordinal(round(FOWPerc))),paste0("Rank: ",round(FOWRank),"/",nrow(allPlayerData)),
                   color="olive",
                   width = 12),
          tags$style(HTML(paste0(".small-box.bg-olive { background-color: ",GrnRedPalette(FOWPerc/100)," !important; color: #000000 !important; }")))
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
          output$text1_1 = renderUI({h2("Wins")})
          output$valueBox1_1 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(winsPerc))),paste0("Rank: ",winsRank,"/",nrow(allPlayerData)),
                       color="red",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-red { background-color: ",GrnRedPalette(winsPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_2 = renderUI({h2("SO")})
          output$valueBox1_2 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(SOPerc))),paste0("Rank: ",round(SORank),"/",nrow(allPlayerData)),
                       color="yellow",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-yellow { background-color: ",GrnRedPalette(SOPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_3 = renderUI({h2("GA")})
          output$valueBox1_3 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(GAPerc))),paste0("Rank: ",round(GARank),"/",nrow(allPlayerData)),
                       color="blue",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-blue { background-color: ",GrnRedPalette(GAPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text2_1 = renderUI({h2("SA")})
          output$valueBox2_1 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(SAPerc))),paste0("Rank: ",round(SARank),"/",nrow(allPlayerData)),
                       color="aqua",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-aqua { background-color: ",GrnRedPalette(SAPerc/100)," !important; color: #000000 !important; }")))
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
          output$text1_1 = renderUI({h2("Save %")})
          output$valueBox1_1 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(savefracPerc))),paste0("Rank: ",savefracRank,"/",nrow(allPlayerData)),
                       color="red",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-red { background-color: ",GrnRedPalette(savefracPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_2 = renderUI({h2("GAA")})
          output$valueBox1_2 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(GAPerc))),paste0("Rank: ",round(GARank),"/",nrow(allPlayerData)),
                       color="yellow",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-yellow { background-color: ",GrnRedPalette(GAPerc/100)," !important; color: #000000 !important; }")))
            )
          })
          
          output$text1_3 = renderUI({h2("SAA")})
          output$valueBox1_3 = renderUI({
            tagList(
              valueBox(paste0(ordinal(round(SAPerc))),paste0("Rank: ",round(SARank),"/",nrow(allPlayerData)),
                       color="aqua",
                       width = 12),
              tags$style(HTML(paste0(".small-box.bg-aqua { background-color: ",GrnRedPalette(SAPerc/100)," !important; color: #000000 !important; }")))
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
  # Display certain UI depending on data source choice
  observeEvent(input$datasource, {
    if (input$datasource == "loc") {
      output$teamloadchoices = renderUI({
        tagList(
          column(width = 6,align = "center",
            fileInput(
              "teamFileLoad",
              label = h2("Load Team"),
              accept = ".csv"
            ),
          ),
          column(width = 6, alight = 'center',
            h2("Save Team"),
            downloadButton(
              "teamFileSave",
              label = "Save",
              icon = icon(lib="glyphicon", "download-alt")
            )
          )
        )
      })
      
    } else {
      output$teamloadchoices = renderUI(withProgress(message = "Loading data...", value = 0.5,{
        leagues <<- y_games(token)
        return(tagList(
          selectizeInput(
            "yahooleague",
            h2("Choose League"),
            choices = c("",leagues$league_name[as.numeric(leagues$league_season)+1 == currentSeason]),
            selected = "",
            width = "75%"
          )
        ))
      }))
    }
  })
  
  # Observe league choice
  observeEvent(input$yahooleague, ignoreInit = T, withProgress(message = "Loading...",value = 0.5,{
    if (input$yahooleague != "") {
      
      teams <<- y_teams(leagues$league_key[leagues$league_name==input$yahooleague &
                                          as.numeric(leagues$league_season)+1 == currentSeason],token) # list of all teams
      leaguerosters <<- y_rosters(leagues$league_key[leagues$league_name==input$yahooleague &
                                                     as.numeric(leagues$league_season)+1 == currentSeason],token) # list of all players on teams
      leaguerosters$selected_position_position[leaguerosters$selected_position_position=='IR+'] <<- 'Misc'
      leaguerosters$selected_position_position[leaguerosters$selected_position_position=='IR'] <<- 'Misc'
      leaguerosters$selected_position_position[leaguerosters$selected_position_position=='BN'] <<- 'Misc'
      leaguerosters$selected_position_position[leaguerosters$selected_position_position=='Util'] <<- 'Misc'
      leaguerosters$selected_position_position[leaguerosters$player_position_type=='G'] <<- 'G'
      
      leaguerosters$player_name_full <<- make.unique(leaguerosters$player_name_full)
      leaguerosters$player_name_full <<- gsub('.1', ' (2)', leaguerosters$player_name_full)
      leaguerosters$player_name_full <<-toTitleCase(tolower(leaguerosters$player_name_full))
      
      output$fantasyteam = renderUI({
        selectizeInput(
          "yahooteam",
          h2("Choose Team"),
          choices = teams$team_name,
          selected = leaguerosters$team_name[!is.na(leaguerosters$team_is_owned_by_current_login)][1]
        )
      })
    }
  }))
  
  #Initialize tracking variables
  numC_UI = 0
  numLW_UI = 0
  numRW_UI = 0
  numD_UI = 0
  numG_UI = 0
  numMisc_UI = 0
  
  numC = reactive({as.numeric(input$numC)})
  numLW = reactive({as.numeric(input$numLW)})
  numRW = reactive({as.numeric(input$numRW)})
  numD = reactive({as.numeric(input$numD)})
  numG = reactive({as.numeric(input$numG)})
  numMisc = reactive({as.numeric(input$numMisc)})

  numC_d = debounce(numC,100)
  numLW_d = debounce(numLW,100)
  numRW_d = debounce(numRW,100)
  numD_d = debounce(numD,100)
  numG_d = debounce(numG,100)
  numMisc_d = debounce(numMisc,100)
  
  reactiveValue = reactiveVal(0)
  reactiveValue1_d = debounce(reactive({reactiveValue()}), 100)
  reactiveValue2_d = debounce(reactive({reactiveValue()}), 500)
  
  updateTeamRV = reactiveVal(0)
  updateTeamRV_d = debounce(reactive({updateTeamRV()}), 500)
  
  # Centers UI
  output$centers = renderUI({
    # Randomize this reactivevalue to trigger other observers
    reactiveValue(runif(1))
    return(lapply(1:numC_d(), function(i) {
      selectizeInput(
        paste0("C",i),
        label = NULL,
        choices = "",
        selected = ""
      )
    }))
  })
  
  observeEvent(c(reactiveValue1_d()),ignoreNULL = FALSE, priority=3,{
    if (numC_d() != numC_UI) {
      numC_UI <<- numC_d()
    }
    for (i in 1:numC_UI) {
        updateSelectizeInput(
          session,
          paste0("C",i),
          choices = c("",sort(allFantasySkaters$Name)),
          server =T
        )
      }
  })
  
  
  # Left wings UI
  output$leftwings = renderUI({
    
    return(lapply(1:numLW_d(), function(i) {
      selectizeInput(
        paste0("LW",i),
        label = NULL,
        choices = "",
        selected = ""
      )
    }))
  })
  
  observeEvent(c(reactiveValue1_d()),ignoreNULL = FALSE, priority=3,{
    if (numLW_d() != numLW_UI) {
      numLW_UI <<- numLW_d()
    }
    for (i in 1:numLW_UI) {
      updateSelectizeInput(
        session,
        paste0("LW",i),
        choices = c("",sort(allFantasySkaters$Name)),
        server =T
      )
    }
  })
  
  # Right wings UI
  output$rightwings = renderUI({
    return(lapply(1:numRW_d(), function(i) {
      selectizeInput(
        paste0("RW",i),
        label = NULL,
        choices = "",
        selected = ""
      )
    }))
  })
  
  observeEvent(c(reactiveValue1_d()),ignoreNULL = FALSE, priority=3,{
    if (numRW_d() != numRW_UI) {
      numRW_UI <<- numRW_d()
    }
    
    for (i in 1:numRW_UI) {
      updateSelectizeInput(
        session,
        paste0("RW",i),
        choices = c("",sort(allFantasySkaters$Name)),
        server =T
      )
    }
  })
  
  # Defensemen  UI
  output$defensemen = renderUI({
    return(lapply(1:numD_d(), function(i) {
      selectizeInput(
        paste0("D",i),
        label = NULL,
        choices = "",
        selected = ""
      )
    }))
  })
  
  observeEvent(c(reactiveValue1_d()),ignoreNULL = FALSE, priority=3,{
    for (i in 1:numD_UI) {
      if (numD_d() != numD_UI) {
        numD_UI <<- numD_d()
      }
      updateSelectizeInput(
        session,
        paste0("D",i),
        choices = c("",sort(allFantasySkaters$Name)),
        server =T
      )
    }
  })
  
  # Goalies  UI
  output$goalies = renderUI({
    return(lapply(1:numG_d(), function(i) {
      selectizeInput(
        paste0("G",i),
        label = NULL,
        choices = "",
        selected = ""
      )
    }))
  })
  
  observeEvent(c(reactiveValue1_d()),ignoreNULL = FALSE, priority=3,{
    if (numG_d() != numG_UI) {
      numG_UI <<- numG_d()
    }
    for (i in 1:numG_UI) {
      updateSelectizeInput(
        session,
        paste0("G",i),
        choices = c("",sort(allFantasyGoalies$Name)),
        server =T
      )
    }
  })
  
  # Misc UI
  output$misc = renderUI({
    return(lapply(1:numMisc_d(), function(i) {
      selectizeInput(
        paste0("Misc",i),
        label = NULL,
        choices = "",
        selected = ""
      )
    }))
  })
  
  observeEvent(c(reactiveValue1_d()),ignoreNULL = FALSE, priority=3,{
    if (numMisc_d() != numMisc_UI) {
      numMisc_UI <<- numMisc_d()
    }
    for (i in 1:numMisc_UI) {
      updateSelectizeInput(
        session,
        paste0("Misc",i),
        choices = c("",sort(allFantasySkaters$Name)),
        server =T
      )
    }
  })
  
  
  # Dynamically update global team dataframe
  observeEvent(c(input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,input$C8,input$C9,
                 input$LW1,input$LW2,input$LW3,input$LW4,input$LW5,input$LW6,input$LW7,input$LW8,input$LW9,
                 input$RW1,input$RW2,input$RW3,input$RW4,input$RW5,input$RW6,input$RW7,input$RW8,input$RW9,
                 input$D1,input$D2,input$D3,input$D4,input$D5,input$D6,input$D7,input$D8,input$D9,
                 input$G1,input$G2,input$G3,input$G4,input$G5,input$G6,input$G7,input$G8,input$G9,
                 input$Misc1,input$Misc2,input$Misc3,input$Misc4,input$Misc5,input$Misc6,input$Misc7,input$Misc8,input$Misc9),{
    updateTeamRV(runif(1))
    
  })
  
  teamGLOB_r = reactiveValues() 
  observeEvent(updateTeamRV_d(),{
    isolate({
      # compile team members into one dataframe
      team = as.data.frame(matrix(ncol=3))
      colnames(team) = c("ID","Name","Position")
      
  
      # append centers
      for (i in 1:numC_UI) {
        playerName = input[[paste0("C",i)]]
        playerID = allFantasySkaters$ID[allFantasySkaters$Name == playerName]
        if (!is.null(playerName)) {
          if (!playerName=="") {
            team = rbind(team,c(playerID,playerName,"C"))
          }
        }
      }
      
      # append left wingers
      for (i in 1:numLW_UI) {
        
        playerName = input[[paste0("LW",i)]]
        playerID = allFantasySkaters$ID[allFantasySkaters$Name == playerName]
        if (!is.null(playerName)) {
          if (!playerName=="") {
            team = rbind(team,c(playerID,playerName,"LW"))
          }
        }
      }    
      
      # append right wingers
      for (i in 1:numRW_UI) {
        
        playerName = input[[paste0("RW",i)]]
        playerID = allFantasySkaters$ID[allFantasySkaters$Name == playerName]
        if (!is.null(playerName)) {
          if (!playerName=="") {
            team = rbind(team,c(playerID,playerName,"RW"))
          }
        }
      }        
      
          
      # append defensemen
      for (i in 1:numD_UI) {
        
        playerName = input[[paste0("D",i)]]
        playerID = allFantasySkaters$ID[allFantasySkaters$Name == playerName]
        if (!is.null(playerName)) {
          if (!playerName=="") {
            team = rbind(team,c(playerID,playerName,"D"))
          }
        }
      }    
      
      # append misc
      for (i in 1:numMisc_UI) {
        
        playerName = input[[paste0("Misc",i)]]
        playerID = allFantasySkaters$ID[allFantasySkaters$Name == playerName]
        if (!is.null(playerName)) {
          if (!playerName=="") {
            team = rbind(team,c(playerID,playerName,"Misc"))
          }
        }
      }
      
      # append goalies
      for (i in 1:numG_UI) {
        
        playerName = input[[paste0("G",i)]]
        playerID = allFantasyGoalies$ID[allFantasyGoalies$Name == playerName]
        if (!is.null(playerName)) {
          if (!playerName=="") {
            team = rbind(team,c(playerID,playerName,"G"))
          }
        }
      }
      
      # Update team df
      team = team[-1,]
      teamGLOB <<- team
      teamGLOB_r$df = teamGLOB
    })
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
  observeEvent(input$teamFileLoad,priority = 4, {
    filePath = input$teamFileLoad
    teamGLOB <<- read.csv(filePath$datapath)
    teamGLOB_r$df  = teamGLOB
    
    # Update team composition filters
    updateSelectInput(session,"numLW",selected ="0")
    updateSelectInput(session,"numC",selected ="0")
    updateSelectInput(session,"numRW",selected ="0")
    updateSelectInput(session,"numD",selected ="0")
    updateSelectInput(session,"numMisc",selected ="0")
    updateSelectInput(session,"numG",selected ="0")
    updateSelectInput(session,"numLW",selected =as.character(sum(teamGLOB$Position=="LW")))
    updateSelectInput(session,"numC",selected =as.character(sum(teamGLOB$Position=="C")))
    updateSelectInput(session,"numRW",selected =as.character(sum(teamGLOB$Position=="RW")))
    updateSelectInput(session,"numD",selected =as.character(sum(teamGLOB$Position=="D")))
    updateSelectInput(session,"numMisc",selected =as.character(sum(teamGLOB$Position=="Misc")))
    updateSelectInput(session,"numG",selected =as.character(sum(teamGLOB$Position=="G")))
  })
  
  # Load team from Yahoo league
  observeEvent(input$yahooteam, {
    if (input$yahooteam != ""){
      team = data.frame()
      team[1:nrow(leaguerosters),'Name'] = leaguerosters$player_name_full
      team$Position = leaguerosters$selected_position_position
      team = left_join(team, playernames[,c('Name','ID')],by = 'Name')
      team = team[leaguerosters$team_name==input$yahooteam,]
      team = team[,c("Name","Position","ID")]
      teamGLOB <<- team
      teamGLOB_r$df  = teamGLOB
      
      # Update team composition filters
      updateSelectInput(session,"numLW",selected ="0")
      updateSelectInput(session,"numC",selected ="0")
      updateSelectInput(session,"numRW",selected ="0")
      updateSelectInput(session,"numD",selected ="0")
      updateSelectInput(session,"numMisc",selected ="0")
      updateSelectInput(session,"numG",selected ="0")
      updateSelectInput(session,"numLW",selected =as.character(sum(teamGLOB$Position=="LW")))
      updateSelectInput(session,"numC",selected =as.character(sum(teamGLOB$Position=="C")))
      updateSelectInput(session,"numRW",selected =as.character(sum(teamGLOB$Position=="RW")))
      updateSelectInput(session,"numD",selected =as.character(sum(teamGLOB$Position=="D")))
      updateSelectInput(session,"numMisc",selected =as.character(sum(teamGLOB$Position=="Misc")))
      updateSelectInput(session,"numG",selected =as.character(sum(teamGLOB$Position=="G")))
      
    }
    
  })
  
  
  
  # fill in player names
  observeEvent(c(reactiveValue2_d()), priority = 2, {

    if (exists("teamGLOB")) {
      for (i in 1:numLW_d()) {
        updateSelectizeInput(session,
                             paste0("LW",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="LW"][i],
                             choices = c("",sort(allFantasySkaters$Name)),
                             server =T)
      }
      for (i in 1:numC_d()) {
        updateSelectizeInput(session,
                             paste0("C",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="C"][i],
                             choices = c("",sort(allFantasySkaters$Name)),
                             server =T)
      }
      for (i in 1:numRW_d()) {
        updateSelectizeInput(session,
                             paste0("RW",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="RW"][i],
                             choices = c("",sort(allFantasySkaters$Name)),
                             server =T)
      }
      for (i in 1:numD_d()) {
        updateSelectizeInput(session,
                             paste0("D",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="D"][i],
                             choices = c("",sort(allFantasySkaters$Name)),
                             server =T)
      }
      for (i in 1:numMisc_d()) {
        updateSelectizeInput(session,
                             paste0("Misc",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="Misc"][i],
                             choices = c("",sort(allFantasySkaters$Name)),
                             server =T)
      }
      for (i in 1:numG_d()) {
        updateSelectizeInput(session,
                             paste0("G",i),
                             selected = teamGLOB$Name[teamGLOB$Position=="G"][i],
                             choices = c("",sort(allFantasyGoalies$Name)),
                             server =T)
      }

      updateTeamRV(runif(1))
    }
  })
  
  

  
  
  
  
  
  ## Fantasy Team - Team Stats Box ===============================
  observeEvent(c(teamGLOB_r$df,input$goalsFP,input$assistsFP,input$pointsFP,
                 input$pppFP,input$shFP,input$shotsFP,input$hitsFP,input$blocksFP,input$fowFP,
                 input$gsFP,input$winsFP,input$gaFP,input$savesFP,input$soFP,input$teamStatRange,
                 input$teamStatType),ignoreInit=T,{
    
    # Get currently chosen fantasy team
    team = teamGLOB_r$df
    
    # Create data table
    teamSkaterData = as.data.frame(matrix(ncol=17,nrow=0))
    teamGoalieData = as.data.frame(matrix(ncol=8,nrow=0))
    if (nrow(team)>0){
      for (i in 1:nrow(team)) {
        if (team$Position[i] != "G") {
          
          # Read player data if it exists, read dummy file if not
          playerID = team$ID[i]
          if (file.exists(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason,".csv"))) {
            playerData = read.csv(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason,".csv"))
            playerTeam = playerData$Tm[nrow(playerData)]
          } else {
            playerData = read.csv(paste0(currDir,"/Data/Players/dummyfileskater.csv"))[-1,]
            playerTeam = ""
          }
          
          if (file.exists(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason-1,".csv"))) {
            playerDataLS = read.csv(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason-1,".csv"))
            playerTeamLS = playerDataLS$Tm[nrow(playerDataLS)]
          } else {
            playerDataLS = read.csv(paste0(currDir,"/Data/Players/dummyfileskater.csv"))[-1,]
            playerTeamLS = ""
          }
          
          # Format dates
          playerData$Date = as.Date(playerData$Date)
          playerDataLS$Date = as.Date(playerDataLS$Date)
          
          # Filter based on chosen date range if needed
          if (input$teamStatRange == "ls") {
            playerData = playerDataLS
            playerTeam = playerTeamLS
          } else if (input$teamStatRange != "s") {
            playerData = playerData[playerData$Date > today()-as.numeric(input$teamStatRange),]
          }
          
          # Get stat totals
          playerData = playerData %>%
            summarise(GP = nrow(playerData),
                      Goals = sum(Scoring_G),
                      Assists = sum(Scoring_A),
                      Points = sum(Scoring_PTS),
                      PPP = sum(Goals_PP)+sum(Assists_PP),
                      SHP = sum(Goals_SH)+sum(Assists_SH),
                      Shots = sum(S),Hits = sum(HIT),
                      Blocks = sum(BLK),
                      FOW = sum(FOW))
          
          # Append line #/pp line #
          lineData = data.frame()
          if (team$Name[i] %in% playerlines$Player) {
            lineData[1,"Linemate 1"] = playerlines$Linemate1[playerlines$Player == team$Name[i]]
            lineData[1,"Linemate 2"] = playerlines$Linemate2[playerlines$Player == team$Name[i]]
            lineData[1,"Line"] = ordinal(playerlines$Line[playerlines$Player == team$Name[i]])
            lineData[1,"PP Line"] = ordinal(playerlines$PP[playerlines$Player == team$Name[i]])
            
          } else {
            lineData[1,"Linemate 1"] = "<i>Unknown</i>"
            lineData[1,"Linemate 2"] = "<i>Unknown</i>"
            lineData[1,"Line"] = "<i>NA</i>"
            lineData[1,"PP Line"] = "<i>NA</i>"
          }
          
          # Append/merge all info
          playerData = cbind(Pos. = team$Position[i],
                             Name = paste0(team$Name[i]," (",playerTeam,")"),
                             lineData,
                             `FT PTS` = NA,
                             playerData)
          
          
          # Calculate fantasy points
          playerData$`FT PTS` = input$goalsFP*playerData$Goals + input$assistsFP*playerData$Assists +
            input$pointsFP*playerData$Points + input$pppFP*playerData$PPP + input$shFP*playerData$SHP + 
            input$shotsFP*playerData$Shots + input$blocksFP*playerData$Blocks + input$hitsFP*playerData$Hits +
            input$fowFP*playerData$FOW
          
          # Calc per game stats if needed
          if (input$teamStatType =="pg") {
            playerData[,-c(1:6,8)] = round(playerData[,-c(1:6,8)]/playerData$GP,2)
          }
          
          # Append player data to team table
          colnames(teamSkaterData) = colnames(playerData)
          teamSkaterData=rbind(teamSkaterData,playerData)
          
        } else {
          
          # Read player data if it exists, read dummy file if not
          playerID = team$ID[i]
          if (file.exists(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason,".csv"))) {
            playerData = read.csv(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason,".csv"))
            playerTeam = playerData$Tm[nrow(playerData)]
          } else {
            playerData = read.csv(paste0(currDir,"/Data/Players/dummyfilegoalie.csv"))[-1,]
            playerTeam = ""
            
          }
          if (file.exists(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason-1,".csv"))) {
            playerDataLS = read.csv(paste0(currDir,"/Data/Players/",playerID,"/",currentSeason-1,".csv"))
            playerTeamLS = playerDataLS$Tm[nrow(playerDataLS)]
          } else {
            playerDataLS = read.csv(paste0(currDir,"/Data/Players/dummyfilegoalie.csv"))[-1,]
            playerTeamLS = ""
          }
          
          # Format dates
          playerData$Date = as.Date(playerData$Date)
          playerDataLS$Date = as.Date(playerDataLS$Date)
          
          # Filter based on chosen date range if needed
          if (input$teamStatRange == "ls") {
            playerData = playerDataLS
            playerTeam = playerTeamLS
          } else if (input$teamStatRange != "s") {
            playerData = playerData[playerData$Date >= today()-as.numeric(input$teamStatRange),]
          }
          
          # Get stat totals
          playerData = playerData %>%
            summarize(GP = max(G), 
                      Wins = sum(playerData$DEC=='W'),
                      Shutouts = sum(Goalie.Stats_SO),
                      Saves = sum(Goalie.Stats_SV),
                      GA = sum(Goalie.Stats_GA))
          playerData = cbind(Pos. = team$Position[i],
                             Name = paste0(team$Name[i]," (",playerTeam,")"),
                             `FT PTS` = NA,
                             playerData)
          
          
          # Calculate fantasy points
          playerData$`FT PTS` = input$gsFP*playerData$GP + input$winsFP*playerData$Wins +
            input$savesFP*playerData$Saves + input$soFP*playerData$Shutouts + input$gaFP*playerData$GA
          
          # Calc per game stats if needed
          if (input$teamStatType =="pg") {
            playerData[,-c(1,2,4)] = round(playerData[,-c(1,2,4)]/playerData$GP,2)
          }
          
          # Append player data to team table
          colnames(teamGoalieData) = colnames(playerData)
          teamGoalieData=rbind(teamGoalieData,playerData)
          
        }
        
      }
    }
    
    # Replace NA/INF with 0
    teamSkaterData[,-c(1:6)][apply(teamSkaterData[,-c(1:6)],c(1,2),is.na)] = 0
    teamSkaterData[,-c(1:6)][apply(teamSkaterData[,-c(1:6)],c(1,2),is.infinite)] = 0
    teamGoalieData[,-c(1:6)][apply(teamGoalieData[,-c(1:6)],c(1,2),is.na)] = 0
    teamGoalieData[,-c(1:6)][apply(teamGoalieData[,-c(1:6)],c(1,2),is.infinite)] = 0
    
    # Skater reactable
    if (nrow(teamSkaterData)>0) {
      # Convert names to actionLinks
      teamSkaterData = setDT(teamSkaterData)
      teamSkaterData$Row = 1:nrow(teamSkaterData)
      teamSkaterData[, inputId := teamSkaterData$Name][, Name := as.character(actionLink(inputId = inputId, label = inputId, onclick = sprintf("Shiny.setInputValue(id = 'playerclick', value = %s);", Row))), by = inputId][, inputId := NULL]

      # Reactable styling
      skaterTableStyle <- function(value, index, name) {
        normalized <- (value - min(teamSkaterData[[name]], na.rm = T)) /
          (max(teamSkaterData[[name]], na.rm = T) - min(teamSkaterData[[name]], na.rm = T))
        color <- GrnRedPalette(normalized)
        list(background = color,fontWeight = 600,fontSize=14,minWidth = 75,maxWidth = 75)
      }   
      
      # Skater table output
      output$teamSkaterStats <- renderReactable({
        reactable(
          teamSkaterData,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 75,maxWidth = 75),
            style = skaterTableStyle,
            width = 0,
            html = T,
            vAlign ="center"
          ),
          columns = list(
            `Pos.` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth = 100,maxWidth = 100),
                            headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 100,maxWidth = 100),
                            sticky = "left",vAlign ="center"),
            Name = colDef(style = list(fontWeight = 600,fontSize=14,minWidth = 200,maxWidth = 200),
                          headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 200,maxWidth = 200),
                          sticky = "left",vAlign ="center"),
            `Linemate 1` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth = 150,maxWidth = 150),
                                  headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 150,maxWidth = 150),
                                  vAlign ="center"),
            `Linemate 2` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth = 150,maxWidth = 150),
                                  headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 150,maxWidth = 150),
                                  vAlign ="center"),
            `Line` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth = 50,maxWidth = 50),
                              headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 50,maxWidth = 50),
                              vAlign ="center"),
            `PP Line` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth = 50,maxWidth = 50),
                                  headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 50,maxWidth = 50),
                                 vAlign ="center"),
            Row = colDef(show=F)
          ),
          outlined = TRUE, 
          borderless = TRUE,
          highlight = TRUE,
          striped = TRUE,
          defaultPageSize = 100,
          fullWidth = T,
          theme = reactableTheme(
            style = list(".rt-tr-striped-sticky" = list(backgroundColor = "#ffffff"),
                         ".rt-tr-highlight-sticky:hover" = list(backgroundColor = "#D7E4EC"),
                         ".rt-tr-striped-sticky:hover" = list(backgroundColor = "#D7E4EC")),
            backgroundColor = "#f6f8fc"
          )
        )
      })
      
      
    }
    # Goalie reactable
    if (nrow(teamGoalieData)>0) {
      teamGoalieData = setDT(teamGoalieData)
      teamGoalieData$Row = (1+nrow(teamSkaterData)):(nrow(teamSkaterData)+nrow(teamGoalieData))
      teamGoalieData[, inputId := teamGoalieData$Name][, Name := as.character(actionLink(inputId = inputId, label = inputId, onclick = sprintf("Shiny.setInputValue(id = 'playerclick', value = %s);", Row))), by = inputId][, inputId := NULL]
      
      
      # Reactable styling
      goalieTableStyle <- function(value, index, name) {
        normalized <- (value - min(teamGoalieData[[name]], na.rm = T)) /
          (max(teamGoalieData[[name]], na.rm = T) - min(teamGoalieData[[name]], na.rm = T))
        color <- GrnRedPalette(normalized)
        list(background = color,fontWeight = 600,fontSize=14,minWidth = 75, maxWidth = 75)
      }   
      
      # Goalie table output
      output$teamGoalieStats <- renderReactable({
        reactable(
          teamGoalieData,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            headerStyle = list(background = "#deedf7",fontSize=16,minWidth = 75, maxWidth=75),
            style = goalieTableStyle,
            maxWidth = 1000,
            html = T,
            vAlign ="center"
          ),
          columns = list(
            `Pos.` = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=100,maxWidth=100),
                            headerStyle = list(background = "#deedf7",fontSize=16,minWidth=100,maxWidth=100),
                            sticky = "left",vAlign ="center"),
            Name = colDef(style = list(fontWeight = 600,fontSize=14,minWidth=200,maxWidth=200),
                          headerStyle = list(background = "#deedf7",fontSize=16,minWidth=200,maxWidth=200),
                          sticky = "left",vAlign ="center"),
            Row = colDef(show=F)
          ),
          outlined = TRUE, 
          borderless = TRUE,
          highlight = TRUE,
          defaultPageSize = 100,
          striped= T,
          fullWidth = T,
          theme = reactableTheme(
            style = list(".rt-tr-striped-sticky" = list(backgroundColor = "#ffffff"),
                         ".rt-tr-highlight-sticky:hover" = list(backgroundColor = "#D7E4EC"),
                         ".rt-tr-striped-sticky:hover" = list(backgroundColor = "#D7E4EC")),
            backgroundColor = "#f6f8fc"
          )
        )
      })
      
    }
    
    
  })
  
  # Switch tabs on playername click
  observeEvent(input$playerclick,priority = 10,{
    if (input$playerclick != 0) {
      shinyjs::runjs("window.scrollTo(0, 0)")
      updateTabItems(session,'tabs',selected = 'playerstats')
      updateSelectizeInput(session,'playerInput',choices = playernames$Name,selected = teamGLOB$Name[input$playerclick],server=T)
    
      }
  })
  
  playerclick_d = debounce(reactive({as.numeric(input$playerclick)}),500)

  # Click load button automatically
  observe({
    num = playerclick_d()
    if (!length(num)==0) {
      if (num != 0) {
        click('loadPlayerStats')
        
        # Reset the input value to 0
        session$sendCustomMessage("playerclick", 0)
      }
    }
    
  })
  
  
  


  ## Account creation/login ===============================
  # Create userbase file if one does not exist, otherwise read in user base
  if (!file.exists(paste0(currDir,"/Data/Users/user_base.rds"))) {
    dir.create(paste0(currDir,"/Data/Users/Tokens"))
    user_base <<- tibble::tibble(
      user = "testuser",
      password = purrr::map_chr("pass1", sodium::password_store),
      permissions = "standard",
      ID = 1,
      favteam = "Detroit Red Wings"
    )
    
    saveRDS(user_base, paste0(currDir,"/Data/Users/user_base.rds"))
  } else {
    user_base <<- readRDS(paste0(currDir,"/Data/Users/user_base.rds"))
  }
  
  
  
  # Observe login button in top right
  observeEvent(input$loginopen, ignoreInit = T,{
    credentials <<- loginServer(
      id = "login",
      data = user_base,
      user_col = user,
      pwd_col = password,
      sodium_hashed = TRUE,
      log_out = reactive(logout_init()),
      reload_on_logout = TRUE,
    )
    
    logout_init = shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )

    showModal(modalDialog(size="s",
                          footer = NULL,
                          easyClose = TRUE,
                          shinyauthr::loginUI(id = "login")
    ))
    
    
    # Observe login button after inputting credentials
    observeEvent(input$`login-button`, ignoreInit = T, {
      req(credentials()$user_auth)    
      removeModal()
      hideElement('loginopen')
      hideElement('createacc')
      output$user <- renderUser({
        dashboardUser(
          name = strong(credentials()$info[1]), 
          image =logos$File[logos$Team==as.character(credentials()$info[5])], 
          footer = p("The footer", class = "text-center"),
          fluidRow(
            dashboardUserItem(
              width = 6,
              uiOutput("yahooconnectui")
            ),
            dashboardUserItem(
              width = 6,
              logoutUI(id = "logout")
            )
          )
        )
      })
    })

  })
  
  observeEvent(input$`login-button`, ignoreInit = T, delay(1000,{
    print(file.exists(paste0(currDir,"/Data/Users/Tokens/",credentials()$info[1],".Rds")))
    if (file.exists(paste0(currDir,"/Data/Users/Tokens/",credentials()$info[1],".Rds"))) {
      token <<- readRDS(paste0(currDir,"/Data/Users/Tokens/",credentials()$info[1],".Rds"))
      token$refresh()
      hideElement("yahooconnect")
      updateRadioGroupButtons(session,"datasource",choices = c("Local" = "loc", "Yahoo" = "yh"),status = "primary")
      output$yahooconnectui = renderUI({HTML('<p style="color:green;font-weight:700;font-size:16px;">Connected to Yahoo!</p>')})
    } else {
      output$yahooconnectui = renderUI({actionButton("yahooconnect","Connect to Yahoo")})
    }
    
  }))
  
  # Create account button
  observeEvent(input$createacc,ignoreInit = T, {
    showModal(
      modalDialog(
        size="s",
        title = fluidRow(column(width = 12,align = "center",
                  h1("Create a New Account")
        )),
        footer = NULL,
        easyClose = TRUE,
        textInput("createUsername","Username"),
        passwordInput("createPass1","Password"),
        passwordInput("createPass2","Re-type Password"),
        selectizeInput("favteam","(Optional) Favourite Team",choices = c("",logos$Team)),
        fluidRow(column(width = 12,align = "center",
          actionButton("createaccdone","Create Account")
        )),
        fluidRow(column(width = 12,align = "center",
          uiOutput("createacctext")
        ))
      )
    )
    
    # Account creation 
    observeEvent(input$createaccdone, ignoreInit = T, {
      # Check if username exists
      if (input$createUsername %in% user_base$user) {
        output$createacctext = renderUI({
          HTML('<p style="color:red;font-weight:700;font-size:16px;">Username already exists!</p>')
        })
      # Check if passwords match 
      } else if (input$createPass1 != input$createPass2) {
        output$createacctext = renderUI({
          HTML('<p style="color:red;font-weight:700;font-size:16px;">Passwords do not match!</p>')
        })
      # Check if password is at least 6 characters  
      } else if (nchar(input$createPass1) < 6) {
        output$createacctext = renderUI({
          HTML('<p style="color:red;font-weight:700;font-size:16px;">Password must be at least 6 characters!</p>')
        })
      } else {
        # Append user to dataframe
        user_base <<- user_base %>% 
          tibble::add_row(
            user = input$createUsername,
            password = purrr::map_chr(input$createPass1, sodium::password_store), 
            permissions = "standard",
            ID = nrow(user_base)+1,
            favteam = ifelse(input$favteam == "","NHL",input$favteam))
        
        # Save to rds file
        saveRDS(user_base, paste0(currDir,"/Data/Users/user_base.rds"))
        
        output$createacctext = renderUI({
          HTML('<p style="color:green;font-weight:700;font-size:16;">Account created successfully, please login.</p>')
        })
        delay(2500,{removeModal()})
        
        
      }
      
    })
  })
  
  
  # "Connect to yahoo" button
  observeEvent(input$yahooconnect,ignoreInit = T, {
    
    if (file.exists(paste0(currDir,"/Data/Users/Tokens/",credentials()$info[1],".Rds"))) {
      token <<- readRDS(paste0(currDir,"/Data/Users/Tokens/",credentials()$info[1],".Rds"))
      token$refresh()
      hideElement("yahooconnect")
      updateRadioGroupButtons(session,"datasource",choices = c("Local" = "loc", "Yahoo" = "yh"),status = "primary")
      output$yahooconnectui = renderUI({HTML('<p style="color:green;font-weight:700;font-size:16px;">Connected to Yahoo!</p>')})
    } else {
      browseURL("https://api.login.yahoo.com/oauth2/request_auth?client_id=dj0yJmk9OXU3WjZCUmxsYkRXJmQ9WVdrOWJXbHhVbEZhUVZNbWNHbzlNQT09JnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PTc4&redirect_uri=oob&response_type=code")
      showModal(modalDialog(size="s",
                            title = h1("Input Yahoo Authorization Code",align="center"),
                            textInput("yahooCode", "",placeholder = 'e.g., 7rtq8c'),
                            footer = tagList(
                              fluidRow(column(width=12,align="center",
                                actionButton("confirm", "Ok"),
                                modalButton("Cancel")
                              )),
                              fluidRow(column(width=12,align="center",
                                uiOutput("codetext")                
                              ))
                            )
      ))
    }
    
    
    
  })
  
  # Yahoo access code input
  observeEvent(input$confirm,ignoreInit = T, {
    
    # Get and save access token
    app_name = "Fantasy Hockey Analyzer"
    my_key = "dj0yJmk9OXU3WjZCUmxsYkRXJmQ9WVdrOWJXbHhVbEZhUVZNbWNHbzlNQT09JnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PTc4"
    my_secret = fromJSON(file = paste0(currDir,"/Data/secret.json"))$secret
    redirect = "oob"
    
    code = input$yahooCode
    myapp = httr::oauth_app(appname = app_name, 
                             key = my_key,
                             secret = my_secret,
                             redirect_uri = redirect)
    result = tryCatch({
       token <<- httr::oauth2.0_token(httr::oauth_endpoints("yahoo"),
                                  myapp,
                                  credentials = oauth2.0_access_token(httr::oauth_endpoints("yahoo"),myapp,code),
                                  cache = 'Data/test.token')
        saveRDS(token,file = paste0(currDir,"/Data/Users/Tokens/",credentials()$info[1],".Rds"))
        output$codetext = renderUI({
          HTML('<p style="color:green;font-weight:700;font-size:16px;">Successfully connected!</p>')
        })
        hideElement("yahooconnect")
        updateRadioGroupButtons(session,"datasource",choices = c("Local" = "loc", "Yahoo" = "yh"),status ="primary")
        delay(2500,{removeModal()})
        
        
        
    }, warning = function(w) {
      output$codetext = renderUI({
        HTML('<p style="color:red;font-weight:700;font-size:16px;">Incorrect code!</p>')
      })
    }, error = function(e) {
      output$codetext = renderUI({
        HTML('<p style="color:red;font-weight:700;font-size:16px;">Incorrect code!</p>')
      })
    })
  })
  
  
  
  
})