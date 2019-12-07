server <-function(input, output, session) {
  allYears <- c('2018','2017','2016','2015','2014')
  team_choices <- unique(vF_teams_DT$long.name)
  arena_choices <- unique(vF_teams_DT$venue.name)
  player_choices <- unique(paste(vF_player_info[vF_player_info$player.id %in% vF_player_season_data[vF_player_season_data$season %in% allYears]$player.id]$firstName, 
                                 vF_player_info[vF_player_info$player.id %in% vF_player_season_data[vF_player_season_data$season %in% allYears]$player.id]$lastName, 
                                 sep=' '))
 
  image_file <- "full-rink.png"
  observeEvent(input$tabs,{
    updateSelectInput(session,'tabs')
  })
  observeEvent(input$leftTeamArena,{
    updateSelectInput(session,inputId='leftTeam', selected = input$leftTeamArena)
  }) 
  observeEvent(input$rightTeamArena,{
    updateSelectInput(session,inputId = 'rightTeam', selected = input$rightArena)
  }) 
  team_Perf1 <- reactive({
    return(input$teamPerf1)
  })
  team_Perf2 <- reactive({
    return(input$teamPerf2)
  })
  team_Perf3 <- reactive({
    return(input$teamPerf3)
  })
  left_team_id <- reactive({
    return(vF_teams_DT[long.name == input$leftTeam]$team.id)
  })  
  right_team_id <- reactive({
    return(vF_teams_DT[long.name == input$rightTeam]$team.id)
  }) 
  selected_year <- reactive({
    return(input$year)
  })
  selected_yearStat <- reactive({
    return(input$yearStat)
  })
  selected_yearPerf <- reactive({
    return(input$yearTeam)
  })
  selected_yearPlayer <- reactive({
    return(input$yearPlayer)
  })
  leftHome <- reactive({
    return(input$leftHome)
  })
  rightHome <- reactive({
    return(input$rightHome)
  })
  df_left <- reactive({
    year <- selected_year()
    upper_lim <- as.numeric(year) +1
    upper_lim <- paste0(as.character(upper_lim),'000000')
    lower_lim <- as.numeric(year) -1
    lower_lim <- paste0( as.character(lower_lim),'999999')
    if (leftHome() == 'Home') {
      games_left <- vF_game_info[home.teamID == left_team_id()]
    }else if (leftHome() == "Away") {
      games_left <- vF_game_info[away.teamID == left_team_id()]
    }
    return(vF_game_plays[team.id.for == left_team_id() & (as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                         & game.id %in% games_left$game.id])
  }) 
  df_right <- reactive({
    year <- selected_year()
    upper_lim <- as.numeric(year) +1
    upper_lim <- paste0(as.character(upper_lim),'000000')
    lower_lim <- as.numeric(year) -1
    lower_lim <- paste0( as.character(lower_lim),'999999')
    if (rightHome() == 'Home') {
      games_right <- vF_game_info[home.teamID == right_team_id()]
    }else if (rightHome() == "Away") {
      games_right <- vF_game_info[away.teamID == right_team_id()]
    }
    return(vF_game_plays[team.id.for == right_team_id() & (as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                         & game.id %in% games_right$game.id])
  }) 
  output$performanceByPlayer <- renderUI({
    tabsetPanel(type = 'tabs',
                tabPanel("Summary",
                         fluidPage(theme = shinytheme("slate"),
                                   fluidRow(
                                     align='center',
                                     #collapsible box for main inputs
                                     box(solidHeader = T, width = '100%',
                                         title = 'Arena', status = "primary", background = "blue",
                                         div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                                             selectInput('yearPlayer', 'By Season', choices = c(allYears,'All'), selected = '2018', multiple = FALSE,
                                                         selectize = TRUE, width = NULL, size = NULL))
                                     )
                                   ),
                                   fluidRow(
                                     align = "center",
                                     DT::dataTableOutput("table_statistic_player")
                                   )
                         )
                ),
                tabPanel("Visualization",
                         fluidPage(theme = shinytheme("slate"),
                                   fluidRow(
                                     align='center',
                                     #collapsible box for main inputs
                                     box(solidHeader = T, width = '100%',
                                         title = 'Player', status = "primary", background = "blue",
                                         div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                                             selectInput('playerStat', 'Select Player', choices = player_choices, selected = '', multiple = FALSE,
                                                         selectize = TRUE, width = NULL, size = NULL))
                                     )
                                   )
                                   #TODO: Add Plots for selected player
                         )
                )
    )
  })
  output$performanceByTeam <- renderUI({
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, width = '100%',
                    title = 'Compare upto 3 teams', status = "primary", background = "blue",
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('teamPerf1', 'Select Team 1', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('teamPerf2', 'Select Team 2', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('teamPerf3', 'Select Team 3', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                        selectInput('yearTeam', 'Year', choices = c('2018','2017','2016','2015','2014','All'), selected = '2018', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align='center',
                box( plotOutput("Plot1"), status = "primary", title = "Title1", width = 4, solidHeader = TRUE),
                box( plotOutput("Plot2"), status = "primary", title = "Title1", width = 4, solidHeader = TRUE),
                box( plotOutput("Plot3"), status = "primary", title = "Title2", width = 4, solidHeader = TRUE)
              )
    )
  })
  output$statisticBySeason <- renderUI({
      tabsetPanel(type = 'tabs',
          tabPanel("Summary",
            fluidPage(theme = shinytheme("slate"),
                      fluidRow(
                        align='center',
                        #collapsible box for main inputs
                        box(solidHeader = T, width = '100%',
                            title = 'Arena', status = "primary", background = "blue",
                            div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                                selectInput('yearStat', 'By Season', choices = c('2018','2017', '2016', '2015', '2014', 'All'), selected = '2018', multiple = FALSE,
                                            selectize = TRUE, width = NULL, size = NULL))
                        )
                      ),
                      fluidRow(
                        align = "center",
                        DT::dataTableOutput("table_statistic_arena")
                      ),
                      fluidRow(
                        align='center',
                        box(solidHeader = T, width = '100%',
                            title = 'Team', status = "primary", background = "blue"
                        )
                      ),
                      fluidRow(
                        align = "center",
                        DT::dataTableOutput("table_statistic_team")
                      )
                    )
                  ),
            tabPanel("Visualization",
                     fluidPage(theme = shinytheme("slate"),
                               fluidRow(
                                 align='center',
                                 box(solidHeader = T, width = '100%',
                                     title = 'Arena', status = "primary", background = "blue"
                                 )
                               ),
                               fluidRow(
                                 align='center',
                                 box( plotOutput("Plot4"), status = "primary", width = 12, solidHeader = TRUE)
                               ),
                               fluidRow(
                                 align='center',
                                 box(solidHeader = T, width = '100%',
                                     title = 'Team', status = "primary", background = "blue"
                                 )
                               ),
                               fluidRow(
                                 align='center',
                                 box( plotOutput("Plot5"), status = "primary", width = 12, solidHeader = TRUE)
                               )
                     )
            )
      )
  })
  output$shotByTeam <- renderUI({
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary", background = "blue",
                    #input selections are inside div so we can place left and right inputs side by side
                    div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: -1em;",
                        titlePanel('Left'),
                        selectInput('leftTeam', 'Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                        titlePanel('Right'),
                        selectInput('rightTeam', 'Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput('rightHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                        selectInput('year', 'By Year', choices = c('2018','2017', '2016', '2015', '2014'), selected = '2018', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink Layout", width = '100%',
                    plotlyOutput("icemap_team"))
              )
    )
  })
  output$shotByArena <- renderUI({
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary", background = "blue",
                    #input selections are inside div so we can place left and right inputs side by side
                    div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: -1em;",
                        titlePanel('Left'),
                        selectInput('leftTeam2', 'Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput('leftHome2', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                        titlePanel('Right'),
                        selectInput('rightTeam2', 'Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput('rightHome2', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                        selectInput('shots2', 'Shot Type', choices = c('SHOT','GOAL'), selected = 'GOAL', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink Layout", width = '100%',
                    plotlyOutput("icemap_Arena"))
              )
    )
  })
  output$table_statistic_player <- DT::renderDataTable({
    playerYear <- selected_yearPlayer()
    if (playerYear != 'All'){
      dataPlayerYear <- vF_player_season_data[vF_player_season_data$season == playerYear]
    } else {
      dataPlayerYear <- vF_player_season_data[vF_player_season_data$season %in% allYears]
    }
    df <- dataPlayerYear[,c('player.id','stat.assists','stat.goals','stat.games','stat.shots','stat.gameWinningGoals')]
    df <- df %>% drop_na()
    dfGroupByPlayer <- aggregate(. ~ player.id, df, sum)
    dfGroupByPlayer <- merge(dfGroupByPlayer, vF_player_info[,c('player.id','firstName','lastName')], by='player.id')
    dfGroupByPlayer$Player <- paste(dfGroupByPlayer$firstName, dfGroupByPlayer$lastName, sep=' ')
    dfGroupByPlayer <- subset(dfGroupByPlayer, select= names(dfGroupByPlayer) != c('player.id', 'firstName', 'lastName'))
    dfGroupByPlayer <- dfGroupByPlayer[,c('Player','stat.games','stat.goals','stat.shots','stat.gameWinningGoals','stat.assists')]
    DT::datatable(dfGroupByPlayer, options = list(orderClasses = TRUE, pageLength = 10))
  })
  output$table_statistic_arena <- DT::renderDataTable({
    statYear <- selected_yearStat()
    if (statYear != 'All'){
      dataStatYear <- vF_game_info[vF_game_info$season == statYear]
    } else {
      dataStatYear <- vF_game_info[vF_game_info$season %in% allYears]
    }
    
    df <- dataStatYear[,c('name','home.win','away.win','home.goals','away.goals')]
    dfGroupByArena <- aggregate(. ~ name , df, sum)
    colnames(dfGroupByArena)[1] <- 'venue.name'
    dfGroupByArena <- merge(dfGroupByArena, vF_teams_DT[,c('venue.name', 'venue.city', 'locationName', 'division.name')], by = 'venue.name')
    DT::datatable(dfGroupByArena, options = list(orderClasses = TRUE, lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })
  output$table_statistic_team <- DT::renderDataTable({
    statYear <- selected_yearStat()
    if (statYear != 'All'){
      dataStatYear <- vF_game_info[vF_game_info$season == statYear]
    } else {
      dataStatYear <- vF_game_info
    }
    homeTeam <- dataStatYear[,c('home.teamID', 'home.win', 'home.goals')]
    awayTeam <- dataStatYear[,c('away.teamID', 'away.win', 'away.goals')]
    colnames(homeTeam)[1] <- colnames(awayTeam)[1] <- 'team.id'
    homeTeamGroupBy <- aggregate( .~team.id, homeTeam, sum)
    awayTeamGroupBy <- aggregate( .~ team.id, awayTeam, sum)
    teamStat <- merge(homeTeamGroupBy, awayTeamGroupBy, by = 'team.id')
    teamStat <- merge(teamStat, vF_teams_DT[,c('team.id', 'long.name', 'venue.name', 'venue.city')], by = 'team.id')
    teamStat <- teamStat[,c('long.name', 'venue.name', 'venue.city', 'home.win', 'away.win', 'home.goals', 'away.goals')]
    DT::datatable(teamStat,options = list(orderClasses = TRUE, lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  output$icemap_team <- renderPlotly({
      df_left <- df_left()
      df_right <- df_right()
      df_left_shots <- df_left[result.eventTypeId == 'SHOT']
      df_left_goals <- df_left[result.eventTypeId == 'GOAL']
      df_right_shots <- df_right[result.eventTypeId == 'SHOT']
      df_right_goals <- df_right[result.eventTypeId == 'GOAL']
      df <- rbind(df_left, df_right)
      #set the rink image and plot
      txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
      df %>% 
        plot_ly()  %>% 
        add_markers(
          data = df_left_shots,
          hoverinfo='skip',
          x = ~l.x, y=~coordinates.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01))
        ) %>%
        add_markers(
          data = df_right_shots,
          hoverinfo='skip',
          x = ~r.x, y=~coordinates.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01))
          #alpha = min(750/nrow(df),0.01)
        ) %>%
        add_markers(
          data = df_left_goals,
          x = ~l.x, y=~coordinates.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_left_goals))
          #alpha = .8
        ) %>%
        add_markers(
          data = df_right_goals,
          x = ~r.x, y=~coordinates.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_right_goals))
          #alpha = .8
        ) %>%
        layout(
          xaxis = list(range = c(-110,110)),
          yaxis = list(range = c(-50,50)),
          images= list(
            source= paste('data:image/png;base64', txt, sep=','),
            xref= "x",
            yref= "y",
            x = 0,
            y = 0,
            sizex = 240,
            sizey = 100,
            opacity = 0.8,
            layer = "below",
            xanchor = "center",
            yanchor = "middle",
            sizing = "stretch"
          )
        )
    })
  output$icemap_Arena <- renderPlotly({
      df_left <- df_left()
      df_right <- df_right()
      df <- rbind(df_left, df_right)
      #set the rink image and plot
      txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
      df %>% 
        plot_ly()  %>% 
        add_markers(
          data = df_left_shots,
          hoverinfo='skip',
          x = ~l.x, y=~coordinates.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01))
        ) %>%
        add_markers(
          data = df_right_shots,
          hoverinfo='skip',
          x = ~r.x, y=~coordinates.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01))
          #alpha = min(750/nrow(df),0.01)
        ) %>%
        add_markers(
          data = df_left_goals,
          x = ~l.x, y=~coordinates.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_left_goals))
          #alpha = .8
        ) %>%
        add_markers(
          data = df_right_goals,
          x = ~r.x, y=~coordinates.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_right_goals))
          #alpha = .8
        ) %>%
        layout(
          xaxis = list(range = c(-110,110)),
          yaxis = list(range = c(-50,50)),
          images= list(
            source= paste('data:image/png;base64', txt, sep=','),
            xref= "x",
            yref= "y",
            x = 0,
            y = 0,
            sizex = 240,
            sizey = 100,
            opacity = 0.8,
            layer = "below",
            xanchor = "center",
            yanchor = "middle",
            sizing = "stretch"
          )
        )
    })
}