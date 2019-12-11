server <-function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  allYears <- c('2018','2017','2016','2015','2014')
  team_choices <- unique(vF_teams_DT$long.name)
  team_choices <- team_choices[order(team_choices)]
  arena_choices <- unique(vF_teams_DT$venue.city)
  arena_choices <- arena_choices[order(arena_choices)]
  player_type_choices <- c('Offence', 'Defence','Goalie')
  vF_player_info$fullName <- paste(vF_player_info$firstName,vF_player_info$lastName,sep=' ')
  vF_game_plays$game_and_event_id <- paste(vF_game_plays$game.id,vF_game_plays$about.eventIdx,sep='')
  vF_game_plays_players$game_and_event_id <- paste(vF_game_plays_players$game.id,vF_game_plays_players$eventIdx,sep='')
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
  observe({
    updateSelectInput(session, inputId = "playerPerf1", choices = player_choices_perf())
  })
  observe({
    updateSelectInput(session, inputId = "playerPerf2", choices = player_choices_perf())
  })
  player_choices <- reactive({
    player_choices <- unique(vF_player_info[vF_player_info$player.id %in% vF_player_season_data[vF_player_season_data$season == input$year]$player.id]$fullName)
    return(player_choices[order(player_choices)])
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
  team_stat_type <- reactive({
    return(input$statType)
  })
  left_team_id <- reactive({
    return(vF_teams_DT[long.name == input$leftTeam]$team.id)
  })  
  right_team_id <- reactive({
    return(vF_teams_DT[long.name == input$rightTeam]$team.id)
  }) 
  arena_team_id <- reactive({
    return(vF_teams_DT[venue.city == input$arena]$team.id)
  })
  arena_team <- reactive({
    return(vF_teams_DT[venue.city == input$arena]$long.name)
  })
  arena_name <- reactive({
    return(vF_teams_DT[venue.city == input$arena]$venue.name)
  })
  left_player_id <- reactive({
    return(vF_player_info[fullName == input$leftPlayer]$player.id)
  })
  right_player_id <- reactive({
    return(vF_player_info[fullName == input$rightPlayer]$player.id)
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
  player_choices_perf <- reactive({
    if (req(input$playerCat) == 'Offence'){
      playerIds <- vF_player_info[primaryPosition.code %in% c('R','L','C')]$player.id 
    } else if (req(input$playerCat) == 'Defence') {
      playerIds <- vF_player_info[primaryPosition.code %in% c('D')]$player.id 
    } else if (req(input$playerCat) == 'Goalie') {
      playerIds <- vF_player_info[primaryPosition.code %in% c('G')]$player.id 
    } else {
      playerIds <- vF_player_info[primaryPosition.code %in% c('D')]$player.id 
    }
    return(sort(unique(paste(vF_player_info[vF_player_info$player.id %in% playerIds]$firstName, 
                              vF_player_info[vF_player_info$player.id %in% playerIds]$lastName, sep=' '))))
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
  df_arena_home <- reactive({
    year <- selected_year()
    upper_lim <- as.numeric(year) +1
    upper_lim <- paste0(as.character(upper_lim),'000000')
    lower_lim <- as.numeric(year) -1
    lower_lim <- paste0( as.character(lower_lim),'999999')
    games_arena <- vF_game_info[name == arena_name()]
    return(vF_game_plays[team.id.for == arena_team_id() & (as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                         & game.id %in% games_arena$game.id])
  }) 
  df_arena_away <- reactive({
    year <- selected_year()
    upper_lim <- as.numeric(year) +1
    upper_lim <- paste0(as.character(upper_lim),'000000')
    lower_lim <- as.numeric(year) -1
    lower_lim <- paste0( as.character(lower_lim),'999999')
    games_arena <- vF_game_info[name == arena_name()]
    return(vF_game_plays[team.id.for != arena_team_id() & (as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                         & game.id %in% games_arena$game.id])
  }) 
  df_left_player <- reactive({
    year <- selected_year()
    upper_lim <- as.numeric(year) +1
    upper_lim <- paste0(as.character(upper_lim),'000000')
    lower_lim <- as.numeric(year) -1
    lower_lim <- paste0( as.character(lower_lim),'999999')
    games_player <- vF_game_plays_players[player.id == left_player_id() & (playerType == "Scorer" | playerType == "Shooter")]
    return(vF_game_plays[(as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                         & game_and_event_id %in% games_player$game_and_event_id])
  }) 
  df_right_player <- reactive({
    year <- selected_year()
    upper_lim <- as.numeric(year) +1
    upper_lim <- paste0(as.character(upper_lim),'000000')
    lower_lim <- as.numeric(year) -1
    lower_lim <- paste0( as.character(lower_lim),'999999')
    games_player <- vF_game_plays_players[player.id == right_player_id() & (playerType == "Scorer" | playerType == "Shooter")]
    return(vF_game_plays[(as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                         & game_and_event_id %in% games_player$game_and_event_id])
  }) 
  output$performanceByTeam <- renderUI({
    fluidPage(theme = shinytheme("sandstone"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, width = '100%',
                    title = 'Compare 3 teams', status = "primary",
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('teamPerf1', 'Select Team 1', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('teamPerf2', 'Select Team 2', choices = team_choices, selected = 'Florida Panthers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('teamPerf3', 'Select Team 3', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)), 
                    div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                        selectInput('statType', 'Statistics Level', choices = c('Macro','Micro'), selected = 'Macro', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align='center',
                box( plotOutput("trendPlot1"), status = "primary",  width = 12, solidHeader = FALSE)
              ),
              fluidRow(
                align='center',
                box( plotOutput("trendPlot2"), status = "primary", width = 12, solidHeader = FALSE)
              ),
              fluidRow(
                align='center',
                box( plotOutput("trendPlot3"), status = "primary", width = 12, solidHeader = FALSE)
              ),
              fluidRow(
                align='center',
                box( plotOutput("trendPlot4"), status = "primary", width = 12, solidHeader = FALSE)
              )
    )
  })
  output$performanceByPlayer <- renderUI({
    fluidPage(theme = shinytheme("sandstone"),
              fluidRow(
                align='center',
                box(solidHeader = T, width = '100%',
                    title = 'Compare 2 players', status = "primary",
                    div(style="display: vertical-align:top; width: 11%; margin-top: 0em;",
                        selectInput('playerCat', 'Select a Catogory', choices = player_type_choices, selected = 'Defence', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('playerPerf1', 'Select Player 1', choices = NULL, multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('playerPerf2', 'Select Player 2', choices = NULL, multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align='center',
                box( plotOutput("playerTrendPlot1"), status = "primary",  width = 6, solidHeader = FALSE),
                box( plotOutput("playerTrendPlot2"), status = "primary",  width = 6, solidHeader = FALSE)
              )
    )
  })
  output$statisticBySeason <- renderUI({
      tabsetPanel(type = 'tabs',
          tabPanel("Summary",
            fluidPage(theme = shinytheme("sandstone"),
                      fluidRow(
                        align='center',
                        #collapsible box for main inputs
                        box(solidHeader = T, width = '100%',
                            title = 'Summary of Matches', status = "primary",
                            div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                                selectInput('yearStat', 'By Season', choices = c(allYears, 'All'), selected = '2018', multiple = FALSE,
                                            selectize = TRUE, width = NULL, size = NULL))
                        )
                      ),
                      fluidRow(
                        align='center',
                        box(solidHeader = F, width = '100%',
                            title = 'Arena', status = "primary"
                        )
                      ),
                      fluidRow(
                        align = "center",
                        DT::dataTableOutput("table_statistic_arena")
                      ),
                      fluidRow(
                        align='center',
                        box(solidHeader = F, width = '100%',
                            title = 'Team', status = "primary"
                        )
                      ),
                      fluidRow(
                        align = "center",
                        DT::dataTableOutput("table_statistic_team")
                      ),
                      fluidRow(
                        align='center',
                        box(solidHeader = F, width = '100%',
                            title = 'Player', status = "primary"
                        )
                      ),
                      fluidRow(
                        align = "center",
                        DT::dataTableOutput("table_statistic_player")
                      )
                    )
                  ),
            tabPanel("Visualization",
                     fluidPage(theme = shinytheme("sandstone"),
                               fluidRow(
                                 align='center',
                                 box(solidHeader = F, width = '100%',
                                     title = 'Arena (by Cities) Home Vs Away Impact by Season', status = "primary"
                                 )
                               ),
                               fluidRow(
                                 align='center',
                                 box(plotOutput("arenaMapGoals"), status = "primary", width = 6, solidHeader = TRUE, title = 'Goals'),
                                 box(plotOutput("arenaMapWins"), status = "primary", width = 6, solidHeader = TRUE, title = 'Wins')
                               ),
                               fluidRow(
                                 align='center',
                                 box(solidHeader = F, width = '100%',
                                     title = 'Team Home Vs Away Performance by Season', status = "primary"
                                 )
                               ),
                               fluidRow(
                                 align='center',
                                 box(plotOutput("teamGoals"), status = "primary", width = 6, solidHeader = TRUE, title = 'Goals'),
                                 box(plotOutput("teamWins"), status = "primary", width = 6, solidHeader = TRUE, title = 'Wins')
                               )
                     )
            )
      )
  })
  output$shotByTeam <- renderUI({
    fluidPage(theme = shinytheme("sandstone"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary", 
                    #input selections are inside div so we can place left and right inputs side by side
                    div(style="display: inline-block;vertical-align:top; width: 30% ; margin-bottom: 0em;",
                        selectInput('leftTeam', 'Team 1', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 15% ; margin-bottom: 0em;",
                        radioButtons('leftHome', '', choices = c('Home','Away'), selected = 'Home',
                                     inline = FALSE, width = NULL, choiceNames = NULL,
                                     choiceValues = NULL)),
                        #selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                        #            selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-bottom: 0em;",
                        selectInput('rightTeam', 'Team 2', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 15% ; margin-bottom: 0em;",
                        radioButtons('rightHome', '', choices = c('Home','Away'), selected = 'Home',
                                     inline = FALSE, width = NULL, choiceNames = NULL,
                                     choiceValues = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('year', 'Year', choices = allYears, selected = '2018', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                tags$style(make_css(list('.box', 
                                         c('font-size', 'font-family', 'color'), 
                                         c('15px', 'arial', 'black')))),
                box(
                  width = '100%',
                  textOutput("teamText")
                )
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink", width = '100%',
                    plotlyOutput("icemap_team"))
              )
    )
  })
  output$shotByArena <- renderUI({
    fluidPage(theme = shinytheme("sandstone"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary",
                    #input selections are inside div so we can place left and right inputs side by side
                    div(style="display: inline-block;vertical-align:top; width: 35% ; margin-top: 0em;",
                        selectInput('arena', 'Arena', choices = arena_choices, multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 35% ; margin-top: 0em;",
                        selectInput('year', 'Year', choices = allYears, selected = '2018', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)
                    )
                )
              ),
              fluidRow(
                tags$style(make_css(list('.box', 
                                         c('font-size', 'font-family', 'color'), 
                                         c('15px', 'arial', 'black')))),
                box(
                  width = '100%',
                  textOutput("arenaText")
                )
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink", width = '100%',
                    plotlyOutput("icemap_Arena"))
              )
    )
  })
  output$shotByPlayer <- renderUI({
    fluidPage(theme = shinytheme("sandstone"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary",
                    #input selections are inside div so we can place left and right inputs side by side
                    # div(style="display: inline-block;vertical-align:top; width: 22% ; margin-top: 0em;",
                    #     selectInput('leftPlayer', 'Left Player', choices = player_choices(), multiple = FALSE,
                    #                 selectize = TRUE, width = NULL, size = NULL)),
                    # div(style="display: inline-block;vertical-align:top; width: 22%; margin-top: 0em;",
                    #     selectInput('rightPlayer', 'Right Player', choices = player_choices(), multiple = FALSE,
                    #                 selectize = TRUE, width = NULL, size = NULL)),
                    # div(style="display: inline-block;vertical-align:top; width: 22% ; margin-top: 0em;",
                    #     selectInput('leftPlayer', 'Left Player', choices = player_choices(), multiple = FALSE,
                    #                 selectize = TRUE, width = NULL, size = NULL)),
                    # div(style="display: inline-block;vertical-align:top; width: 22%; margin-top: 0em;",
                    #     selectInput('rightPlayer', 'Right Player', choices = player_choices(), multiple = FALSE,
                    #                 selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: 0em;",
                        selectInput('leftPlayer', 'Player 1', choices = player_choices(), selected = "Nikita Kucherov", multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: 0em;",
                        selectInput('rightPlayer', 'Player 2', choices = player_choices(), selected = "Alex Ovechkin",  multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: 0em;",
                        selectInput('year', 'Year', choices = allYears, selected = '2018', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink", width = '100%',
                    plotlyOutput("icemap_player"))
              )
    )
  })
  output$table_statistic_player <- DT::renderDataTable({
    playerYear <- selected_yearStat()
    if (playerYear != 'All'){
      dataPlayerYear <- vF_player_season_data[vF_player_season_data$season == playerYear]
    } else {
      dataPlayerYear <- vF_player_season_data[vF_player_season_data$season %in% allYears]
    }
    df <- dataPlayerYear[,c('player.id','stat.assists','stat.goals','stat.games','stat.shots')]
    df <- df %>% drop_na()
    dfGroupByPlayer <- aggregate(. ~ player.id, df, sum)
    dfGroupByPlayer <- merge(dfGroupByPlayer, vF_player_info[,c('player.id','firstName','lastName')], by='player.id')
    dfGroupByPlayer$Player <- paste(dfGroupByPlayer$firstName, dfGroupByPlayer$lastName, sep=' ')
    dfGroupByPlayer <- subset(dfGroupByPlayer, select= names(dfGroupByPlayer) != c('player.id', 'firstName', 'lastName'))
    dfGroupByPlayer <- dfGroupByPlayer[,c('Player','stat.games','stat.goals','stat.shots','stat.assists')]
    colnames(dfGroupByPlayer) <- c('Name', 'Games', 'Goals', 'Shots', 'Game Winning Goals', 'Assists')
    DT::datatable(dfGroupByPlayer, options = list(orderClasses = TRUE, pageLength = 5))
  })
  output$table_statistic_team <- DT::renderDataTable({
    statYear <- selected_yearStat()
    if (statYear != 'All'){
      dataStatYear <- vF_game_info[vF_game_info$season == statYear]
    } else {
      dataStatYear <- vF_game_info
    }
    homeTeam <- dataStatYear[,c('home.teamID', 'home.win', 'away.win','home.goals','away.goals')]
    #awayTeam <- dataStatYear[,c('away.teamID', 'away.win', 'away.goals')]
    colnames(homeTeam)[1] <- 'team.id'
    homeTeamGroupBy <- aggregate( .~team.id, homeTeam, sum)
    #awayTeamGroupBy <- aggregate( .~ team.id, awayTeam, sum)
    #teamStat <- merge(homeTeamGroupBy, awayTeamGroupBy, by = 'team.id')
    teamStat <- merge(homeTeamGroupBy, vF_teams_DT[,c('team.id', 'long.name', 'venue.name', 'venue.city')], by = 'team.id')
    teamStat <- teamStat[,c('long.name', 'venue.name', 'venue.city', 'home.win', 'away.win', 'home.goals', 'away.goals')]
    colnames(teamStat) <- c('Name', 'Home Venue', 'City', 'Home Wins', 'Away Wins', 'Home Goals', 'Away Goals')
    DT::datatable(teamStat,options = list(orderClasses = TRUE, lengthMenu = c(5, 30, 50), pageLength = 5))
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
    colnames(dfGroupByArena) <- c('Name', 'Home Wins', 'Away Wins', 'Home Goals', 'Away Goals', 'City', 'Location', 'Division')
    DT::datatable(dfGroupByArena, options = list(orderClasses = TRUE, lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })
  output$trendPlot1 <- renderPlot({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'won')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = won, x = as.factor(team.id), fill = HoA ), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Wins') + theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'blocked')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = blocked, x = as.factor(team.id), fill = HoA), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Blocked') + theme(plot.title = element_text(hjust = 0.5))
    }
    
  })
  output$trendPlot2 <- renderPlot({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'goals')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = goals, x = as.factor(team.id), fill = HoA),  size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Goals') + theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'takeaways')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = takeaways, x = as.factor(team.id), fill = HoA),  size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Takeaways') + theme(plot.title = element_text(hjust = 0.5))
    }
    
  })
  output$trendPlot3 <- renderPlot({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro'){
      df <- df[,c('game.id', "team.id", 'HoA', 'shots')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = shots, x = as.factor(team.id), fill = HoA), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Shots') + theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'giveaways')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = giveaways, x = as.factor(team.id), fill = HoA),  size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Giveaways') + theme(plot.title = element_text(hjust = 0.5))
    }
    
  })
  output$trendPlot4 <- renderPlot({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'pim')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = pim, x = as.factor(team.id), fill = HoA), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Penalty Infraction Minute') + theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'hits')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() + 
        geom_bar(data =df, aes(y = hits, x = as.factor(team.id), fill = HoA), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Hits') + theme(plot.title = element_text(hjust = 0.5))
    }
    
  })
  output$playerTrendPlot1 <- renderPlot({
    playerId <- vF_player_info[(fullName == input$playerPerf1)]$player.id 
    df <- vF_player_season_data[(player.id == playerId) & (season %in% allYears)]
    if (input$playerCat %in% c('Defence','Offence')){
      df <- df[,c('season', 'stat.assists', 'stat.goals', 'stat.games', 
                  'stat.points','stat.shots', 'stat.penaltyMinutes','stat.hits','stat.blocked')]
      df <- rename(df, c('stat.assists' = 'Assists', 'stat.goals'='Goals', 'stat.games'='Games', 
                   'stat.points' = 'Points','stat.shots'='Shots', 'stat.penaltyMinutes'='Penalty Minutes','stat.hits'='Hits','stat.blocked'='Blocked'))
    } else {
      df <- df[,c('season', 'stat.wins','stat.losses','stat.shutouts')]
      df <- rename(df, c('stat.wins'='Wins','stat.losses'='Losses','stat.shutouts'='Shutouts'))
    }
    
    df$season = as.factor(df$season)
    df <- unique(df, by='season')
    #dft <- df %>%
    #  rownames_to_column %>% 
    #  gather(var, value, -rowname) %>% 
    #  spread(rowname, value)
    #colnames(dft) <- dft[1,]
    #dft <- dft[-c(1),]
    #dft[,2:ncol(dft)] <- sapply(dft[,2:ncol(dft)], as.numeric)
    #ggparcoord(dft, columns=2:ncol(dft), groupColumn = 1, 
    #           title='', scale = 'globalminmax')
    data_long <- melt(df, id="season") 
    ggplot(data=data_long,
           aes(x=season, y=value, colour=variable, group = variable)) +
            scale_y_continuous(breaks= pretty_breaks()) + 
            geom_line() + geom_point() 
  })
  output$playerTrendPlot2 <- renderPlot({
    playerId <- vF_player_info[(fullName == input$playerPerf2)]$player.id 
    df <- vF_player_season_data[(player.id == playerId) & (season %in% allYears)]
    if (input$playerCat %in% c('Defence','Offence')){
      df <- df[,c('season', 'stat.assists', 'stat.goals', 'stat.games', 
                  'stat.points','stat.shots', 'stat.penaltyMinutes','stat.hits','stat.blocked')]
      df <- rename(df, c('stat.assists' = 'Assists', 'stat.goals'='Goals', 'stat.games'='Games', 
                         'stat.points'='Points','stat.shots'='Shots', 'stat.penaltyMinutes'='Penalty Minutes','stat.hits'='Hits','stat.blocked'='Blocked'))
    } else {
      df <- df[,c('season', 'stat.wins','stat.losses','stat.shutouts')]
      df <- rename(df, c('stat.wins'='Wins','stat.losses'='Losses','stat.shutouts'='Shutouts'))
    }
    df$season = as.factor(df$season)
    df <- unique(df, by='season')
    #dft <- df %>%
    #  rownames_to_column %>% 
    #  gather(var, value, -rowname) %>% 
    #  spread(rowname, value)
    #colnames(dft) <- dft[1,]
    #dft <- dft[-c(1),]
    
    #dft[,2:ncol(dft)] <- sapply(dft[,2:ncol(dft)], as.numeric)
    #ggparcoord(dft, columns=2:ncol(dft), groupColumn = 1, 
    #           title='', scale = 'globalminmax')
    data_long <- melt(df, id="season") 
    ggplot(data=data_long,
          aes(x=season, y=value, colour=variable, group = variable)) +
         scale_y_continuous(breaks= pretty_breaks()) +
          geom_line() + geom_point()
  })
  output$teamGoals <- renderPlot({
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
    teamStat<- rename(teamStat, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    teamStatGather <- teamStat %>% 
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    ggplot(teamStatGather, aes(Goals, fct_reorder2(long.name, GoalType == 'away.goals', Goals, .desc = FALSE), color = GoalType)) + 
      geom_point() + ylab('')
  })
  output$teamWins <- renderPlot({
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
    teamStat<- rename(teamStat, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    teamStatGather <- teamStat %>% 
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    ggplot(teamStatGather, aes(Wins, fct_reorder2(long.name, WinType == 'away.win', Wins, .desc = FALSE), color = WinType)) + 
      geom_point() + ylab('')
  })
  output$arenaMapGoals <- renderPlot({
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
    colnames(dfGroupByArena)[6] <- 'name'
    us_cities <- us.cities
    us_cities$name <- str_sub(us_cities$name, end = -4)
    #dfGroupByArena$locationName <- c('TX', )
    #colnames(dfGroupByArena)[7] <- 'country.etc'
    arenaCities <- merge(us_cities, dfGroupByArena, by = 'name')
    arenaCities <- arenaCities %>% distinct(name, .keep_all = TRUE)
    arenaCities<- rename(arenaCities, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    arenaCitiesGather <- arenaCities %>% 
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    set_breaks = function(limits) {
      seq(limits[1], limits[2], by = round((limits[2]-limits[1])/4))
    }
    MainStates <- map_data("state")
    ggplot(data=arenaCitiesGather) + geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
         color="white", fill="white" ) + geom_point(aes(x=long, y=lat, size = Goals, color = GoalType), 
         alpha = .5) + geom_text_repel(data = arenaCities, aes(x=long, y=lat, label=name), hjust=0, vjust=0, 
          size=3.5) + scale_size_continuous(breaks = set_breaks) 
  })
  output$arenaMapWins <- renderPlot({
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
    colnames(dfGroupByArena)[6] <- 'name'
    us_cities <- us.cities
    us_cities$name <- str_sub(us_cities$name, end = -4)
    arenaCities <- merge(us_cities, dfGroupByArena, by = 'name') 
    arenaCities <- arenaCities %>% distinct(name, .keep_all = TRUE)
    arenaCities<- rename(arenaCities, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    arenaCitiesGather <- arenaCities %>% 
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    set_breaks = function(limits) {
      seq(limits[1], limits[2], by = round((limits[2]-limits[1])/4))
    }
    MainStates <- map_data("state")
    ggplot(data=arenaCitiesGather) + geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                    color="white", fill="white" ) + geom_point(aes(x=long, y=lat, size = Wins, color=WinType), 
                    alpha = .5) + geom_text_repel(data = arenaCities, aes(x=long, y=lat, label=name), hjust=0, vjust=0, 
                    size=3.5) + scale_size_continuous(breaks = set_breaks)
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
          x = ~l.x, y=~l.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01)), name = paste("Shotmap of",input$leftTeam)
        ) %>%
        add_markers(
          data = df_right_shots,
          hoverinfo='skip',
          x = ~r.x, y=~r.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01)), name = paste("Shotmap of",input$rightTeam)
        ) %>%
        add_markers(
          data = df_left_goals,
          hoverinfo='text',
          hovertext=paste(df_left_goals$result.secondaryType),
          x = ~l.x, y=~l.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_left_goals)), name = "Goals"
        ) %>%
        add_markers(
          data = df_right_goals,
          hoverinfo='text',
          hovertext=paste(df_right_goals$result.secondaryType),
          x = ~r.x, y=~r.y, showlegend = FALSE, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_right_goals))
        ) %>%
        layout(
          xaxis = list(range = c(-110,110), title = 'x'),
          yaxis = list(range = c(-50,50), title = 'y'),
          legend = list(
            orientation = "h", 
                        y = 1.1,
                        xanchor = "center",
                        x = 0.5),
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
      df_left <- df_arena_home()
      df_right <- df_arena_away()
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
          x = ~l.x, y=~l.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01)), name = paste("Shotmap of",arena_team())
        ) %>%
        add_markers(
          data = df_right_shots,
          hoverinfo='skip',
          x = ~r.x, y=~r.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01)), name = paste("Shotmap of teams visiting",input$arena )
        ) %>%
        add_markers(
          data = df_left_goals,
          hoverinfo='text',
          hovertext=paste(df_left_goals$result.secondaryType),
          x = ~l.x, y=~l.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_left_goals)), name = "Goals"
        ) %>%
        add_markers(
          data = df_right_goals,
          hoverinfo='text',
          hovertext=paste(df_right_goals$result.secondaryType),
          x = ~r.x, y=~r.y, showlegend = FALSE, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_right_goals))
        ) %>%
        layout(
          xaxis = list(range = c(-110,110), title = 'x'),
          yaxis = list(range = c(-50,50), title = 'y'),
          legend = list(
            orientation = "h", 
            y = 1.1,
            xanchor = "center",
            x = 0.5),
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
  output$icemap_player <- renderPlotly({
    df_left <- df_left_player()
    df_right <- df_right_player()
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
        x = ~l.x, y=~l.y, marker = list(size = 15, color = 'blue', opacity = min(20/nrow(df_left_shots),0.3)), name = paste("Shotmap of",input$leftPlayer)
      ) %>%
      add_markers(
        data = df_right_shots,
        hoverinfo='skip',
        x = ~r.x, y=~r.y, marker = list(size = 15, color = 'red', opacity = min(20/nrow(df_right_shots),0.3)), name = paste("Shotmap of",input$rightPlayer)
      ) %>%
      add_markers(
        data = df_left_goals,
        hoverinfo='text',
        hovertext=paste(df_left_goals$result.secondaryType),
        x = ~l.x, y=~l.y, marker = list(size = 4, color = 'black', opacity = 1), name = "Goals"
      ) %>%
      add_markers(
        data = df_right_goals,
        hoverinfo='text',
        hovertext=paste(df_right_goals$result.secondaryType),
        x = ~r.x, y=~r.y, showlegend = FALSE, marker = list(size = 4, color = 'black', opacity =1)
      ) %>%
      layout(
        xaxis = list(range = c(-110,110),title='x'),
        yaxis = list(range = c(-50,50),title='y'),
        legend = list(
          orientation = "h", 
          y = 1.1,
          xanchor = "center",
          x = 0.5),
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
  output$teamText <- renderText({
    if (input$leftHome == "Home") {
      left_home <- "at home"
    }else{
      left_home <- "away"
    }
    if (input$rightHome == "Home") {
      right_home <- "at home"
    }else{
      right_home <- "away"
    }
    paste0("This is the shot map of ", input$leftTeam,
           " playing ", left_home, " on the left and ",input$rightTeam, " playing ",right_home," on the right.\n The density of the shot map shows the frequency of shots taken in each area of the rink. Individual dot points show where goals have been scored from.")
  })
  output$arenaText <- renderText({
    paste0("This is the shot map of ", arena_team(),
           " playing at home on the left and teams visiting ", input$arena, " on the right.\n The density of the shot map shows the frequency of shots taken in each area of the rink. Individual dot points show where goals have been scored from.")
  })
}