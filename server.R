server <-function(input, output, session) {
  allYears <- c('2018','2017','2016','2015','2014')
  team_choices <- unique(vF_teams_DT$long.name)
  team_choices <- team_choices[order(team_choices)]
  arena_choices <- unique(vF_teams_DT$venue.city)
  arena_choices <- arena_choices[order(arena_choices)]
  vF_player_info$fullName <- paste(vF_player_info$firstName,vF_player_info$lastName,sep=' ')
  vF_game_plays$game_and_event_id <- paste(vF_game_plays$game.id,vF_game_plays$about.eventIdx,sep='')
  vF_game_plays_players$game_and_event_id <- paste(vF_game_plays_players$game.id,vF_game_plays_players$eventIdx,sep='')
  
  player_choices <- reactive({
    player_choices <- unique(vF_player_info[vF_player_info$player.id %in% vF_player_season_data[vF_player_season_data$season == input$year]$player.id]$fullName)
    return(player_choices[order(player_choices)])
  }) 
  player_choices_performance <- unique(paste(vF_player_info[vF_player_info$player.id %in% vF_player_season_data[vF_player_season_data$season %in% allYears]$player.id]$fullName)) 
  #                               vF_player_info[vF_player_info$player.id %in% vF_player_season_data[vF_player_season_data$season %in% allYears]$player.id]$lastName, 
  #                               sep=' '))
 
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
  player_id <- reactive({
    return(vF_player_info[fullName == input$player]$player.id)
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
  df_player <- reactive({
    year <- selected_year()
    upper_lim <- as.numeric(year) +1
    upper_lim <- paste0(as.character(upper_lim),'000000')
    lower_lim <- as.numeric(year) -1
    lower_lim <- paste0( as.character(lower_lim),'999999')
    games_player <- vF_game_plays_players[player.id == player_id()]
    plays <- vF_game_plays[(as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                           & game_and_event_id %in% games_player$game_and_event_id]
    return(merge(plays,games_player,by="game_and_event_id",all.x=TRUE))
  }) 
  output$performanceByTeam <- renderUI({
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, width = '100%',
                    title = 'Compare up to 3 teams', status = "primary", background = "blue",
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
                box( plotlyOutput("trendPlot1"), status = "primary",  width = 12, solidHeader = FALSE)
              ),
              fluidRow(
                align='center',
                box( plotlyOutput("trendPlot2"), status = "primary", width = 12, solidHeader = FALSE)
              ),
              fluidRow(
                align='center',
                box( plotlyOutput("trendPlot3"), status = "primary", width = 12, solidHeader = FALSE)
              ),
              fluidRow(
                align='center',
                box( plotlyOutput("trendPlot4"), status = "primary", width = 12, solidHeader = FALSE)
              )
    )
  })
  output$performanceByPlayer <- renderUI({
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, width = '100%',
                    title = 'Compare up to 3 players', status = "primary", background = "blue",
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('playerPerf1', 'Select Team 1', choices = player_choices_performance, multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('playerPerf2', 'Select Team 2', choices = player_choices_performance, multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-top: 0em;",
                        selectInput('playerPerf3', 'Select Team 3', choices = player_choices_performance, multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)), 
                    div(style="display: inline-block;vertical-align:top; width: 11%; margin-top: 0em;",
                        selectInput('playerEvent', 'Statistic to Compare', choices = c('Shots','Goals','Assists','Hits','Blocked Shots'), selected = 'Shots', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align='center',
                box( plotlyOutput("playerTrendPlot"), status = "primary",  width = 12, solidHeader = FALSE)
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
                                selectInput('yearStat', 'By Season', choices = c(allYears, 'All'), selected = '2018', multiple = FALSE,
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
                                     title = 'Arena (by Cities) Home Vs Away Impact by Season', status = "primary", background = "blue"
                                 )
                               ),
                               fluidRow(
                                 align='center',
                                 box(plotOutput("arenaMapGoals"), status = "primary", width = 6, solidHeader = TRUE, title = 'Goals'),
                                 box(plotOutput("arenaMapWins"), status = "primary", width = 6, solidHeader = TRUE, title = 'Wins')
                               ),
                               fluidRow(
                                 align='center',
                                 box(solidHeader = T, width = '100%',
                                     title = 'Team Home Vs Away Performance by Season', status = "primary", background = "blue"
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
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary", background = "blue",
                    #input selections are inside div so we can place left and right inputs side by side
                    div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: 0em;",
                        selectInput('leftTeam', 'Left Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: 0em;",
                        selectInput('rightTeam', 'Right Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput('rightHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: 0em;",
                        selectInput('year', 'Year', choices = allYears, selected = '2018', multiple = FALSE,
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
  output$arenaText <- renderText({
    paste0("This is the shotmap of ", arena_team(),
           " playing at home on the left and teams visiting ", input$arena, " on the right.")
  })
  output$shotByArena <- renderUI({
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary", background = "blue",
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
                box(status = "primary", title = "Rink Layout", width = '100%',
                    plotlyOutput("icemap_Arena"))
              )
    )
  })
  output$shotByPlayer <- renderUI({
    fluidPage(theme = shinytheme("slate"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = T, collapsible = T, width = '100%',
                    title = "Filters", status = "primary", background = "blue",
                    #input selections are inside div so we can place left and right inputs side by side
                    div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: 0em;",
                        selectInput('player', 'Player', choices = player_choices(), multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: 0em;",
                        selectInput('year', 'Year', choices = allYears, selected = '2018', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink Layout", width = '100%',
                    plotlyOutput("icemap_player"))
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
  output$trendPlot1 <- renderPlotly({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'won')]
      ggplot() + 
        geom_bar(data =df, aes(y = won, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Wins')
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'blocked')]
      ggplot() + 
        geom_bar(data =df, aes(y = blocked, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Blocked')
    }
    
  })
  output$trendPlot2 <- renderPlotly({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'goals')]
      ggplot() + 
        geom_bar(data =df, aes(y = goals, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Goals')
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'takeaways')]
      ggplot() + 
        geom_bar(data =df, aes(y = takeaways, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Takeaways')
    }
    
  })
  output$trendPlot3 <- renderPlotly({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro'){
      df <- df[,c('game.id', "team.id", 'HoA', 'shots')]
      ggplot() + 
        geom_bar(data =df, aes(y = shots, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Shots')
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'giveaways')]
      ggplot() + 
        geom_bar(data =df, aes(y = giveaways, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Giveaways')
    }
    
  })
  output$trendPlot4 <- renderPlotly({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'pim')]
      ggplot() + 
        geom_bar(data =df, aes(y = pim, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Penalty Infraction Minute')
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'hits')]
      ggplot() + 
        geom_bar(data =df, aes(y = hits, x = as.factor(team.id), fill = HoA, size=0.25, width=0.6, alpha= 0.5), stat = "identity", position = 'stack') + 
        theme_bw() + 
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
        xlab('') +ylab('') + ggtitle('Hits')
    }
    
  })
  output$playerTrendPlot <- renderPlotly({
    playerIds <- vF_player_info[fullName %in% c(input$playerPerf1, input$playerPerf2, input$playerPerf3)]$player.id
    df <- vF_game_plays_players[player.id %in% playerIds]
    df$year <- substr(df$game.id,1,4)
    df <- as.data.frame(df %>% group_by(player.id, year, playerType) %>% tally())
    
    if (input$playerEvent == "Goals") {
      df <- filter(df,playerType == "Scorer")
    }else if (input$playerEvent == "Shots") {
      df <- filter(df,playerType == "Shooter")
    }else if (input$playerEvent == "Hits") {
      df <- filter(df,playerType == "Hitter")
    }else if (input$playerEvent == "Blocked Shots") {
      df <- filter(df,playerType == "Blocker")
    }else if (input$playerEvent == "Assists") {
      df <- filter(df,playerType == "Assist")
    }
    df <- df[,c('player.id', 'year', 'n')]
    ggplot() + 
      geom_bar(data =df, aes(y = n, x = as.factor(player.id),fill=player.id, size=0.25, width=0.6, alpha= 0.5), stat = "identity") +
      theme_bw() + 
      facet_grid(~year) +
      #scale_fill_manual("legend", values = c("cyan1", "bisque4")) + 
      xlab('') +ylab('')
    
    
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
    teamStatGather <- teamStat %>% 
                    gather(key = 'GoalType', value = 'Goals', home.goals, away.goals) %>%
                    gather(key = 'WinType', value = 'Wins', home.win, away.win)
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
    teamStatGather <- teamStat %>% 
      gather(key = 'GoalType', value = 'Goals', home.goals, away.goals) %>%
      gather(key = 'WinType', value = 'Wins', home.win, away.win)
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
    arenaCitiesGather <- arenaCities %>% 
      gather(key='GoalType', value = 'Goals', home.goals, away.goals) %>%
      gather(key = 'WinType', value = 'Wins', home.win, away.win)
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
    arenaCitiesGather <- arenaCities %>% 
      gather(key='GoalType', value = 'Goals', home.goals, away.goals) %>%
      gather(key = 'WinType', value = 'Wins', home.win, away.win)
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
          x = ~l.x, y=~l.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01)), name = "Left Team Shots"
        ) %>%
        add_markers(
          data = df_right_shots,
          hoverinfo='skip',
          x = ~r.x, y=~r.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01)), name = "Right Team Shots"
        ) %>%
        add_markers(
          data = df_left_goals,
          hoverinfo='text',
          hovertext=paste(df_left_goals$result.secondaryType),
          x = ~l.x, y=~l.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_left_goals))
        ) %>%
        add_markers(
          data = df_right_goals,
          hoverinfo='text',
          hovertext=paste(df_right_goals$result.secondaryType),
          x = ~r.x, y=~r.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_right_goals))
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
          x = ~l.x, y=~l.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01))
        ) %>%
        add_markers(
          data = df_right_shots,
          hoverinfo='skip',
          x = ~r.x, y=~r.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01))
        ) %>%
        add_markers(
          data = df_left_goals,
          hoverinfo='text',
          hovertext=paste(df_left_goals$result.secondaryType),
          x = ~l.x, y=~l.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_left_goals))
        ) %>%
        add_markers(
          data = df_right_goals,
          hoverinfo='text',
          hovertext=paste(df_right_goals$result.secondaryType),
          x = ~r.x, y=~r.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_right_goals))
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
  output$icemap_player <- renderPlotly({
    df <- df_player()
    #set the rink image and plot
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
    df <- df[playerType != "PlayerID" & playerType != "Winner" & playerType != "Loser" & playerType != "ServedBy" & playerType != "DrewBy" & playerType != "PenaltyOn"]
    df %>% 
      plot_ly()  %>% 
      add_markers(
        data = df,
        hoverinfo='text',
        hovertext=paste(df$playerType),
        x = ~p.x, y=~p.y, color=~playerType, marker = list(size = 7, opacity = max(0.4,50/nrow(df)))
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