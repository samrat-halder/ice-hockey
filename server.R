server <-function(input, output, session) {
  observeEvent(input$tabs,{
    updateSelectInput(session,'tabs')
  })
  observeEvent(input$leftTeamArena,{
    updateSelectInput(session,inputId='leftTeam', selected = input$leftTeamArena)
  }) 
  observeEvent(input$rightTeamArena,{
    updateSelectInput(session,inputId = 'rightTeam', selected = input$rightArena)
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
  output$icemap_team <- renderPlotly(
    {
      df_left <- df_left()
      df_right <- df_right()
      df_left_shots <- df_left[result.eventTypeId == 'SHOT']
      df_left_goals <- df_left[result.eventTypeId == 'GOAL']
      df_right_shots <- df_right[result.eventTypeId == 'SHOT']
      df_right_goals <- df_right[result.eventTypeId == 'GOAL']
      #set the rink image and plot
      image_file <- "full-rink.png"
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
    }
  )
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
  output$icemap_Arena <- renderPlotly(
    {
    df_left <- df_left()
    df_right <- df_right()
    #set the rink image and plot
    image_file <- "full-rink.png"
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
    }
  )

}