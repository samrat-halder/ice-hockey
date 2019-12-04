server <-function(input, output, session) {
  observeEvent(input$tabs,{
    updateSelectInput(session,'tabs')
  })
  observeEvent(input$leftTeam2,{
    updateSelectInput(session,inputId='leftTeam', selected = input$leftTeam2)
  }) 
  observeEvent(input$rightTeam2,{
    updateSelectInput(session,inputId = 'rightTeam', selected = input$rightTeam2)
  }) 
  observeEvent(input$goals2,{
    updateSelectInput(session,inputId = 'goals', selected = input$goals2)
  }) 
  observeEvent(input$shots2,{
    updateSelectInput(session,inputId = 'shots', selected = input$shots2)
  }) 


  #Here goes the plots
  left_team_id <- reactive({
    print(input$leftTeam)
    return(vF_teams_DT[long.name == input$leftTeam]$team.id)
  })  
  right_team_id <- reactive({
    return(vF_teams_DT[long.name == input$rightTeam]$team.id)
  }) 
  shots <- reactive({
    tmp <- vF_game_plays[result.eventTypeId %in% c('SHOT','GOAL')]
    if (input$goals == FALSE ) {
      tmp <- tmp[result.eventTypeId != 'GOAL']
    }
    if (input$shots == FALSE ) {
      tmp <- tmp[result.eventTypeId != 'SHOT']
    }
    print(input$goals)
    print(input$shots)
  
    return(tmp)
  }
  )
  df_left <- reactive({
    tmp <- shots()
    return(tmp[team.id.for == left_team_id()])
  }) 
  df_right <- reactive({
    tmp <- shots()
    return(tmp[team.id.for == right_team_id()])
  }) 
  output$shot2018 <- renderUI({
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
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              #checkboxes for each event type (more to be added)
              fluidRow(
                align = "center",
                div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('shots', 'Shots', value = FALSE, width = NULL)),
                div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('goals', 'Goals', value = FALSE, width = NULL))
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink Layout", width = '100%',
                    plotlyOutput("icemap_2018"))
              )
    )
  })
  output$icemap_2018 <- renderPlotly(
    {
      df_left <- df_left()
      df_right <- df_right()
      #filtering by teams and also by event types - probably a more elegant way to do this
      df_left <- df_left[(as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000)]
      df_right <- df_right[(as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000)]
      df <- rbind(df_left,df_right)
      
      #set the rink image and plot
      image_file <- "full-rink.png"
      txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
      df %>% 
        plot_ly(x = ~coordinates.x, y=~coordinates.y, marker = list(size = 10))  %>% 
        add_markers(
          alpha = 0.25,
          color = ~factor(team.id.for),
          colors = c("dodgerblue", "darksalmon"),
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
  output$shot2017 <- renderUI({
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
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              #checkboxes for each event type (more to be added)
              fluidRow(
                align = "center",
                div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('shots2', 'Shots', value = input$shots2, width = NULL)),
                div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('goals2', 'Goals', value = input$goals2, width = NULL))
              ),
              fluidRow(
                align = "center", 
                box(status = "primary", title = "Rink Layout", width = '100%',
                    plotlyOutput("icemap_2017"))
              )
    )
  })
  output$icemap_2017 <- renderPlotly(
    {
    df_left <- df_left()
    df_right <- df_right()
    #filtering by teams and also by event types - probably a more elegant way to do this
    df_left <- df_left[(as.numeric(game.id) > 2016999999 & as.numeric(game.id) < 2018000000)]
    df_right <- df_right[(as.numeric(game.id) > 2016999999 & as.numeric(game.id) < 2018000000)]
    df <- rbind(df_left,df_right)
    
    #set the rink image and plot
    image_file <- "full-rink.png"
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
    df %>% 
      plot_ly(x = ~coordinates.x, y=~coordinates.y, marker = list(size = 10))  %>% 
      add_markers(
                  alpha = 0.25,
                  color = ~factor(team.id.for),
                  colors = c("dodgerblue", "darksalmon"),
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