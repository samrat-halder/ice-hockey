server <-function(input, output) {
  #Here goes the plots
  output$icemap_2018 <- renderPlotly(
    {
      #get the ID of each team in the inputs
      left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
      right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
      #filtering by teams and also by event types - probably a more elegant way to do this
      df <- vF_game_plays[(team.id.for == left_team_id |  team.id.for == right_team_id) & 
                                    (result.eventTypeId == 'SHOT' | result.eventTypeId == 'HIT' | result.eventTypeId == 'GOAL') &
                                    (as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000)]
      if (input$shots == FALSE) {
        df <- df[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df <- df[result.eventTypeId != 'GOAL']
      }
      if (input$hits == FALSE) {
        df <- df[result.eventTypeId != 'HIT']
      }
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
  output$icemap_2017 <- renderPlotly(
    {
    #get the ID of each team in the inputs
    left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
    right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
    #filtering by teams and also by event types - probably a more elegant way to do this
    df <- vF_game_plays[(team.id.for == left_team_id |  team.id.for == right_team_id) & 
                               (result.eventTypeId == 'SHOT' | result.eventTypeId == 'HIT' | result.eventTypeId == 'GOAL') &
                               (as.numeric(game.id) > 2016999999 & as.numeric(game.id) < 2018000000)]
    if (input$shots == FALSE) {
      df <- df[result.eventTypeId != 'SHOT']
    }
    if (input$goals == FALSE) {
      df <- df[result.eventTypeId != 'GOAL']
    }
    if (input$hits == FALSE) {
      df <- df[result.eventTypeId != 'HIT']
    }
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
  output$icemap_2016 <- renderPlotly(
    {
      #get the ID of each team in the inputs
      left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
      right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
      #filtering by teams and also by event types - probably a more elegant way to do this
      df <- vF_game_plays[(team.id.for == left_team_id |  team.id.for == right_team_id) & 
                                    (result.eventTypeId == 'SHOT' | result.eventTypeId == 'HIT' | result.eventTypeId == 'GOAL') &
                                    (as.numeric(game.id) > 2015999999 & as.numeric(game.id) < 2017000000)]
      if (input$shots == FALSE) {
        df <- df[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df <- df[result.eventTypeId != 'GOAL']
      }
      if (input$hits == FALSE) {
        df <- df[result.eventTypeId != 'HIT']
      }
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
  output$icemap_2015 <- renderPlotly(
    {
      #get the ID of each team in the inputs
      left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
      right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
      #filtering by teams and also by event types - probably a more elegant way to do this
      df <- vF_game_plays[(team.id.for == left_team_id |  team.id.for == right_team_id) & 
                                    (result.eventTypeId == 'SHOT' | result.eventTypeId == 'HIT' | result.eventTypeId == 'GOAL') &
                                    (as.numeric(game.id) > 2014999999 & as.numeric(game.id) < 2016000000)]
      if (input$shots == FALSE) {
        df <- df[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df <- df[result.eventTypeId != 'GOAL']
      }
      if (input$hits == FALSE) {
        df <- df[result.eventTypeId != 'HIT']
      }
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
  output$icemap_2014 <- renderPlotly(
    {
      #get the ID of each team in the inputs
      left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
      right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
      #filtering by teams and also by event types - probably a more elegant way to do this
      df <- vF_game_plays[(team.id.for == left_team_id |  team.id.for == right_team_id) & 
                            (result.eventTypeId == 'SHOT' | result.eventTypeId == 'HIT' | result.eventTypeId == 'GOAL') &
                            (as.numeric(game.id) > 2013999999 & as.numeric(game.id) < 2015000000)]
      if (input$shots == FALSE) {
        df <- df[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df <- df[result.eventTypeId != 'GOAL']
      }
      if (input$hits == FALSE) {
        df <- df[result.eventTypeId != 'HIT']
      }
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