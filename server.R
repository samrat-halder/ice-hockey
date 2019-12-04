server <-function(input, output) {
  #Here goes the plots
  
  output$icemap_2018 <- renderPlotly(
    {
      #get the ID of each team in the inputs
      left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
      right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
      
      if (input$leftTeam == "All") {
        df_left <- vF_game_plays
      }else if (input$leftTeam == "Western Conference") {
        df_left <- vF_game_plays[team.id.for %in% teams_west]
      }else if (input$leftTeam == "Eastern Conference") {
        df_left <- vF_game_plays[team.id.for %in% teams_east]
      }else{
        df_left <- vF_game_plays[team.id.for == left_team_id]
      }
      
      
      if (input$leftTeam == "All") {
        df_right <- vF_game_plays
      }else if (input$rightTeam == "Western Conference") {
        df_right <- vF_game_plays[team.id.for %in% teams_west]
      }else if (input$rightTeam == "Eastern Conference") {
        df_right <- vF_game_plays[team.id.for %in% teams_east]
      }else{
        df_right <- vF_game_plays[team.id.for == right_team_id]
      }
      
      
      
      #filtering by teams and also by event types - probably a more elegant way to do this
      df_left <- df_left[(result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL') &
                           (as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000)]
      if (input$shots == FALSE) {
        df_left <- df_left[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df_left <- df_left[result.eventTypeId != 'GOAL']
      }
      
      df_right <- df_right[(result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL') &
                             (as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000)]
      if (input$shots == FALSE) {
        df_right <- df_right[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df_right <- df_right[result.eventTypeId != 'GOAL']
      }
      
      #home v away
      if (input$leftHome == "Home") {
        games_left <- vF_game_info[as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000 & home.teamID == left_team_id]
        df_left <- df_left[game.id %in% games_left$game.id]
      }else if (input$leftHome == "Away") {
        games_left <- vF_game_info[as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000 & away.teamID == left_team_id]
        df_left <- df_left[game.id %in% games_left$game.id]
      }
        
      if (input$rightHome == "Home") {
        games_right <- vF_game_info[as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000 & home.teamID == right_team_id]
        df_right <- df_right[game.id %in% games_right$game.id]
      }else if (input$rightHome == "Away") {
        games_right <- vF_game_info[as.numeric(game.id) > 2017999999 & as.numeric(game.id) < 2019000000 & away.teamID == right_team_id]
        df_right <- df_right[game.id %in% games_right$game.id]
      }
      
      df_left_shots <- df_left[result.eventTypeId == 'SHOT']
      df_left_goals <- df_left[result.eventTypeId == 'GOAL']
      df_right_shots <- df_right[result.eventTypeId == 'SHOT']
      df_right_goals <- df_right[result.eventTypeId == 'GOAL']
      #df$home <- game.id
      
      #set the rink image and plot
      image_file <- "full-rink.png"
      txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
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
  output$icemap_2017 <- renderPlotly(
    {
    #get the ID of each team in the inputs
    left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
    right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
    
    if (input$leftTeam == "All") {
      df_left <- vF_game_plays
    }else if (input$leftTeam == "Western Conference") {
      df_left <- vF_game_plays[team.id.for %in% teams_west]
    }else if (input$leftTeam == "Eastern Conference") {
      df_left <- vF_game_plays[team.id.for %in% teams_east]
    }else{
      df_left <- vF_game_plays[team.id.for == left_team_id]
    }
    
    
    if (input$leftTeam == "All") {
      df_right <- vF_game_plays
    }else if (input$rightTeam == "Western Conference") {
      df_right <- vF_game_plays[team.id.for %in% teams_west]
    }else if (input$rightTeam == "Eastern Conference") {
      df_right <- vF_game_plays[team.id.for %in% teams_east]
    }else{
      df_right <- vF_game_plays[team.id.for == right_team_id]
    }
    
    
    
    
    #filtering by teams and also by event types - probably a more elegant way to do this
    df_left <- df_left[(result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL') &
                               (as.numeric(game.id) > 2016999999 & as.numeric(game.id) < 2018000000)]
    if (input$shots == FALSE) {
      df_left <- df_left[result.eventTypeId != 'SHOT']
    }
    if (input$goals == FALSE) {
      df_left <- df_left[result.eventTypeId != 'GOAL']
    }
    
    df_right <- df_right[(result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL') &
                         (as.numeric(game.id) > 2016999999 & as.numeric(game.id) < 2018000000)]
    if (input$shots == FALSE) {
      df_right <- df_right[result.eventTypeId != 'SHOT']
    }
    if (input$goals == FALSE) {
      df_right <- df_right[result.eventTypeId != 'GOAL']
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
                                    (result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL') &
                                    (as.numeric(game.id) > 2015999999 & as.numeric(game.id) < 2017000000)]
      if (input$shots == FALSE) {
        df <- df[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df <- df[result.eventTypeId != 'GOAL']
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
                                    (result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL') &
                                    (as.numeric(game.id) > 2014999999 & as.numeric(game.id) < 2016000000)]
      if (input$shots == FALSE) {
        df <- df[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df <- df[result.eventTypeId != 'GOAL']
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
                            (result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL') &
                            (as.numeric(game.id) > 2013999999 & as.numeric(game.id) < 2015000000)]
      if (input$shots == FALSE) {
        df <- df[result.eventTypeId != 'SHOT']
      }
      if (input$goals == FALSE) {
        df <- df[result.eventTypeId != 'GOAL']
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