server <-function(input, output) {
  
  #Here goes the plots
  output$icemap <- renderPlotly({
    
    
    #library(plotly)
    
    #get the ID of each team in the inputs
    left_team_id <- vF_teams_DT[long.name == input$leftTeam]$team.id
    right_team_id <- vF_teams_DT[long.name == input$rightTeam]$team.id
    
    #filtering by teams and also by event types - probably a more elegant way to do this
    df <- vF_game_plays_2017[(team.id.for == left_team_id |  team.id.for == right_team_id) & (result.eventTypeId == 'SHOT' | result.eventTypeId == 'HIT' | result.eventTypeId == 'GOAL')]
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
      plot_ly(x = ~coordinates.x, y=~coordinates.y)  %>% 
      add_markers(
        #size = ~event_type,
                  size = 150,
                  alpha = 0.75,
                  color = ~factor(team.id.for),
                  colors = c("dodgerblue", "darksalmon")
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