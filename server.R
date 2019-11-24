server <-function(input, output) {
  
  load("~/GitHub/Ice-Hockey/data/2019-11-21_nhl-cleaned-data.RData")
  #Here goes the plots
  output$icemap <- renderPlotly({
    
    
    library(plotly)
    #df <- structure(list(game_date = structure(c(17689, 17689, 17689, 17689, 
    #                                             17689), class = "Date"), event_team = c("WSH", "WSH", "WSH", 
    #                                                                                    "VGK", "VGK"), 
    #                     event_description = c("WSH ONGOAL - #8 OVECHKIN, Deflected, Off. Zone, 10 ft. Expected Goal Prob: 25.7%", 
    #                                           "WSH ONGOAL - #8 OVECHKIN, Wrist, Off. Zone, 38 ft. Expected Goal Prob: 4.9%", 
    #                                           "WSH ONGOAL - #29 DJOOS, Wrist, Off. Zone, 65 ft. Expected Goal Prob: 1%", 
    #                                           "VGK ONGOAL - #5 ENGELLAND, Wrist, Off. Zone, 38 ft. Expected Goal Prob: 1.5%", 
    #                                           "VGK ONGOAL - #88 SCHMIDT, Wrist, Off. Zone, 62 ft. Expected Goal Prob: 1.3%"
    #                     ), event_type = c(0, 0, 0, 0, 0), home_team = c("VGK", "VGK", "VGK", "VGK", "VGK"), away_team = c("WSH", "WSH", "WSH", "WSH", 
    #                            "WSH"), coords_x = c(-80, -53, -31, 56, 34), coords_y = c(1, -14, 30, -17, -26)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L)) 
    
    
    df <- vF_game_plays_2017[game.id == 2017020001 & (result.eventTypeId == 'SHOT' | result.eventTypeId == 'HIT' | result.eventTypeId == 'GOAL')]
    if (input$shots == FALSE) {
      df <- df[result.eventTypeId != 'SHOT']
    }
    if (input$goals == FALSE) {
      df <- df[result.eventTypeId != 'GOAL']
    }
    if (input$hits == FALSE) {
      df <- df[result.eventTypeId != 'HIT']
    }
  
    
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
        xaxis = list(range = c(-120,120)),
        yaxis = list(range = c(-60,60)),
        images= list(
          source= paste('data:image/png;base64', txt, sep=','),
          xref= "x",
          yref= "y",
          x = 0,
          y = 0,
          sizex = 260,
          sizey = 160,
          opacity = 0.8,
          layer = "below",
          xanchor = "center",
          yanchor = "middle"
        )
      )
  })
}