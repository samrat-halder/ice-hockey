library(plotly)
df <- structure(list(game_date = structure(c(17689, 17689, 17689, 17689, 
                                             17689), class = "Date"), event_team = c("WSH", "WSH", "WSH", 
                                                                                     "VGK", "VGK"), 
                     event_description = c("WSH ONGOAL - #8 OVECHKIN, Deflected, Off. Zone, 10 ft. Expected Goal Prob: 25.7%", 
                                           "WSH ONGOAL - #8 OVECHKIN, Wrist, Off. Zone, 38 ft. Expected Goal Prob: 4.9%", 
                                           "WSH ONGOAL - #29 DJOOS, Wrist, Off. Zone, 65 ft. Expected Goal Prob: 1%", 
                                           "VGK ONGOAL - #5 ENGELLAND, Wrist, Off. Zone, 38 ft. Expected Goal Prob: 1.5%", 
                                           "VGK ONGOAL - #88 SCHMIDT, Wrist, Off. Zone, 62 ft. Expected Goal Prob: 1.3%"
                     ), event_type = c(0, 0, 0, 0, 0), home_team = c("VGK", "VGK", "VGK", "VGK", "VGK"), away_team = c("WSH", "WSH", "WSH", "WSH", 
                                         "WSH"), coords_x = c(-80, -53, -31, 56, 34), coords_y = c(1, 
                                         -14, 30, -17, -26)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L)) 


image_file <- "full-rink.png"
txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
df %>% 
  plot_ly(x = ~coords_x, y=~coords_y, 
          text= ~event_description)  %>% 
  add_markers(size = ~event_type,
              sizes = c(150, 700),
              
              alpha = 0.75,
              color = ~factor(event_team),
              colors = c("black", "slategrey")
  ) %>%
  layout(
    xaxis = list(autorange = TRUE), 
    yaxis = list(autorange = TRUE),
    images= list(
        source= paste('data:image/png;base64', txt, sep=','),
        xref= "x",
        yref= "y",
        x = 0,
        y = 0,
        sizex = 200,
        sizey = 300,
        opacity = 0.8,
        layer = "below",
        xanchor = "center",
        yanchor = "middle"
        )
  )