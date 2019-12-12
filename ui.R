header <- dashboardHeader(
  title = "NHL Ice Hockey"
  #title = span("NHL Ice Hockey", 
   #                 style = "color: black; font-size: 20px; font-family: Open Sans")
)
sidebar <- dashboardSidebar(
    #Here goes more customization
    collapsed = TRUE,
    useShinyjs(),
    sidebarMenu(
      shinyjs::useShinyjs(),
      id = "tabs",
      menuItem('Shot Map',
      startExpanded = TRUE,
       menuSubItem('Team', tabName = 'team_shots'),
       menuSubItem('Arena', tabName = 'arena_shots'),
       menuSubItem('Player', tabName = 'player_shots')
      ),
      menuItem('Season Statistics', tabName = 'season'),
      menuItem('Player Performance', tabName = 'player'),
      menuItem('Team Performance', tabName = 'team')
    )
)
body <- dashboardBody(
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 20
    ),
    tabItems(
      tabItem('team_shots', uiOutput('shotByTeam')),
      tabItem('arena_shots', uiOutput('shotByArena')),
      tabItem('player_shots', uiOutput('shotByPlayer')),
      tabItem('season', uiOutput('statisticBySeason')),
      tabItem('player', uiOutput('performanceByPlayer')),
      tabItem('team', uiOutput('performanceByTeam'))
  )
)
ui <- dashboardPage(header, sidebar, body, skin = 'black')