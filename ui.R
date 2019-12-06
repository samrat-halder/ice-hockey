library(shinydashboard)
library(plotly)
library(shinythemes)
library(shinyjs)
load("./data/2019-11-26_nhl-cleaned-data.RData")

header <- dashboardHeader(
  title = span("NHL ICE HOCKEY", 
               span("dashboard", 
                    style = "color: yellow; font-size: 36px; font-weight: bold"))
)
sidebar <- dashboardSidebar(
  #Here goes more customizations
  collapsed = TRUE,
  sidebarMenu(
    shinyjs::useShinyjs(),
    id = "tabs",
    menuItem('Shots Map',
             menuSubItem('Team', tabName = 'team_shots'),
             menuSubItem('Arena', tabName = 'arena_shots')
    ),
    menuItem('Season Statistics',
             menuSubItem('2018', tabName = '2018_stat'),
             menuSubItem('2017', tabName = '2017_stat')
    ),
    menuItem('Team Performance',
             menuSubItem('2018', tabName = '2018_perf'),
             menuSubItem('2017', tabName = '2017_perf')
    )
  )
)
body <- dashboardBody(
  tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 20
  ),
  tabItems(
    tabItem('team_shots', uiOutput('shotByTeam')),
    tabItem('Arena_shots', uiOutput('shotByArena'))
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")