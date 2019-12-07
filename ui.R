library(shinydashboard)
library(plotly)
library(shinythemes)
library(shinyjs)
library(DT)
library(tidyr)
library(ggplot2)
library(maps) 
library(dplyr)
library(stringr)
library(tidyverse)
load("./data/2019-11-26_nhl-cleaned-data.RData")

header <- dashboardHeader(
  title = span("NHL ICE HOCKEY", 
   span("dashboard",style = "color: yellow; font-size: 36px; font-weight: bold"))
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
      tabItem('season', uiOutput('statisticBySeason')),
      tabItem('player', uiOutput('performanceByPlayer')),
      tabItem('team', uiOutput('performanceByTeam'))
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")