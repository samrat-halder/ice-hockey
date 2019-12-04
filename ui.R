library(shinydashboard)
library(plotly)
library(shinythemes)
library(shinyjs)
load("./data/2019-11-26_nhl-cleaned-data.RData")

team_choices <- list()
teams_west <- list()
teams_east <- list()
for (i in 1:nrow(vF_teams_DT)) {
  team_choices <- append(team_choices,vF_teams_DT$long.name[i])
  if (vF_teams_DT$conference.name[i] == "Western") {
    teams_west <- append(teams_west,vF_teams_DT$team.id[i])
  }else{
      teams_east <- append(teams_east,vF_teams_DT$team.id[i])
  }
}
team_choices <- append(team_choices,c("All","Western Conference","Eastern Conference"))

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
             menuSubItem('2018', tabName = '2018_shots'),
             menuSubItem('2017', tabName = '2017_shots'),
             menuSubItem('2016', tabName = '2016_shots'),
             menuSubItem('2015', tabName = '2015_shots'),
             menuSubItem('2014', tabName = '2014_shots')
    ),
    menuItem('Season Statistics',
             menuSubItem('2018', tabName = '2018_stat'),
             menuSubItem('2017', tabName = '2017_stat'),
             menuSubItem('2016', tabName = '2016_stat'),
             menuSubItem('2015', tabName = '2015_stat'),
             menuSubItem('2014', tabName = '2015_stat')
    ),
    menuItem('Team Performance',
             menuSubItem('2018', tabName = '2018_perf'),
             menuSubItem('2017', tabName = '2017_perf'),
             menuSubItem('2016', tabName = '2016_perf'),
             menuSubItem('2015', tabName = '2015_perf'),
             menuSubItem('2014', tabName = '2014_perf')
    )
  )
)
body <- dashboardBody(
  tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 20
  ),
  tabItems(
    tabItem('2018_shots', uiOutput('shot2018')),
    tabItem('2017_shots', uiOutput('shot2017'))
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")