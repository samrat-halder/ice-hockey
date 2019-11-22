library(shinydashboard)
source("chooser.R")

header <- dashboardHeader(
  title = "Ice Hockey"
)

sidebar <- dashboardSidebar(
  #Here goes more customizations
  collapsed = TRUE,
  sidebarMenu(
  )
)
body <- dashboardBody(
  fluidRow(
    box(solidHeader = T, collapsible = T, width = '80%',
        title = "Inputs", status = "primary", background = "blue",
      selectInput('homeTeam', 'Home Team', choices = c('Boston Bruins','New York Rangers'), selected = 'Boston Bruins', multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL),
      selectInput('awayTeam', 'Away Team', choices = c('Boston Bruins','New York Rangers'), selected = 'New York Rangers', multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
    )
    
  ),
  fluidRow(
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('shots', 'Shots', value = FALSE, width = NULL)),
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('goals', 'Goals', value = FALSE, width = NULL)),
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('hits', 'Hits', value = FALSE, width = NULL))
  ),
  fluidRow(
    tags$img(src='full-rink-2.png', width = '100%'))
  
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")