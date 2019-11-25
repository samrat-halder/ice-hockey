library(shinydashboard)
#load("~/GitHub/Ice-Hockey/data/2019-11-21_nhl-cleaned-data.RData")

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
    #collapsible box for main inputs
    box(solidHeader = T, collapsible = T, width = '100%',
        title = "Filters", status = "primary", background = "blue",
        #input selections are inside div so we can place left and right inputs side by side
        div(style="display: inline-block;vertical-align:top; width: 45%;",
            titlePanel('Left'),
            selectInput('leftTeam', 'Team', choices = vF_teams_DT$long.name, selected = 'Boston Bruins', multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
            selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL)),
        div(style="display: inline-block;vertical-align:top; width: 45%;",
            titlePanel('Right'),
            selectInput('rightTeam', 'Team', choices = vF_teams_DT$long.name, selected = 'New York Rangers', multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            selectInput('rightHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL))
    )
    
  ),
  #checkboxes for each event type (more to be added)
  fluidRow(
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('shots', 'Shots', value = TRUE, width = NULL)),
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('goals', 'Goals', value = TRUE, width = NULL)),
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('hits', 'Hits', value = TRUE, width = NULL))
  ),
  fluidRow(
    mainPanel(plotlyOutput("icemap"), status = "warning", title = "title4", width = '100%', solidHeader = TRUE)
  
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")