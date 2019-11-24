library(shinydashboard)

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
    box(solidHeader = T, collapsible = T, width = '100%',
        title = "Filters", status = "primary", background = "blue",
        div(style="display: inline-block;vertical-align:top; width: 45%;",
            titlePanel('Left'),
            selectInput('leftTeam', 'Team', choices = c('Boston Bruins','New York Rangers'), selected = 'Boston Bruins', multiple = TRUE,
                    selectize = TRUE, width = NULL, size = NULL),
            selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL)),
        div(style="display: inline-block;vertical-align:top; width: 45%;",
            titlePanel('Right'),
            selectInput('leftTeam', 'Team', choices = c('Boston Bruins','New York Rangers'), selected = 'Boston Bruins', multiple = TRUE,
                        selectize = TRUE, width = NULL, size = NULL),
            selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL))
    )
    
  ),
  fluidRow(
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('shots', 'Shots', value = TRUE, width = NULL)),
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('goals', 'Goals', value = TRUE, width = NULL)),
    div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput('hits', 'Hits', value = TRUE, width = NULL))
  ),
  fluidRow(
    #tags$img(src='full-rink-2.png', width = '100%'))
    mainPanel(plotlyOutput("icemap"), status = "warning", title = "title4", width = '100%', solidHeader = TRUE)
  
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")