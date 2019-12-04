library(shinydashboard)
library(plotly)
library(shinythemes)
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
    tabItem('2018_shots',
      fluidPage(theme = shinytheme("slate"),
        fluidRow(
          align='center',
          #collapsible box for main inputs
          box(solidHeader = T, collapsible = T, width = '100%',
            title = "Filters", status = "primary", background = "blue",
            #input selections are inside div so we can place left and right inputs side by side
            div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: -1em;",
                titlePanel('Left'),
                selectInput('leftTeam', 'Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                            selectize = TRUE, width = NULL, size = NULL),
                selectInput('leftHome', 'Home or Away', choices = c('All','Home','Away'), selected = 'Home', multiple = FALSE,
                            selectize = TRUE, width = NULL, size = NULL)),
            div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                titlePanel('Right'),
                selectInput('rightTeam', 'Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                            selectize = TRUE, width = NULL, size = NULL),
                selectInput('rightHome', 'Home or Away', choices = c('All','Home','Away'), selected = 'Away', multiple = FALSE,
                            selectize = TRUE, width = NULL, size = NULL))
          )
        ),
        #checkboxes for each event type (more to be added)
        fluidRow(
          align = "center",
          div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('shots', 'Shots', value = TRUE, width = NULL)),
          div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('goals', 'Goals', value = TRUE, width = NULL))
        ),
        fluidRow(
          align = "center", 
          box(status = "primary", title = "Rink Layout", width = '100%',
              plotlyOutput("icemap_2018"))
        )
      )
    ),
    tabItem('2017_shots',
        fluidPage(theme = shinytheme("slate"),
          fluidRow(
            align='center',
            #collapsible box for main inputs
            box(solidHeader = T, collapsible = T, width = '100%',
                title = "Filters", status = "primary", background = "blue",
                #input selections are inside div so we can place left and right inputs side by side
                div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: -1em;",
                    titlePanel('Left'),
                    selectInput('leftTeam', 'Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                            selectize = TRUE, width = NULL, size = NULL),
                    selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                            selectize = TRUE, width = NULL, size = NULL)),
                div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                    titlePanel('Right'),
                    selectInput('rightTeam', 'Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    selectInput('rightHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL))
            )
            
          ),
          #checkboxes for each event type (more to be added)
          fluidRow(
            align = "center",
            div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('shots', 'Shots', value = TRUE, width = NULL)),
            div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('goals', 'Goals', value = TRUE, width = NULL))
          ),
          fluidRow(
            align = "center", 
            box(status = "primary", title = "Rink Layout", width = '100%',
            plotlyOutput("icemap_2017"))
          )
      )
    ),
    tabItem('2016_shots',
        fluidPage(theme = shinytheme("slate"),
            fluidRow(
              align='center',
              #collapsible box for main inputs
              box(solidHeader = T, collapsible = T, width = '100%',
                title = "Filters", status = "primary", background = "blue",
                #input selections are inside div so we can place left and right inputs side by side
                div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: -1em;",
                    titlePanel('Left'),
                    selectInput('leftTeam', 'Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL)),
                div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                    titlePanel('Right'),
                    selectInput('rightTeam', 'Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    selectInput('rightHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL))
            )
              
            ),
            #checkboxes for each event type (more to be added)
            fluidRow(
              align = "center",
              div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('shots', 'Shots', value = TRUE, width = NULL)),
              div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('goals', 'Goals', value = TRUE, width = NULL))
            ),
            fluidRow(
              align = "center", 
              box(status = "primary", title = "Rink Layout", width = '100%',
                  plotlyOutput("icemap_2016"))
            )
        )
    ),
    tabItem('2015_shots',
        fluidPage(theme = shinytheme("slate"),
          fluidRow(
            align='center',
            #collapsible box for main inputs
            box(solidHeader = T, collapsible = T, width = '100%',
              title = "Filters", status = "primary", background = "blue",
              #input selections are inside div so we can place left and right inputs side by side
              div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: -1em;",
                  titlePanel('Left'),
                  selectInput('leftTeam', 'Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL),
                  selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)),
              div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                  titlePanel('Right'),
                  selectInput('rightTeam', 'Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL),
                  selectInput('rightHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL))
            )
              
            ),
            #checkboxes for each event type (more to be added)
            fluidRow(
              align = "center",
              div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('shots', 'Shots', value = TRUE, width = NULL)),
              div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('goals', 'Goals', value = TRUE, width = NULL))
            ),
            fluidRow(
              align = "center", 
              box(status = "primary", title = "Rink Layout", width = '100%',
                  plotlyOutput("icemap_2015"))
            )
        )
    ),
    tabItem('2014_shots',
            fluidPage(theme = shinytheme("slate"),
                      fluidRow(
                        align='center',
                        #collapsible box for main inputs
                        box(solidHeader = T, collapsible = T, width = '100%',
                            title = "Filters", status = "primary", background = "blue",
                            #input selections are inside div so we can place left and right inputs side by side
                            div(style="display: inline-block;vertical-align:top; width: 45% ; margin-top: -1em;",
                                titlePanel('Left'),
                                selectInput('leftTeam', 'Team', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                            selectize = TRUE, width = NULL, size = NULL),
                                selectInput('leftHome', 'Home or Away', choices = c('Home','Away'), selected = 'Home', multiple = FALSE,
                                            selectize = TRUE, width = NULL, size = NULL)),
                            div(style="display: inline-block;vertical-align:top; width: 45%; margin-top: -1em;",
                                titlePanel('Right'),
                                selectInput('rightTeam', 'Team', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                            selectize = TRUE, width = NULL, size = NULL),
                                selectInput('rightHome', 'Home or Away', choices = c('Home','Away'), selected = 'Away', multiple = FALSE,
                                            selectize = TRUE, width = NULL, size = NULL))
                        )
                        
                      ),
                      #checkboxes for each event type (more to be added)
                      fluidRow(
                        align = "center",
                        div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('shots', 'Shots', value = TRUE, width = NULL)),
                        div(style="display: inline-block;vertical-align:top; width: 150px; margin:-2em",checkboxInput('goals', 'Goals', value = TRUE, width = NULL))
                      ),
                      fluidRow(
                        align = "center", 
                        box(status = "primary", title = "Rink Layout", width = '100%',
                            plotlyOutput("icemap_2014"))
                      )
            )
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")