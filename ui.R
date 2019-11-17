library(shinydashboard)

header <- dashboardHeader(
  title = "Ice Hockey"
)

sidebar <- dashboardSidebar(
  #Here goes more customizations
  sidebarMenu(
    menuItem(tabName = "Tab1", "Charts 1"),
    menuItem(tabName = "Tab2", "Charts 2")
    
  )
)
body <- dashboardBody(
  #Here goes UI for each tab
  tabItems(
    tabItem(
      tabName = "Tab1",
      h2(
        fluidRow(
          box( plotOutput("wc1"), status = "primary", title = "title1", width = 4, solidHeader = TRUE),
          box( plotOutput("wc2"), status = "primary", title = "Title2", width = 4, solidHeader = TRUE),
          box( plotOutput("wc3"), status = "primary", title = "Title3", width = 4, solidHeader = TRUE)
        )
        ,
        fluidRow(
          box( plotOutput("Plot1"), status = "primary", title = "title4", width = 4, solidHeader = TRUE),
          box( plotOutput("Plot2"), status = "primary", title = "Title5", width = 4, solidHeader = TRUE),
          box( plotOutput("Plot3"), status = "primary", title = "Title6", width = 4, solidHeader = TRUE)
        )
      )
    ),
    tabItem(
      tabName = "Tab2",
      h2(
        fluidRow(
          box( plotOutput("wc4"), status = "primary", title = "title1", width = 4, solidHeader = TRUE),
          box( plotOutput("wc5"), status = "primary", title = "Title2", width = 4, solidHeader = TRUE),
          box( plotOutput("wc6"), status = "primary", title = "Title3", width = 4, solidHeader = TRUE)
        )
        ,
        fluidRow(
          box( plotOutput("Plot4"), status = "primary", title = "title4", width = 4, solidHeader = TRUE),
          box( plotOutput("Plot5"), status = "primary", title = "Title5", width = 4, solidHeader = TRUE),
          box( plotOutput("Plot6"), status = "primary", title = "Title6", width = 4, solidHeader = TRUE)
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "red")