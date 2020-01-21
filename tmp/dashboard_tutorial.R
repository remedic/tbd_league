
#
#Sidebar Menu
#
#icons - crown (for rankings),
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Roster", icon = icon("user-circle"), tabName = "roster"),
    menuItem("Match History", icon = icon("history"), tabName = "match_history"),
    menuItem("Generate Lineup", icon = icon("bolt"), tabName = "generate_lineup")
  )
)

#
#Body
#
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "roster",
            h2("Roster content")
    ),
    
    tabItem(tabName = "match_history",
            h2("Match history content")
    ),
    tabItem(tabName = "generate_lineup",
            h2("generate lineup content")
    )
  )
)


######################################
ui <- dashboardPage(
  dashboardHeader(title = "Tampa Bay Doubles"),
  sidebar,
  body
)

server <- function(input, output) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
    )
  })
}

shinyApp(ui, server)