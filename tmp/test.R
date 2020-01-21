library(shiny)
library(shinyjs)
library(shinydashboard)

# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              name = "Name", 
              phone = "Phone Number", 
              email = "Email")
  
  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
GetNextId <- function() {
  if (exists("roster") && nrow(roster) > 0) {
    max(as.integer(rownames(roster))) + 1
  } else {
    return (1)
  }
}

#C
CreateData <- function(data) {
  
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("roster")) {
    roster <<- rbind(roster, data)
  } else {
    roster <<- data
  }
}

#R
ReadData <- function() {
  if (exists("roster")) {
    roster
  }
}



#U
UpdateData <- function(data) {
  data <- CastData(data)
  roster[row.names(roster) == row.names(data), ] <<- data
}

#D
DeleteData <- function(data) {
  roster <<- roster[row.names(roster) != unname(data["id"]), ]
}


# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(name = data["name"], 
                      phone = data["phone"], 
                      email = data["email"],
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}




# Return an empty, new record
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", name = "", phone = "", email = ""))
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "name", value = unname(data["name"]))
  updateTextInput(session, "phone", value = unname(data["phone"]))
  updateTextInput(session, "email", value = unname(data["email"]))

}


ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  #data table
  DT::dataTableOutput("roster", width = 300), 
  
  #input fields
  tags$hr(),
  shinyjs::disabled(textInput("id", "Id", "0")),
  textInput("name", "Name", ""),
  textInput("phone", "Phone Number", ""),
  textInput("email", "Email", ""),
  
  #action buttons
  actionButton("submit", "Submit"),
  actionButton("new", "New"),
  actionButton("delete", "Delete")
)

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)


server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$roster_rows_selected, {
    if (length(input$roster_rows_selected) > 0) {
      data <- ReadData()[input$roster_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display table
  output$roster <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
  
  
}

shinyApp(ui = dashboardPage, server = server)