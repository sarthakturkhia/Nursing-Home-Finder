# loading all the libraries required for the project
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)

# the available range of the overall ratings of the nursing homes
ratings.range<- range(1,5)

# loads the data set for the 50 states
data("state")

# function to manipulate the default information in the drop down menu
customSentence <- function(numItems, type) {
  strong("Data abbrevations defined (Scrollable):")
}

# Function to set the dropdown Menu for the glossary terms
dropdownMenuCustom <- function (..., type = c("messages", "notifications", "tasks"), 
                                badgeStatus = "primary", icon = NULL, .list = NULL, 
                                customSentence = customSentence) {
  
  # choses the type of dropdown menu selected
  type <- match.arg(type)
  
  # denotes the number of glossary terms
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  
  # creates a vector for the list of items
  items <- c(list(...), .list)
  
  # outputs the list of glossary items
  lapply(items, shinydashboard:::tagAssert, type = "li")
  
  # renames the dropdown menu
  dropdownClass <- paste0("dropdown ", type, "-menu")
  
  # sets the icon for the glossary
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  
  # sets the number of items
  numItems <- length(items)
  
  # labels the number of items in the glossary according to priority
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus), 
                  numItems)
  }
  
  # sets the different tags for the glossary, giving the user to scrolll through each item
  tags$li(
    class = dropdownClass, 
    a(href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)), 
      tags$li(
        tags$ul(class = "menu", items))
    )
  )
}

# Define a UI using the 'fluidPage()` layout
ui <- fluidPage(
  
  # sets the logo for the project
  imageOutput("logo", height = "200px", width = "600px"),
  
  # sets the dashboard for the layout
  dashboardPage(skin = "blue", 
                
                # the title for the dashboard
                dashboardHeader(title = "Dashboard",
                                
                                # customizes the glossary dropdown on the side of the dashboard
                                dropdownMenuCustom(type = "messages", icon = em(strong("Glossory")), 
                                                   customSentence = customSentence,
                                                   
                                                   # glossary term one details
                                                   messageItem(
                                                     from = "Certified Nursing Assistant (CNA):",
                                                     message = HTML("Helps patients or clients with
                                                                    healthcare <br/> needs
                                                                    under the supervision of <br/> 
                                                                    Registered Nurse (RN) or a 
                                                                    <br/> Licensed Practical Nurse 
                                                                    (LPN)."),
                                                     icon = icon("user-md")),
                                                   # glossary term two details
                                                   messageItem(
                                                     from = "Registered Nurse (RN):",
                                                     message = HTML("A nurse who
                                                                    has graduated from a <br/>
                                                                    nursing program and has 
                                                                    sucessfully <br/>obtained 
                                                                    a license."),
                                                     icon = icon("user-md")),
                                                   
                                                   # glossary term three details
                                                   messageItem(
                                                     from = "Licensed Practical Nurse (LPN):",
                                                     message = HTML("A nurse who cares for
                                                                    people who are sick, <br/>
                                                                    injured, convalescent, or 
                                                                    disabled. LPNs <br/>
                                                                    work under the direction 
                                                                    of registered <br/>nurses or 
                                                                    physicians."),
                                                     icon = icon("user-md")))),
                
                # sets the side panel for the dashboard
                dashboardSidebar(
                  
                  # sets the different side bar options for the dashboard
                  sidebarMenu(
                    
                    # sets the first tab, the introduction page
                    menuItem("About", tabName = "introduction", icon = icon("users")),
                    
                    # sets the second tab, the data table and the map page
                    menuItem("Find Your Home", tabName = "datatable", icon = icon("search")),
                    
                    # sets the third tab, the data overview page with the statistical analysis
                    menuItem("Data Overview", tabName = "graph", icon = icon("table")),
                    
                    # sets the dropdown widget for selecting the state
                    selectInput("state", "Location", c("National", state.name)),
                    
                    # sets the slider widget for fitering theough the overall ratings
                    sliderInput("ratings", "Filter by Nursing Home Ratings:",
                                min=ratings.range[1], max=ratings.range[2], value= c(3, 3)),
                    
                    # sets radio buttons for choosing appropriate penalties by user
                    radioButtons("radio", "Filter by Fines:",
                                 choices = list("Has a fine" = 1,"Doesn't have a fine" = 2, 
                                                "All" = 3),selected = 3)
                  )
                ),
                
                # organizes the main body of the dashboard
                dashboardBody(
                  
                  # sets the primary box for the main body 
                  box(width = 20, status = "primary",
                      tabItems(
                        
                        # adds the intoduction tab
                        tabItem(tabName = "introduction",
                                htmlOutput("introduction")
                        ),
                        
                        # adds the main data set tab 
                        tabItem(tabName = "datatable",
                                
                                # sets the title for the data table 
                                tabBox(title = "Selection Summary", 
                                       id = "tabset1", 
                                       height = "470px", 
                                       width = 11, 
                                       side = "right",
                                       
                                       # a tab to toggle to the map 
                                       tabPanel("Map", leafletOutput("lemap"), 
                                                icon = icon("map-marker")),
                                       
                                       # a tab to toggle to the rating and penalties
                                       tabPanel("Ratings & Penalties", 
                                                verbatimTextOutput('ratings'), 
                                                hr(),
                                                verbatimTextOutput('penalties')),
                                       
                                       # a tab to toggle to the other relavent information 
                                       tabPanel("Other", verbatimTextOutput('other')),
                                       
                                       # a tab to toggle to the general provider information
                                       # for the nursing home
                                       tabPanel("General Info", verbatimTextOutput('general'))
                                ),
                                
                                # creates the data table in the main data tab below the map
                                box(title = "Data Table:", status = "primary", solidHeader = TRUE,
                                    br(), width = 11, 
                                    column(5, DT::dataTableOutput('table'))
                                )
                        ),
                        
                        # adds the statistical analysis tab
                        tabItem(tabName = "graph",
                                
                                # adds a description for the graph tab describing the scatter plot
                                # and the pie chart
                                p("The default graphs represent national data. 
                                  In order to alter the graphs, use the location dropdown.",
                                  style = "font-size:20px;"),
                                br(),
                                
                                # adds the title, the header style and the status of the pie chart
                                box(title = textOutput("viz"), solidHeader = TRUE, 
                                    status = "primary",
                                    plotOutput("pie")),
                                
                                # adds the title, the header style and the status of the scatter plot
                                box(title = textOutput("viz2"), solidHeader = TRUE, 
                                    status = "primary",
                                    plotOutput("bar")),
                                
                                # adds the title and status for the summary of the pie chart
                                box(title = "Pie Chart Summary", status = "primary",
                                    textOutput("pie.chart.summary")),
                                
                                # adds the title and status for the summary of the scatter plot
                                box(title = "Point Graph Summary", status = "primary",
                                    textOutput("point.graph.summary"))
                                
                        )
                      )
                  )
                )
  )
)

shinyUI(ui)