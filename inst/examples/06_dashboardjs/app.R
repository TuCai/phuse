library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(SASxport)
library(Hmisc)
library(rhandsontable)

header <- dashboardHeader(
  title = "Creating Simplified TS",

  # Dropdown menu for messages
  dropdownMenu(type = "messages", badgeStatus = "success",
    messageItem("Support Team",
               "This is the content of a message.",
               time = "5 mins"
    ),
    messageItem("Support Team",
               "This is the content of another message.",
               time = "2 hours"
    ),
    messageItem("New User",
               "Can I get some help?",
               time = "Today"
    )
  ),

  # Dropdown menu for notifications
  dropdownMenu(type = "notifications", badgeStatus = "warning",
    notificationItem(icon = icon("users"), status = "info",
        "5 new members joined today"
    ),
    notificationItem(icon = icon("warning"), status = "danger",
        "Resource usage near limit."
    ),
    notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
        status = "success", "25 sales made"
    ),
    notificationItem(icon = icon("user", lib = "glyphicon"),
        status = "danger", "You changed your username"
    )
  ),

  # Dropdown menu for tasks, with progress bar
  dropdownMenu(type = "tasks", badgeStatus = "danger",
     taskItem(value = 20, color = "aqua",
              "Refactor code"
     ),
     taskItem(value = 40, color = "green",
                "Design new layout"
     ),
     taskItem(value = 60, color = "yellow",
            "Another task"
     ),
     taskItem(value = 80, color = "red",
            "Write documentation"
     )
  )
)

sidebar <- dashboardSidebar(
  sidebarUserPanel("User Name",
      subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
       # Image file should be in www/ subdir
       image = "userimage.png"
  ),
  sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
             badgeColor = "green"),
    menuItem("Charts", icon = icon("bar-chart-o"),
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2")
    )
  )
)

ui <- dashboardPage(
  # dashboardHeader(),
  # dashboardSidebar(),
  header,
  sidebar,
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = 'shinyjs.hideSidebar = function(params) {
      $("body").addClass("sidebar-collapse");
      $(window).trigger("resize"); }'),
    extendShinyjs(text='shinyjs.showSidebar = function(params) {
      $("body").removeClass("sidebar-collapse");
      $(window).trigger("resize"); }'),
    bsButton("showpanel", "Show/Hide sidebar",icon = icon("toggle-off"),
      type = "toggle",style = "info", value = TRUE),
    fluidRow(tabsetPanel(id='tabs',
      tabPanel(value=1,title="Tab1"),
      tabPanel(value=2,title="Tab2"),
      tabPanel(value=3, title="Plot",
        fluidRow(column(12,plotlyOutput('plot', height=800))))
    )),
    tabItems(
      tabItem("dashboard",
              div(p("Dashboard tab content"))
      ),
      tabItem("widgets",
              "Widgets tab content"
      ),
      tabItem("subitem1",
              "Sub-item 1 tab content"
      ),
      tabItem("subitem2",
              "Sub-item 2 tab content"
      )
    )
  )
)

server <- function(input, output, session) {

  output$plot <- renderPlotly({
    plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
  })

  observe({
    if(input$showpanel == TRUE) {
      js$showSidebar()
    }
    else {
      js$hideSidebar()
    }
  })
}

shinyApp(ui, server)
