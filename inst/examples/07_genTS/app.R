library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(SASxport)
library(Hmisc)
library(rhandsontable)

header <- dashboardHeader(
  title = "Creating Simplified TS"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Simplified TS", icon = icon("cog"),
             menuSubItem("Clinical Study", tabName = "subitem1"),
             menuSubItem("Non-clinical Study", tabName = "subitem2")
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

    titlePanel("Mimicking a Google Form with a Shiny app"),
    div(
      id = "form",

      textInput("name", "Name", ""),
      textInput("favourite_pkg", "Favourite R package"),
      checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
      sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
      selectInput("os_type", "Operating system used most frequently",
                  c("",  "Windows", "Mac", "Linux")),
      actionButton("submit", "Submit", class = "btn-primary")
    ),


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
