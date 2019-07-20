library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(SASxport)
library(Hmisc)
library(rhandsontable)
library(phuse)
library(DT)
library(V8)

is_empty <- phuse::is_empty;

header <- dashboardHeader(
  title = "Creating TS Domain"
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

tabP1a <- radioButtons("studytp", " Study Type: "
        , c("Clinical" = "clin", "Non-clinical" = "Non")
        , selected = NULL)

ui <- dashboardPage(
  # dashboardHeader(),
  # dashboardSidebar(),
  header,
  sidebar,
  dashboardBody(
    useShinyjs()
    , extendShinyjs(text = 'shinyjs.hideSidebar = function(params) {
      $("body").addClass("sidebar-collapse");
      $(window).trigger("resize"); }')
    , extendShinyjs(text='shinyjs.showSidebar = function(params) {
      $("body").removeClass("sidebar-collapse");
      $(window).trigger("resize"); }')
   , bsButton("showpanel", "Show/Hide sidebar",icon = icon("toggle-off"),
      type = "toggle",style = "info", value = TRUE)
    , tags$head(
      tags$style(type="text/css",
        "label{ display: table-cell; text-align: right; vertical-align: middle; }
         .form-group { display: table-row;}")
    ),
    fluidRow(tabsetPanel(id='tabs'
      , tabPanel("Create", tabP1a, uiOutput("tabP1"))
      # , tabPanel("View", DT::dataTableOutput("tabP2"))
      , tabPanel("View", uiOutput("tabP2"))
    ))
   #, tabItems(
  #    tabItem("subitem1",
  #            "Sub-item 1 tab content"
  #    ),
  #    tabItem("subitem2",
  #            "Sub-item 2 tab content"
  #    )
  #  )
  , tags$footer("PhUSE Code Sharing (Repository) Project"
    , align = "center"
    , style = "position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: blue;
              z-index: 1000;"
    )
  )
)

server <- function(input, output, session) {
  ts_content <- reactive({
    if (is_empty(input$tsval) || input$tsval == 'YYYY-MM-DD') {
      tsval <- strftime(as.Date(Sys.time()),"%Y-%m-%d");
    } else {
      tsval <- input$tsval
    }

    ts <-data.frame(STUDYID=input$studyid
      , TSPARMCD=input$tsparmcd
      , TSVAL=tsval
      , TSVALNF=input$tsvalnf
      # , stringAsFactors=FALSE
    );

    label (ts)       <- 'Trial Summary';
    label (ts$STUDYID)   <- 'Study Identifier';
    label (ts$TSPARMCD)  <- 'Trial Summary Parameter Short Name';
    label (ts$TSVAL)     <- 'Parameter Value';
    label (ts$TSVALNF)   <- 'Parameter Null Flavor';
    ts
  })

  # ofn <- reactive({
  #  gen_simplified_ts(input$studyid, input$tsparmcd, input$tsval, input$tsvalnf)
  # })

  # -------------------- 1 tabPanel: Create  --------------------------------
  ts_val <- reactive({
    ifelse(input$studytp=='clin','SSTCTC','STSTDTC')
  })
  output$tabP1 <- renderUI({
    tabPanel("Create"
      , div(id = "form"
      , textInput("studyid", " STUDYID: ", "Study Identifier")
      , textInput("tsparmcd", " TSPARMCD: ", ts_val())
      , textInput("tsval", " TSVAL: ","YYYY-MM-DD")
      , textInput("tsvalnf", " TSVALNF: ")
      # , actionButton("submit", " Submit ", class = "btn-primary")
      )
      , downloadButton('downloadData', 'Download')
   )
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("ts", ".xpt",sep="") },
    content  = function(file) {
      write.xport(ts_content(), file=file)
     }
   )

  # -------------------- 2 tabPanel: View  ----------------------------------
  output$tabP2 <- renderUI({
    tabPanel("View"
      , DT::dataTableOutput("DT2")
      , downloadButton('downloadData', 'Download')
    )
  })

  output$DT2 <- renderDataTable({
    df <- ts_content()
    datatable(df);  # from DT package
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
