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
    id = "tab1",
    menuItem("Simplified TS", icon = icon("cog"),
      menuSubItem("Creation Guide", href = 'Simplified_TS_Creation_Guide_v2.pdf', newtab = FALSE),
      menuSubItem("About phuse Package", href = 'install_phuse_pkg.png', newtab = TRUE)
    )
  )
)

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
         .form-group { display: table-col;}")
    ),
    fluidRow(tabsetPanel(id='tabs'
      , tabPanel("Create", uiOutput("tabP1"))
      # , tabPanel("View", DT::dataTableOutput("tabP2"))
      , tabPanel("View", uiOutput("tabP2"))
    ))
   # , tabItems(
    , fluidRow(
      tabItem("tab1", hr()
        , menuItem('About phuse Pkg',icon=icon('code'),href='install_phuse_pkg.png')
        , menuItem('Creation Guide',icon=icon('code'),href='Simplified_TS_Creation_Guide_v2.pdf')
        , menuItem('Source Code',icon=icon('code'),href='https://github.com/TuCai/phuse/blob/master/inst/examples/07_genTS/app.R')
      )
  #    , tabItem("subitem2",
  #            "Sub-item 2 tab content"
  #    )
    )
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
    if ( # is_empty(as.character(input$tsval)) ||
        as.character(input$tsval) == 'YYYY-MM-DD') {
      tsval <- as.character( strftime(as.Date(Sys.time()),"%Y-%m-%d") );
    } else {
      tsval <- as.character(input$tsval)
      # tsval <- strftime(as.Date(input$tsval),"%Y-%m-%d");
    }

    ts <-data.frame(STUDYID=input$studyid
      , TSPARMCD=input$tsparmcd
      , TSVAL=tsval
      , TSVALNF=input$tsvalnf
      # , stringAsFactors=FALSE
    );

    label (ts)           <- 'Trial Summary';
    label (ts$STUDYID)   <- 'Study Identifier';
    label (ts$TSPARMCD)  <- 'Trial Summary Parameter Short Name';
    label (ts$TSVAL)     <- 'Parameter Value';
    label (ts$TSVALNF)   <- 'Parameter Null Flavor';
    ts
  })


  # -------------------- 1 tabPanel: Create  --------------------------------

  output$tabP1 <- renderUI({
    tabPanel("Create"
      , div(id = "form"
      , textInput("studyid", "Study ID (STUDYID)", "")
      , selectInput("tsparmcd", "Study Type Parameter (TSPARMCD)"
          , choices = list("Clinical (SSTDTC)" = "SSTDTC"
                           , "Nonclinical (STSTDTC)" = "STSTDTC")
          , selected = "")
      , dateInput("tsval", label = "Study Start Date (TSVAL)"
                  , ifelse( is_empty(input$tsvalnf), as.character(input$tsval), "")  )
      # , textInput("tsval", " TSVAL: ","YYYY-MM-DD")
       , textInput("tsvalnf", "Exception Code (TSVALNF)"
          , ifelse( is_empty(as.character(input$tsval)) , as.character(input$tsvalnf), "")  )

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
    #, validate(
    #  need(input$studyid, 'StudyID is required!')
      #      , need(input$tsparmcd, 'Param Code is required!')
      #     , need(input$tsval, 'Date is required!')
    #)
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
