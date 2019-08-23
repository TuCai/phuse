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
    if (is_empty(input$tsval) || (is.character(input$tsval) && input$tsval == 'YYYY-MM-DD')) {
      tsval <- strftime(as.Date(Sys.time()),"%Y-%m-%d");
    } else {
      tsval <- input$tsval
      # tsval <- strftime(as.Date(input$tsval),"%Y-%m-%d");
    }
    
    ts <-data.frame(STUDYID=input$studyid
                    , DOMAIN="TS"
                    , TSSEQ = 1
                    , TSGRPID = ""
                    , TSPARMCD=input$tsparmcd
                    , TSPARM=input$tsparm
                    , TSVAL=tsval
                    , TSVALNF=input$tsvalnf
                    # , stringAsFactors=FALSE
    );
    ts
  })
  
  #ofn <- reactive({
  #gen_simplified_ts(input$studyid, input$tsparmcd, input$tsval, input$tsvalnf)
  #})
  
  # -------------------- 1 tabPanel: Create  --------------------------------
  ts_val <- reactive({
    ifelse(input$studytp=='clin','SSTCTC','STSTDTC')
  })
  ts_val2 <- reactive({
    ifelse(input$studytp=='clin2','Study Start Date','Study Start Date')
  })
  output$tabP1 <- renderUI({
    tabPanel("Create"
             , div(id = "form"
                   , textInput("studyid", " STUDYID: ", "Study Identifier")
                   , textInput("tsparmcd", " TSPARMCD: ", ts_val())
                   , textInput("tsparm", " TSPARM: ", ts_val2())
                   , dateInput("tsval", label = " TSVAL: ", value = "YYYY-MM-DD")
                   # , textInput("tsval", " TSVAL: ","YYYY-MM-DD")
                   # , textInput("tsvalnf", " TSVALNF: ")
                   , selectInput("tsvalnf", label = " TSVALNF: ",
                                 choices = list("Blank" = "", "Missing" = "NA"),
                                 selected = "")
                   
                   # , actionButton("submit", " Submit ", class = "btn-primary")
             )
             , downloadButton('downloadData', 'Download')
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("ts", ".xpt",sep="") },
    content  = function(file) {
      # get dataframe with thedata
      studyData <- ts_content()
      studyData=transform(studyData, TSSEQ = as.numeric(TSSEQ))
      # set to characters not factors
      studyData$STUDYID <- as.character(studyData$STUDYID)
      studyData$DOMAIN <- as.character(studyData$DOMAIN)
      studyData$TSGRPID <- as.character(studyData$TSGRPID)
      studyData$TSPARMCD <- as.character(studyData$TSPARMCD)
      studyData$TSPARM <- as.character(studyData$TSPARM)
      studyData$TSVAL <- as.character(studyData$TSVAL)
      studyData$TSVALNF <- as.character(studyData$TSVALNF)
      # Set length for character fields
      SASformat(studyData$DOMAIN) <-"$2."	
      # Label list
      Hmisc::label(studyData$DOMAIN)<- "Domain Abbreviation" 
      Hmisc::label(studyData$STUDYID)<- "Study Identifier" 
      Hmisc::label(studyData$TSSEQ)<-  "Sequence Number" 
      Hmisc::label(studyData$TSGRPID)<-  "Group Identifier" 
      Hmisc::label(studyData$TSPARMCD)<-  "Trial Summary Parameter Short Name" 
      Hmisc::label(studyData$TSPARM)<-  "Trial Summary Parameter Name" 
      Hmisc::label(studyData$TSVAL)<- "Parameter Value" 
      Hmisc::label(studyData$TSVALNF)<- "Parameter Null FLavor"
      # place this dataset into a list with a name
      aList = list(studyData)
      # name it
      names(aList)[1]<-"TS"
      # and label it
      attr(aList,"label") <- "TRIAL SUMMARY"
      # write out dataframe
      write.xport(
        list=aList,
        file = file,
        verbose=FALSE,
        sasVer="7.00",
        osType="R 3.5.2",	
        cDate=Sys.time(),
        formats=NULL,
        autogen.formats=TRUE
      )
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

