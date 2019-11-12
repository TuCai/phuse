# Purpose: simple app to create a simplified TS domain dataset
# Development History:
#   MM/DD/2019 (developer) - description
#   07/12/2019 (htu) - initial creation based on FDA guide:
#     https://github.com/TuCai/phuse/blob/master/inst/examples/07_genTS/www/Simplified_TS_Creation_Guide_v2.pdf
#   08/13/2019 (htu) - modified the field labels and control based on new requirements:
#   08/16/2019 (bfriedman) - modified the download handler to use ts.xpt as
#      download file name and change all fields to character fields
#   08/23/2019 (htu) - merged Bob's code
#   08/27/2019 (htu) - added JS codes to make studyid as required and TSVAL and TSVALNF mutually exclusive
#   11/04/2019 (htu) - implemented FDA E-1 ~ E-8 and D-1
#

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
library(stringr)

is_empty <- phuse::is_empty;

header <- dashboardHeader(
  title = "Creating TS Domain"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tab1",
    menuItem("Simplified TS", icon = icon("cog"),
      menuSubItem("Simplified ts.xpt Creation Guide", href = 'https://www.fda.gov/industry/study-data-standards-resources/study-data-submission-cder-and-cber', newtab = FALSE)
    , menuSubItem("About phuse Package", href = 'install_phuse_pkg.png', newtab = TRUE)
    # , menuSubItem('Utility Requirements',href = 'XPT_Utility_Requirements.xlsx', newtab = TRUE)
    , menuSubItem('Source Code',href='https://github.com/TuCai/phuse/blob/master/inst/examples/07_genTS/app.R', newtab = TRUE)

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
    )
    , fluidRow(tabsetPanel(id='tabs'
      , tabPanel("Create", uiOutput("tabP1"))
      , tabPanel("View", uiOutput("tabP2"))
    ))
#    , bsAlert(inputID = "alert_anchor")
    # , tabItems(
    , fluidRow(
      tabItem("tab1", hr()
        , menuItem('About phuse Pkg',icon=icon('code'),href='install_phuse_pkg.png')
        , menuItem('Simplified ts.xpt Creation Guide',icon=icon('code'),href='https://www.fda.gov/industry/study-data-standards-resources/study-data-submission-cder-and-cber')
    #   , menuItem('Utility Requirements',icon=icon('code'),href='XPT_Utility_Requirements.xlsx')
        , menuItem('Source Code',icon=icon('code'),href='https://github.com/TuCai/phuse/blob/master/inst/examples/07_genTS/app.R')
              , hr()
      )
    )
    , tags$footer("PhUSE Code Sharing (Repository) Project"
                  , align = "center"
                  , style = "position:dynamic;
              bottom:0;
              width:100%;
              height:30px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: blue;
              z-index: 1000;"
    )
  )
)

server <- function(input, output, session) {
  ts_content <- reactive({
    validate(
      need(input$studyid != "", "Please provide Study ID.")
    )
    validate(
      need(input$tsparmcd != "", "Please provide Study Type Parameter.")
    )
    req(input$studyid)
    req(input$tsparmcd)
    ts <-data.frame(STUDYID=input$studyid
                #    , DOMAIN="TS"
                #    , TSSEQ = 1
                #    , TSGRPID = ""
                    , TSPARMCD=input$tsparmcd
                #    , TSPARM=input$tsparm
                    , TSVAL=get_tsval()
                    , TSVALNF=input$tsvalnf
                    # , stringAsFactors=FALSE
    );
    ts
  })

  # -------------------- 1 tabPanel: Create  --------------------------------
  get_tsval <- reactive({
    if (is_empty(input$tsval)) {
      tsval <-  as.character("")
    } else {
      tsval <- as.character(input$tsval)
    }
    tsval
  })

  output$tabP1 <- renderUI({
    tabPanel("Create"
      , div(id = "form"
         , textInput("studyid", "Study ID (STUDYID) *" )
         , bsAlert("alert")
         , selectInput("tsparmcd", "Study Type Parameter (TSPARMCD) *"
            , choices = list("Clinical (SSTDTC)" = "SSTDTC"
            , "Nonclinical (STSTDTC)" = "STSTDTC")
            , selected = "STSTDTC")
         , dateInput("tsval", value = NA, label = "Study Start Date (TSVAL)"
             , format = "yyyy-mm-dd" )
         , selectInput("tsvalnf", "Exception Code (TSVALNF)"
             , choices = list("NA" = "NA", "__Blank__" = ""), selected = "")
         # , textInput("tsvalnf", "Exception Code (TSVALNF)" )
        )
      , hr()
      , hidden(downloadButton('downloadData', 'Download'))
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("ts", ".xpt",sep="") },
    content  = function(file = filename) {
      str(file)
      # get dataframe with thedata
      studyData <- ts_content()
    #  studyData=transform(studyData, TSSEQ = as.numeric(TSSEQ))
      # set to characters not factors
      studyData$STUDYID   <- as.character(studyData$STUDYID)
    #  studyData$DOMAIN    <- as.character(studyData$DOMAIN)
    #  studyData$TSGRPID   <- as.character(studyData$TSGRPID)
      studyData$TSPARMCD  <- as.character(studyData$TSPARMCD)
    #  studyData$TSPARM    <- as.character(studyData$TSPARM)
      studyData$TSVAL     <- as.character(studyData$TSVAL)
      studyData$TSVALNF   <- as.character(studyData$TSVALNF)
      # Set length for character fields
    #  SASformat(studyData$DOMAIN) <-"$2."
      # Label list
    #  Hmisc::label(studyData$DOMAIN)  <- "Domain Abbreviation"
      Hmisc::label(studyData$STUDYID) <- "Study Identifier"
    #  Hmisc::label(studyData$TSSEQ)   <- "Sequence Number"
    #  Hmisc::label(studyData$TSGRPID) <- "Group Identifier"
      Hmisc::label(studyData$TSPARMCD)<- "Trial Summary Parameter Short Name"
    #  Hmisc::label(studyData$TSPARM)  <- "Trial Summary Parameter Name"
      Hmisc::label(studyData$TSVAL)   <- "Parameter Value"
      Hmisc::label(studyData$TSVALNF) <- "Parameter Null FLavor"
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
  output$DT2 <- renderDataTable({
    df <- ts_content()
    str(df)
    datatable(df, options = list(dom = 't'));  # from DT package
  })

  output$tabP2 <- renderUI({
    tabPanel("View"
      , DT::dataTableOutput("DT2")
      , hr()
      , hidden(downloadButton('downloadData', 'Download'))
      # , downloadButton('downloadData', 'Download')
    )
  })

  observe({
    if(input$showpanel == TRUE) {
      js$showSidebar()
    }
    else {
      js$hideSidebar()
    }
  })

  observeEvent(input$studyid, {
    if (input$studyid == "")
      hide("downloadData")
    else
      show("downloadData")
  })

  # 11/04/2019: For FDA D-1 in PhUSE Utility feedback v4.0
  observeEvent(input$studyid, {
    if(input$studyid != "") {
      if (str_length(input$studyid)>200) {
        updateTextInput(session, "studyid", value = str_sub(input$studyid, end=200) )
        createAlert(session, "alert", "StudyIDAlert", title = "StudyID Alert"
          , content = "Study ID must not exceed 200 characters and will be truncated."
          , dismiss = TRUE, style = "warning", append = FALSE)
        Sys.sleep(3)
      } else {
        closeAlert(session, "StudyIDAlert")
      }
    }
  })

  # 11/04/2019: Commented out for FDA E-1
#  observeEvent(input$tsvalnf, {
#    if (input$tsvalnf != "") {
#      # runjs('var x = document.getElementById("tsval"); x.value = "";x.disabled=true;')
#      updateDateInput(session, "tsval", value = NA )
#    }
#   })

#  observeEvent(input$tsval, {
#    # str(input$tsval);
#    # str(length(input$tsval));
#    if (length(input$tsval) == 1) {
#      # runjs('var x = document.getElementById("tsvalnf"); alert("TSVALNF=" + x.value);x.value = "";')
#      # runjs('var x = document.getElementById("tsvalnf"); x.value = "";')
#      updateTextInput(session, "tsvalnf", value = "")
#    }
#  })
}

shinyApp(ui, server)

