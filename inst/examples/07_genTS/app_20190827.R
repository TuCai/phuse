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
    # , extendShinyjs(text="$('#studyid').attr('maxlength', 200)")
    # , shinyjs::runjs("$('#studyid').attr('maxlength', 50)")
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

    # , tabItems(
    , fluidRow(
      tabItem("tab1", hr()
              , menuItem('About phuse Pkg',icon=icon('code'),href='install_phuse_pkg.png')
              , menuItem('Creation Guide',icon=icon('code'),href='Simplified_TS_Creation_Guide_v2.pdf')
              , menuItem('Utility Requirements',icon=icon('code'),href='XPT_Utility_Requirements.xlsx')
              , menuItem('Source Code',icon=icon('code'),href='https://github.com/TuCai/phuse/blob/master/inst/examples/07_genTS/app.R')
              , hr()
      )
    )
#    , tags$script('
#              $(document).on("shiny:inputchanged", function(e) {
#                var x1 = document.getElementById("tsvalnf");
#                var x2 = document.getElementById("tsval");
#                if (e.name === "tsvalnf") {
#                  alert("TSVALNF Changed - " + x1.value);
#                  if (x1.value != "") {x2.value = "";}
#                  alert("TSVALNF Changed 2 - " + x2.value);
#                } else {
#                  if (e.name === "tsval") {
#                    alert("TSVAL Changed - " + x2.value);
#                    if (x2.value != "") {x1.value = "";}
#                    alert("TSVAL Changed 2 - " + x1.value);
#                  }
#                }
#              });
#            ')
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
    # if ( (is.character(input$tsval) && input$tsval == 'YYYY-MM-DD')) {
    #  tsval <- as.character(strftime(as.Date(Sys.time()),"%Y-%m-%d"));
    #} else {
      if (is_empty(input$tsval)) {
        tsval <-  as.character("")
      } else {
        tsval <- as.character(input$tsval)
      }
    #}
    tsval
  })

  output$tabP1 <- renderUI({
    tabPanel("Create"
      , div(id = "form"
         , textInput("studyid", "Study ID (STUDYID) *" )
         , selectInput("tsparmcd", "Study Type Parameter (TSPARMCD) *"
            , choices = list("Clinical (SSTDTC)" = "SSTDTC"
            , "Nonclinical (STSTDTC)" = "STSTDTC")
            , selected = "STSTDTC")
         , dateInput("tsval", label = "Study Start Date (TSVAL)", format = "yyyy-mm-dd" )
         , textInput("tsvalnf", "Exception Code (TSVALNF)" )
        )
      , hr()
      , hidden(downloadButton('downloadData', 'Download'))
    )

  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("ts", ".xpt",sep="") },
    content  = function(file) {
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
    datatable(df);  # from DT package
  })

  output$tabP2 <- renderUI({
    tabPanel("View"
      , DT::dataTableOutput("DT2")
      , hr()
      , downloadButton('downloadData', 'Download')
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

  observeEvent(input$tsvalnf, {
    if (input$tsvalnf != "") {
      runjs('var x = document.getElementById("tsval"); x.value = "";x.disabled=true;')
      updateDateInput(session, "tsval", value = NA )
    } else {
      runjs('var x = document.getElementById("tsval"); x.disabled=false;')
      # updateTextInput(session, "tsval", label = "Study Start Date (TSVAL)")
    }
   })

  observeEvent(input$tsval, {
    # str(input$tsval);
    # str(length(input$tsval));
    if (length(input$tsval) == 1) {
      # runjs('var x = document.getElementById("tsvalnf"); alert("TSVALNF=" + x.value);x.value = "";')
      runjs('var x = document.getElementById("tsvalnf"); x.value = "";')
      updateTextInput(session, "tsvalnf", value = "")
    }
  })

}

shinyApp(ui, server)

