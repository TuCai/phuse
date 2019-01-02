# ---------------------------------------------------------------------------
# HISTORY
#   MM/DD/YYYY (developer) - explanation
#   03/13/2017 (htu) - initial creation
#   ...
#   10/18/2018 (htu) - started using search_github
#   10/19/2019 (htu) - added output$show_search and output$search_for
#   01/01/2019 (htu) - merged Bob Friedman's code into output$script_inputs
#
# rm(list=ls())

library(shiny)
library(RCurl)
library(phuse)
library(ggvis)
library(diffobj)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(httr)
library(DT)

# to plot a message in the main panel
plotMessage <- function(inString){
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.34, y = 0.9, paste(inString),
       cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
}


fns <- search_github('*.yml',out_type = 'fnlist');
ff  <- fns[,1]; names(ff) <- fns[,2];
sel <- ff[order(names(ff))]


# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Phuse Script Web Application Framework"),
  includeHTML("www/links.txt"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      htmlOutput("selectUI"),
      radioButtons("src", "File Source:",
                 c("Local" = "loc", "Repository" = "rep", "Search" = "search")),
      # textOutput("result"),
      div(id="yml_name",class="shiny-text-output",style="display: none;"),
      div(id="sel_fn",class="shiny-text-output",style="display: none;"),
      # textInput("yn", "YML File Name: ", verbatimTextOutput("yml_name")),
      br(),
      conditionalPanel(
        condition = "output.show_script_ui",
        uiOutput("script_inputs")
      ),
      conditionalPanel(condition="output.show_script",uiOutput("script_ins")),
      conditionalPanel(condition="output.show_search",uiOutput("search_for"))

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Script", pre(id="script",class="shiny-html-output")),
                  tabPanel("YML", pre(id="yml",class="shiny-html-output")),
                  tabPanel("Info", tableOutput("finfo")),
                  tabPanel("Metadata", tableOutput("mtable")),
                  tabPanel("Verify", tableOutput("verify")),
                  tabPanel("Download", tableOutput("dnload")),
                  tabPanel("Diff", verbatimTextOutput("diff")),
                  tabPanel("Merge", tableOutput("merge")),
                  # tabPanel("Execute", verbatimTextOutput("execute"))
                  tabPanel("Execute", plotOutput("execute")),
                  tabPanel("Search", DT::dataTableOutput("search"))
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {

  m1 <- reactive({ fns })
  # u1 <- reactive({ URLencode(as.character(f1()[input$file,"file_url"])) })
  m2 <- reactive({ # URLencode(m1()[input$file,4])
    f1 <- ifelse(input$src=="loc","file_path", "file_url")
    f2 <- gsub('\r','', fns[input$file,f1], perl = TRUE)
    f3 <- ifelse(input$src=="loc",f2, URLencode(as.character(f2)))
    try(r <- cvt_list2df(read_yml(f3)), silent = TRUE)
    if (is.null(r)) { paste0("Error parsing ", f3) } else { r }
    })
  fn <- reactive({
    f1 <- ifelse(input$src=="loc","file_path", "file_url")
    f2 <- gsub('\r','', fns[input$file,f1], perl = TRUE)
    f3 <- ifelse(input$src=="loc",f2, URLencode(as.character(f2)))
    f3
  })

  # -------------------- Sidebar: Select Script  ----------------------------
  output$selectUI <- renderUI({ selectInput("file", "Select Script:", sel) })

  output$result <- renderText({ paste("Script File ID: ", input$file) })
  output$sel_fn <- renderText({ paste(fns[input$file,"file"]) })
  output$yml_name <- renderText({ as.character(fn()) })
  output$show_script <- reactive({
    f1 <- paste(fns[input$file,"file"])
    grepl('^Draw_Dist', f1, ignore.case = TRUE)
  })
  outputOptions(output, 'show_script', suspendWhenHidden = FALSE)
  output$show_script_ui <- reactive({
    # Show Shiny inputs if available, react if input file changes therefore access it
    testForShinyUI <- FALSE
    f1 <- input$file
    r <- read_yml(fn())
    if (!is.null(r$Inputs))	 {
      y1 <- get_inputs(fn())
      testForShinyUI <- !is.null(y1$RShinyUIs)
    }
    testForShinyUI
  })
  outputOptions(output, 'show_script_ui', suspendWhenHidden = FALSE)

  output$show_search <- reactive({ ifelse(input$src =="search",TRUE, FALSE) })
  outputOptions(output, 'show_search', suspendWhenHidden = FALSE)
  output$search_for <- renderUI({
    if (input$src =="search") {
    tagList(
    textInput("filename", label="File name:", value = "*.yml"
              , placeholder = "Pattern for searching file names"),
    textInput("stext", label="Key word: ", value = ""
              , placeholder = "key words in files")
    )
    }
  })


  # -------------------- tabPanel: Script  ----------------------------------
  output$script <- renderText({
    # formulaText()
    # getURI(d())
    # getURLContent(d())
    # convert /x/y/a_b_sas.yml to /x/y/a_b.sas
    f1 <- gsub('_([[:alnum:]]+).([[:alnum:]]+)$','.\\1',fn())
    if (!exists("f1")) { return('') }
    if (is.na(f1) || is.null(f1) || length(f1) < 1 ) {
      return('ERROR: Missing file name.')
    }
    if (!file.exists(f1) && !url.exists(f1)) {
      return(paste0('ERROR: file ', f1, ' does not exist.'))
    }
    ft <- gsub('.+\\.(\\w+)$','\\1', f1)
    if (length(ft) > 0 && grepl('^(zip|exe|bin)', ft, ignore.case = TRUE) ) {
      paste(paste0("File: ", f1),"     Could not be displayed.", sep = "\n")
    } else {
      paste(paste0("File: ", f1),readChar(f1,nchars=1e6), sep = "\n")
    }
  })

  # -------------------- tabPanel: YML  -------------------------------------
  output$yml <- renderText({
    f1 <- fn()
    if (!file.exists(f1) && !url.exists(f1)) {
      return(paste0('ERROR: file ', f1, ' does not exist.'))
    }
    paste(paste0("File: ", f1),readChar(f1,nchars=1e6), sep = "\n")
  })

  # -------------------- tabPanel: Info  ------------------------------------
  output$finfo <- renderTable({
    t1 <- t(fns[input$file,])
    # t1["file_url",] <- sprintf(h_a, t1["file_url",], t1["file_url",], t1["file",])
    t1
  }, rownames = TRUE )

  # -------------------- tabPanel: Metadata  --------------------------------
  output$mtable <- renderTable({
    # yaml.load_file(URLencode(f1()[input$file,4]))
    m2()
  })

  # -------------------- tabPanel: Verify  ----------------------------------
  output$verify <- renderTable({ extract_fns(read_yml(fn())) }, rownames = TRUE )

  # -------------------- tabPanel: Download  --------------------------------
  output$dnload <- renderTable({
    y1 <- extract_fns(read_yml(fn()))
    y2 <- download_fns(y1)
    f1 <- y2[y2$tag=="script_name","file_path"]
    if (exists("session$clientdata")) { session$clientdata$ofn <- f1 }
    y2
  }, rownames = TRUE )

  # -------------------- tabPanel: Merge  -----------------------------------
  output$merge <- renderTable({
    f1 <- fn()
    file_name <- basename(f1)
    work_dir  <- crt_workdir(to_crt_dir = FALSE)
    f2 <- paste(work_dir, "scripts", file_name, sep = '/')
    if (file.exists(f2)) {
      a  <- read_yml(f1)
      b  <- read_yml(f2)
      c  <- merge_lists(a,b)
      cvt_list2df(c)
    } else {
      msg <- c(paste0("Repo: ", f1), paste0("Loc : ", f2, " does not exists."));
      data.frame(msg)
    }
    }, rownames = TRUE )

  # -------------------- tabPanel: Diff  ------------------------------------
  output$diff <- renderPrint({
    f1 <- fn()
    file_name <- basename(f1)
    work_dir  <- crt_workdir(to_crt_dir = FALSE)
    f2 <- paste(work_dir, "scripts", file_name, sep = '/')
    if (file.exists(f2)) {
      # diffFile(f1,f2, pager = "off")
      diffFile(f1,f2)
    } else {
      msg <- c(paste0("Repo: ", f1), paste0("Loc : ", f2, " does not exists."));
      msg
    }
  })

  # -------------------- tabPanel: Execute  ---------------------------------
  output$execute <- renderPlot({
    # if (!is.null(input$yml_name)) {
    #  y2 <- input$yml_name
    # } else if (is.null(session$clientdata$ofn)) {
    #  f1 <- ifelse(input$src=="loc","file_path", "file_url")
    #  y1 <- download_fns(extract_fns(read_yml(fn())))
    #  y2 <- as.character(y1[y1$tag=="script_name",f1])
    # } else {
    #   y2 <- session$clientdata$ofn
    # }
    y2 <- gsub('_([[:alnum:]]+).([[:alnum:]]+)$','.\\1',fn())
    # str(y2)
    commandArgs <- function() list(prg="phuse", p1=input$p1, p2=input$p2, script_name=y2)
    if ( endsWith(toupper(y2),"R")) {
      source(y2, local = TRUE)
    } else {
      # show an error
      print (append("Not an R script: ",y2))
      plotMessage("Cannot execute other than an R script.")
    }
  })

  # -------------------- tabPanel: Search  ----------------------------------
  in_fn <- reactive({ input$filename })
  output$search <- renderDataTable({
    ff <- search_github(filename=in_fn(), out_type = 'fnlist')
    cn <- c("fn_id", "script", "file");
    datatable(ff[,cn]);
  })

  # -------------------- tabPanel: Script Inputs  ---------------------------
  output$script_inputs <- renderUI({
    # get inputs that are of the type RShinyUI
    y1 <- get_inputs(fn())
    if (!is.null(y1$RShinyUIs)) {
      tagList(
        # Below is actual rending from reading inputs in yml file
        eval(parse(text=y1$RShinyUIs[[1]]$control)),
        if (length(y1$RShinyUIs)>1) {
          eval(parse(text=y1$RShinyUIs[[2]]$control))
        },
        if (length(y1$RShinyUIs)>2) {
          eval(parse(text=y1$RShinyUIs[[3]]$control))
        },
        if (length(y1$RShinyUIs)>3) {
          eval(parse(text=y1$RShinyUIs[[4]]$control))
        },
        if (length(y1$RShinyUIs)>4) {
          eval(parse(text=y1$RShinyUIs[[5]]$control))
        },
        if (length(y1$RShinyUIs)>5) {
          eval(parse(text=y1$RShinyUIs[[6]]$control))
        },
        if (length(y1$RShinyUIs)>6) {
          eval(parse(text=y1$RShinyUIs[[7]]$control))
        },
        if (length(y1$RShinyUIs)>7) {
          eval(parse(text=y1$RShinyUIs[[8]]$control))
        },
        if (length(y1$RShinyUIs)>8) {
          eval(parse(text=y1$RShinyUIs[[9]]$control))
        },
        if (length(y1$RShinyUIs)>9) {
          eval(parse(text=y1$RShinyUIs[[10]]$control))
        }
      )}

  })

  output$script_ins <- renderUI({
    f1 <- paste(fns[input$file,"file"])
    if (grepl('^Draw_Dist', f1, ignore.case = TRUE)) {
      # y1 <- build_inputs(fn())
      c1 <- c("Normal"="rnorm","Uniform"="runif","Log-normal"="rlnorm","Exponential"="rexp");
      p1 <- c("p1","Distribution type:", c1);
      c2 <- c(value = 500,min = 1,max = 1000);
      p2 <- c("p2","Number of observations:",c2);
      tagList(
        radioButtons("p1","Distribution type:", c1)
      , sliderInput("p2","Number of observations:",value = 500,min = 1,max = 1000)
      )
     tagList(
        sliderInput("p2","Number of observations:",value = 500,min = 1,max = 1000)
      , radioButtons("p1","Distribution type:",
        c("Normal"="rnorm","Uniform"="runif","Log-normal"="rlnorm","Exponential"="rexp")
        )
     )
     ## eval(call(y1))
     ## includeScript("www/s01.R")
    }
  })
}

# Create Shiny app ----
shinyApp(ui, server)
