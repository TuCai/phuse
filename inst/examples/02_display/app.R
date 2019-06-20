# ---------------------------------------------------------------------------
# HISTORY
#   MM/DD/YYYY (developer) - explanation
#   03/13/2017 (htu) - initial creation
#   ...
#   10/18/2018 (htu) - started using search_github
#   10/19/2019 (htu) - added output$show_search and output$search_for
#   01/01/2019 (htu) - merged Bob Friedman's code into output$script_inputs
#   06/16/2019 (htu) - added clone tab
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

options(shiny.trace = TRUE)

prg <- "02-Display/app.R"; echo_msg(prg,0.0,'Started', 1)

# to plot a message in the main panel
plotMessage <- function(inString){
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.34, y = 0.9, paste(inString),
       cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
}
dft_rp_url <- 'https://github.com/phuse-org/phuse-scripts.git';

sys_name <- Sys.info()[["sysname"]]
work_dir <- crt_workdir();
if (grepl("^(Darwin|Linux)", sys_name, ignore.case = TRUE)) {
  dft_repo <- paste(work_dir, "phuse-scripts",sep='/');
} else {
  dft_repo <- "C:/myCodes/phuse-org/phuse-scripts";
}

fns <- search_api(loc_base = dft_repo);
ff  <- fns[,1]; names(ff) <- fns[,2];
sel <- ff[order(names(ff))];

echo_msg(prg,1.1,paste("Default repo: ", dft_repo),1);

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
      textInput("loc_repo", label = "Loc Repo: ", value = dft_repo
                , placeholder = "Local repo name"),
      div(id="locrepo_status",class="shiny-text-output",style="display: yes;"),
      # textOutput("result"),
      checkboxInput("clone_repo", label="Clone Repo", value = FALSE, width = NULL),

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
                  tabPanel("Search", DT::dataTableOutput("search")),
                  tabPanel("Clone", verbatimTextOutput("clone"))
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
    f1 <- ifelse(input$src=="loc","file_path", "raw_url")
    f2 <- gsub('\r','', fns[input$file,f1], perl = TRUE)
    f3 <- ifelse(input$src=="loc",f2, URLencode(as.character(f2)))
    f3
  })
  chk_locrepo <- reactive({
    ifelse(dir.exists(input$loc_repo),"Exists", "Does Not exists")
  })

  # -------------------- 0 Sidebar: Select Script  --------------------------
  output$selectUI <- renderUI({ selectInput("file", "Select Script:", sel) })

  output$result <- renderText({ paste("Script File ID: ", input$file) })
  output$sel_fn <- renderText({ paste(fns[input$file,"file"]) })
  output$yml_name <- renderText({ as.character(fn()) })
  output$locrepo_status <- renderText({ as.character(chk_locrepo()) })
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
  output$script_inputs <- renderUI({
    # get inputs that are of the type RShinyUI
    y1 <- get_inputs(fn())
    n  <- length(y1$RShinyUIs)
    # if (!is.null(y1$RShinyUIs)) {
    if (n > 0) {
      if (y1$RShinyUIs[[1]]$control == "app") {
        source(fn())
        # runApp(fn());
      } else {
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
    }
  })

  output$show_search <- reactive({ ifelse(input$src =="search",TRUE, FALSE) })
  outputOptions(output, 'show_search', suspendWhenHidden = FALSE)
  output$search_for <- renderUI({
    if (input$src =="search") {
    tagList(
    textInput("file_ext", label="File Extension:", value = "yml"
                , placeholder = "sas,r,py"),
    textInput("filename", label="File name:", value = ""
              , placeholder = "app,test,etc."),
    textInput("stext", label="Key word: ", value = ""
              , placeholder = "key words in files"),
    textInput("size", label="File Size: ", value = ""
              , placeholder = "100..800,>1000"),
    textInput("path", label="Path: ", value = ""
              , placeholder = "/foo/bar/quz")
        )
    }
  })


  # -------------------- 1 tabPanel: Script  --------------------------------
  output$script <- renderText({
    # formulaText()
    # getURI(d())
    # getURLContent(d())

    f_tmp <- gsub('\r','', fns[input$file,'html_url'], perl = TRUE)
    f1 <- ifelse(input$src=="loc",f_tmp, URLencode(as.character(f_tmp)))
    # convert /x/y/a_b_sas.yml to /x/y/a_b.sas
    # f2 <- gsub('_([[:alnum:]]+).([[:alnum:]]+)$','.\\1',fn())
    f2 <- gsub('_([[:alnum:]]+).([[:alnum:]]+)$','.\\1',fn())
    if (!exists("f2")) { return('') }
    if (is.na(f2) || is.null(f2) || length(f2) < 1 ) {
      return(paste('ERROR: Missing file name.', f2));
    }
    if (!file.exists(f2) && !phuse::url.exists(f2)) {
      return(paste0('ERROR: file ', f2, ' does not exist.'))
    }
    ft <- gsub('.+\\.(\\w+)$','\\1', f2)
    if (length(ft) > 0 && grepl('^(zip|exe|bin)', ft, ignore.case = TRUE) ) {
      paste(paste0("File: ", f2),"     Could not be displayed.", sep = "\n")
    } else {
      f_base <- basename(f2);
      paste(paste0("HTML File: ", f1), paste0("JSON File: ", f2)
            , paste("-----------", f_base, "----------")
            , readChar(f2,nchars=1e6), sep = "\n")
      # f_json <- read_json(f2);
      # f_txt  <- base64Decode(f_json$content);
      # paste(paste0("HTML File: ", f1), paste0("JSON File: ", f2), f_txt, sep = "\n")
    }
  })

  # -------------------- 2 tabPanel: YML  -----------------------------------
  output$yml <- renderText({
    f1 <- fn()
    if (!file.exists(f1) && !phuse::url.exists(f1, show = TRUE)) {
      return(paste0('ERROR: file ', f1, ' does not exist.'))
    }
    f_base <- basename(f1);
    paste(paste0("File: ", f1)
          , paste("-----------", f_base, "----------")
          , readChar(f1,nchars=1e6), sep = "\n")
  })

  # -------------------- 3 tabPanel: Info  ----------------------------------
  output$finfo <- renderTable({
    t1 <- t(fns[input$file,])
    # t1["file_url",] <- sprintf(h_a, t1["file_url",], t1["file_url",], t1["file",])
    t1
  }, rownames = TRUE )

  # -------------------- 4 tabPanel: Metadata  ------------------------------
  output$mtable <- renderTable({
    # yaml.load_file(URLencode(f1()[input$file,4]))
    m2()
  })

  # -------------------- 5 tabPanel: Verify  --------------------------------
  output$verify <- renderTable({ extract_fns(read_yml(fn())) }, rownames = TRUE )

  # -------------------- 6 tabPanel: Download  ------------------------------
  output$dnload <- renderTable({
    y1 <- extract_fns(read_yml(fn()))
    y2 <- download_fns(y1)
    f1 <- y2[y2$tag=="script_name","file_path"]
    if (exists("session$clientdata")) { session$clientdata$ofn <- f1 }
    y2
  }, rownames = TRUE )

  # -------------------- 7 tabPanel: Merge  ---------------------------------
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

  # -------------------- 8 tabPanel: Diff  ----------------------------------
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

  # -------------------- 9 tabPanel: Execute  -------------------------------
  output$execute <- renderPlot({
    # f1 <- gsub('\r','', fns[input$file,'file'], perl = TRUE)
    # f2 <- ifelse(input$src=="loc",f1, URLencode(as.character(f1)))

    y2 <- gsub('_([[:alnum:]]+).([[:alnum:]]+)$','.\\1',fn());
    # str(y2)
    commandArgs <- function() list(prg="phuse", p1=input$p1, p2=input$p2, script_name=y2)
    if ( endsWith(toupper(y2),"R")) {
      source(y2, local = TRUE)
    } else {
      # show an error
      print (append("Not an R script: ",y2))
      plotMessage(paste("Cannot execute other than an R script: ", y2))
    }
  })

  # -------------------- 10 tabPanel: Search  -------------------------------
  in_fn  <- reactive({ input$filename })
  in_ext <- reactive({ input$file_ext })
  in_txt <- reactive({ input$stext })
  in_sz  <- reactive({ input$size })
  in_dir <- reactive({ input$path })
  in_locrep <- reactive({ input$loc_repo  })
  output$search <- renderDataTable({
    ff <- search_api(file_ext=in_ext(),filename = in_fn(),search_for=in_txt()
                     , size=in_sz(),path=in_dir(), loc_base = in_locrep()
                     )
    cn <- c("fn_id", "script", "file","html_url");
    datatable(ff[,cn]);
  })

  # -------------------- 11 tabPanel: Clone  -------------------------------
  output$clone <- renderPrint({
    if (input$clone_repo) {
      if (dir.exists(dft_repo)) {
        paste("Default repo", dft_repo, "exists.")
      } else {
       clone_github(repo_url = dft_rp_url, repo_dir = in_locrep())
      }
    } else {
      'Did not click "Clone Repo".'
    }
#    observe({
#      updateTextInput(session,"clone_repo",  value = FALSE);
#    })
  })

}

# Create Shiny app ----
shinyApp(ui, server)
