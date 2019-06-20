#' Start Phuse Web Application
#' @description start phuse web appllication framework. This includes all the
#'   functions in start_phuse plus starting standalone application by name.
#' @param app_name app or script name
#' @param n Example number
#' @param pkg  package name
#' @param pt Port number
#' @param lb define the browser- shiny.launch.browser
#' @param ht define the host or ip address
#' @param dm display modes are auto, normal or showcase
#' @param msg_lvl message level
#' @param loc location of the scirpt: local|github; default to 'local'
#' @importFrom utils URLencode
#' @export
#' @examples
#'\dontrun{
#'   library(phuse)
#'   start_appe()  # default to "02_display"
#'   start_app(1)  # start "01_html"
#'}
#' @author Hanming Tu
#' @name start_app
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  06/10/2019 (htu) - initial creation based on start_phuse
#  06/11/2019 (htu) - added loc parameter so that you can use local repository
#
#
# start_app <- function (n = 2)
#{
#  app_name <- paste0(sprintf("%02d", n), "_display")
#  run_example(app_name)
# }
#
# rm(list=ls())

start_app <- function (app_name = NULL, n = 2, pkg = "phuse"
                        , pt = NULL
                        , lb = getOption("shiny.launch.browser",interactive())
                        , ht = getOption("shiny.host", "127.0.0.1")
                        #, dm = c("auto", "normal", "Normal")
                        , dm =  "normal"
                        , msg_lvl = NULL
                       , loc = 'local'
) {
  if (is.null(msg_lvl)) {
    Sys.setenv("g_lvl"=0, "d_lvl"=0)
  } else {
    Sys.setenv("g_lvl"=msg_lvl, "d_lvl"=msg_lvl)
  }

  prg <- "start_app"; echo_msg(prg,0.0,'Started', 1)
#  msg <- paste("Inputs: app_name=",app_name,",n=",n, ",pkg=", pkg
#               , ",pt=",pt,",lb=", lb,",ht=", ht,",dm=", dm,"
#               , msg_lvl=", msg_lvl, ",loc=", loc);
#  echo_msg(prg,0.1,msg,1);

  echo_msg(prg,1.0,'Check parameters...', 1)

  if (phuse::is_empty(app_name)) {
    if (n < 1) {
      echo_msg(prg,1.1,"Missing app_name and app number: returned.",1);
    } else {
      echo_msg(prg,1.1,"Starting phuse...",1);
      start_phuse(n = n, msg_lvl = msg_lvl);
    }
    return();
  }

  msg <- paste("Start app", app_name, "...")
  echo_msg(prg,2.0,msg, 1)
  fns <- search_api();
  f1 <- ifelse(grepl("^local", loc, ignore.case = TRUE), "file_path", "raw_url")
  n1 <- length(fns[,f1])
  nr <- 0
  for (i in 1:n1) {
    if (grepl(app_name,fns[i,f1],ignore.case = TRUE)) {
      nr <- i
      break;
    }
  }
  if (nr < 1) {
    msg <- paste("Could not find app", app_name);
    echo_msg(prg,2.2,msg,1);
    return()
  }

  f2 <- gsub('\r','', fns[nr,f1], perl = TRUE)
  f3 <- ifelse(loc=='local',f2, URLencode(as.character(f2)))
  if (!file.exists(f3) && !url.exists(f3)) {
    msg <- paste("Could not find app file", f3);
    echo_msg(prg,2.3,msg,1);
    return()
  }
  y1 <- get_inputs(f3);
  n  <- length(y1$RShinyUIs)
  if (n<1) {
    msg <- paste("No RShinyUI configuration in ", f3);
    echo_msg(prg,2.4,msg,1);
    return()
  }
  c1 <- toupper(y1$RShinyUIs[[1]]$control);
  if (c1 != 'APP') {
    msg <- paste("Not a R Shiny app in ", f3);
    echo_msg(prg,2.5,msg,1);
    return()
  }

  app_fn <- gsub('_([sas|SAS|r|R|Rnw|zip|gz|py]+).yml$'
             ,'.\\1',f3);

  shiny::runApp(app_fn
                  , port = pt, host = ht, launch.browser = lb, display.mode = dm)
}

