#' Get Inputs from Input Sources
#' @description Get inputs from interactive session (shiny webpage),
#'   command line or script metadata.
#' @param fn a file name or URL pointing to script metadata file
#' @param input the input parameter from shiny webpage
#' @param cmd the cmmandArgs
#' @return a list of input values provided for the script
#' @export
#' @examples
#'   a <- "https://github.com/phuse-org/phuse-scripts/raw/master"
#'   b <- "development/R/scripts"
#'   c <- "Draw_Dist2_R.yml"
#'   f1 <- paste(a,b,c, sep = '/')
#'   r1 <- get_inputs(f1)
#' @author Hanming Tu
#' @name get_inputs
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  09/17/2017 (htu) - initial creation
#
get_inputs <- function(fn = NULL, input = NULL, cmd = NULL) {

  # 1. check input parameters
  if (! is.null(fn)) { return(get_yml_inputs(fn)) }

  r <- list()
  # 2. get inputs from interactive session first
  k <- 0
  if (exists("input") && ! is.null(input)) {
    str("Getting inputs from shiny app...")
    return(input);
    # for (i in 1:20) { v <- paste0("p", i); if (v %in% names("input")) { k <- k + 1; r[k] <- input[v]}}
    # if (k > 0 ) { return(r) }
  }

  # 3. check inputs from phuse command line
  #
  if (! is.null(cmd)) {
    str(cmd)
    if (grepl('phuse', cmd[1],  ignore.case = TRUE)) {
      str("Getting inputs from phuse cmd ...")
      r <- cmd[-1]
      if ("script_name" %in% names(r)) { r$script_name <- NULL }
      if (length(r)>0) { return(r) }
    }
    str("Getting inputs from cmd ...")
    if ("script_name" %in% names(cmd)) {
      yml_name    <- gsub('.([[:alnum:]]+)$','_\\1.yml', cmd$script_name)
      return(get_yml_inputs(yml_name))
    }
  }

  # 4. check inputs from command line
  cm <- commandArgs()
  str("From commandArgs: ")
  if (grepl('phuse', cm[1],  ignore.case = TRUE)) {
    r <- cm[-1]
    if ("script_name" %in% names(r)) { r$script_name <- NULL }
    if (length(r)>0) { return(r) }
  }
  str(cm)
  if ("script_name" %in% names(cm) && !is.null(cm$script_name)) {
    yml_name <- gsub('.([[:alnum:]]+)$','_\\1.yml', cm$script_name);
    return(get_yml_inputs(yml_name))
  }

  # 5. get inputs from script metadata
  str("Getting inputs from YML file...")
  sfo <- sys.frame(1)$ofile
  if (is.null(sfo)) {
    str("ERROR: no script name is provided."); return(r)
  }
  return(get_yml_inputs(sfo))
}

