#' Generate Simplified TS domain xpt file
#' @description This procedure creates a simplified trial summary SAS xpt file.
#' @param studyid is Study Identifier(STUDYID); required.
#' @param tsparmcd is Trial Summary Parameter Short Name(TSPARMCD); defaults
#'   to 'SSTDTC'
#' @param tsval is Parameter Value(TSVAL); defaults to current date in format
#'   of "YYYY-MM-DD"
#' @param tsvalnf is Parameter Null Flavor(TSVALNF); default to blank
#' @param ofn is output file name
#' @importFrom SASxport write.xport
#' @importFrom SASxport label
#' @export
#' @examples
#'\dontrun{
#'   library(phuse)
#'   fn <- gen_simplified_ts();
#'}
#' @author Hanming Tu
#' @name gen_simplified_ts
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  07/10/2019 (htu) - initial creation based on FDA document -
#   "Creating Simplified TS.XPT Filwa" provided by Ethan Chen & Heather Crandle
#
# rm(list=ls())

gen_simplified_ts <- function (
    studyid
  , tsparmcd = "SSTDTC"
  , tsval = format(Sys.time(), "%Y-%m-%d")
  , tsvalnf = " "
  , ofn = "ts.xpt"
) {
  prg <- "gen_simplified_ts"; echo_msg(prg,0.0,'Started', 1)

  # utils::globalVariables("label");
  # label <- SASxport::label();

  echo_msg(prg,1.0,'Check parameters...', 1)
  if (is.null(studyid)) {
    echo_msg(prg,1.1,'ERR: No STUDYID is provided.', 1);
    return;
  }
  if (is_empty(tsval) || tsval == 'YYYY-MM-DD') {
    tsval <- strftime(as.Date(Sys.time()),"%Y-%m-%d");
  }

  echo_msg(prg,2.0,'Form TS data frame...', 1)
  ##Create data file##
  abc <- data.frame(STUDYID=studyid,
                  TSPARMCD=tsparmcd,
                  TSVAL=tsval,
                  TSVALNF=tsvalnf
                  # , stringAsFactors=FALSE
                  );

  ##Add data label and variable labels##
  SASxport::label (abc)       <- 'Trial Summary';
  SASxport::label (abc$STUDYID)   <- 'Study Identifier';
  SASxport::label (abc$TSPARMCD)  <- 'Trial Summary Parameter Short Name';
  SASxport::label (abc$TSVAL)     <- 'Parameter Value';
  SASxport::label (abc$TSVALNF)   <- 'Parameter Null Flavor';

  echo_msg(prg,3.0,'Write TS file...', 1)
  ##Write data into xpt format##
  tmp_wkdir <- crt_workdir();
  out_fn    <- paste(tmp_wkdir,ofn, sep = '/')
  echo_msg(prg,3.1,paste('  To ', out_fn), 1)

  write.xport (abc, file=out_fn);

  return(out_fn);
}

