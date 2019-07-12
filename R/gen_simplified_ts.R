#' Generate Simplified TS domain xpt file
#' @description This procedure creates a simplified trial summary SAS xpt file.
#' @param studyid is Study Identifier(STUDYID); required.
#' @param tsparmcd is Trial Summary Parameter Short Name(TSPARMCD); defaults
#'   to 'SSTDTC'
#' @param tsval is Parameter Value(TSVAL); defaults to current date in format
#'   of "YYYY-MM-DD"
#' @param tsvalnf is Parameter Null Flavor(TSVALNF); default to blank
#' @importFrom SASxport write.xport
#' @importFrom Hmisc  label
#' @export
#' @examples
#'\dontrun{
#'   library(phuse)
#'   gen_simplified_ts()
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

  echo_msg(prg,1.0,'Check parameters...', 1)
  if (is.null(studyid)) {
    echo_msg(prg,1.1,'ERR: No STUDYID is provided.', 1);
    return;
  }
  if (is_empty(tsval)) {
    tsval <- strftime(as.Date(tsval),"%Y-%m-%d");
  }

  echo_msg(prg,2.0,'Form TS data frame...', 1)
  ##Create data file##
  abc<-data.frame(STUDYID=studyid,
                  TSPARMCD=tsparmcd,
                  TSVAL=tsval,
                  TSVALNF=tsvalnf
                  # , stringAsFactors=FALSE
                  );

  ##Add data label and variable labels##
  label (abc)       <- 'Trial Summary';
  label (abc$STUDYID)   <- 'Study Identifier';
  label (abc$TSPARMCD)  <- 'Trial Summary Parameter Short Name';
  label (abc$TSVAL)     <- 'Parameter Value';
  label (abc$TSVALNF)   <- 'Parameter Null Flavor';

  echo_msg(prg,3.0,'Write TS file...', 1)
  ##Write data into xpt format##
  tmp_wkdir <- crt_workdir();
  out_fn    <- paste(tmp_wkdir,ofn, sep = '/')
  echo_msg(prg,3.1,paste('  To ', out_fn), 1)

  write.xport (abc, file=out_fn)
}

