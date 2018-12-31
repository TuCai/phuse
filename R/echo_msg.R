#' Echo message
#' @description This method displays or writes the message based on
#' debug level. The filehandler is provided through environment
#' variable 'log_fn', and the outputs are written to the file.
#' This method will display message or a hash array based on
#' debug level ('d_level'). If 'd_level' is set to '0', no message
#' or array will be displayed. If 'd_level' is set to '2', it
#' will only display the message level (lvl) is less than or equal
#' to '2'. If you call this method without providing a message level,
#' the message level (lvl) is default to '0'.  Of course, if no message
#' is provided to the method, it will be quietly returned.
#' If 'd_level' is set to '1', all the messages with default message
#' level, i.e., 0, and '1' will be displayed. The higher level messages
#' will not be displayed.
#' @param prg program name calling from
#' @param step step in the program
#' @param msg the message to be displayed. No newline is needed in the end
#'  of the message. It will add the newline code at the end of the message.
#' @param lvl the message level is assigned to the message. If it is higher
#' than the debug level, then the message will not be displayed.
#' @param fn log file name
#' @return message
#' @export
#' @examples
#'   NULL;
#' @author Hanming Tu
#' @name echo_msg
# ---------------------------------------------------------------------------
# HISTORY
#   MM/DD/YYYY (developer) - explanation
#   03/13/2018 (htu) - initial creation
#   05/18/2018 (htu) - added '(password:)(\\w+)' pattern
#
echo_msg <- function (prg, step, msg, lvl = 0
                      , fn = NULL
) {
  # is_empty <- bldsql::is_empty;

  # set variables and get environment variables
  fmt   <- "%s: %.1f - %s\n";
  f1    <- "<h2>%s</h2>";
  f2    <- "<font color=%s>%s</font>";
  g_lvl <- Sys.getenv("g_lvl")   # message level
  d_lvl <- Sys.getenv("d_lvl")   # debug level
  logfn <- Sys.getenv("log_fn")  # log file name
  wrt2log  <- Sys.getenv("write2log")
  query_str <- Sys.getenv("QUERY_STRING")
  http_host <- Sys.getenv("HTTP_HOST")
  is_web  <- !(is_empty(query_str) || is_empty(http_host))

  g_lvl <- ifelse(is_empty(g_lvl), 1, g_lvl)
  d_lvl <- ifelse(is_empty(d_lvl), 1, d_lvl)
  ofn   <- ifelse(is_empty(fn), logfn, fn)
  if (is_empty(msg)) { return() }

  # hide passwords
  msg <- gsub('(\\w+)\\/(\\w+)\\@(\\w+)', '\\1\\/\\*\\*\\*\\@\\3', msg)
  msg <- gsub('(password:)(\\w+)', '\\1\\/\\*\\*\\*', msg, ignore.case = TRUE)

  # e.vars <- do.call(rbind, strsplit(sub("=", "\n", Sys.getenv()), "\n"))
  # e.vars <- as.list(structure(e.vars[,2], names = e.vars[,1]))
  if (is_web) {
    if (grepl('^\\s*\\d+\\.\\s+\\w+',msg)) {
      str(sprintf(f1, sprintf(fmt,prg, step, msg)));
    }
    if (grepl('^ERR:',msg, ignore.case = TRUE)) {
      str(sprintf(f2, 'red', sprintf(fmt,prg, step, msg)));
    }
    if (grepl('^WARN:',msg, ignore.case = TRUE)) {
      str(sprintf(f2, 'orange', sprintf(fmt,prg, step, msg)));
    }
    if (grepl('^INFO:',msg, ignore.case = TRUE)) {
      str(sprintf(f2, 'cyan', sprintf(fmt,prg, step, msg)));
    }
    if (grepl('^CMD:',msg, ignore.case = TRUE)) {
      str(sprintf(f2, 'blue', sprintf(fmt,prg, step, msg)));
    }
    if (grepl('^\\s*\\d+\\.\\s+\\w+:',msg)) {
      str("<br>");
    }
  }

  if (lvl <= d_lvl || lvl <= g_lvl) {
    str(sprintf(fmt, prg, step, msg));
    # if (!is_empty(ofn) && file.exists(ofn)) {
    if (!is_empty(ofn) && wrt2log) {
      write(t, file = ofn, append = TRUE, sep = "\n")
    }
  }
}
