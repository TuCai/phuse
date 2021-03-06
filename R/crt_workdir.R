#' Create work directory
#' @description define and create a work directory.
#' @param top_dir a top or root directory; default to '/Users/{user} for Mac
#'   or getwd for other OS
#' @param sub_dir a sub directory
#' @param to_crt_dir whether to create the dir; default to TRUE.
#'   If FALSE, just return the dir name
#' @return the created directory
#' @export
#' @examples
#'\dontrun{
#'   d1 <- tempdir()
#'   r1 <- crt_workdir(d1)
#'   r2 <- crt_workdir(d1, to_crt_dir = FALSE) # just return the dir
#'}
#' @author Hanming Tu
#' @name crt_workdir
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  09/12/2017 (htu) - initial creation
#  09/14/2017 (htu) - added to_crt_dir = FALSE to just return dir name
#    and Linux, Windows options
#  11/26/2017 (htu) - changed the default folder from /Users to tempdir()
#  06/09/2019 (htu) - checked sub_dir before adding it to the path
#  10/17/2019 (htu) - added usub_dir to consider null sub_dir case
#
crt_workdir <- function(
  top_dir = NULL,
  sub_dir = 'myRepo',
  to_crt_dir = TRUE
) {
  if (is.null(top_dir))     {
    sys_name <- Sys.info()[["sysname"]]
    usr_name <- Sys.info()[["user"]]
    rtop <- tempdir()
    if (is.null(sub_dir)) {
      usub_dir <- usr_name;
    } else {
      usub_dir <- paste(usr_name, sub_dir, sep = '/');
    }
    if (grepl("^(htu|hanming)", usr_name, ignore.case = TRUE)) {
      if (grepl("^(Darwin|Linux)", sys_name, ignore.case = TRUE)) {
        r <- paste('/Users', usub_dir, sep = '/');
      } else if (grepl("^Windows", sys_name, ignore.case = TRUE)) {
        r <- paste('c:/tmp', usub_dir, sep = '/');
      } else {
        r <- paste(getwd(), usub_dir, sep = '/');
      }
    } else  {
      r <- paste(rtop, usub_dir, sep = '/');
    }
  } else {
    if (is.null(sub_dir)) {
      r <- top_dir;
    } else{
      r <- paste(top_dir, sub_dir, sep = '/')
    }
  }
  if (!to_crt_dir) { return(r) }
  if (!dir.exists(r) && to_crt_dir) { dir.create(r, recursive = TRUE); }
  return(r)
}
