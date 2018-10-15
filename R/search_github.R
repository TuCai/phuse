#' Search GitHub and build a script index data frame
#' @description Grep all the YML files, parse the metadata and build
#'   a data frame containing key metadata tags.
#' @param repo_url a URL for a remote repository and default to
#'   'https://github.com/phuse-org/phuse-scripts.git'
#' @param repo_base a URL for repository base folder; default to
#'   "https://github.com/phuse-org/phuse-scripts/raw/master"
#' @param repo_dir a local directory to host the repository;
#'   default to work_dir from crt_workdir if not specified
#' @param work_dir a local directory to host the files containing
#'   a list of YML files; default to {tempdir()}/myRepo
#' @param output_fn a CSV file name for outputing a list of YML files;
#'   default to "{repo_name}_yml.csv
#' @param days_to_update number of days before the output_fn is updated;
#'   default to 7 days.
#'   Set it to a negative number make it to update immediately.
#' @param fn_only return file name only; default to FALSE
#' @param upd_opt update option: File|Repo|Both
#' @return a data frame containing a list of script metadata
#' @export
#' @importFrom utils download.file
#' @importFrom utils url.show
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom utils str
#' @importFrom yaml yaml.load
#' @importFrom yaml yaml.load_file
# #' @importFrom RCurl url.exists
#' @importFrom git2r clone
#' @importFrom git2r init
#' @importFrom git2r is_empty
#' @importFrom stats setNames
#' @examples
#'\dontrun{
#'   r1 <- search_github()
#'   r2 <- search_github(upd_opt = "file")
#'   r3 <- search_github(upd_opt = "repo")
#'   r4 <- search_github(upd_opt = "both")
#'}
#' @author Hanming Tu
#' @name search_github
#
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  10/10/2018 (htu) - initial creation
#
search_github <- function(
    filename = '*.yml'
  , folder   = 'https://github.com/phuse-org/phuse-scripts'
  , rep_url  = 'https://github.com/phuse-org/phuse-scripts'
) {
  # 1. check inputs

  if (url.exists(rep_url)) {
    str(sprintf("%s","repo URL does not exists")); return();
  }
  u <- paste0(rep_url,'/search?q=filename:',filename,'+in:',folder);
  u1 <- paste0(rep_url,'/search?q=filename:',filename);

  h <- GET(u1)
  h[["content"]]
  j <- content(h,type = "text")


}
