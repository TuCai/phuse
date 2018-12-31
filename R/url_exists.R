#' Check URL based on httr package
#' @description Check if URL exists.
#' @param url a URL for a remote repository and default to
#'   'https://github.com/phuse-org/phuse-scripts.git'
#' @param show bolean variable; default to FALSE
#' @return TRUE or FALSE
#' @importFrom RCurl basicTextGatherer
#' @importFrom httr GET
#' @export
#' @examples
#'   url.exists('https://github.com/phuse-org/phuse-scripts.git')
#' @author Hanming Tu
#' @name url.exists
#
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  03/04/2018 (htu) - initial creation
#

url.exists <- function (
  url='https://github.com/phuse-org/phuse-scripts.git',
  show=FALSE)
{
  # check existance using httr library
  g = basicTextGatherer()
  failed = FALSE
  ans = tryCatch(GET(url), COULDNT_RESOLVE_HOST = function(x) failed <<- TRUE,
                 error = function(x) failed <<- TRUE)
  if (failed)
    return(FALSE)
  else if (grepl("Page not found", ans)) {
    if (show) print(ans)
    return(FALSE)
  }
  else{
    if (show) print(ans)
    return(TRUE)
  }
}
