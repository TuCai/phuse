#' Check if a variable is na or null or space
#' @description check if string or list is empty (na, null or blank spaces).
#' @param x a list or string
#' @return true or false
#' @export
#' @examples
#'   is_empty(NULL);
#'   is_empty('');
#'   is_empty(NA);
#' @author Hanming Tu
#' @name is_empty
# ---------------------------------------------------------------------------
# HISTORY
#  MM/DD/YYYY (developer) - explanation
#  11/21/2017 (htu) - initial creation
#
is_empty <- function(x) {
  is.null(x) || is.na(x) || length(x) == 0 || grepl("^\\s*$", x)
}
