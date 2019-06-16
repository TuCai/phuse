#' Compare and merge two lists
#' @description compare two lists using the first list as a base;
#'   update the values of the first list if the second one has
#'   different values; add varibles to the first if they doe not
#'   exist in the first list.
#' @param a the 1st list
#' @param b the 2nd list
#' @return a list containing the merged configuration
#' @importFrom rlist list.merge
#' @export
#' @examples
#'   a <- "https://github.com/phuse-org/phuse-scripts/raw/master"
#'   b <- "development/R/scripts"
#'   c <- "Draw_Dist1_R.yml"
#'   f1 <- paste(a,b,c, sep = '/')
#'   dr <- resolve(system.file("examples", package = "phuse"), "02_display")
#'   f2 <- paste(dr, "www", "Draw_Dist_R.yml", sep = '/')
#'   r1 <- read_yml(f1)
#'   r2 <- read_yml(f2)
#'   r3 <- merge_lists(r1, r2)
#' @author Hanming Tu
#' @name merge_lists
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  06/08/2017 (htu) - initial creation
#  12/29/2017 (htu) - used merge.list from RCurl
#
merge_lists <- function(a, b) {
  # merge.list is a method that merges the contents of one list with another
  #  by adding the named elements in the second that are not in the first.
  #  In other words, the first list is the target template, and the second
  #  one adds any extra elements that it has.
  # function (x, y, ...) {
  #  if (length(x) == 0)       return(y)
  #  if (length(y) == 0)       return(x)
  #  i = match(names(y), names(x))
  #  i = is.na(i)
  #  if (any(i))       x[names(y)[which(i)]] = y[which(i)]
  #  x
  # }
  # # <environment: namespace:RCurl>
  # > list.merge
  # function (...)  {
  #   lists <- list(...)
  #   if (any(vapply(lists, function(x) is.null(names(x)), logical(1L))))
  #     stop("All arguments must be named list", call. = FALSE)
  #   reduce(modifyList, lists, list())
  # }
  # # <bytecode: 0x110683070>
  # # <environment: namespace:rlist>

  # c <- merge.list(a, b)
  c <- list.merge(a, b)

  return(c)
}
