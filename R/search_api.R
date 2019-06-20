#' Search GitHub and build a script index data frame
#' @description Use GitHub search API to search for YML files in phuse-scripts
#'   repository and output the list.
#' @param file_ext file extension; default to 'yml'
#' @param gh_api GitHub API URL; default to 'https://api.github.com/search/code'
#' @param rp_name repository name; default to 'phuse-org/phuse-scripts'
#' @param rep_url a URL for a remote repository and default to
#'   'https://github.com/phuse-org/phuse-scripts'
#' @param rep_dir rep dir for file name; default to 'tree/master'
#' @param rep_base a URL for repository base folder; default to
#'   "https://github.com/phuse-org/phuse-scripts/raw/master"
#' @param loc_base a URL for repository base folder; default to
#'   "C:/myCodes/phuse-org/phuse-scripts"
#' @param filename file names to be searched; default to null
#' @param search_for text or key word to be searched in the files; defualt to null.
#' @param size file size; default to null
#' @param path file path; default to null
#' @return a list of YML files
#' @export
#' @importFrom jsonlite fromJSON
#' @examples
#'\dontrun{
#'   r1 <- search_api('yml')
#'}
#' @author Hanming Tu
#' @name search_api
#
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  06/06/2019 (htu) - initial creation based on search_github
#  06/07/2019 (htu) - used GitHub API to search
#    https://api.github.com/search/code?q=extension:sas+repo:phuse-org/phuse-scripts
#  06/09/2019 (htu) - used is_empty function
#  06/16/2019 (htu) - changed loc_base = "C:/myCodes/phuse-org/phuse-scripts/trunk"
#                     to loc_base = "C:/myCodes/phuse-org/phuse-scripts"
#
# curl https://github.com/phuse-org/phuse-scripts/search?q=filename:*.yml -I -H "Accept: application/json"
# u1 <- 'https://api.github.com/search/code?q=repo:phuse-org/phuse-scripts+extension:yml&per_page=500'
#
# rm(list=ls())

search_api <- function(
    file_ext = 'yml'
  , gh_api = 'https://api.github.com/search/code'
  , rp_name = 'phuse-org/phuse-scripts'
  , rep_url  = 'https://github.com/phuse-org/phuse-scripts'
  , rep_dir  = 'tree/master'
  , rep_base = "https://raw.githubusercontent.com/phuse-org/phuse-scripts/master"
  , loc_base = "C:/myCodes/phuse-org/phuse-scripts"
  , filename = NULL
  , search_for = NULL
  , size = NULL
  , path = NULL
) {
  prg <- "search_api"; echo_msg(prg,0.0,'Started', 1)
  msg <- paste("Inputs: file_ext",file_ext,",gh_api=",gh_api
    , ",rp_name=",rp_name,",rep_url=", rep_url
    , ",rep_dir=", rep_dir,",filename=",filename);
  echo_msg(prg,0.1,msg,1);
  is_empty <- phuse::is_empty;

  # 1. check inputs
  msg <- "check parameters..."; echo_msg(prg,1.0,msg,1);

  fn_idx <- paste(loc_base, "yml_file_list.txt", sep = "/");
  if (file.exists(fn_idx)) {
    msg <- paste(fn_idx, "exists."); echo_msg(prg,1.2,msg,1);
    f_inf <- file.info(fn_idx);
    tm1   <- f_inf[,5];
    tm2   <- Sys.time()-7*24*60*60;
    msg <- paste("T1=",tm1, ", T2=", tm2); echo_msg(prg,1.3,msg,3);
    to_update <- ifelse(tm1>tm2,FALSE,TRUE);
  } else {
    to_update <- TRUE;
  }
  if (file_ext == 'yml' && is_empty(filename) && is_empty(size)
      && is_empty(path) && is_empty(search_for)) {
    to_upd2 <- TRUE;
  } else {
    to_upd2 <- FALSE;
  }
  if (file.exists(fn_idx) && !to_update && to_upd2) {
    msg <- paste("reading", fn_idx, "..."); echo_msg(prg,1.4,msg,1);
    r <- read.csv(file=fn_idx, header=TRUE, sep=",");
    return(r);
  }
  if (! phuse::url.exists(rep_url)) {
    msg <- paste("repo URL", rep_url, "does not exists.");
    echo_msg(prg,1.1,msg,0);
    return(data.frame("fn_id"=msg,"script"="","file"=""));
  }

  # 2. set parameters
  msg <- "set parameters..."; echo_msg(prg,2.0,msg,1);

  # u1 <- paste0(rep_url,'/search?q=filename:',filename,'+in:',folder);
  u1 <- paste0(gh_api,'?q=repo:',rp_name);
  if  (!is_empty(file_ext)) {
    u1 <- paste0(u1, '+extension:',file_ext);
  }
  if (!is_empty(filename)) {
    u1 <- paste0(u1,'+filename:',filename);
  }
  if (!is_empty(search_for)) {
    u1 <- paste0(u1,'+',search_for,'+in:file');
  }
  if (!is_empty(size)) {
    u1 <- paste0(u1,'+size:',size);
  }
  if (!is_empty(path)) {
    u1 <- paste0(u1,'+path:',path);
  }
  u1 <- paste0(u1, '&per_page=500');
  echo_msg(prg,2.1,u1,2);
  rr <- list(); p <- 0

  # 3. Search GitHub
  msg <- "search github..."; echo_msg(prg,3.0,msg,1);
  echo_msg(prg,3.1,msg,2);

  t <- fromJSON(u1);
  n <- t$total_count;

  nr <- ifelse(n<1, 1, n);
  r <- setNames(data.frame(matrix(ncol=10, nrow=nr)),
                c("fn_id","script", "file", "rel_path","file_url"
                  , "file_path","file_link","html_url","git_url","raw_url"));
  f_a <- '<a href="%s">%s</a>';
  echo_msg(prg,3.2,"Processing records...",2);
  for (i in 1:nr) {
    f  <- t$items[1]$name[i];
    if (is.null(f)) { next; }
    fn <- gsub('_([sas|SAS|r|R|Rnw|zip|gz|py]+).yml$'
               ,'.\\1', basename(f));
    f1 <- paste(rep_base, f, sep="/");
    f2 <- sprintf(f_a, f1, fn);
    r$fn_id[i]     <- i;
    r$file[i]      <- basename(f);
    r$script[i]    <- fn;
    r$rel_path[i]  <- t$items[2]$path[i];
    r$file_url[i]  <- t$items[4]$url[i];
    r$git_url[i]   <- t$items[5]$git_url[i];
    r$html_url[i]  <- t$items[6]$html_url[i];
    r$file_path[i] <- paste(loc_base,t$items[2]$path[i], sep="/");
    r$file_link[i] <- f2;
    r$raw_url[i]   <- paste(rep_base, t$items[2]$path[i], sep="/");
  }

  if (to_upd2) {
    msg <- paste("Writing CSV to ", fn_idx); echo_msg(prg,3.3,msg,1);
    write.csv(r, file = fn_idx, row.names=FALSE, na="")
  } else {
    msg <- paste("Skipped Writing CSV to ", fn_idx); echo_msg(prg,3.4,msg,1);
  }

  return(r)
}
