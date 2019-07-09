#' Clone a GitHub repositoru
#' @description Clone a specified GitHub.
#' @param repo_url a URL for a remote repository and default to
#'   'https://github.com/phuse-org/phuse-scripts.git'
#' @param repo_dir a local directory to host the repository;
#'   default to work_dir from crt_workdir if not specified
#' @param repo_name repo name; default to the repo name in repo_url.
#' @param upd_opt update option: Repo
#' @return nothing.
#' @export
#' @importFrom git2r clone
#' @importFrom git2r init
#' @examples
#'\dontrun{
#'   r1 <- clone_github()
#'}
#' @author Hanming Tu
#' @name clone_github
#
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  06/16/2019 (htu) - initial creation based on build_script_df
#
clone_github <- function(
  repo_url = 'https://github.com/phuse-org/phuse-scripts.git',
  repo_dir = NULL,
  repo_name = NULL,
  upd_opt = NULL
) {
  prg <- "clone_github"; echo_msg(prg,0.0,'Started', 1)
  fmt   <- "%s: %.1f - %s\n";
  txt <- sprintf(fmt, prg, 0.0, "Started.");

  # rm(list=ls())
  if (phuse::is_empty(repo_url))     {
    str(sprintf("%s","repo is null"));
    msg <- sprintf(fmt, prg, 0.1, "repo is null.");
    txt <- paste(txt, msg, sep = "\n")
    return(txt);
  }
  if (!url.exists(repo_url)) {
    msg <- sprintf("%s",paste(repo_url, " does not exist!"));
    str(msg);
    msg <- sprintf(fmt, prg, 0.2, msg);
    txt <- paste(txt, msg, sep = "\n");
    return(txt);
  }
  msg <- paste("Repo", repo_url, 'exists.');  echo_msg(prg,1.0,msg, 1);
  txt <- paste(txt, msg, sep = "\n");

  if (phuse::is_empty(repo_name)) {
    repo_name  <- gsub('.*\\/([\\-\\w]+).git$', '\\1', repo_url, perl=TRUE);
  }
  work_dir <- crt_workdir();
  if (phuse::is_empty(repo_dir)) {
    repo_dir <- paste(work_dir, repo_name, sep = '/');
  }

  if (!phuse::is_empty(upd_opt) && grepl("^(Repo)", upd_opt, ignore.case = TRUE)) {
    if (dir.exists(repo_dir)) {
      if (chk_workdir(repo_dir)) {
        msg <- paste("Removing repo ", repo_dir); echo_msg(prg,1.1, msg, 1);
        txt <- paste(txt, msg, sep = "\n");
        # only remove the dir if it is in the default workdir
        unlink(repo_dir, recursive = TRUE)
      }
    }
  }

  # we need to get it from the repository
  # if (is.null(repo_dir)) { repo_dir <- paste(tmp_dir, 'repo', rp_name, sep = '/'); }
  if (!dir.exists(repo_dir)) {
    msg <- paste("Creating", repo_dir, "...");  echo_msg(prg,2.1, msg, 1);
    txt <- paste(txt, msg, sep = "\n");
      dir.create(repo_dir, recursive = TRUE);
    nsg <- paste("Cloning", repo_url, "..."); echo_msg(prg,2.2, msg, 1);
    txt <- paste(txt, msg, sep = "\n");
    str(paste("  To ", repo_dir))
      rp <- clone(repo_url,repo_dir)
  } else {
    msg <- paste("Repo ", repo_dir, "exists.");  echo_msg(prg,2.3, msg, 1);
    txt <- paste(txt, msg, sep = "\n");
    # rp <- init(repo_dir)
  }
  return(txt)
}


