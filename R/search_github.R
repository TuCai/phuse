#' Search GitHub and build a script index data frame
#' @description Use GitHub search API to search for YML files in phuse-scripts
#'   repository and output the list.
#' @param filename file names to be searched; default to *.yml.
#' @param rep_url a URL for a remote repository and default to
#'   'https://github.com/phuse-org/phuse-scripts'
#' @param rep_dir rep dir for file name; default to 'tree/master'
#' @param rep_base a URL for repository base folder; default to
#'   "https://github.com/phuse-org/phuse-scripts/raw/master"
#' @param out_type output type; default to 'fn' - just file names.
#' @param work_dir a local directory to host the files containing
#'   a list of YML files; default to {tempdir()}/myRepo
#' @param output_fn a CSV file name for outputing a list of YML files;
#'   default to "{repo_name}_yml.csv
#' @param days_to_update number of days before the output_fn is updated;
#'   default to 7 days.
#'   Set it to a negative number make it to update immediately.
#' @param fn_only return file name only; default to FALSE
#' @return a list of YML files
#' @export
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom stringr regex
#' @importFrom stringr str_extract_all
#' @examples
#'\dontrun{
#'   r1 <- search_github('*.yml')
#'}
#' @author Hanming Tu
#' @name search_github
#
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  10/10/2018 (htu) - initial creation
#  10/18/2018 (htu) - added rep_dir, out_type
#
# curl https://github.com/phuse-org/phuse-scripts/search?q=filename:*.yml -I -H "Accept: application/json"
search_github <- function(
    filename = '*.yml'
  , rep_url  = 'https://github.com/phuse-org/phuse-scripts'
  , rep_dir  = 'tree/master'
  # , rep_base = "https://github.com/phuse-org/phuse-scripts/raw/master"
  , rep_base = "https://raw.githubusercontent.com/phuse-org/phuse-scripts/master"
  , out_type = 'fn'
  , work_dir = NULL
  , output_fn = NULL
  , days_to_update = 7
  , fn_only = FALSE
) {
  prg <- "search_github"; echo_msg(prg,0.0,'Started', 1)
  msg <- paste("Inputs: filename=",filename,",rep_url=", rep_url
               , ",rep_dir=", rep_dir, ",out_type=",out_type);
  echo_msg(prg,0.1,msg,1);

  # 1. check inputs
  msg <- "check parameters..."; echo_msg(prg,1.0,msg,1);

  rp_name  <- basename(rep_url);
  # define output file name
  work_dir <- crt_workdir()
  if (is.null(output_fn))    { output_fn <- paste0(rp_name, '_yml.csv'); }
  wk_fn <- paste(work_dir, output_fn, sep = '/');
  # Only return the workdir
  if (fn_only && file.exists(wk_fn)) { return(wk_fn); }

  if (! phuse::url.exists(rep_url)) {
    msg <- paste("repo URL", rep_url, "does not exists.");
    echo_msg(prg,1.1,msg,0);
    return(data.frame("fn_id"=msg,"script"="","file"=""));
  }

  # 2. set parameters
  msg <- "set parameters..."; echo_msg(prg,2.0,msg,1);

  if (grepl('\\.(\\w+)$', filename, ignore.case = TRUE)) {
    ext <- gsub('(.+)\\.(\\w+)$', '\\2', filename);
    re1 <- paste0(">(.+)<em>", ext, "</em></a>");
  } else {
    ext <- filename;
    re1 <- paste0(">(.+)<em>", ext, "</em>(.+)?</a>");
  }
  re2 <- ">([^<]+)";
  msg <- paste("RE1:", re1); echo_msg(prg,2.1,msg,1);

  # u1 <- paste0(rep_url,'/search?q=filename:',filename,'+in:',folder);
  u1 <- paste0(rep_url,'/search?q=filename:',filename);
  rr <- list(); p <- 0
  has_page <- TRUE;

  # 3. Search GitHub
  msg <- "search github..."; echo_msg(prg,3.0,msg,1);
  echo_msg(prg,3.1,u1,2);

  while ( has_page ) {
    p <- p + 1
    u <- paste0(u1,'&p=',p)
    echo_msg(prg,3.2,u,3);
    h <- GET(u);
    # h[["content"]];
    t <- content(h, type = "text", encoding = 'UTF-8');
    echo_msg(prg,3.2, paste("T len =", length(t)),3);
    s0 <- str_extract_all(t, regex(re1,ignore_case=TRUE));
    a <- unique(s0[[1]]);
    echo_msg(prg,3.2, paste("A len =", length(a)),3);
    if (length(a) < 1 && p < 2) {
      # fn4t <- paste0(work_dir,'/',ext,'_', p,'.htm');
      # echo_msg(prg,3.2, paste("write to", fn4t),3);
      # write(t, fn4t);
      s1 <- stringr::str_extract_all(t, "<p>(.+)<br>\\n(.+)</p>");
      a1 <- unique(s1[[1]]);
      echo_msg(prg,3.2, a1,3);
      return(data.frame("fn_id"=a1,"script"="","file"=""))
    }
    b <- str_extract_all(a, regex(re2,ignore_case=TRUE));
    echo_msg(prg,3.2, paste("B len =", length(b)),3);
    if (length(b) < 1) { has_page <- FALSE; next() }
    for (i in 1:length(b)) {
      k <- length(rr) + 1; rr[k] <- '';
      for (j in 1:length(b[[i]])) {
        rr[k] <- paste0(rr[k],sub('>','',b[[i]][j]))
      }
      echo_msg(prg,3.2, paste("K=", k,':', rr[k]),3);
    }
  }

  nr <- ifelse(length(rr)<1, 1, length(rr));
  r <- setNames(data.frame(matrix(ncol=7, nrow=nr)),
                c("fn_id","script", "file", "rel_path","file_url"
                  , "file_path","file_link"));
  if (length(rr) < 1) {
    if (grepl('yml',filename,ignore.case = TRUE)) {
      # let's check if the index file exists; if yes, let's read it
      if (file.exists(wk_fn)) {
        msg <- paste("reading from",wk_fn); echo_msg(prg,3.3,msg,1);
        r <- read.csv(file=wk_fn, header=TRUE, sep=",");
        return(r);
      } else {
        msg <- paste(wk_fn, "does not exist."); echo_msg(prg,3.4,msg,1);
      }
    }
    msg <- "Did not find any record. It might be due to connection.Try it again.";
    echo_msg(prg,3.5,msg,1);
    # r <- data.frame(matrix(ncol = 1, nrow = 1))
    r$fn_id[1]      <- 1;
    r$file_link[1]  <- msg;
    return(data.frame("fn_id"=msg,"script"="","file"=""))
  }

  # 4. output results
  msg <- "output results..."; echo_msg(prg,4.0,msg,1);

  if (out_type == 'fn') { return(rr) }

  if (length(rr) < 1) {
    msg <- paste("No result found for", filename);
    echo_msg(prg,4.1,msg,1); return(rr)
  } else {
    msg <- paste("Found", length(rr), "records.");
    echo_msg(prg,4.1,msg,1)
  }

  msg <- "build data frame..."; echo_msg(prg,4.2,msg,1);

  f_a <- '<a href="%s">%s</a>';

  for (i in 1:length(rr)) {
      f  <- rr[[i]][1];
      fn <- gsub('_([sas|SAS|r|R|Rnw|zip|gz]+).yml$'
                 ,'.\\1', basename(f));
      f1 <- paste(rep_base, f, sep="/");
      f2 <- sprintf(f_a, f1, fn);
      r$fn_id[i]     <- i;
      r$file[i]      <- basename(f);
      r$script[i]    <- fn;
      r$rel_path[i]  <- dirname(f);
      r$file_url[i]  <- f1;
      r$file_path[i] <- paste(work_dir, f, sep="/");
      r$file_link[i] <- f2;
  }

  if (file.exists(wk_fn)) {
      msg <- paste(wk_fn, "exists."); echo_msg(prg,4.3,msg,1);
      f_inf <- file.info(wk_fn);
      tm1   <- f_inf[,5];
      tm2   <- Sys.time()-days_to_update*24*60*60;
      msg <- paste("T1=",tm1, ", T2=", tm2); echo_msg(prg,4.3,msg,3);
      to_update <- ifelse(tm1>tm2,FALSE,TRUE);
  } else {
      msg <- paste(wk_fn, "does not exist."); echo_msg(prg,4.3,msg,1);
      to_update <- TRUE
  }
  if (to_update) {
    msg <- paste("Writing CSV to ", wk_fn); echo_msg(prg,4.4,msg,1);
    write.csv(r, file = wk_fn, row.names=FALSE, na="")
  }

  if (fn_only && file.exists(wk_fn)) {
    return(wk_fn);
  } else { return(r) }

}
