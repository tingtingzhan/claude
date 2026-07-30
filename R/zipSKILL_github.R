
#' @title To create a \link[utils]{zip} SKILL File for Claude from GitHub
#' 
#' @param repo \link[base]{character} scalar, in the format of `'[user]/[repo]'`
#' 
#' @param ... parameters of the function [zipSKILL()], except for `from`
#' 
#' @examples
#' \dontrun{
#' zipSKILL_github('tingtingzhan/factcheck')
#' }
#' 
#' @importFrom utils download.file unzip
#' @export
zipSKILL_github <- function(repo, ...) {
  
  destfile <- tempfile(fileext = '.zip')
  # print(destfile)
  
  destdir <- destfile |>
    dirname()
  system(sprintf(fmt = 'open %s', destdir))
  
  repo |>
    sprintf(fmt = 'https://github.com/%s/archive/refs/heads/main.zip') |>
    download.file(url = _, destfile = destfile, quiet = TRUE)
  
  all_files <- destfile |>
    unzip(list = TRUE)
  
  root_dir <- all_files$Name |>
    sub(pattern = '/.*', replacement = '') |>
    unique()
  if (length(root_dir) != 1L) stop()
  if (!endsWith(root_dir, suffix = '-main')) stop('shouldnt happen')
  
  destfile |> 
    unzip(exdir = dirname(destfile))
  
  dest_root <- file.path(destdir, root_dir)
  if (!file.exists(file.path(dest_root, 'SKILL.md'))) stop()
  
  dest_root |>
    zipSKILL(from = _, ...)
  
}


