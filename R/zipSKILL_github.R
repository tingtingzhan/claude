
#' @title To create a \link[utils]{zip} File from GitHub
#' 
#' @param repo ..
#' 
#' @param ... ..
#' 
#' @examples
#' \dontrun{
#' zipSKILL_github('tingtingzhan/factcheck')
#' }
#' 
#' @importFrom utils download.file unzip
#' @export
zipSKILL_github <- function(
    repo,
    ...
) {
  
  destfile <- tempfile(fileext = '.zip')
  # print(destfile)
  
  destdir <- destfile |>
    dirname()

  #destdir |>
  #  sprintf(fmt = 'open %s') |>
  #  system()
  
  repo |>
    sprintf(fmt = 'https://github.com/%s/archive/refs/heads/main.zip') |>
    download.file(url = _, destfile = destfile, quiet = TRUE)
  
  all_files <- destfile |>
    unzip(list = TRUE)
  
  root_dir <- all_files$Name |>
    sub(pattern = '/.*', replacement = '') |>
    unique()
  
  if (length(root_dir) != 1L) stop()
  if (!endsWith(root_dir, '-main')) stop()
  
  noout <- destfile |> 
    unzip(exdir = dirname(destfile))
  
  file.path(destdir, root_dir) |>
    zipSKILL(from = _, ...)
  
}


