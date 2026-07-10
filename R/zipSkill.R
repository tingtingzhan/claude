

#' @title To create a \link[utils]{zip} File with Only `.md` files
#' 
#' @param from \link[base]{character} scalar, input directory, 
#' default value is the working directory `.`.
#' 
#' @param to \link[base]{character} scalar, input directory,
#' default value is the `~/document` directory.
#' 
#' @importFrom utils zip
#' @export
zipSKILL <- \(from = '.', to = '~/document') {
  
  filename <- from |>
    normalizePath() |>
    basename()
  
  zipf <- filename |>
    sprintf(fmt = '%s.zip') |>
    file.path(to, . = _)
  
  if (file.exists(zipf)) file.remove(zipf)
  
  list.files(path = from, pattern = '\\.md$', full.names = TRUE, recursive = TRUE) |>
    grepv(pattern = '_ignore', x = _, invert = TRUE) |>
    zip(zipfile = zipf, files = _)
  
}

