

#' @title To create a \link[utils]{zip} File with Only `.md` files
#' 
#' @param path \link[base]{character} scalar
#' 
#' @export
zipSKILL = \(path = '.') {
  
  zipf <- path |>
    normalizePath() |>
    basename() |>
    sprintf(fmt = '%s.zip') |>
    file.path(path, . = _)
  
  zipf |> 
    file.remove() |> 
    suppressWarnings()
  
  list.files(pattern = '\\.md$', full.names = TRUE, recursive = TRUE) |>
    grepv(pattern = '_ignore', x = _, invert = TRUE) |>
    utils::zip(zipfile = zipf, files = _)
  
}

