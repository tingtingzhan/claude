

#' @title To create a \link[utils]{zip} File with Only `.md` files
#' 
#' @param from \link[base]{character} scalar, input directory, 
#' default value is the working directory `.`.
#' 
#' @param to \link[base]{character} scalar, input directory,
#' default value is the `~/Documents` directory.
#' 
#' @param pattern_exclude \link[base]{regex} (\link[base]{character} scalar)
#' 
#' @importFrom utils zip
#' @export
zipSKILL <- \(from = '.', to = '~/Documents', pattern_exclude = '_ignore') {
  
  filename <- from |>
    normalizePath() |>
    basename()
  
  zipf <- filename |>
    sprintf(fmt = '%s.zip') |>
    file.path(to, . = _)
  
  if (file.exists(zipf)) file.remove(zipf)
  
  list.files(path = from, pattern = '\\.md$', full.names = TRUE, recursive = TRUE) |>
    grepv(pattern = pattern_exclude, x = _, invert = TRUE) |>
    zip(zipfile = zipf, files = _)
  
}

