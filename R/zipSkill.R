

#' @title To create a \link[utils]{zip} SKILL File for Claude
#' 
#' @param from \link[base]{character} scalar, input directory, default value is the working directory `.`.
#' 
#' @param to \link[base]{character} scalar, output directory, default value is the `~/Documents` directory.
#' 
#' @param flags,extras,... additional parameters of the function \link[utils]{zip}
#' 
#' @importFrom utils zip
#' @export
zipSKILL <- function(
    from = '.', 
    to = '~/Documents',
    flags = '-q',
    extras = '-x _ignore/*',
    ...
) {
  
  filename <- from |>
    normalizePath() |>
    basename()
  
  zipf <- filename |>
    sprintf(fmt = '%s.zip') |>
    file.path(to, . = _)
  
  if (file.exists(zipf)) file.remove(zipf)
  
  old_dir <- setwd(from)
  on.exit(setwd(old_dir), add = TRUE)
  
  list.files(
    path = from, pattern = '\\.md$', 
    full.names = FALSE, # super important!!!
    recursive = TRUE
  ) |>
    zip(zipfile = zipf, files = _, flags = flags, extras = extras, ...)
  
}

