# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


hello <- function() {
  print("Hello, world!")
}



#' diffSummary
#'
#' Generates a JSON summary of differential analysis and writes it to a file.
#'
#' @param tbl Named numeric vector with elements "Up", "Down", and "NS" representing counts.
#' @param criteria Character string specifying the filtering criteria (e.g., "p < 0.05 & abs(coef)>0").
#' @param title Character string for the summary title.
#' @param filename Character string, name of the output file (without extension).
#'
#' @return Character string, the full path of the output file created.
#'
#' @importFrom stringr str_glue
#' @export
diffSummary <- function(tbl,criteria,title,filename ){
  file_path <-  str_glue("output/{filename}.diff")
  list(
    criteria = str_glue(criteria),
    title = title,
    total = sum(tbl),
    up = unname(tbl["Up"]),
    down = unname(tbl["Down"]),
    ns = unname(tbl["NS"])
  )|>toJSON( pretty = TRUE, auto_unbox = TRUE) |>
    cat(file = file_path )
  return(file_path)
}

