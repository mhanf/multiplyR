#' wrapper of downloadButton to suppress btn-default
#'
#' @param ... parameters of downloadButton
#' @import shiny
#' @import htmltools
#' @return a download button without btn-default
#' @export

dwld_button_app <- function(...){
  button <- htmltools::tagQuery(shiny::downloadButton(...))$
    removeClass("btn-default")$
    allTags()
}
