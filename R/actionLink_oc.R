#' Create a shiny actionLink that triggers an off canvas
#'
#' @param id_oc id of the off canvas to trigger
#' @param ... Same parameters as shiny::actionButton(...)
#' @importFrom shiny actionLink
#' @importFrom htmltools tagQuery
#' @return A shiny actionLink that triggers a particular off canvas
#' @export
#'
#' @examples
#' actionLink_oc(
#' inputId = "ok",
#' label = "Trigger",
#' id_oc = "id_oc",
#' class="text-danger"
#' )

actionLink_oc <- function(id_oc, ...){
  # add class to button
  button <- htmltools::tagQuery(shiny::actionLink(...))$
    addAttrs('data-bs-toggle'="offcanvas")$
    addAttrs('data-bs-target' = paste0("#",id_oc))$
    addAttrs('aria-controls' = paste0(id_oc,'label'))$
    allTags()
  # return
  return(button)
}
