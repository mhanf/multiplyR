# button function
#' Creation of a bootstrap 5 button
#'
#' @param id button id
#' @param color button bootstrap color (primary, secondary, info, success, warning, danger, dark, light)
#' @param href  link to an external website (default = NULL)
#' @param outline outline button ? (TRUE or FALSE)
#' @param shadow  shadow around button ? (TRUE/FALSE)
#' @param oc Trigger off canvas ? (TRUE or FALSE)
#' @param id_oc if oc is TRUE id of the oc to trigger
#' @param text Text label (default = NULL)
#' @param size size of button (sm, md or lg)
#' @param icon Icon label (default = NULL)
#' @param disabled disabled state (TRUE or FALSE)
#' @param add_class button supplementary class
#'
#' @import shiny
#' @import htmltools
#' @return a bootstrap 5 button
#' @export

button_app <- function(id,
                       color = "primary",
                       href = NULL,
                       outline = FALSE,
                       shadow = TRUE,
                       disabled = FALSE,
                       size = "md",
                       oc = FALSE,
                       id_oc = NULL,
                       text = NULL,
                       icon = NULL,
                       add_class = NULL
                       ) {
  # test id
  if (is.null(id) == TRUE) {
    stop("id can not be null")
  }
  # test color
  if (test_bs_color(color) == FALSE) {
    stop("btn color must be primary, secondary, dark, light, info, danger, warning, success")
  }
  # test outline
  if (is.logical(outline) == FALSE) {
    stop("outline must be logical")
  }
  # test shadow
  if (is.logical(shadow) == FALSE) {
    stop("shadow must be logical")
  }
  # test disabled
  if (is.logical(disabled) == FALSE) {
    stop("disabled must be logical")
  }
  # test oc
  if (is.logical(oc) == FALSE) {
    stop("oc must be logical")
  }
  # test id_oc
  if (isTRUE(oc) & is.null(id_oc) == TRUE) {
    stop("id_oc must be not be null")
  }
  # size
  if (size %ni% c("sm", "md", "lg")) {
    stop("size must be sm, md or lg")
  }
  # default class
  class <- "btn action-button"
  # button color
  if (isTRUE(outline)) {
    class <- paste0(class, " btn-outline-", color)
  }
  else{
    class <- paste0(class, " btn-", color)
  }
  # shadow
  if (isTRUE(shadow)) {
    class <- paste0(class, " shadow")
  }
  # size
  if (size != "md") {
    class <- paste0(class, " btn-", size)
  }
  # disabled
  if (isTRUE(disabled)) {
    class <- paste0(class, " disabled")
  }
  # add class
  if (is.null(add_class) == FALSE){
    class <- paste(class, add_class)
  }
  # label
  label <- div(icon, tags$span(text))
  # basic shiny button
  button <- tags$button(type = "button",
                        id = id,
                        class = class,
                        label)
  # off canvas trigger
  if (isTRUE(oc)) {
    button <- htmltools::tagQuery(button)$
      addAttrs('data-bs-toggle' = "offcanvas")$
      addAttrs('data-bs-target' = paste0("#", id_oc))$
      allTags()
  }
  # href
  if (is.null(href) == FALSE) {
    button <- htmltools::tagQuery(button)$
      addAttrs('onclick' = paste0("window.open('", href, "', '_blank')"))$
      allTags()
  }
  # return
  return(button)
}