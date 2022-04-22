# button function
#' Creation of a bootstrap 5 button **work in progress**
#'
#' @param id button id
#' @param color button bootstrap color (primary, secondary, info, success, warning, danger, dark, light)
#' @param href  link to an external website (default = NULL)
#' @param outline outline button ? (TRUE/FALSE)
#' @param shadow  shadow around button ? (TRUE/FALSE)
#' @param oc Trigger off canvas ? (TRUE/FALSE)
#' @param id_oc if oc is TRUE id of the oc to trigger
#' @param text Text label (default = NULL)
#' @param icon Icon label (default = NULL)
#' @import shiny
#' @import htmltools
#' @return a bootstrap 5 button
#' @export

button_app <- function(id, 
                        color = "primary",
                        href = NULL,
                        outline = FALSE,
                        shadow = TRUE,
                        oc = FALSE,
                        id_oc = NULL,
                        text = NULL,
                        icon = NULL){
  
  # default class
  class <- "btn w-100 action-button"
  # button color
  if (isTRUE(outline)){ 
    class <- paste0(class, " btn-outline-",color)
  } 
  else{ 
    class <- paste0(class, " btn-",color)
  }
  # shadow
  if (isTRUE(shadow)){ class <- paste0(class, " shadow") }
  # label
  label <- div(icon,tags$span(text))
  # basic shiny button
  button <- tags$button(type = "button",
                        id = id,
                        class = class,
                        label
  )
  # off canvas trigger
  if (isTRUE(oc)){
    button <- htmltools::tagQuery(button)$
      addAttrs('data-bs-toggle'="offcanvas")$
      addAttrs('data-bs-target' = paste0("#",id_oc))$
      allTags()
  }
  
  if (is.null(href) == FALSE){
    button <- htmltools::tagQuery(button)$
      addAttrs('onclick' = paste0("window.open('",href,"', '_blank')"))$
      allTags()
  }
  # return
  return(button)
}