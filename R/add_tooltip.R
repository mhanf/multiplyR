#' Add a tooltip for a specific element
#'
#' @param tag Element on which a tooltip will be added
#' @param position Tooltip position (top, bottom, left or right)
#' @param trigger Trigger of the tooltip (hover, focus or both)
#' @param text Tooltip message
#' @param color Tooltip color (black and white exclude)
#' @import htmltools
#' @import shiny
#' @return A tooltip for a specific element
#' @export
#'
#' @examples
#' # Simple example
#' add_tooltip(
#' tag = shiny::actionButton("btn_id","tooltip"),
#' position = "top",
#' trigger = "hover focus",
#' text = "Tooltip message",
#' color = "primary"
#' )
#' # Advanced example
#' add_tooltip(
#' tag = shiny::actionButton("btn_id","tooltip"),
#' position = "top",
#' trigger = "hover focus",
#' text = shiny::icon('github',class='m-1'),
#' color = "dark"
#' )

add_tooltip <- function(tag = NULL,
                        position = c("top", "bottom", "left", "right"),
                        trigger = c("focus", "hover", "hover focus"),
                        text = NULL,
                        color = c("primary", 
                                  "secondary", 
                                  "dark", 
                                  "light", 
                                  "info", 
                                  "danger", 
                                  "warning", 
                                  "success", 
                                  "black", 
                                  "white")) {
  # dependencies from text
  text_dep <- htmltools::findDependencies(text)
  # denpendencies from tag
  tag_dep <-  htmltools::htmlDependencies(tag)
  # dependencies tooltip
  tooltip_dep <- htmltools::htmlDependency(
    name = "tooltip",
    version = "0.0.1",
    src = "assets",
    script = "tooltip.js",
    stylesheet =  c(file = "tooltip.css"),
    package = "multiplyR" # user package
  )
  # test tag
  if (is.null(tag) == TRUE) {
    stop("tag must not be NULL")
  }
  # test position
  if (position %ni% c("top", "bottom", "left", "right")) {
    stop("tooltip position must be in top, bottom, left, or right")
  }
  # test trigger
  if (trigger %ni% c("hover", "focus", "hover focus", "focus hover")) {
    stop("tooltip trigger must be hover, focus or hover focus")
  }
  # test text
  if (is.null(text) == TRUE) {
    stop("tooltip text must not be NULL")
  }
  # test color
  if (test_bs_color(color, bw = TRUE) == FALSE){
    stop("tooltip color must be primary, secondary, dark, light, info, danger, warning, success, black or white")
  }
  # class definition
  text_color <- "white"
  if (color == "light" | color == "white"){text_color = "black"}
  class_arrow <- sprintf("popover-arrow popover-arrow-%s", color)
  class_inner <- sprintf("tooltip-inner tooltip-inner-%s text-%s", color, text_color)  
  # template definition
  template_tooltip <- shiny::div(
    class = "popover",
    role = "tooltip",
    shiny::div(class = class_arrow),
    shiny::div(class = class_inner, 
    text
  ))
  # tooltip definition
  tooltip <- htmltools::tagQuery(tag)$addAttrs("data-bs-toggle" = "popover")$
    addAttrs('data-bs-placement' = position)$
    addAttrs('data-bs-trigger' = trigger)$
    addAttrs('data-bs-html' = 'true')$
    addAttrs('role' = "button")$
    addAttrs('tabindex' = "0")$
    addAttrs("data-bs-content" = " ")$
    addAttrs("data-bs-template" = template_tooltip)$allTags()
  # attach dependencies
  tooltip <- tagList(text_dep, tooltip)
  tooltip <- tagList(tag_dep, tooltip)
  tooltip <- tagList(tooltip_dep, tooltip)
  # return
  return(tooltip)
}
