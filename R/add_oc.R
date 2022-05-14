#' Create a BS5 off canvas menu
#'
#' @param id id of the off canvas menu
#' @param title HTML title of the off canvas menu
#' @param position position of the off-canvas (top, bottom, start or end)
#' @param body HTML body of the off canvas menu
#' @param scroll allow body scrolling (TRUE or FALSE)
#' @param backdrop allow body backdrop (TRUE or FALSE)
#' @param close_btn close button in the header ? (TRUE or FALSE)
#' @param header Has the off canvas a header ? (TRUE or FALSE)
#' @param class_btn Additional HTML class for the close button
#' @param class_oc Additional HTML class for the off canvas
#' @param class_header Additional HTML class for the off canvas title
#' @param class_body Additional HTML class for the off canvas body
#'
#' @import htmltools
#' @return a BS5 off canvas menu
#' @export
#'
#' @examples
#' add_oc(
#' id = "id_oc",
#' header = TRUE,
#' title = "Title !",
#' body ="Body !",
#' position = "end",
#' class_header = "bg-primary",
#' class_body = "bg-light",
#' class_btn = "btn-success",
#' class_oc ="border border-primary",
#' scroll = TRUE,
#' backdrop = TRUE,
#' close_btn = TRUE
#' )

add_oc <- function(id = NULL,
                   body = NULL,
                   position = c("top", "bottom", "start", "end"),
                   scroll = FALSE,
                   backdrop = TRUE,
                   header = TRUE,
                   title = NULL,
                   close_btn = TRUE,
                   class_btn = NULL,
                   class_oc = NULL,
                   class_header = NULL,
                   class_body = NULL) {
  # test id
  if (is.null(id) == TRUE) {
    stop("id can not be null")
  }
  # test body
  if (is.null(body) == TRUE) {
    stop("body can not be null")
  }
  #test position
  if (position %ni% c("top", "bottom", "start", "end")) {
    stop("Position must ne in top, bottom, start or end")
  }
  # test close button
  if (is.logical(close_btn) == FALSE) {
    stop("close_btn must be logical")
  }
  # test scroll
  if (is.logical(scroll) == FALSE) {
    stop("scroll must be logical")
  }
  # test backdrop
  if (is.logical(backdrop) == FALSE) {
    stop("backdrop must be logical")
  }
  # header definition
  tag_header <- NULL
  if (header == TRUE) {
    tag_button <- NULL
    if (close_btn == TRUE) {
      tag_button <- tags$button(
        type = "button",
        class = paste("btn-close", class_btn),
        'data-bs-dismiss' = "offcanvas",
        'aria-label' = "Close"
      )
    }
    # header class
    tag_header <- div(
      class = sprintf("offcanvas-header bg-default %s",class_header),
      title,
      tag_button
    )
  }
  # off canvas
  tag <- div(
    class = sprintf("offcanvas offcanvas-%s %s",position, class_oc),
    tabindex = "-1",
    id = id,
    'aria-labelledby' = paste0(id, "label"),
    'data-bs-scroll' = tolower(as.character(scroll)),
    'data-bs-backdrop' = tolower(as.character(backdrop)),
    tag_header,
    div(class = sprintf("offcanvas-body %s",class_body),
        body)
  )
  # return
  return(tag)
}

