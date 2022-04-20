#' Test the bootstrap validity of a color
#'
#' @param color color to test
#' @param transparent Is transparent color authorized ?
#' @param default Is default color authorized ?
#' @param bw  Is black ans white authorized ?
#'
#' @return A error message if the color is invalid
#' @export
#' 
#' @examples
#' test_bs_color(
#' color = "default" 
#' )
#' test_bs_color(
#' color = "default",
#' default = TRUE
#' )

test_bs_color <- function(color,
                          transparent = FALSE,
                          default = FALSE,
                          bw = TRUE
){
  result <- TRUE
  # test transparent
  if (is.logical(transparent) == FALSE) {
    stop("transparent must be logical")
  }
  # test default
  if (is.logical(default) == FALSE) {
    stop("default must be logical")
  }
  # test bw
  if (is.logical(bw) == FALSE) {
    stop("bw must be logical")
  }
  # bootstrap color
  bs_color <- c("primary",
                "secondary",
                "light",
                "dark",
                "info",
                "success",
                "warning",
                "danger"
  )
  # optional add of transparent and default color
  if (default == TRUE){ bs_color = c("default", bs_color) }
  if (transparent == TRUE){ bs_color = c(bs_color, "transparent") }
  if (bw == TRUE){ bs_color = c(bs_color, "black", "white") }
  # stop if color is incorrect
  if (color %ni% bs_color){ result <- FALSE }
  # return 
  return(result)
}
