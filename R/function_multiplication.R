#' Calculate coordinates of polygon vertices
#'
#' @param nb_vertice number of polygon segments
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom magrittr %>%
#' @return return coordinates of polygon vertices
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' poly <- coord_pg(3)
#' ggplot()+
#' geom_point(data = poly, aes(x = x, y = y))

coord_pg <- function(nb_vertice){
  tt <- seq(0, 2*pi, length.out = (nb_vertice+1))[-(nb_vertice+1)]
  xx <- cos(tt)
  yy <- sin(tt)
  tibble::tibble(x = xx, y = yy)%>%
    dplyr::mutate(segment = 1:n())
}

#' calculate coordinates of points regularly distributed on polygon segments
#'
#' @param modulo number of points
#' @param nb_vertice number of polygon segments
#'
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom magrittr %>%
#' @return return coordinates of points
#' @export
#'
#' @examples
#' library(dplyr)
#' poly <- coord_pg(3)
#' point <- coord_point_pg(nb_vertice=3,modulo=20)

coord_point_pg <- function(nb_vertice,modulo){

  x <- y <- x1 <- x2 <- y1 <- y2 <- segment <- segment2 <- NULL
  
  coord <- coord_pg(nb_vertice)
  coord2 <- coord%>%
    dplyr::mutate(segment = segment - 1)%>%
    dplyr::mutate(segment = ifelse(segment == 0,max(segment)+1,segment))

  hypothenus <- sqrt((coord$x[1] -coord$x[2])**2 + (coord$y[1] -coord$y[2])**2)

  data <- tibble::tibble(raw = seq(0, nb_vertice*hypothenus, (nb_vertice*hypothenus)/modulo)[ -(modulo+1) ])%>%
    dplyr::mutate(segment = floor(raw/hypothenus))%>%
    dplyr::mutate(segment2 = segment*hypothenus)%>%
    dplyr::mutate(diff=raw - segment2)%>%
    dplyr::mutate(segment = segment + 1)%>%
    dplyr::left_join(coord, by = "segment")%>%
    dplyr::rename(x1 = x,y1 = y)%>%
    dplyr::left_join(coord2, by = "segment")%>%
    dplyr::rename(x2 = x,y2 = y)%>%
    dplyr::mutate(x = diff/hypothenus*x2 + (1-diff/hypothenus)*x1)%>%
    dplyr::mutate(y = diff/hypothenus*y2 + (1-diff/hypothenus)*y1)%>%
    dplyr::select(x,y)

  return(data)
}

#' calculate start and end coordinates of segments
#'
#' @param modulo number of points
#' @param nb_vertice number of polygon segments
#' @param table desired multiplication table
#'
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggfx
#' @return return coordinates of segments
#' @export
#'
#' @examples
#' library(dplyr)
#' segment <- coord_line(nb_vertice = 5, modulo = 100,table = 2)

coord_line <- function(nb_vertice, modulo, table){

  `.` <- x <- y <- depart <- x1 <- x2 <- y1 <- y2 <- NULL
  
  dest <- coord_point_pg(nb_vertice = nb_vertice, modulo = modulo)%>%
    dplyr::mutate(destination = seq_len(nrow(.)))%>%
    dplyr::rename(x2 = x,
                  y2 = y)

  lines <- coord_point_pg(nb_vertice = nb_vertice, modulo = modulo)%>%
    dplyr::mutate(depart = seq_len(nrow(.)))%>%
    dplyr::mutate(destination = (depart * table) %% nrow(.))%>%
    dplyr::mutate(destination = dplyr::case_when(destination == 0 ~ as.numeric(nrow(.)), TRUE ~ destination))%>%
    dplyr::rename(x1 = x,y1 = y)%>%
    dplyr::left_join(dest,by = "destination")%>%
    dplyr::select(x1, x2,y1, y2)

  return(lines)
}

#' Graphic representation of a particular multiplication table
#'
#' @param nb_vertice number of polygon segments
#' @param modulo number of points
#' @param zoom level of zoom
#' @param bgcolor Background color
#' @param curvature Level of segment curvature 
#' @param angle Angle of segment curvature
#' @param alpha Alpha level of segment color
#' @param colour Segment color
#' @param table desired multiplication table
#' @param outer_glow outer glow (TRUE/FALSE)
#' @param outer_glow_color outer glow color
#' @param outer_glow_sigma outer glow sigma
#' @param outer_glow_expand outer glow expand
#' @param inner_glow inner glow (TRUE/FALSE)
#' @param inner_glow_color inner glow color
#' @param inner_glow_sigma inner glow sigma
#' @param inner_glow_expand inner glow expand
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @return a graphic of the multiplication table
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' graph_line(nb_vertice = 5,
#' modulo = 20000,
#' table = 3,
#' alpha = 0.05,
#' curvature = 0,
#' angle = 0,
#' colour = "#2c3e50")

graph_line <- function(nb_vertice, 
                       modulo, 
                       table, 
                       zoom = 1, 
                       bgcolor = "transparent", 
                       curvature, 
                       angle, 
                       alpha, 
                       colour,
                       outer_glow = TRUE,
                       outer_glow_color = "#18BC9C",
                       outer_glow_sigma = 1,
                       outer_glow_expand = 1,
                       inner_glow = TRUE,
                       inner_glow_color = "#18BC9C",
                       inner_glow_sigma = 0.5,
                       inner_glow_expand = 1
                       ){
  
  x1 <- x2 <- y1 <- y2 <- NULL
  
  curve <- ggplot2::geom_curve(curvature = curvature, 
                               angle = angle, 
                               alpha = alpha, 
                               size =1.5,
                               colour = colour)
  
  # outer glow
  if (isTRUE(outer_glow)) {
    curve <- ggfx:: with_outer_glow(curve, 
                                    colour = outer_glow_color, 
                                    sigma = outer_glow_sigma, 
                                    expand = outer_glow_expand)
  }
  # inner glow
  if (isTRUE(inner_glow)) {
    curve <- ggfx:: with_inner_glow(curve, 
                                    colour = inner_glow_color, 
                                    sigma = inner_glow_sigma, 
                                    expand = inner_glow_expand)
  }
  # suppression of "points" segments
  lines <- coord_line(nb_vertice = nb_vertice, 
                      modulo = modulo, 
                      table = table
                      )%>%
    dplyr::filter(!(x1 == x2 & y1 == y2))
  # graph
  graph <- ggplot2::ggplot(data=lines, 
                           aes(x = x1, 
                               y = y1, 
                               xend = x2,
                               yend = y2
                               )
                           )+
    curve +
    xlim(-zoom,zoom)+
    ylim(-zoom,zoom)+
    ggplot2::theme_void()+
    theme(plot.background = element_rect(fill = bgcolor ,color = bgcolor))

  return(graph)
}

#' Graphic representation of multiplication tables from 2 to 10
#'
#' @param nb_vertice number of polygon segments
#' @param bgcolor background color
#' @param alpha  alpha color
#' @param curvature curvature
#' @param zoom zoom
#' @param angle angle
#' @param colour colour
#' @param modulo number of points
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @return a graphic of the multiplication tables from 2 to 10
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' p <- graph_all(nb_vertice = 8,
#' modulo = 5000,
#' alpha = 0.02,
#' curvature = 2,
#' angle = 3,
#' zoom = 2.2,
#' colour = "pink",
#' bgcolor = "#232323"
#' )

graph_all <- function(nb_vertice = 13,
                      modulo = 100,
                      bgcolor = "transparent",
                      alpha = 0.02,
                      curvature = 4,
                      zoom = 4.5,
                      angle = 213,
                      colour = "#007fdf"
){

  x1 <- x2 <- y1 <- y2 <- NULL
  
  name <- paste0("nbv_",nb_vertice,
                 "_mod_",modulo,
                 "_bgcol_",bgcolor,
                 "_alpha_",alpha,
                 "_curv_",curvature,
                 "_zoom_",zoom,
                 "_angle_",angle,
                 "_colour_",colour)

  lines <- lapply(2:10,function(k){
    tab <- coord_line(nb_vertice = nb_vertice, modulo = modulo, table = as.numeric(k))%>%
      dplyr::filter(!(x1 == x2 & y1 == y2))%>%
      mutate(table = as.numeric(k))
  }) %>% bind_rows()

  graph <- ggplot2::ggplot(data=lines, aes(x = x1, y = y1, xend = x2, yend = y2))+
    ggplot2::geom_curve(curvature = curvature, angle = angle, alpha = alpha, colour = colour)+
    xlim(-zoom,zoom)+
    ylim(-zoom,zoom)+
    ggplot2::theme_void()+
    ggplot2::theme(
      plot.background = element_rect(fill = bgcolor ,color = bgcolor),
      strip.background = element_blank(),
      strip.text.x = element_blank()
      )+
    ggplot2::facet_wrap(~ table, ncol = 3)

  #ggplot2::ggsave(plot = graph,paste0("C:/Users/mhanf/Desktop/rtistry/result/",name,".png"),width=20,height=20,unit="cm")

  return(graph)
}
