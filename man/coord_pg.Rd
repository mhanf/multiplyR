% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_multiplication.R
\name{coord_pg}
\alias{coord_pg}
\title{Calculate coordinates of polygon vertices}
\usage{
coord_pg(nb_vertice)
}
\arguments{
\item{nb_vertice}{number of polygon segments}
}
\value{
return coordinates of polygon vertices
}
\description{
Calculate coordinates of polygon vertices
}
\examples{
library(dplyr)
library(ggplot2)
poly <- coord_pg(3)
ggplot()+
geom_point(data = poly, aes(x = x, y = y))
}
