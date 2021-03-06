% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_multiplication.R
\name{graph_all}
\alias{graph_all}
\title{Graphic representation of multiplication tables from 2 to 10}
\usage{
graph_all(
  nb_vertice = 13,
  modulo = 100,
  bgcolor = "transparent",
  alpha = 0.02,
  curvature = 4,
  zoom = 4.5,
  angle = 213,
  colour = "#007fdf"
)
}
\arguments{
\item{nb_vertice}{number of polygon segments}

\item{modulo}{number of points}

\item{bgcolor}{background color}

\item{alpha}{alpha color}

\item{curvature}{curvature}

\item{zoom}{zoom}

\item{angle}{angle}

\item{colour}{colour}
}
\value{
a graphic of the multiplication tables from 2 to 10
}
\description{
Graphic representation of multiplication tables from 2 to 10
}
\examples{
library(dplyr)
library(ggplot2)
p <- graph_all(nb_vertice = 8,
modulo = 5000,
alpha = 0.02,
curvature = 2,
angle = 3,
zoom = 2.2,
colour = "pink",
bgcolor = "#232323"
)
}
