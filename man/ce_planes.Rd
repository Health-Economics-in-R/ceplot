% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ce_planes_dummy.R, R/my_contour2.R,
%   R/my_contour2_facet.R
\name{ce_planes}
\alias{ce_planes}
\alias{my_contour2}
\alias{my_contour2.bcea}
\alias{my_contour2_facet}
\title{Cost Effectiveness Planes}
\usage{
my_contour2(...)

\method{my_contour2}{bcea}(
  res_bcea,
  wtp = 20000,
  xlim = NULL,
  ylim = NULL,
  comparison = NULL,
  withpoints = FALSE,
  graph = c("base", "ggplot2"),
  CONTOUR_PC = "5\%",
  facet = FALSE,
  ...
)

my_contour2_facet(dat, ...)
}
\arguments{
\item{...}{}

\item{res_bcea}{res_bcea}

\item{wtp}{wtp}

\item{xlim}{xlim}

\item{ylim}{ylim}

\item{comparison}{comparison}

\item{graph}{graph}

\item{CONTOUR_PC}{CONTOUR_PC}

\item{facet}{(TRUE/FALSE)}

\item{dat}{List of class dectree output}
}
\value{
plot

plot
}
\description{
Cost Effectiveness Planes

my_contour2

my_contour2_facet
}
