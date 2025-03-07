% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot.flowSet}
\alias{autoplot.flowSet}
\alias{autoplot}
\alias{autoplot.ncdfFlowList}
\alias{autoplot.cytoset}
\alias{autoplot.cytoframe}
\alias{autoplot.flowFrame}
\alias{autoplot.GatingSetList}
\alias{autoplot.GatingSet}
\alias{autoplot.GatingHierarchy}
\title{Plot cytometry data in one or two dimension with the ggcyto package.}
\usage{
\method{autoplot}{flowSet}(object, x, y = NULL, bins = 30, ...)

\method{autoplot}{ncdfFlowList}(object, ...)

\method{autoplot}{cytoset}(object, ...)

\method{autoplot}{cytoframe}(object, ...)

\method{autoplot}{flowFrame}(object, x, ...)

\method{autoplot}{GatingSetList}(object, ...)

\method{autoplot}{GatingSet}(
  object,
  gate,
  x = NULL,
  y = "SSC-A",
  bins = 30,
  axis_inverse_trans = TRUE,
  ...
)

\method{autoplot}{GatingHierarchy}(
  object,
  gate,
  y = "SSC-A",
  bool = FALSE,
  arrange.main = sampleNames(object),
  arrange = TRUE,
  merge = TRUE,
  projections = list(),
  strip.text = c("parent", "gate"),
  path = "auto",
  ...
)
}
\arguments{
\item{object}{The data source. A core cytometry data structure. A flowFrame, flowSet, GatingSet or GatingHierarchy object}

\item{x}{define the x dimension of the plot (not used when object is a GatingSet). When object is a flowFrame, it can be missing, which plots 1d density plot on all the channels.}

\item{y}{define the y dimension of the plot. Default is NULL, which means 1d densityplot.}

\item{bins}{passed to geom_hex}

\item{...}{other arguments passed to ggplot}

\item{gate}{the gate to be plotted}

\item{axis_inverse_trans}{logical flag indicating whether to add \link{axis_x_inverse_trans} and axis_x_inverse_trans layers.}

\item{bool}{whether to plot boolean gates}

\item{arrange.main}{the main title of the arranged plots}

\item{arrange}{whether to use arrangeGrob to put multiple plots in the same page}

\item{merge}{wehther to merge multiple gates into the same panel when they share the same parent and projections}

\item{projections}{a list of customized projections}

\item{strip.text}{either "parent" (the parent population name) or "gate "(the gate name). The latter usually is used when merge is FALSE}

\item{path}{the gating path format (passed to \link{gs_get_pop_paths})}
}
\value{
a ggcyto object
}
\description{
Overloaded autoplot methods for the cytometry data structure: \code{flowFrame} or \code{flowSet}, \code{Gatinghierarchy}, \code{GatingSet}.
It plots the cytometry data with \code{geom_histogram}, \code{geom_density} or \code{geom_hex}.
When autoplot is called on a \code{GatingSet}/\code{Gatinghierarchy}, the second argument should be a gate or population node. And the dimensions(channels/markers) are deduced from the gate dimensions.
}
