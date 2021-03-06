% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{plot_pie}
\alias{plot_pie}
\title{Plot pie graph from data frame}
\usage{
plot_pie(x, ax, ay)
}
\arguments{
\item{x}{A data.frame.}

\item{ax}{(string with quotes) A name of X axis.  (what will be in legend)}

\item{ay}{(string with quotes) A name of Y axis. (number of every item in legend)}
}
\value{
The pie plot of \code{x}.
}
\description{
Plot pie graph from data frame
}
\examples{
plot_pie(dataframe, "column1","column2")
}
