% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dc.R
\name{distinctiveness}
\alias{distinctiveness}
\title{The main function; oversees the calculations of Distinctiveness Centrality}
\usage{
distinctiveness(
  G,
  alpha = 1,
  normalize = FALSE,
  measures = c("D1", "D2", "D3", "D4", "D5")
)
}
\arguments{
\item{G}{the given graph}

\item{alpha}{the given exponent for penalizing connections to highly connected nodes}

\item{normalize}{when TRUE, the function normalizes output metrics 
to allow for comparison with other graphs. Defaults to FALSE}

\item{measures}{the measures of Distinctiveness Centrality to be computed}
}
\value{
a data frame containing the specified calculated measures of
Distinctiveness Centrality for the given graph
}
\description{
The main function; oversees the calculations of Distinctiveness Centrality
}
\examples{

g <- igraph::erdos.renyi.game(20, 50, type = "gnm", directed = FALSE)
plot(g)
distinctiveness(g)
distinctiveness(g, alpha = list(2, 1, 3, 2, 4), measures = c("D1", "D3", "D4"))

g_dir <- igraph::erdos.renyi.game(20, 50, type = "gnm", directed = TRUE)
plot(g_dir)
distinctiveness(g_dir)
distinctiveness(g_dir, alpha = 2, normalize = TRUE, measures = c("D2", "D5"))

}
