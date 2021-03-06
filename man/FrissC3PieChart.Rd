% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/FrissC3Charts.R
\name{FrissC3PieChart}
\alias{FrissC3PieChart}
\title{Friss C3 pie or donut chart}
\usage{
FrissC3PieChart(value = data.frame(group1 = 20, group2 = 50, group3 = 70),
  type = "pie", dataHidden = list(), width = NULL, height = 250,
  maxGroups = NA, maxStringSize = 10, legendPosition = "bottom",
  transition = 250, displayValues = FALSE)
}
\arguments{
\item{value}{dataframe with values for the pie, the dataframe should consists of one row with a column for each group.}

\item{type}{type of the chart, either donut or pie}

\item{width}{width of the div the gauge is put in}

\item{height}{height of the div the gauge is put in}
}
\description{
Friss C3 pie or donut chart
}
\examples{
\dontrun{
data <- data.frame(group1=20,group2=50,group3=70)
FrissC3PieChart(data)
}
}

