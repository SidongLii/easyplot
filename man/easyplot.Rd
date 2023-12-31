% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyplot.R
\name{easyplot}
\alias{easyplot}
\title{make it easy to use ggplot2 for plot!}
\usage{
easyplot(
  data,
  plot = "barplot",
  xlab = "type",
  ylab = "mean",
  rank = "none",
  color = yanse,
  angle = 58,
  hjust = 0.99,
  vjust = 0.9,
  savename = "plot.jpg",
  width = 8,
  height = 6,
  alpha = 0.05,
  method = "more"
)
}
\arguments{
\item{data}{your data,data's fist column should be different type,and the second is value}

\item{plot}{choose a plot type between barplot and boxplot}

\item{xlab}{the label for x axis}

\item{ylab}{the label for y axis}

\item{rank}{rank the data}

\item{color}{fill with the color you like}

\item{angle}{the angle of xlab}

\item{hjust}{position of axis.text.text}

\item{vjust}{position of axis.text.text}

\item{savename}{save the plot to a file end with .jpg .png or .pdf}

\item{width}{image's size}

\item{height}{image's size}

\item{alpha}{level of significance test}

\item{method}{it can be 'more' when there are more than three replicates per experimental treatment;otherwise,it should be 'less'}
}
\value{
p
}
\description{
make it easy to use ggplot2 for plot!
}
\examples{
easyplot(data=mydata,plot="barplot")
}
\author{
Sidong-Li 2875620735@qq.com
}
