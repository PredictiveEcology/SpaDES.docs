% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{wrapStrFun}
\alias{wrapStrFun}
\title{Wrap/break strings by size}
\usage{
wrapStrFun(x, size, breakChar = "\\n")
}
\arguments{
\item{x}{the string, or a vector or list of strings to break}

\item{size}{desired size of string chunks. Strings whose
original size is <= than \code{size} will not be cut.}

\item{breakChar}{break symbol/character used to separate chunks}
}
\value{
the "broken" and collapsed strings, in the same class
as \code{x}
}
\description{
Unlike \code{strwrap} and \code{strsplit}, \code{wrapStrFun}
"breaks" a string based on length creating chunks of a
specified size that are collapsed into a single string but
separated using a special character
}
