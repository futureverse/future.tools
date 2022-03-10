% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/journal.R
\name{journal}
\alias{journal}
\title{The exported version of \pkg{future}'s private \code{journal()}}
\usage{
journal(...)
}
\arguments{
\item{\ldots}{Arguments passed to \code{future:::journal()}.}
}
\value{
Returns a data.frame of class \code{FutureJournal}.
}
\description{
The exported version of \pkg{future}'s private \code{journal()}
}
\details{
The \code{journal()} function of the \pkg{future} package is not exported
at the time being because it undergoes lots of development.  In the
meanwhile, one can use the version provided here, but please note that
the arguments and the format of the returned value is likely to change.
Please do not depend on this in other packages or production code.
}