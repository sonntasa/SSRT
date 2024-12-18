% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSRT.R
\name{meanSSRT}
\alias{meanSSRT}
\title{meanSSRT}
\usage{
meanSSRT(dat, RTexc = FALSE, RTmin = 100, RTmax = 600)
}
\arguments{
\item{dat}{a dataframe containing vp number, rt, accuracy, the conditions
and stop-signal delays}

\item{vp_num}{Name of the column containing the vp identifier.}

\item{type}{the column that reflects the current trial type (stop vs. go)}

\item{corr}{boolean that reflects wether the current trial is Correct}

\item{ssd}{Name of the column containing the current stop signal delay}

\item{rt}{Reaction times}
}
\value{
a dataframe containing an estimate of the SSRT, the mean reaction
times on go trials and the mean stop signal delay for each VP
}
\description{
Estimates the SSRT using the mean of all SSDs in the
experiment. This calculation hinges upon the assumption that p(response |
signal = 0.5)
}
\examples{
dat <- meanSSRT(dat)
}
