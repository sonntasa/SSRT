% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSRT.R
\name{intSSRT}
\alias{intSSRT}
\title{intSSRT}
\usage{
intSSRT(dat, replace_slow = TRUE)
}
\arguments{
\item{dat}{a dataframe containing vp number, rt, accuracy, the conditions
and stop-signal delays}

\item{replace_slow}{boolean that indicates whether slow values should be
raplaced with the slowest correct RT, to correct distribution (see
Verbruggen et al., 2019)}

\item{vp_num}{Name of the column containing the vp identifier.}

\item{type}{the column that reflects the current trial type (stop vs. go)}

\item{corr}{boolean that reflects wether the current trial is Correct}

\item{ssd}{Name of the column containing the current stop signal delay}

\item{rt}{Reaction times}
}
\value{
a dataframe containing the vp identifier, an estimate of the SSRT,
nth reaction time, used to compute the SSRT, the mean Stop Signal delay,
the p(response | signal) and the estimated SSRT.
}
\description{
utilises the RT distribution of correct Go Trials in a
Stop-Signal task to estimate the Stop-Signal Reaction Time
}
\examples{
dat <- intSSRT(dat)
}
