\name{cvalest.bin}
\alias{cvalest.bin}

\title{
  Estimation of \eqn{C}-statistics
}
\description{
  Calculates \eqn{C}-statistics. Individual case  weight can be
  incorporated. 
}
\usage{
  cvalest.bin(Y, scr, wgt = NULL)
}

\arguments{
  \item{Y}{
    a numerical vector of inary outcome, either 0 or 1. 
  }
  \item{scr}{
    a numerical vector of continuous variable. 
  }
  \item{wgt}{
    a numerical vector corresponding to individuatl weight.
  }
}
\value{
  \eqn{C}-statistics is provided. 
}
