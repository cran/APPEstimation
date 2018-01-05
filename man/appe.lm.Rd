\name{appe.lm}
\alias{appe.lm}

\title{
  \eqn{L_1} and \eqn{L_2} errors adjusted for predictor distributions
}
\description{
  Calculates adjusted \eqn{L_1} and \eqn{L_2} errors by predictor
  distributions for a linear model. 
}
\usage{
appe.lm(mdl, dat.train, dat.test, method = "uLSIF", sigma = NULL,
        lambda = NULL, kernel_num = NULL, fold = 5, stabilize = TRUE,
        qstb = 0.025, reps = 2000, conf.level = 0.95)
}

\arguments{
  \item{mdl}{
    a \code{lm} object describing a prediction model to be evaluated. 
  }
  \item{dat.train}{
    same as in \code{appe.glm}.
  }
  \item{dat.test}{
    same as in \code{appe.glm}.
  }
  \item{method}{
    same as in \code{appe.glm}.
  }
  \item{sigma}{
    same as in \code{appe.glm}.
  }
  \item{lambda}{
    same as in \code{appe.glm}.
  }
  \item{kernel_num}{
    same as in \code{appe.glm}.
  }
  \item{fold}{
    same as in \code{appe.glm}.
  }
  \item{stabilize}{
    same as in \code{appe.glm}.
  }
  \item{qstb}{
    same as in \code{appe.glm}.
  }
  \item{reps}{
    same as in \code{appe.glm}.
  }
  \item{conf.level}{
    same as in \code{appe.glm}.
  }
}
\value{
  Adjusted and non-adjusted estimates of \eqn{L_1} and \eqn{L_2} errors
  are provided as matrix form. 
  "L1" and "L2" indicate non-adjusted versions, "L1 adjusted by score"
  and "L2 adjusted by score" indicate adjusted versions by linear
  predictors distribution, "L1 adjusted by predictors" and
  "L2 adjusted by predictors" indicate adjusted versions by
  predictor distributions (multi-dimensionally). 
  For confidence intervals, "Percentile" indicates a confidence interval
  by percentile method and "Approx" indicates approximated versions
  by Normal distribution.

}
\examples{
set.seed(100)

# generating development data
n0  = 100
Z   = cbind(rbeta(n0, 3, 3), rbeta(n0, 3, 3))
Y   = apply(Z, 1, function(xx) { rlnorm(1, sum(c(1, 1) * xx), 0.3) })
dat = data.frame(Za=Z[,1], Zb=Z[,2], Y=Y)

# the model to be evaluated
mdl = lm(Y~ Za + Zb, data=dat)

# generating validation dataset 
n1   = 100
Z1   = cbind(rbeta(n0, 3.5, 2.5), rbeta(n0, 3.5, 2.5))
Y1   = apply(Z1, 1, function(xx) { rlnorm(1, sum(c(1, 1) * xx), 0.3) })
dat1 = data.frame(Za=Z1[,1], Zb=Z1[,2], Y=Y1)

# calculation of L1 and L2 for this model
appe.lm(mdl, dat, dat1, reps=0)
}
