\name{appe.glm}
\alias{appe.glm}

\title{
  \eqn{C}-statistics adjusted for predictor distributions
}
\description{
  Calculates adjusted \eqn{C} statistics by predictor distributions for
  a generalized linear model with binary outcome. 
}
\usage{
appe.glm(mdl, dat.train, dat.test, method = "uLSIF", sigma = NULL,
         lambda = NULL, kernel_num = NULL, fold = 5, stabilize = TRUE, 
         qstb = 0.025, reps = 2000, conf.level = 0.95)
}

\arguments{
  \item{mdl}{
    a \code{glm} object describing a prediction model to be evaluated. 
  }
  \item{dat.train}{
    a dataframe used to construct a prediction model (specified in
    \code{mdl}), corresponding to a training data. Need to include
    outcome and all predictors. 
  }
  \item{dat.test}{
    a dataframe corresponding to a validation (testing) data. 
    Need to include outcome and all predictors. 
  }
  \item{method}{
    uLSIF or KLIEP.
    Same as the argument in \code{densratio} function from densratio
    package.
  }
  \item{sigma}{
    a positive numeric vector corresponding to candidate values of a
    bandwidth for Gaussian kernel.
    Same as the argument in \code{densratio} function from densratio
    package.
  }
  \item{lambda}{
    a positive numeric vector corresponding to candidate values of a
    regularization parameter.
    Same as the argument in \code{densratio} function from densratio
    package.
  }
  \item{kernel_num}{
    a positive integer corresponding to number of kernels.
    Same as the argument in \code{densratio} function from densratio
    package.
  }
  \item{fold}{
    a positive integer corresponding to a number of the folds of
    cross-validation in the KLIEP method.
    Same as the argument in \code{densratio} function from densratio
    package.
  }
  \item{stabilize}{
    a logical value as to whether tail weight stabilization is performed
    or not. 
    If TRUE, both tails of the estimated density ratio distribution are
    replaced by the constant value which is specified at \code{qstb}
    option. 
  }
  \item{qstb}{
    a positive numerical value less than 1 to control the degree of
    weight stabilization. Default value is 0.025, indicating estimated
    density ratio values less than the 2.5 percentile and more than the
    97.5 percentile are set to 2.5 percentile and 97.5 percentile,
    respectively.  
  }
  \item{reps}{
    a positive integer to specify bootstrap repetitions. If 0, bootstrap
    calculations are not performed. 
  }
  \item{conf.level}{
    a numerical value indicating a confidence level of interval. 
  }
}
\value{
  Adjusted and non-adjusted estimates of \eqn{C}-statistics are provided
  as matrix form.
  "Cstat" indicates non-adjusted version, "C adjusted by score"
  indicates adjusted version by linear predictors distribution, and
  "C adjusted by predictors" indicates adjusted version by
  predictor distributions (multi-dimensionally). 
  For confidence intervals, "Percentile" indicates a confidence interval
  by percentile method and "Approx" indicates approximated versions
  by Normal distribution.
}
\examples{
set.seed(100)

# generating learning data
n0  = 100
Z   = cbind(rbeta(n0, 5, 5), rbeta(n0, 5, 5))
Y   = apply(Z, 1, function (xx) {
        rbinom(1, 1, (1/(1+exp(-(sum(c(-2,2,2) * c(1,xx)))))))})
dat = data.frame(Y=Y, Za=Z[,1], Zb=Z[,2])

# the model to be evaluated
mdl = glm(Y~., binomial, data=dat)

# validation dataset, with different centers on predictors
n1   = 100
Z1   = cbind(rbeta(n1, 6, 4), rbeta(n1, 6, 4))
Y1   = apply(Z1, 1, function (xx) {
         rbinom(1, 1, (1/(1+exp(-(sum(c(-2,2,2) * c(1,xx)))))))})
dat1 = data.frame(Y=Y1, Za=Z1[,1], Zb=Z1[,2])

# calculation of L1 and L2 for this model
appe.glm(mdl, dat, dat1, reps=0)
}
