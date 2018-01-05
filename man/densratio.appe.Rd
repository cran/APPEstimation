\name{densratio.appe}
\alias{densratio.appe}

\title{
  A wrapper function
}
\description{
  A wrapper function to use "densratio" function from the densratio package. 
}
\usage{
densratio.appe(xtrain, xtest, method = "uLSIF", sigma = NULL,
               lambda = NULL, kernel_num = NULL, fold = 5,
               stabilize = TRUE,  qstb = 0.025)
}

\arguments{
  \item{xtrain}{
    a dataframe used to construct a prediction model. 
  }
  \item{xtest}{
    a dataframe corresponding to a validation (testing) data. 
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
}
