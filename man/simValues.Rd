\name{simValues}
\alias{simValues}
\title{simValues}
\description{Simulates continuous distribution given quantiles}
\usage{simValues(probabilities, quantiles, n)}
\arguments{
  \item{probabilities}{numeric vector giving probabilities at which quantiles are
estimated in the original dataset, e.g. for quartiles would be
seq(0, 1, by=0.25).}
  \item{quantiles}{
}
  \item{n}{number of observations to simulate from a continuous distribution}
}



\author{Levi Waldron and Ragheed Al Dulaimi}




\examples{
##Simulate an original variable from a random exponential distribution
origvar <- rexp(1000)
## Use percentiles to simulate the distribution
probs <- seq(0, 1, by=0.01)
simvar <- simValues(probabilities=probs, quantiles=quantile(origvar, probs=probs), 1000)
##Use a QQ plot to show these distributions are comparable:
qqplot(origvar, simvar);
abline(a=0, b=1)

}
