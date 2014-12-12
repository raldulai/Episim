simValues <-  structure(
  function # function top simulate values using splines.
 ### This function interpolates given quantiles and samples them to create a synthetic continuous variable that simulates
 ### the distribution of another variable.
 (probabilities,
  ### numeric vector giving probabilities at which quantiles are
  ### estimated in the original dataset, e.g. for quartiles would be
  ### seq(0, 1, by=0.25).
  quantiles,
  ### the quantiles of the continuous variable in the list based on imported excel template
  n
  ### the number of observation in the synthetic dataset.
  ){
  splinefit <- spline(probabilities, quantiles, n=max(10000*n, 1e8), method="hyman")
  interpolated.quantiles <- splinefit$y
  ### missing stuff here where sim.values is created
  sample(interpolated.quantiles, size=n)
  },ex=function(){
  ## Simulate an original variable from a random exponential distribution
  origvar <- rexp(1000)
  ## Use percentiles to simulate the distribution
  probs <- seq(0, 1, by=0.01)
  simvar <- simValues(probabilities=probs, quantiles=quantile(origvar, probs=probs), 1000)
  ##Use a QQ plot to show these distributions are comparable:
  qqplot(origvar, simvar);
  abline(a=0, b=1)
  })

.simMissing <- function(input,n) {
  z <- round (n*input$frac.missing)
  rep(NA, z)
  }
