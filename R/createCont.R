
createCont<- 
function #function to simulate a variable with continuous distribution given quantiles.
### This function will simply create a continuos variable based on the information specified in the imported excel template
### including the quantiles (deciles) and their probabilities and the fraction missing as specified in the template excel sheet.
(input
 ### the list of dataframes based on imported template excel file with information about the variable name, quantiles and their 
 ### probabilities.
 , n
 ### the number of observation in the synthetic dataet
){
  quantile.data <- input[grepl("_p[0-9]", colnames(input))]
  var.name <- sub("_.+", "", names(quantile.data)[1])
  dat2 <- data.frame(quantiles=as.numeric(sub(".+_p", "", names(quantile.data))) / 100,
                     values=t(quantile.data)[, 1])
  sim.values <- simValues(dat2$quantiles, dat2$values,n)
  sim.missing<- .simMissing(input,n)
  sim.values[sample(1:length(sim.values), size=length(sim.missing))] <- NA
  output <- list()
  output[[var.name]] <- sim.values
  return(output)
  ### a list of one numeric variable that is simply the continuous variable simulated based on the quantiles specified in the imported file.
}








