createCat<- 
function #function to simulate a categorical variable based on a predefined categories.
### This function will simply create a categorical variable based on the information specified in the imported excel template
### including the categories and their probabilities and the fraction missing as specified in the template excel sheet.
(input,
 ### the list of dataframes based on imported template excel file with information about the variable name, categories and their 
 ### probabilities.
 n
 ### the number of observation in the synthetic dataset.
){
  possible.values <- input[, 1]
  probabilities <-input[, 2]
  var.name <- colnames(input)[1]
  output <- list()
  output[[var.name]] <- sample(possible.values, size=n, replace=TRUE, prob=probabilities)
  return(output)
  ### a list of one numeric variable that is simply the categorical variable with categories given numeric number.
}