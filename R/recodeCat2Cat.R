recodeCat2cat<- 
function  # function to create a categorical variable based on a previuosly defined categorical variable
### This function will simply create a new categorical variable with different categories than 
### an already created categorical variable, the number of categories for the new variables will be usually less than the 
### number in the predefined categorical variables by merging some of the categories of the predefined variables.
### The information should be specified in the template excel sheet that was imported as data frame.
### Information also include the variable name and the name of categories.
(input
 ### the list of dataframes based on imported template excel file with information about the variable name, categories 
 ### and the categories from the another (predefined) categorical variable.
 ,output
 ### the dataset that contain the already created categorical variable
 ,n
 ### the number of observation in the synthetic dataet
){
  var.name <- strsplit(colnames(input)[1], split="__")[[1]][1]
  orig.var <-strsplit(colnames(input)[2], split="__")[[1]][1]
  cat.splits <- strsplit(input[, 2], split="///")
  z<- length(cat.splits)
  output <- list()
  output[[var.name]] <- rep(NA, n)
  for (i in 1:z){
    output[[var.name]][output[[orig.var]] %in% cat.splits[[i]]] <- input[i, 1]
  }
  return(output)
  ### a list of one variable that is simply the new categorical variable
  ### with categories's probabilities based on the categories of a previuosly defined categorical variable.
  ### Also the missing fraction is based on the fraction missing in the orginal continuopus variable.
}