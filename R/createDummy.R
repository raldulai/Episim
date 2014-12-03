createDummy <- 
  function  # function to create dummy variables from already created categorical variable.
### This function will simply create a number of dummy variable for each of the categories of a previously created categorical variable.
### The categories and name of each dummy variable will be based on the information specified in the imported excel template. 
(input
 ### the list of dataframes based on imported template excel file with information about the variable name, categories and the
 ### name of the previuosly defined categorical variable that will be recoded as dummy vairables.
 ,n
 ### the number of observation in the synthetic dataet
)
{rm(var.name)  #not used here
 if(any(input[, 1] %in% names(output))){
   stop(paste("Check sheet", sheet, ": variables re-used from a previous sheet."))
 }
 output <- list()
 for (i in 1:nrow(var.input)){
   output[[var.input[i, 1]]] <- rep(0, n)
   output[[var.input[i, 1]]][output[[orig.var]]==var.input[i, 2]] <- 1
 }
 return(output)
 ### a list of dummy variables that is created based on the elements of a previously defined categorical variable.
}
