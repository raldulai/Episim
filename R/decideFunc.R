decideFunc <- 
  function  #function to decide the type of the variable and the function could be used to create the vairable for the synthetic data
### This function will simply decide what is the type of the variable, and what is the function from the package that
### can be used to simulate hte variable. This will be based on the information specified in the excel template imported into R as 
### a dataframe.
(object
### The dataframe from the imported list that is based on excel template file 
 ){
  output <- NULL
  if(grepl(".prob", colnames(object)[2], fixed=TRUE))
    output <- c(output, "a categorical variable that can be created using (createCat) function")
  if(grepl("__p0", colnames(object)[2], fixed=TRUE))
    output <- c(output, "a continuous variable that can be created using (createCont) function")
  if(grepl("__cat2dummy", colnames(object)[1], fixed=TRUE))
    output <- c(output, "group of dummy variables for a previously created categorical variable, (createDummy)function should be used")
  if(grepl("cont2cat", colnames(object)[1], fixed=TRUE))
    output <- c(output, "a categorical variable based on a previously defined continuous variable, (recodeCont2cat) function can be used")
  if(grepl("cat2cat", colnames(object)[1]))
    output <- c(output, "a categorical variable based on a previously defined categorical variable, (recodeCat2cat) function can be used")
  if(length(output) > 1)
      stop(paste("Input object matches more than one rule:", paste(output, collapse=", ")))
  if(is.null(output))
    stop("Input object does not match any rule.")
  return(output)
  ### a message about the type of the variable and the R function from the package that  should be used.
}