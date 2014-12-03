## Function (9): Get Function #
getFunc <- 
  function  #function to decide and intitiate the function that sould be used to create the vairable for the synthetic data
### This function will simply decide what is what is the function from the package that
### should be used to simulate hte variable, then it will run this function to create the variable based on the inofrmation in the dataframe
### imported into R  from the excel template file.
(object
 ### The dataframe from the imported list that is based on excel template file 
 ){
   output <- NULL
  if(grepl(".prob", colnames(object)[2], fixed=TRUE))
    output <- c(output, "createCat")
  if(grepl("__p0", colnames(object)[2], fixed=TRUE))
    output <- c(output, "createCont")
  if(grepl("__cat2dummy", colnames(object)[1], fixed=TRUE))
    output <- c(output, "createDummy")
  if(grepl("cont2cat", colnames(object)[1], fixed=TRUE))
    output <- c(output, "recodeCont2cat")
  if(grepl("cat2cat", colnames(object)[1]))
    output <- c(output, "recodeCat2cat")
     if(length(output) > 1)
      stop(paste("Input object matches more than one rule:", paste(output, collapse=", ")))
  if(is.null(output))
    stop("Input object does not match any rule.")
  data <- get(output)
  return(data)
  ### the output will be a dataframe that contain the simulated variable.
}