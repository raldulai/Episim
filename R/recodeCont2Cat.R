recodeCont2cat <- 
function  # function to create a categorical variable based on a previously defined continuous variable
### This function will simply define a categorical variable by divided an already created continuous variable 
### into several categories based on cutpoint defined in the template excel sheet that was imported as data frame.
### Information also include the variable name and the name of categories.
(input,
 ### the list of dataframes based on imported template excel file with information about the variable name, categories 
 ### and the cut point to be used on the continuous variable.
 output,
 ### the dataframe that contain the predefined continuos variable.
 n
 ### the number of observation in the synthetic dataet.
) {
  var.name <- strsplit(colnames(input)[1], split="__")[[1]][1]
  ##variable name is what comes before "__" in the first column header
  orig.var <-strsplit(colnames(input)[2], split="__")[[1]][1]
  ##original continuous variable is what comes before "__" in the
  ##second column header
  if("missing" %in% input[, 2]){
    missing.val <- input[match("missing", input[, 2]), 1]
    input <- input[-match("missing", input[, 2]), ]
    input[, 2] <- as.numeric(input[, 2])
  }
  as.character(cut(output[[orig.var]], breaks=c(0, input[, 2], Inf), labels=input[, 1]))
  output <- list()
  if(exists("missing.val")){
    output[[var.name]][is.na(output[[var.name]])] <- missing.val
    ### a list of one variable that is simply the new categorical variable
    ### with categories's probabilities based on cutpoints for the continuous variable.
    ### Also the missing fraction is based on the fraction missing in the orgiinal continuopus variable.
  }
}
