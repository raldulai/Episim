readTemplate <- 
function #function to read the excel template for the synthetic dataset into R as a dataframe
###This function simply load the all the excel worksheets from an excel spreadsheets into R and use it to create a 
###a list of dataframe.  Each variable represented into one sheet in excel file (name and summary statistics)
### returned in the output as a dataframe.
(filename
 ### the name and path of the template excel file
){
  Temp <- loadWorkbook(filename)
  input <- readWorksheet(Temp,
   ### The excel workbook
   sheet = getSheets(Temp))
   i<- length(input)
  message(paste("Import was successful for",
  i, 
  ### numeric vector represent the number of variables in the synthetic dataset
  "variables"))
  return(input)
  ### a list of dataframe for the variables names and descriptive statistics from the imported excel template
}
