###Install necessary package#
if(!require("XLConnect")){
  install.packages("XLConnect", repos="http://cran.us.r-project.org")
}
### install XLConnect R package to read excel sheets 

if(!require("XLConnectJars")){
  install.packages("XLConnectJars", repos="http://cran.us.r-project.org")
}
### install XLConnectJars R package which is a dependency for XLConnect package

library(XLConnect)
### Load XLConnect R package to read excel sheets 
library(XLConnectJars)
### Load XLConnectJars R package 




readTemplate <- function
###Load the excel template for the synthetic dataset#
(filename
### the name and path of the template file
  ) {
  Temp <- loadWorkbook(filename)
  ### load the excel workbook in R
  input <- readWorksheet(Temp,
  ### The excel workbook
  sheet = getSheets(Temp))
  ### read all the worksheets
  i<- length(input)
  message(paste("Import was successful for",
  ### print the message if the import of the template was succesful
  i, 
  ### numeric vector represent the number of variables in the synthetic dataset
  "variables"))
  return(input)
  ### print the input from the excel template
}

#example Function 1#
tmp <- readTemplate("Overall_Synthetic_CCP_Oct14.xls")

#function(2): Create New Categorical Variable #

createCat<- function
### Create a categorical variable based on the probabilities of its categoricies as specified in the template excel sheet
(input
 ### the imported template file
 ,n
 ### the number of observation in the synthetic dataset
  ){
  possible.values <- input[, 1]
  probabilities <-input[, 2]
  var.name <- colnames(input)[1]
  output <- list()
  output[[var.name]] <- sample(possible.values, size=n, replace=TRUE, prob=probabilities)
  return(output)
}


# example function 2 #
createCat(tmp[[1]], 5)
data.frame(c(createCat(tmp[[1]], 5), createCat(tmp[[2]], 5)))

lapply(tmp[24:27], createCat, n=10)

# function (3): simulated values using splines#
simValues <-  structure(function
### Simulates continuous distribution given quantiles
(probabilities,
### numeric vector giving probabilities at which quantiles are
### estimated in the original dataset, e.g. for quartiles would be
### seq(0, 1, by=0.25).
quantiles,
n
### number of observations to simulate from a continuous distribution
){
splinefit <- spline(probabilities, quantiles, n=n, method="hyman")
interpolated.probs <- splinefit$x
interpolated.quantiles <- splinefit$y
### TODO: Need to generate uniformly distributed values within each
### range of quantiles to get a random sample with the same
### distribution as the original.
sample(sim.values, size=n)
##Details<< This function interpolates given quantiles and samples
## them to create a synthetic continuous variable that simulates
## the distribution of another variable.
## end<<
},ex=function(){
##Simulate an original variable from a random exponential distribution
origvar <- rexp(1000)
## Use percentiles to simulate the distribution
probs <- seq(0, 1, by=0.01)
simvar <- simValues(probabilities=probs, quantiles=quantile(origvar, probs=probs), 1000)
##Use a QQ plot to show these distributions are comparable:
qqplot(origvar, simvar);
abline(a=0, b=1)

})


# function (4): missing values as NA#
simMissing <- function
### assign NA to certain amount of values of the new variables based on the fraction missing specified in the template
(input
 ### the imported excel template
 ,n
 ### the number of observation in the synthetic dataet
 ) {
  z <- round
  ### round the number specified in the template for missing fraction to the closest integer
  (n*input$frac.missing)
  ###
  rep(NA, z)
  ### assign NA for a fraction of the values of the new variable
}

## TODO: Simmissing seems no longer working??

# example function 4#
simMissing(tmp[[7]],10)



createCont<- function
### Create Continuous Variable
(input
 ### the imported template file
 , n
 ### the number of observation in the synthetic dataet
 ){
    quantile.data <- input[grepl("_p[0-9]", colnames(input))]
  ##Use the part before the "_" as the variable name:
  var.name <- sub("_.+", "", names(quantile.data)[1])
  ##
  dat2 <- data.frame(quantiles=as.numeric(sub(".+_p", "", names(quantile.data))) / 100,
                     values=t(quantile.data)[, 1])
  sim.values <- simValues(dat2$quantiles, dat2$values,n)
  sim.missing<- simMissing(input,n)
  sim.values[sample(1:length(sim.values), size=length(sim.missing))] <- NA
  output <- list()
  output[[var.name]] <- sim.values
  return(output)
}


# example function 5#
createCont(tmp[[7]],10)



  
  recodeCont2cat <- function
### create categorical variable from continuous variable
(input
 ### the imported template file
 ,output
 ### the dataset that contain the already created variable
 ,n
 ### the number of observation in the synthetic dataet
 ) {
  ##original continuous variable is what comes before "__" in the
  ##second column header:
  ##variable name is what comes before "__" in the first column header
  
  var.name <- strsplit(colnames(input)[1], split="__")[[1]][1]
  orig.var <-strsplit(colnames(input)[2], split="__")[[1]][1]

  if("missing" %in% input[, 2]){
    missing.val <- input[match("missing", input[, 2]), 1]
    input <- input[-match("missing", input[, 2]), ]
    input[, 2] <- as.numeric(input[, 2])
  }
 as.character(cut(output[[orig.var]], breaks=c(0, input[, 2], Inf), labels=input[, 1]))

 output <- list()
 if(exists("missing.val")){
  output[[var.name]][is.na(output[[var.name]])] <- missing.val
}
  }



recodeCat2cat<- function 
### create categorical variable from another categorical variable
(input
 ### the imported template file
 ,output
 ### the dataset that contain the already created variable
 ,n
 ### the number of observation in the synthetic dataet
  ){
  var.name <- strsplit(colnames(input)[1], split="__")[[1]][1]
  orig.var <-strsplit(colnames(input)[2], split="__")[[1]][1]
  ### Create a list, one element per level of the new variable,
  ### each element contains the elements of the old variable:
  cat.splits <- strsplit(input[, 2], split="///")
  ##Create the new variable:
  z<- length(cat.splits)
  output <- list()
  output[[var.name]] <- rep(NA, n)
  for (i in 1:z){
    output[[var.name]][output[[orig.var]] %in% cat.splits[[i]]] <- input[i, 1]
    }
  return(output)
}
#example function7#
  recodeCat2cat(tmp[[9]], Income_C, 5)



 
  createDummy <- function
 ### create dummy variables from already created categorical variable
(input
 ### the imported template file
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
  }
   
   
   
   # Function (9): Decide Function #
   decideFunc <- function(object){
     ## Rule for creating a categorical variable
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
     if(grepl("cat2dummy", colnames(object)[1]))
       if(length(output) > 1)
         stop(paste("Input object matches more than one rule:", paste(output, collapse=", ")))
     if(is.null(output))
       stop("Input object does not match any rule.")
     return(output)
   }
   

# example function(9)#
   paste("createCont") == decideFunc(tmp[[7]])
   lapply(tmp[1:27], decideFunc)
 
## Date Function
genDates <- function(dates.vec, probs, N){
  ## example data:
  ## dates.vec <- c("1994-01-01", "2004-01-01", "2014-01-01")
  ## probs <- c(0.4, 0.6)
  ## N <- 100
  if(!length(probs) == (length(dates.vec)-1)) stop("probs must have one fewer values than dates.vec")
  dates.orig <- lapply(1:length(probs), function(i){
    interval.days <- as.integer(as.Date(dates.vec[i+1]) - as.Date(dates.vec[i]))
    sample(as.Date(1:interval.days, origin=dates.vec[i]), N*probs[i], replace=TRUE)
  })
  sample(do.call(c, dates.orig))
}

##genDates(dates.vec=c("1994-01-01", "2004-01-01", "2014-01-01"), probs=c(0.4, 0.6), N=10)   
   
   