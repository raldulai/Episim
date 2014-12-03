\name{recodeCat2cat}
\alias{recodeCat2cat}
\title{function to create a categorical variable based on a previuosly defined categorical variable}
\description{This function will simply create a new categorical variable with different categories than 
an already created categorical variable, the number of categories for the new variables will be usually less than the 
number in the predefined categorical variables by merging some of the categories of the predefined variables.
The information should be specified in the template excel sheet that was imported as data frame.
Information also include the variable name and the name of categories.}
\usage{recodeCat2cat(input, output, n)}
\arguments{
  \item{input}{the list of dataframes based on imported template excel file with information about the variable name, categories 
and the categories from the another (predefined) categorical variable.}
  \item{output}{the dataset that contain the already created categorical variable}
  \item{n}{the number of observation in the synthetic dataet}
}

\value{a list of one variable that is simply the new categorical variable
with categories's probabilities based on the categories of a previuosly defined categorical variable.
Also the missing fraction is based on the fraction missing in the orginal continuopus variable.}

\author{Levi Waldron and Ragheed Al Dulaimi}





