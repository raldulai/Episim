\name{recodeCont2cat}
\alias{recodeCont2cat}
\title{function to create a categorical variable based on a previously defined continuous variable}
\description{This function will simply define a categorical variable by divided an already created continuous variable 
into several categories based on cutpoint defined in the template excel sheet that was imported as data frame.
Information also include the variable name and the name of categories.}
\usage{recodeCont2cat(input, output, n)}
\arguments{
  \item{input}{the list of dataframes based on imported template excel file with information about the variable name, categories 
and the cut point to be used on the continuous variable.}
  \item{output}{the dataframe that contain the predefined continuos variable.}
  \item{n}{the number of observation in the synthetic dataet.}
}

\value{a list of one variable that is simply the new categorical variable
with categories's probabilities based on cutpoints for the continuous variable.
Also the missing fraction is based on the fraction missing in the orginal continuopus variable.}

\author{Levi Waldron and Ragheed Al Dulaimi}





