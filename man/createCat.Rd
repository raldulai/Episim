\name{createCat}
\alias{createCat}
\title{function to simulate a categorical variable based on a predefined categories.}
\description{This function will simply create a categorical variable based on the information specified in the imported excel template
including the categories and their probabilities and the fraction missing as specified in the template excel sheet.}
\usage{createCat(input, n)}
\arguments{
  \item{input}{the list of dataframes based on imported template excel file with information about the variable name, categories and their 
probabilities.}
  \item{n}{the number of observation in the synthetic dataset.}
}

\value{a list of one numeric variable that is simply the categorical variable with categories given numeric number.}

\author{Levi Waldron and Ragheed Al Dulaimi}





