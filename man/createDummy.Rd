\name{createDummy}
\alias{createDummy}
\title{function to create dummy variables from already created categorical variable.}
\description{This function will simply create a number of dummy variable for each of the categories of a previously created categorical variable.
The categories and name of each dummy variable will be based on the information specified in the imported excel template. }
\usage{createDummy(input, n)}
\arguments{
  \item{input}{the list of dataframes based on imported template excel file with information about the variable name, categories and the
name of the previuosly defined categorical variable that will be recoded as dummy vairables.}
  \item{n}{the number of observation in the synthetic dataet}
}

\value{a list of dummy variables that is created based on the elements of a previously defined categorical variable.}

\author{Levi Waldron and Ragheed Al Dulaimi}





