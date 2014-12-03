\name{readTemplate}
\alias{readTemplate}
\title{function to read the excel template for the synthetic dataset into R as a dataframe}
\description{This function simply load the all the excel worksheets from an excel spreadsheets into R and use it to create a 
a list of dataframe.  Each variable represented into one sheet in excel file (name and summary statistics)
returned in the output as a dataframe.}
\usage{readTemplate(filename)}
\arguments{
  \item{filename}{the name and path of the template excel file}
}

\value{a list of dataframe for the variables names and descriptive statistics from the imported excel template}

\author{Levi Waldron and Ragheed Al Dulaimi}





