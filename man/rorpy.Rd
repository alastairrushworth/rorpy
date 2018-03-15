\name{rorpy}
\alias{rorpy}
\title{R or Python?  Classification of webpages by code content.}
\usage{
rorpy(url)
}
\arguments{
\item{url}{A valid url}
}
\value{
\item{}{tibble containing the probability that the input url contains R, python or other.  If no code is found, the probability vector will be 0.}
}
\description{
This function fetches code chunks from a url and calculates the probability that the code is R, Python or something else.
}
\examples{
# not run:
# rorpy("https://google.com") # no code here...
# 
}