% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{getAccountData}
\alias{getAccountData}
\title{Get historical account data for a specified account}
\usage{
getAccountData(address, order = NULL, limit = NULL)
}
\arguments{
\item{address}{Stellar address}

\item{order}{parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"}

\item{limit}{The number of elements that will be in the resulting data frame.}
}
\value{
List containing information, transactions, operations, payments, effects, and offers for the account.
}
\description{
Get historical account data for a specified account
}
