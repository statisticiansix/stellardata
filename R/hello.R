# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
library(jsonlite)
library(tidyverse)
library(magrittr)
library(microbenchmark)

unnestDF <- function(data){
  out <- list()
  for(col in names(data)){
    if(class(data[[col]])=='data.frame'){
      colnames(data[[col]]) <- sprintf('%s.%s',col,colnames(data[[col]]))
      for(nested in names(data[[col]])){
        out[[nested]] <- data[[col]][[nested]]
      }
    }else{
      out[[col]] <- data[[col]]
    }
  }
  if('data.frame'%in%unlist(lapply(out,class))){
    return(unnestDF(out))
  }else{
    return(bind_cols(out))
  }
}

# Removes the links from the API Data
cleanAPIData <- function(API_data){
  return(unnestDF(API_data[['_embedded']][['records']]))
}

# A recursive function that gets the last N ledger entries
getStellarData <- function(N=200,endpoint='ledgers',link=NULL,data=NULL){
  n <- ifelse(N>200,200,N)

  if(is.null(link)){
    endpointAddress <- sprintf("https://horizon.stellar.org/%s?limit=%i&order=desc",endpoint,n)
  }else{
    endpointAddress <- gsub('limit=.*\\&',sprintf('&limit=%s\\&',n),link)
  }

  raw <- fromJSON(endpointAddress)
  nextLink <- raw[['_links']][['next']][['href']]

  if(is.null(data)){
    data <- cleanAPIData(raw)
  }else{
    data <- bind_rows(
      data,
      cleanAPIData(raw)
    )
  }

  if(N>200){
    return(getStellarData(N=N-n,endpoint=endpoint,link=nextLink,data=data))
  }else{
    if(endpoint=='ledgers'){
      tempEndpoint <- gsub('limit=.*\\&','limit=1\\&',raw[['_links']][['next']][['href']])
      plusOne <- cleanAPIData(fromJSON(tempEndpoint))

      data <- bind_rows(data,plusOne) %>%
        mutate('closed_at'=as.POSIXct(strptime(closed_at, "%Y-%m-%dT%H:%M:%SZ")),
               'closed_time'=c(abs(as.numeric(diff(closed_at))),NA))%>%
        .[-nrow(.),]
    }

    return(data)
  }
}

a <- getStellarData(210,'ledgers')
b <- getStellarData(200,'operations')
c <- getStellarData(200,'effects')
d <- getStellarData(200,'payments')
e <- getStellarData(200,'transactions')
f <- getStellarData(200,'trades')

ledgerBenchmark <- microbenchmark(times=50,
                                  getStellarData(1600,'ledgers'),
                                  getStellarData(800,'ledgers'),
                                  getStellarData(400,'ledgers'),
                                  getStellarData(200,'ledgers'),
                                  getStellarData(100,'ledgers'),
                                  getStellarData(50,'ledgers')
)
ledgerBenchmark;autoplot(ledgerBenchmark)
save(ledgerBenchmark,file='Benchmarking.Rd')
