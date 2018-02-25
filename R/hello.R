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

# library(jsonlite)
# library(tidyverse)
# library(magrittr)
# library(microbenchmark)

#' Unnest JSON pulled from Horizon API.
#'
#' @param data Embedded records data from Horizon API.
#' @return Formatted data frame.
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

#' Unnest JSON pulled from Horizon API.
#'
#' @param API_data JSON output from the Horizon API.
#' @return Formatted data frame.
cleanAPIData <- function(API_data){
  return(unnestDF(API_data[['_embedded']][['records']]))
}

#' A recursive function that gets the last N entries from a Horizon API endpoint
#'
#' @param endpoint Endpoint from the Horizon API such as 'ledgers' or 'transactions'.
#' @param limit The number of elements that will be in the resulting data frame.
#' @param link Direct link to the Horizon API such as 'https://horizon.stellar.org/ledgers'
#' @param data Dataframe from getStellarData() to append new data to
#' @param order Either 'asc' or 'desc' which is passed as a parameter to the API call.
#' @return Formatted data frame.
#' @examples
#' ledger_data <- getStellarData('ledgers',limit=500)
#' effects_data <- getStellarData('effects',limit=250)
#' transaction_effects_data <- getStellarData(link="https://horizon.stellar.org/transactions/f93dfc81770019f2e842c6a1f3ecb2746db8e2b9b5b30b316128e6504de52f06/effects")
getStellarData <- function(endpoint=NULL,limit=NULL,link=NULL,data=NULL,order='desc'){

  if(is.null(limit)){
    startString <- sprintf("https://horizon.stellar.org/%%s")
  }else{
    n <- ifelse(limit>200,200,limit)
    startString <- sprintf("https://horizon.stellar.org/%%s?limit=%i",n)
  }

  if(is.null(link)){
    endpointAddress <- sprintf(startString,endpoint)
  }else{
    if(!is.null(limit)){
      if(!stringr::str_detect(link,'limit=')){ # Checking for previous limit
        if(!stringr::str_detect(link,'\\?')){ # Checking for other parameters
          endpointAddress <- paste0(link,'?limit=',n) # Creating parameters
        }else{
          endpointAddress <- paste0(link,'&limit=',n) # Appending parameters
        }
      }else{
        #Check to see if limit=n&
        if(stringr::str_detect(link,'limit=.*\\&')){
          endpointAddress <- gsub('limit=.*\\&',sprintf('limit=%s\\&',n),link)
        }else{
          endpointAddress <- gsub('limit=.*',sprintf('limit=%s',n),link) # End of string
        }
      }
    }else{
      endpointAddress <- link
    }
  }

  if(!is.null(order)){
      if(!stringr::str_detect(endpointAddress,'order=.*')){ # Checking for previous order
        if(!stringr::str_detect(endpointAddress,'\\?')){ # Checking for other parameters
          endpointAddress <- paste0(endpointAddress,'?order=',order) # Creating parameters
        }else{
          endpointAddress <- paste0(endpointAddress,'&order=',order) # Appending parameters
        }
      }else{
        if(order=='desc'){
          endpointAddress <- gsub('order=asc','order=desc',endpointAddress)
        }else{
          endpointAddress <- gsub('order=desc','order=asc',endpointAddress)
        }

    }
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

  if(ifelse(!is.null(limit),limit>200,FALSE)){
    return(getStellarData(limit=limit-n,endpoint=endpoint,link=nextLink,data=data))
  }else{
    if(ifelse(!is.null(endpoint),endpoint=='ledgers',FALSE)){
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

#' Get account creation history for a specified account
#'
#' @param stellarAddress Stellar address
#' @param generationsMax The maximum number of generations to return
#' @param generation Indicator informing the number of generations for recurring calls
#' @param lineage output from previous getLineage() function to append new data to
#' @return Formatted data frame giving parent account, child account and creation date
getLineage <- function(stellarAddress,generationsMax=10,generation=NULL,lineage=NULL){
  if(is.null(generation)){
    generation <- 1
  }
  data <- getStellarData(link=sprintf('https://horizon.stellar.org/accounts/%s/operations?order=asc',stellarAddress),limit=1,order=NULL)
  parent <- data %>%
    .[['funder']]

  birth <- as.POSIXct(strptime(data %>%
    .[['created_at']], "%Y-%m-%dT%H:%M:%SZ"))

  new <- tibble('parent'=parent,'child'=stellarAddress,'birth'=birth)
  if(is.null(lineage)){
    lineage <- new
  }else{
    lineage <- bind_rows(lineage,new)
  }

  if(parent!=stellarAddress){ # Checking for when the parent account is equal to the child account - potentially
    if(generation<generationsMax){
      getLineage(parent,generationsMax,generation+1,lineage)
    }else{
      return(lineage)
    }
  }else{
    return(lineage)
  }
}

#' Get historical account data for a specified account
#'
#' @param address Stellar address
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return List containing information, transactions, operations, payments, effects, and offers for the account.
getAccountData <- function(address,order=NULL,limit=NULL){
  accountData <- list()
  rawData <- fromJSON(sprintf('https://horizon.stellar.org/accounts/%s',address))
  accountData$information <- rawData[names(rawData)[-1]]
  accountData$transactions <- getStellarData(link=sprintf('https://horizon.stellar.org/accounts/%s/transactions',address),order=order,limit=limit)
  accountData$operations <- getStellarData(link=sprintf('https://horizon.stellar.org/accounts/%s/operations',address),order=order,limit=limit)
  accountData$payments <- getStellarData(link=sprintf('https://horizon.stellar.org/accounts/%s/payments',address),order=order,limit=limit)
  accountData$effects <- getStellarData(link=sprintf('https://horizon.stellar.org/accounts/%s/effects',address),order=order,limit=limit)
  accountData$offers <- getStellarData(link=sprintf('https://horizon.stellar.org/accounts/%s/offers',address),order=order,limit=limit)
  return(accountData)
}

#' Get assets information from the Horizon API
#'
#' @param assetCode Code for an asset available on the API
#' @param assetCode Issuer for an asset available on the API
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return Formatted data frame
getAssets <- function(assetCode=NULL,assetIssuer=NULL,limit=NULL,order=NULL){
  link <- sprintf("https://horizon.stellar.org/assets")

  options <- c()

  if(!is.null(assetCode))
    options[length(options)+1] <- sprintf('asset_code=%s',assetCode)
  if(!is.null(assetIssuer))
    options[length(options)+1] <- sprintf("asset_issuer=%s",assetIssuer)

  if(length(options)>0){
    link <- paste(link,paste(options,collapse='&'),sep='?')
  }

  return(getStellarData(link=link,limit=limit,order=order))
}

#' Get historical effects from the Stellar API
#'
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return Formatted data frame.
getEffects <- function(order=NULL,limit=NULL){
  return(getStellarData('effects',limit=limit,order=order))
}

#' Get historical ledgers from the Stellar API
#'
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return Formatted data frame.
getLedgers <- function(order=NULL,limit=NULL){
  getStellarData('ledgers',order=order,limit=limit)
}

#' Get historical ledger data for a specified ledger
#'
#' @param ledger Ledger ID
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return List containing information, transactions, operations, payments, and effects for the ledger.
getLedgerData <- function(ledger,order=NULL,limit=NULL){
  ledgerData <- list()
  rawData <- fromJSON(sprintf('https://horizon.stellar.org/ledgers/%s',ledger))
  ledgerData$information <- rawData[names(rawData)[-1]]
  ledgerData$transactions <- getStellarData(link=sprintf('https://horizon.stellar.org/ledgers/%s/transactions',ledger),order=order,limit=limit)
  ledgerData$operations <- getStellarData(link=sprintf('https://horizon.stellar.org/ledgers/%s/operations',ledger),order=order,limit=limit)
  ledgerData$payments <- getStellarData(link=sprintf('https://horizon.stellar.org/ledgers/%s/payments',ledger),order=order,limit=limit)
  ledgerData$effects <- getStellarData(link=sprintf('https://horizon.stellar.org/ledgers/%s/effects',ledger),order=order,limit=limit)
  return(ledgerData)
}

#' Get historical operations from the Stellar API
#'
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return Formatted data frame.
getOperations <- function(order=NULL,limit=NULL){
  getStellarData('operations',order=order,limit=limit)
}

#' Get account creation history for a specified account
#'
#' @param address Stellar address
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return List containing information, transactions, operations, payments, effects, and offers for the account.
getOperationData <- function(operation,order=NULL,limit=NULL){
  operationData <- list()
  rawData <- fromJSON(sprintf('https://horizon.stellar.org/operations/%s',operation))
  operationData$information <- rawData[names(rawData)[-1]]
  operationData$effects <- getStellarData(link=sprintf('https://horizon.stellar.org/operations/%s/effects',operation),order=order,limit=limit)
  return(operationData)
}

#' Get historical trades from the Stellar API
#'
#' @param order Parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @param offerid Offer id for a trade
#' @return Formatted data frame.
getTrades <- function(order=NULL,limit=NULL,offerid=NULL){
  if(!is.null(offerid)){
    getStellarData(link=sprintf('https://horizon.stellar.org/trades?offer_id=%s',offerid),order=order,limit=limit)
  }else{
    getStellarData('trades',order=order,limit=limit)
  }
}

#' Get historical payments from the Stellar API
#'
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return Formatted data frame.
getPayments <- function(order=NULL,limit=NULL){
  getStellarData('payments',order=order,limit=limit)
}

#' Get historical transactions from the Stellar API
#'
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return Formatted data frame.
getTransactions <- function(order=NULL,limit=NULL){
  getStellarData('transactions',order=order,limit=limit)
}

#' Get transaction data for a specified transaction
#'
#' @param transaction Transaction ID
#' @param order parameter to pass to the API call can be NULL, "asc", or "desc". If NULL the API default is "asc"
#' @param limit The number of elements that will be in the resulting data frame.
#' @return List containing information, operations, and effects for the transaction.
getTransactionData <- function(transaction,order=NULL,limit=NULL){
  transactionData <- list()
  rawData <- fromJSON(sprintf('https://horizon.stellar.org/transactions/%s',transaction))
  transactionData$information <- rawData[names(rawData)[-1]]
  transactionData$operations <- getStellarData(link=sprintf('https://horizon.stellar.org/transactions/%s/operations',transaction),order=order,limit=limit)
  transactionData$effects <- getStellarData(link=sprintf('https://horizon.stellar.org/transactions/%s/effects',transaction),order=order,limit=limit)
  return(transactionData)
}

