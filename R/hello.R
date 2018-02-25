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
getStellarData <- function(endpoint=NULL,limit=NULL,link=NULL,data=NULL,order='desc'){

  if(is.null(limit)){
    startString <- sprintf("https://horizon.stellar.org/%%s")
  }else{
    n <- ifelse(limit>200,200,limit)
    startString <- sprintf("https://horizon.stellar.org/%%s?limit=%i",n)
  }

  if(is.null(link)){
    endpointAddress <- sprintf(startString,endpoint)
    if(order=='desc'){
      if(!stringr::str_detect(endpointAddress,'order=.*')){ # Checking for previous order
        if(!stringr::str_detect(endpointAddress,'\\?')){ # Checking for other parameters
          endpointAddress <- paste0(endpointAddress,'?order=desc') # Creating parameters
        }else{
          endpointAddress <- paste0(endpointAddress,'&order=desc') # Appending parameters
        }
      }else{
        endpointAddress <- gsub('order=asc','order=desc',endpointAddress)
      }
    }
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
    }else if(order=='desc'){
      if(!stringr::str_detect(link,'order=.*')){ # Checking for previous order
        if(!stringr::str_detect(link,'\\?')){ # Checking for other parameters
          endpointAddress <- paste0(link,'?order=desc') # Creating parameters
        }else{
          endpointAddress <- paste0(link,'&order=desc') # Appending parameters
        }
      }else{
        endpointAddress <- gsub('order=asc','order=desc',link)
      }
    }else{
      endpointAddress <- link
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

getLineage <- function(stellarAddress,generationsMax=10,generation=NULL,lineage=NULL){
  if(is.null(generation)){
    generation <- 1
  }
  data <- getStellarData(link=sprintf('https://horizon.stellar.org/accounts/%s/operations',stellarAddress),limit=1)
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

# lineage=getLineage(stellarAddress)
# # a <- getStellarData('ledgers',210)
# b <- getStellarData('effects',1000)
#
# effectsTypeSummary <- b %>%
#   group_by(type)%>%
#   summarise(n=n())%>%
#   ungroup %>%
#   mutate(Percentage=n/sum(n)*100)
#
# ggplot(effectsTypeSummary)+
#   geom_bar(aes(x=type,y=Percentage),colour='black',stat='identity')+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle=60,hjust=1))
#
# # Account credited and debited are duplicated in some fields
# payments <- b %>%
#   filter(type=='account_debited')%>%
#   mutate(asset_code=ifelse(asset_type=='native','XLM',asset_code))
# c <- getStellarData('operations',1000)
# d <- getStellarData('payments',200)
# # e <- getStellarData('transactions',200)
# f <- getStellarData('trades',200)
# # g <- getStellarData(link="https://horizon.stellar.org/transactions/f93dfc81770019f2e842c6a1f3ecb2746db8e2b9b5b30b316128e6504de52f06/effects")
#
# # ledgerBenchmark <- microbenchmark(times=50,
# #                                   getStellarData('ledgers',1600),
# #                                   getStellarData('ledgers',800),
# #                                   getStellarData('ledgers',400),
# #                                   getStellarData('ledgers',200),
# #                                   getStellarData('ledgers',100),
# #                                   getStellarData('ledgers',50)
# # )
# #
# # ledgerBenchmark;autoplot(ledgerBenchmark)
# save(ledgerBenchmark,file='BenchmarkingNew.Rdata')
# autoplot(ledgerBenchmark)
#
# # ggplot(a)+
# #   geom_bar(aes(x=as.factor(closed_time)))
# #
# # aSummarised <- a %>%
# #   mutate(non_transaction=operation_count-transaction_count)%>%
# #   group_by(non_transaction,transaction_count)%>%
# #   summarise(mean_closed_time=mean(closed_time))
# #
# # ggplot(aSummarised)+aes()
# # #
# # ggplot(aSummarised)+
# #   geom_tile(aes(x=non_transaction,y=transaction_count,fill=mean_closed_time),colour='black',size=1)+
# #   coord_equal()
# #
# # ggplot(a)+
# #   geom_jitter(aes(x=operation_count-transaction_count,y=transaction_count,colour=closed_time),size=1)
#
# View(b)

# ```{r}
# historicValues <- getCoins('stellar')
# birthday <- lineage%>%
#   separate(birth,c('date','time'),' ')%>%
#   mutate('date'=as.Date(date))
# birthInfo <- left_join(birthday,select(historicValues,'date','close'))
# plot <- ggplot(historicValues)+
#   geom_line(aes(x=date,y=close))+
#   geom_point(data=birthInfo,aes(x=date,y=close),size=1,colour='red')
# ggplotly(plot)
# ```
