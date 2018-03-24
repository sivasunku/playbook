#' graph - plot the graph into a given pdf
#' 
#' This will plot the OHLC bars & corresponding trades performed. 
#' 
#' Green uptriangle & Green downtriangle represents the Long Entry & Long Exits. Longs are drawn above the corresponding bar.
#' 
#' Red   uptriangle & red   downtriangle represents the short Entry & short Exits. Shorts are drawn below the corresponding bar.
#'
#' @author Siva Sunku
#' @keywords graph,plot
#' @note
#' 
#' @param  pf - portfolio which needs to be graphed
#' @param  prices    - OHLC of the shares/equity
#' @param  by        - frequncy for each page (whether monthly, weekly, quarterly) etc. 
#' e.g. monthly makes each page of pdf to contain one months data. 
#' All values accepted by endpoints function are available + halfyear. default - quarters.
#' @param  file      - filename where pdf to be stored. Please be advised, it will overwrite if any file exists.
#' @param  inTrades  - if given only these trades are graphed. portfolio is ignored.
#' @param  overFun   - overFun is a overlay function that helps to overlay graph wiht any of the indicators or values. 
#' @param  parms     - trade parms
#' @details overFun - is a function supplied by user. This is executed in the graph function environment, so all the variables
#' available for the graph function can be used by overFun. However caution to be exercised not to stop the function abruptly
#' @rdname graph
#' @export
#' 
myGraph <- function(pf, prices, 
                    file = NULL, 
                    by = 'quarters',
                    parms = NULL,
                    inTrades = NULL,
                    inPositions = NULL,
                    overFun = NULL,...) {
  
  width <- 5
  ## --- Housekeeping -----------------------------------------------------------------------------------
  if ( !is.valid.portfolio(pf) ) {
    stop("graph - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  if ( missing(inTrades) ){
    inTrades <- get.trades(pf)
  }
  if ( missing(inPositions)){
    inPositions <- get.positions.table(pf)
  }
  if (!is.null(file) && file.exists(file)) { 
    msg <- paste( "File", file, " Exists already. It is overwritten",sep="")
    warnings(msg)
  }
  if ( !missing(overFun) && !is.function(overFun) ){
    stop("graph - overFun should be a function")
  }
  k <- 1
  if (by == "halfyear"){
    by <- "months"
    k  <- 6
  }
  
  splitPrices  <- split.xts(prices,f = by, k=k)
  #Make the slp/pb price table
  pbPrices <- .makePBtable(inPositions,parms)
  
  pieces <- length(splitPrices)
  temp.plots <- vector(pieces,mode = 'list')
  
  ## --- Loop -----------------------------------------------------------------------------------
  p <- 0
  i <- 1
  for (i in 1:(pieces)){
    c  <- splitPrices[[i]]
    a  <- OHLC(splitPrices[[i]])
    pb <- pbPrices[index(c)]
    
    #Keep more y-range & x-range to keep the room for PB price lines.
    prices <- pb[,grep("Price",colnames(pb))]
    holdings <- pb[,"qty"]
    hyrange <- c(min(pb$qty,0,na.rm = TRUE),max(pb$qty,0,na.rm = TRUE))
    
    maxPrice <- max(prices,c$High,na.rm = TRUE)
    minPrice <- min(prices,c$Low,na.rm = TRUE)
    yrange <- c(minPrice,maxPrice)
    
    #Draw the Longs
    chartSeries(c,type = 'bars',yrange = yrange)
    
    #plot the PB price, trail price etc
    plot(addTA(pb[,c("pbPrice","slpPrice","trailSlpPrice","trailPrice")],
               col = c("green","red","blue","orange"),
               type = c('o','o','o','o'),
               on = 1
    ))
    plot(addTA(res[,c("bullFlow","bearFlow")],col=c("green","red")))
    plot(addTA(holdings$qty,type="b",yrange = hyrange,lwd = 2,col="red"))
    temp.plots[[i]] <- recordPlot()
  }
  
  #print to pdf
  if ( !is.null(file) ) {
    pdf(file,onefile = TRUE)
    for(i in temp.plots){
      replayPlot(i)
    }
    graphics.off()
  } 
  
} #end of function graph


#' @details .entryexit - internal function to return 4 columns with '1' in corresponding rows where trade is done.
#' @param  t - trades
#' @param  p - prices,OHLC
#' @return returns xts which has 4 columns - lE,lX,sE,sX longEntry,longExit,shortEntry,shortExit etc
#'
.entryexit <- function(t,p){
  
  res <- xts( order.by = index(p) )
  
  #long Trades
  lT <- t[t$direction == "LONG",]
  sT <- t[t$direction == "SHORT",]
  
  if (nrow(lT) > 0 ){
    #Long Entries
    lE <- xts(order.by = lT$entryTime)
    lE <- merge(lE,lE = 1)
    
    #Long Exits
    lX <- xts(order.by = lT$exitTime)
    lX <- merge(lX,lX = 1)
    
    res <- merge.xts(res,lE,lX)
  }
  
  if(nrow(sT) > 1){
    #Short Entries
    sE <- xts(order.by = sT$entryTime)
    sE <- merge(sE,sE = 1)
    
    #short Exits
    sX <- xts(order.by = sT$exitTime)
    sX <- merge(sX,sX = 1)
    res <- merge.xts(res,sE,sX)
  }
  
  return(res)
}

#' @details .makePBtable - internal function to return profit booking,slp, trailing prices
#' @param  p - trades
#' @return returns xts which has 4 columns - lE,lX,sE,sX longEntry,longExit,shortEntry,shortExit etc
#'
.makePBtable <- function(p,parms){
  b     <- p[,c("onDate","slpPrice","trailPrice","trailSlpPrice","pbPrice")]
  b$qty <-  ifelse(p$direction == "SHORT",p$openQty * -1,p$openQty)
  a <- xts(x = b[,-1],order.by = b$onDate)
  return(a)
}
