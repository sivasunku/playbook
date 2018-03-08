#' Functions for rulesNtrades
#' BOP - Balance of power
#' 
#' This will return if the given price is hit by the candlebar.
#' 
#' @author Siva Sunku
#' @keywords price
#' @note
#' 
#' @param  price - price, that needs to be evaluated
#' @param  bars  - bars with which, to be evaluated
#' @param  closeLag - if only close of previous bar to be evaluated. In this, only latest candle row is considered
#' @export

BOP <- function(bar){
  if (!is.xts(bar)){
    stop("In is.price.hit bars is not an xts object")
  }
  
  if (!is.OHLC(bar)){
    stop("In is.price.hit bars is not an OHLC object")
  }
  length <- bar$High - bar$Low
  wick   <- bar$Close - bar$Low
  
  return( round(wick / length, digits = 2) )
}


#' Functions for rulesNtrades
#' is.price.hit
#' 
#' This will return if the given price is hit by the candlebar.
#' 
#' @author Siva Sunku
#' @keywords price
#' @note
#' 
#' @param  price - price, that needs to be evaluated
#' @param  bars  - bars with which, to be evaluated
#' @param  closeLag - if only close of previous bar to be evaluated. In this, only latest candle row is considered
#' @export

is.price.hit <- function(price, bars, closeLag = FALSE){
  if (!is.xts(bars)){
    stop("In is.price.hit bars is not an xts object")
  }
  
  if (!is.OHLC(bars)){
    stop("In is.price.hit bars is not an OHLC object")
  }
  
  if (nrow(bars) > 1) {
    bars <- make.candle(bars)
  }
  
  if ( (price >= bars$Low)  && (price <= bars$High) ){
    return (TRUE)
  }
  
  return(FALSE)
  
}

#' make.candle
#' 
#' This will return a single bar of all the candles
#' 
#' @author Siva Sunku
#' @keywords make.candle
#' @note
#' 
#' @param  bars  - bars with which, to be evaluated
#' @export
#' 
make.candle <- function(bars){
  if (!is.xts(bars)){
    stop("In make.candle bars is not an xts object")
  }
  
  if (!is.OHLC(bars)){
    stop("In make.candle bars is not an OHLC object")
  }
  
  res <- bars[nrow(bars),]
  if (has.Vo(bars)){
    res$Volume <- sum(bars$Volume,na.rm = TRUE)
  }
  
  res$Open  <- bars[1,]$Open
  res$Low   <- min(bars$Low,na.rm = TRUE)
  res$High  <- max(bars$High,na.rm = TRUE)
  return(res)
}
