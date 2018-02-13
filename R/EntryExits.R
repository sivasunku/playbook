#########################################################################
# This will have all the functions that make entry / exits
# New functions to be created in the similar format for both long&Short entries & exits
#########################################################################

#' This is a sample 
longEntry <- function(parms,m,i,...) {
  
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)
  cPos <- position(instr = parms$instrument)
  
  ##  --- Check the Entry Condition -------------------------------------------------
  if (bu > be){
    cPos$openDate   <- index(bar)
    cPos$direction  <- "LONG"
    cPos$justOpened <- TRUE
    cPos$openPrice  <- Cl
    cPos$openQty    <- parms$qty
    cPos$openFlag   <- TRUE
    cPos$openReason <- "Case#1"
    cPos$openCase   <- "Case#1"
    return(cPos)
  }
  
  ## --- End -----
  return(cPos)
}

#' This is a sample 
longExit <- function(cPos,parms,m,i,...){
  
  if (! isopen(cPos) ) { return(cPos)}
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)

  ##  --- Actual Inidcator check ---------------------------------------------------------
  if ( (bu < be) && FALSE){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- Cl
    cPos$closeReason <- "Case#1"
    return(cPos)
  }

  
  ##  --- SLP check ---------------------------------------------------------
    #1. If open itself is less than slpPrice close the position with open price
    if ( (parms$slpFlag) && (Op < cPos$slpPrice) ){
      cPos$closeDate  <- index(bar)
      cPos$closeFlag  <- TRUE
      cPos$closeQty   <- parms$qty
      cPos$closePrice <- Op
      cPos$closeReason <- "SLP hit in Open"
      return(cPos)
    }
    
    #2. If in the candle slpPrice is hit, close the position
    if ( (parms$slpFlag) && is.price.hit(cPos$slpPrice,bar) ){
      cPos$closeDate  <- index(bar)
      cPos$closeFlag  <- TRUE
      cPos$closeQty   <- parms$qty
      cPos$closePrice <- cPos$slpPrice
      cPos$closeReason <- "SLP hit"
      return(cPos)
    }
  
  ##  --- Trailing SLP check -------------------------------------------------
  if ( (parms$trlFlag) && (Op <= cPos$trailSlpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- Op
    cPos$closeReason <- "TrailingSLP hit in Open"
    return(cPos)
  }
  
  if ( (parms$trlFlag) && is.price.hit(cPos$trailSlpPrice,bar) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- cPos$trailSlpPrice
    cPos$closeReason <- "TrailingSLP hit"
    return(cPos)
  }
 
  ##  --- Profit Booking check -------------------------------------------------
      #1. If Open itself more than pbPrice, close the position with Openprice
      if ( (parms$pbFlag) && (Op > cPos$pbPrice) ){
        cPos$closeDate  <- index(bar)
        cPos$closeFlag  <- TRUE
        cPos$closeQty   <- parms$pbQty
        cPos$closePrice <- Op
        cPos$closeReason <- "Profit Booking in Open"
        return(cPos)
      }
     #2. If Candle hit the pbPrice, close the position
      if ( (parms$pbFlag) && is.price.hit(cPos$pbPrice,bar) ){
        cPos$closeDate  <- index(bar)
        cPos$closeFlag  <- TRUE
        cPos$closeQty   <- parms$pbQty
        cPos$closePrice <- cPos$pbPrice
        cPos$closeReason <- "Profit Booking"
        return(cPos)
      }
  
  return(cPos)
}

#' This is a sample ShortEntry
shortEntry <- function(parms,m,i,...){
  
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)
  cPos <- position(instr = parms$instrument)
  
  ##  --- Check the Short Entry Condition -------------------------------------------------
  if (be > bu){
    cPos$openDate   <- index(bar)
    cPos$direction  <- "SHORT"
    cPos$justOpened <- TRUE
    cPos$openPrice  <- Cl
    cPos$openQty    <- parms$qty
    cPos$openFlag   <- TRUE
    cPos$openReason <- "Case#1"
    cPos$openCase   <- "Case#1"
    return(cPos)
  }
  
  ## --- Return --------------
  return(cPos)

}

#' This is a sample 
shortExit <- function(cPos,parms,m,i,...){
  if (! isopen(cPos) ) { return(cPos)}
  
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)
  
  
  ##  --- Actual Inidcator check ---------------------------------------------------------
  if (bu > be){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- Cl
    cPos$closeReason <- "Case#2"
    return(cPos)
  }
  
  ##  --- SLP check ---------------------------------------------------------
  if ( (parms$slpFlag) && (Hi > cPos$slpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- cPos$slpPrice
    cPos$closeReason <- "SLP hit"
    return(cPos)
  }
  
  ##  --- Trailing SLP check -------------------------------------------------
  if ( (parms$trlFlag) && (Hi > cPos$trailSlpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- cPos$trailSlpPrice
    cPos$closeReason <- "TrailingSLP hit"
    return(cPos)
  }
  
  ##  --- Profit Booking check -------------------------------------------------
  if ( (parms$pbFlag) && (Lo < cPos$pbPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$pbQty
    cPos$closePrice <- cPos$pbPrice
    cPos$closeReason <- "Profit Booking"
    return(cPos)
  }
  
  ## ---- Return ----------
  return(cPos)
}

## --- Calculate limits -------------------------------------------------------------
myCalcLimits      <- function(pos,tradeParms,bar,...) {
  t <- tradeParms
  browser()
  ##  --- Set the Variables ---------------------------------------------------------
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  ## --- For Long positions -------------------------------
  if ( islong(pos) ) {
  ## --- SLP ------------------------------------------------
    pos$slpPrice <- ifelse( t$pctFlag,
                            pos$openPrice * (1 - (t$slpAmt/100) ) ,
                            pos$openPrice - t$slpAmt )
    pos$slpPrice <- round(pos$slpPrice,2)
    

  ## --- PB Long ---------------------------------------------
    pos$pbPrice  <- ifelse( t$pctFlag,
                            pos$openPrice * (1 + (t$pbAmt/100) )  ,
                            pos$openPrice + t$pbAmt )
    pos$pbPrice <- round(pos$pbPrice,2)
    
    if (is.price.hit(pos$pbPrice,bar) ) {
      
      temp  <- ifelse( t$pctFlag,
                              pos$pbPrice * (1 + (t$pbAmt/100) )  ,
                              pos$pbPrice + t$pbAmt )
      temp <- round(temp,digits = 2)
      pos$pbPrice <- max(pos$pbPrice,temp)
    }
    
  ## --- TrailPrice, TrailSLP --------------------------------------
    #Set initial trailPrice
    if ( is.na(pos$trailPrice)) {
      pos$trailPrice <- ifelse( t$pctFlag,
                                pos$openPrice * (1 + (t$trlAmt/100) ) , 
                                pos$openPrice + t$trlAmt)
      pos$trailPrice <- round(pos$trailPrice,digits = 2)
      
      pos$trailSlpPrice <- ifelse( t$pctFlag,
                                   pos$openPrice * (1 - (t$trlSlpAmt/100)),
                                   pos$openPrice - t$trlSlpAmt )
      pos$trailSlpPrice <- round(pos$trailSlpPrice,digits = 2)
      
    } 
    
    # If price hit the trail price, increase the trailSlpPrice
    if ( is.price.hit(pos$trailPrice,bar) ) {
      temp <- ifelse( t$pctFlag,
                      pos$trailPrice * (1 + (t$trlAmt/100) ) , 
                      pos$trailPrice + t$trlAmt)
      pos$trailPrice <- round(temp,digits = 2)
      
      temp  <- ifelse( t$pctFlag,
                       pos$trailSlpPrice * (1 + (t$trlSlpAmt/100)),
                       pos$trailSlpPrice + t$trlSlpAmt )
      temp  <- round(temp,digits = 2)
      
      pos$trailSlpPrice <- max(pos$trailSlpPrice,temp)
    }
    
  }
  
  ## --- For Short positions -------------------------------
  if ( isshort(pos) ) {
    ## SLP
    pos$slpPrice <- ifelse( t$pctFlag,
                            pos$openPrice * (1 + (t$slpAmt/100) ) ,
                            pos$openPrice + t$slpAmt )
    pos$slpPrice <- round(pos$slpPrice,2)
    
    ## PB
    pos$pbPrice  <- ifelse( t$pctFlag,
                            pos$openPrice * (1 - (t$pbAmt/100) )  ,
                            pos$openPrice - t$pbAmt )
    pos$pbPrice <- round(pos$pbPrice,2)
    
    
    #Initial trailing Price os calculated based on the openPrice, subsequent once based on the existing trailingPrice.
    if ( is.na(pos$trailPrice)) {
      pos$trailPrice <- ifelse( t$pctFlag,
                                pos$openPrice * (1 - (t$trlAmt/100) ) , 
                                pos$openPrice - t$trlAmt)
      pos$trailPrice <- round(pos$trailPrice,digits = 2)
    } else {
      #This is to be trailed only if Price is hit by the candle.
      if ( Lo <= pos$trailPrice) {
        temp <- ifelse( t$pctFlag,
                        pos$trailPrice * (1 - (t$trlAmt/100) ) , 
                        pos$trailPrice - t$trlAmt)
        pos$trailPrice <- round(temp,digits = 2)
      }
    } #trailPrice End
    
    pos$trailSlpPrice <- ifelse( t$pctFlag,
                                 pos$trailPrice * (1 + (t$trlSlpAmt/100)),
                                 pos$trailPrice + t$trlSlpAmt )
    pos$trailSlpPrice <- round(pos$trailSlpPrice,digits = 2)
    
  } #End of Short position adjustments
  
  return(pos)
}
