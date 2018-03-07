#########################################################################
# This will have all the functions that make entry / exits
# New functions to be created in the similar format for both long&Short entries & exits
#########################################################################

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
  if ( be > bu ){
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
  if ( (bu > be) && (FALSE) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- Cl
    cPos$closeReason <- "Case#2"
    return(cPos)
  }
  
  
  ##  --- SLP check for open---------------------------------------------------------
  if ( (parms$slpFlag) && (Op > cPos$slpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- cPos$openQty
    cPos$closePrice <- Op
    cPos$closeReason <- "SLP hit in Open"
    return(cPos)
  }
  
  
  ##  --- Trail SLP check for open -------------------------------------------
  if ( (parms$trlFlag) && (cPos$trailTrigFlag) && (Op > cPos$trailSlpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- cPos$openQty
    cPos$closePrice <- Op
    cPos$closeReason <- "TrailingSLP hit in Open"
    return(cPos)
  }
  
  
  ##  ---  SLP check -------------------------------------------
  if ( (parms$slpFlag) && is.price.hit(cPos$slpPrice,bar) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- cPos$openQty
    cPos$closePrice <- cPos$slpPrice
    cPos$closeReason <- "SLP hit"
    return(cPos)
  }
  
  ##  --- Trailing SLP check ------------------------------------------------
  if ( (parms$trlFlag) && (cPos$trailTrigFlag) && is.price.hit(cPos$trailSlpPrice,bar)){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- cPos$openQty
    cPos$closePrice <- cPos$trailSlpPrice
    cPos$closeReason <- "TrailingSLP hit"
    return(cPos)
  }
  
  
  
  ##  --- Profit Booking check -------------------------------------------------
  
  if ( (parms$pbFlag) && (Op < cPos$pbPrice) && (cPos$openQty > parms$pbRunQty) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- min(parms$pbQty,cPos$openQty)
    cPos$closePrice <- Op
    cPos$closeReason <- "Profit Booking in Open"
    return(cPos)
  }
  
  if ( (parms$pbFlag) && is.price.hit(cPos$pbPrice,bar) && (cPos$openQty > parms$pbRunQty) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- min(parms$pbQty,cPos$openQty)
    cPos$closePrice <- cPos$pbPrice
    cPos$closeReason <- "Profit Booking"
    return(cPos)
  }
  
  
  ## ---- Return ----------
  return(cPos)
}
