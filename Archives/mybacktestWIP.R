mybacktest  <- function(pf,dataset,parms,startat = 2,
                      longE,longX,
                      shortE,shortX,
                      calcLimits = calcLimits,
                      ...) {
  if (!is.OHLC(dataset) || !is.xts(dataset) ){
    stop("backtest - dataset is not OHLC")
  }
  maxR <- nrow(dataset)
  i <- max(2,startat)
  
  #default the instrument
  dots  <- list(...)
  instr <- ifelse(is.null(dots$instr),"default",dots$instr)
  
  ## --- Long Positions Check --------------------------------------------------
  pos <- position(instr)
  doAct <- TRUE
  for (i in 1:maxR){
    actFlag <- FALSE
    ### ------ Check Exits and conds-------------------------------------------
    if ( isopen(pos) ){
      pos$barCount <- pos$barCount + 1
      ## ---- Check for Exit --------------------------------------------------
      if( isshort(pos) ){
        pos <- shortX(pos,parms,dataset,i,...)
      } else {
        pos <-  longX(pos,parms,dataset,i,...)
      }
      add.table.positions(pf,bar = dataset[i,],pos)
      ## ---- Check if Closed or profit booking -----------------------------
        if ( isclosed(pos) ) {
          add.trxns.position(pf,pos,type = 'CLOSE')
          add.trades.position(pf,pos)
          
          pos$openQty   <- pos$openQty - pos$closeQty
          #Create new dummy position if position becomes complete zero
          if ( pos$openQty == 0) {
            pos$closeFlag <- TRUE
            pos <- position(instr)
            actFlag <- TRUE
          } else {
            pos$closeFlag <- FALSE
            pos$closeQty <- 0
            pos$closeReason <- "None"
          }
        }
      }
      
      #If same day new positions can be taken or not.
      if ( parms$sameDayFlag == FALSE ) {
        doAct <- ifelse(actFlag,FALSE,TRUE)
      }
      
      ### ------ OPEN the position -----------------------------------------------
      tempPos <- longE(parms,dataset,i,...)
      if ( (!isopen(pos)) && islong(tempPos) && (doAct) && (parms$longTrades) ){
        pos <- tempPos
        #Increase the id when position is created
        incr.positions.id(pf)
        pos$id <- get.positions.id(pf)
        #if(pos$id >=3) {browser()}
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }
      tempPos <- shortE(parms,dataset,i,...)
      if ( (!isopen(pos)) && isshort(tempPos) && (doAct) && (parms$shortTrades) ){
        pos <- tempPos
        #Increase the id when position is created
        incr.positions.id(pf)
        pos$id <- get.positions.id(pf)
        #if(pos$id >=3) {browser()}
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }

      ### ------ Calculate the limits---------------------------------------------
      pos <- calcLimits(pos,parms,dataset,i)
    } #End of Long positions For loop
  
 ## ---- Return Code --------------------------------------------------------------
 return(pf) 
}
  