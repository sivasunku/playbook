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
  if (parms$longTrades) {
    pos <- position(instr)
    for (i in 1:maxR){
      if ( isopen(pos) ){
        pos$barCount <- pos$barCount + 1
        pos <- longX(pos,parms,dataset,i,...)
        add.table.positions(pf,bar = dataset[i,],pos)
        if ( isclosed(pos) ) {
          add.trxns.position(pf,pos,type = 'CLOSE')
          add.trades.position(pf,pos)
          
          pos$openQty   <- pos$openQty - pos$closeQty
          pos$closeFlag <- ifelse(pos$openQty == 0,TRUE,FALSE)
          #Create new dummy position if position becomes complete zero
          if ( isclosed(pos) ){ pos <- position(instr) }
        }
      }  
      
      tempPos <- longE(parms,dataset,i,...)
      if ( (!isopen(pos)) && islong(tempPos) ){
        pos <- tempPos
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }
      
      # if ( islong(pos) && islong(tempPos) && (tradeParms$pyramidFlag == TRUE) ){
      #   pod <- pos + tempPos
      #   add.trxns.position(pf,tempPos,type = 'OPEN')
      # }
      browser()
      pos <- calcLimits(pos,parms,dataset[i,])
    } #End of Long positions For loop
  }
  
  ## --- Short Positions Check --------------------------------------------------
  if (parms$shortTrades) {
    pos <- position(instr)
    for (i in 1:maxR){
      if ( isopen(pos) ){
        pos$barCount <- pos$barCount + 1
        pos <- shortX(pos,parms,dataset,i,...)
        if ( isclosed(pos) ) {
          add.trxns.position(pf,pos,type = 'CLOSE')
          add.trades.position(pf,pos)
          
          pos$openQty   <- pos$openQty - pos$closeQty
          pos$closeFlag <- ifelse(pos$openQty == 0,TRUE,FALSE)
          #Create new dummy position if position becomes complete zero
          if ( isclosed(pos) ){ pos <- position(instr) }
        }
      }  
      
      tempPos <- shortE(parms,dataset,i,...)
      if ( (!isopen(pos)) && isshort(tempPos) ){
        pos <- tempPos
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }
      add.table.positions(pf,bar = dataset[i,],pos)
      pos <- calcLimits(pos,parms,dataset[i,])
    }
  }
}

