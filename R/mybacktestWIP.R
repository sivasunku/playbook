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
    doAct <- TRUE
    for (i in 1:maxR){
      actFlag <- FALSE
      if ( isopen(pos) ){
        pos$barCount <- pos$barCount + 1
        pos <- longX(pos,parms,dataset,i,...)
        add.table.positions(pf,bar = dataset[i,],pos)
        
        ## ---- If position is closed, check if it is for Profit booking ----
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
      
      tempPos <- longE(parms,dataset,i,...)
      if ( (!isopen(pos)) && islong(tempPos) && (doAct) ){
        pos <- tempPos
        #Increase the id when position is created
        incr.positions.id(pf)
        pos$id <- get.positions.id(pf)
        #if(pos$id >=3) {browser()}
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }
      
      # if ( islong(pos) && islong(tempPos) && (tradeParms$pyramidFlag == TRUE) ){
      #   pod <- pos + tempPos
      #   add.trxns.position(pf,tempPos,type = 'OPEN')
      # }
      pos <- calcLimits(pos,parms,dataset,i)
      #print(sprintf("Bar index: %s, pb: %f",index(dataset[i,]),pos$pbPrice))
    } #End of Long positions For loop
  }
  
  
  if (parms$shortTrades) {
    pos <- position(instr)
    doAct <- TRUE
    for (i in 1:maxR){
      actFlag <- FALSE
      if ( isopen(pos) ){
        pos$barCount <- pos$barCount + 1
        pos <- shortX(pos,parms,dataset,i,...)
        add.table.positions(pf,bar = dataset[i,],pos)
        
        ## ---- If position is closed, check if it is for Profit booking ----
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
      if (as.Date(index(dataset[i,])) >= "2022-02-15") {browser()}
      tempPos <- shortE(parms,dataset,i,...)
      if ( (!isopen(pos)) && isshort(tempPos) && (doAct) ){
        pos <- tempPos
        #Increase the id when position is created
        incr.positions.id(pf)
        pos$id <- get.positions.id(pf)
        #if(pos$id >=3) {browser()}
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }
      
      # if ( islong(pos) && islong(tempPos) && (tradeParms$pyramidFlag == TRUE) ){
      #   pod <- pos + tempPos
      #   add.trxns.position(pf,tempPos,type = 'OPEN')
      # }
      pos <- calcLimits(pos,parms,dataset,i)
      print(sprintf("Bar index: %s, pb: %f",index(dataset[i,]),pos$pbPrice))
    } #End of Long positions For loop
  }

}

