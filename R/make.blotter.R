#' make.blotter - Add all the transactions to a blotter package
#' 
#' This will add the transactions in a given rulesNtrades portfolio to blotter package
#' 
#' @author Siva Sunku
#' @keywords graph,plot
#' @note
#' 
#'


# --- rm commands for blotter environment ----------------
# rm(list=ls(envir=.blotter), envir = .blotter) 
# rm("portfolio.CUD",pos=.blotter)
# rm("account.CUD",pos=.blotter)
# rm("order_book.CUD",pos=.strategy)


# --- assign commands -------------------------------------
make.blotter <- function(in.port, 
                         out.port = "temp.blot",
                         cur = "USD",
                         symbol = "SBIN",
                         initEq = 100000,
                         initDate='1950-12-31',
                         verboseFlag = TRUE,
                         forceClose = FALSE){
  
  currency(cur)
  if( is.portfolio(out.port) ) {
    if ( forceClose == TRUE){ 
      rm(list=ls(envir=.blotter), envir = .blotter)
      rm(list=ls(envir=.strategy), envir = .strategy)
    } else {
      tempStr <- sprintf("%s - portfolio is already present. Delete it before proceeding",out.port)
      stop(tempStr)
    }
  }
  
  stock(symbol, currency=cur, multiplier=1)
  initPortf(out.port, symbols=symbol, initDate=initDate)
  initAcct(out.port, portfolios=out.port, initDate=initDate)
  initOrders(portfolio=out.port, initDate=initDate)
  initDate='1950-12-31'
  tx <- get.trxns(in.port)
  #Check if apply function can be used. They are losing the consistency
  for ( i in 1:nrow(tx) ){
    x <- tx[i,]
    addTxn(out.port,
           Symbol    = x$symbols,
           TxnDate   = x$date,
           TxnPrice  = x$price,
           TxnQty    = ifelse(x$type == "SHORT",-1 * x$qty, x$qty),
           TxnFees   = -1 * x$fees,
           verbose  = verboseFlag
           )
    print(i)
  }
  #apply(tx,MARGIN = 1,FUN=addTxn.blotter,out.port)
  #addTxn(out.port, Symbol='SBIN',TxnDate='2016-03-21', TxnPrice=196.75, TxnQty = -3000,TxnFees=0,verbose=TRUE)
  return(out.port)
}


