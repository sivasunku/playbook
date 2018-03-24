## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----Environment set-up-------------------------------------------------------
library(rulesNtrades)
library(dataNindicators)
clean.all.portfolios()
rm(list =ls())
load("data/temp.rda")

source("R/longs.R")
source("R/shorts.R")
source("R/graph.R")

## ----Prepare the data----------------------------------------------------n
n<- 9
#data("SBIN")
#d <- SBIN["2016-03::2016-05"]
d <- NIFTY["2016-05-02::2016-05-09"]
d  <- to.minutes5(d,indexAt = 'startof')
colnames(d) <- c("Open","High","Low","Close","Volume")

# Calculate 
#   hema, lema - High & Low EMAs
#   length of each candle, length of two candles, difference between hema - lema
#   elder ray with respect to hema
#   elder ray with respect to lema
res <- d
res$hema      <- EMA(res$High,n = n)
res$lema      <- EMA(res$Low,n=n)
res$diffema  <- res$hema - res$lema
res$length    <- res$High - res$Low
res$length2   <- rollapply(res,
                           width = 2,
                           FUN = function(x){max(x$High) - min(x$Low)},
                           by.column = FALSE,
                           align = "right")

#
d1 <- elderRayM(res,res$hema)
colnames(d1) <- c("hemaHi","hemaLo")
d1$hemaHi <- ifelse(d1$hemaHi > 0,d1$hemaHi,0)
d1$hemaLo <- ifelse(d1$hemaLo > 0,0,d1$hemaLo)
d1 <- abs(d1)
d1$hemaHiSum <- rollapply(d1$hemaHi,
                          width = n,
                          FUN=sum,
                          by.column = FALSE,align="right")
                          
d1$hemaLoSum <- rollapply(d1$hemaLo,
                          width = n,
                          FUN=sum,
                          by.column = FALSE,align="right")

##
d2 <- elderRayM(res,res$lema)
colnames(d2) <- c("lemaHi","lemaLo")
d2$lemaHi <- ifelse(d2$lemaHi > 0,d2$lemaHi,0)
d2$lemaLo <- ifelse(d2$lemaLo > 0,0,d2$lemaLo)
d2 <- abs(d2)

d2$lemaHiSum <- rollapply(d2$lemaHi,
                          width = n,
                          FUN=sum,
                          by.column = FALSE,align="right")

d2$lemaLoSum <- rollapply(d2$lemaLo,
                          width = n,
                          FUN=sum,
                          by.column = FALSE,align="right")

res <- merge.xts(res,d1,d2)
res <- na.trim(res)
res <- round(res,digits = 2)
# Cal
d1 <- flow(d)

d2 <- rollapply(d1,
                width = myWidth,
                FUN = flowDex,
                fFUN = sum,
                by.column = FALSE,
                align = "right")
res <- merge.xts(d1,d2)
res <- merge.xts(d,res,join = "inner")
res <- na.trim(res)

## ----Set all the parameters     ----------------------------------------
parms <- tradeParms()
parms$longTrades  <- TRUE
parms$shortTrades <- TRUE
parms$sameDayFlag <- TRUE

parms$instrument <- "NIFTY"
parms$qty        <- 300
#
parms$pctFlag    <- FALSE
#
parms$slpFlag    <- FALSE
parms$slpAmt     <- 9
# 
parms$pbFlag     <- FALSE
parms$pbAmt      <- 15
parms$pbQty      <- 3000
parms$pbRunQty   <- 3000
#
parms$trlFlag     <- FALSE
parms$trlInitAmt  <- 12
parms$trlIncrAmt  <- 6
parms$trlSlpAmt   <- NA

parms$intraday    <- TRUE
parms$idStartTime <- "09:30"
parms$idEndTime   <- "15:15"


## ----Create the default portfolio----------------------------------------
#if( exists("pf") && is.portfolio(pf)) {delete.portfolio(pf = "default") }
pf <- "default"
delete.portfolio()
pf <- portfolio(pf = "default")

## ----backtest the strategies---------------------------------------------
options(warn = 2)

backtest(pf,res,parms,
         longE = longEntry,
         longX = longExit,
         shortE = shortEntry,
         shortX = shortExit,
         calcLimits = calcLimits
)
## ----Get the trades  ----------------------------------------------------
t  <- get.trades(pf)
st <- t[t$direction == "SHORT",]
lt <- t[t$direction == "LONG",]
wt <- t[t$netP > 0,]
loosingT <- t[t$netP <0,]

## ----Get the stats   ----------------------------------------------------
tStats  <- stats.trades(t)
stStats <- stats.trades(st)
ltStats <- stats.trades(lt)
cbind(tStats,stStats,ltStats)
t %>% group_by(direction,openReason,closeReason) %>% summarise(Profit = sum(netP))

## ----make blotter  ----------------------------------------------------
make.blotter(pf)

## ----Graph the trades  ----------------------------------------------------
#myGraph(pf,prices = res[,1:4],file = "temp4.pdf",by = "quarters")

myGraph(res,
        #file = "temp5.pdf",
        by = "days")
