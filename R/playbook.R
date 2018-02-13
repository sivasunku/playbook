## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
rm(list =ls())
delete.portfolio()
library(knitr)
library(rulesNtrades)
library(evalStrats)
source("R/EntryExits.R")
source("R/graph.R")
source("R/extraFunctions.R")
source("R/mybacktest.R")
## ----Prepare the data----------------------------------------------------
myWidth <- 9
data("SBIN")
d <- SBIN["2016-02::2016-06"]
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
parms$shortTrades <- FALSE

parms$instrument <- "SBIN"
parms$qty <- 6000
parms$pbQty <- 3000

parms$pbFlag <- TRUE
parms$pbAmt <- 12
# # 
parms$slpFlag <- FALSE
parms$slpAmt <- 12
# 
parms$trlFlag <- FALSE
parms$trlAmt <- 6
parms$trlSlpAmt <- 6


## ----Create the default portfolio----------------------------------------
if( exists("pf") && is.portfolio(pf)) {delete.portfolio(pf = "default") }
pf <- portfolio()


## ----backtest the strategies---------------------------------------------
options(warn = 2)
mybacktest(pf,res,parms,
         longE = longEntry,
         longX = longExit,
         shortE = shortEntry,
         shortX = shortExit,
         calcLimits = myCalcLimits
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

## ----Graph the trades  ----------------------------------------------------
#myGraph(pf,prices = res[,1:4],file = "temp4.pdf",by = "quarters")
myGraph(pf,
        prices = res[,1:4],
        parms = parms,
        by = "quarters")
