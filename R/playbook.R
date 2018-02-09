
## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
library(knitr)
library(rulesNtrades)
library(evalStrats)
source("R/EntryExits.R")
source("R/graph.R")
## ----Prepare the data----------------------------------------------------
myWidth <- 9
data("SBIN")
d1 <- flow(SBIN)

d2 <- rollapply(d1,
                width = myWidth,
                FUN = flowDex,
                fFUN = sum,
                by.column = FALSE,
                align = "right")
res <- merge.xts(d1,d2)
res <- merge.xts(SBIN,res,join = "inner")
res <- na.trim(res)

## ----Set all the parameters     ----------------------------------------
parms <- tradeParms()
parms$instrument <- "SBIN"
parms$qty <- 6000
parms$pbQty <- 6000

parms$pbFlag <- TRUE
parms$pbAmt <- 6.0

parms$slpFlag <- TRUE
parms$slpAmt <- 3.0


## ----Create the default portfolio----------------------------------------
if( exists("pf") && is.portfolio(pf)) {delete.portfolio(pf) }
pf <- portfolio()


## ----backtest the strategies---------------------------------------------
backtest(pf,res,parms,
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

## ----Graph the trades  ----------------------------------------------------
myGraph(pf,prices = SBIN[1:90,],file = "temp3.pdf")
