library(Quandl)
library(dplyr)
#Quandl.api_key("TyiXJXHTBB9SG2-GZPvC")
## --- Get Diwali days -----------------------------------------------------------------
d <- read.csv("diwaliDates",header = FALSE,stringsAsFactors = FALSE)
d <- strptime(d[,1], format = "%d %B %Y")[1:18]
d <- as.Date(d)

## --- Get goldbees pric----------------------------------------------------------------
g <- Quandl("NSE/GOLDBEES",collapse="daily",start_date="2005-01-01",type="raw")
g <- xts(g[c("Open","High","Low","Close")],order.by = g$Date)

## --- Get prevN data of each diwali day -----------------------------------------------
prevN <- 60

subDays  <- lapply(d,FUN = function(x){return( (x-prevN):x) })
subDays  <- as.Date(unlist(subDays))
dummyXts <- xts(order.by = subDays)
res <- g[index(dummyXts)]

#Make High,Low,Close for each year
res <- to.yearly(res)
colnames(res) <- colnames(g)

#Relative Close is the point, where the price closed w.r.t. High/Low. 
#If it is close to 0, price closed near lows. 
#If it is close to 1, price closed near highs
res$relCl <- (res$Close - res$Low) / (res$High - res$Low)
res$relCl <- round(res$relCl, digits = 2)
res


