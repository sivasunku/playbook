myChartSeries <- function (x, type = c("auto", "candlesticks", "matchsticks", 
                      "bars", "line"), subset = NULL, show.grid = TRUE, name = NULL, 
          time.scale = NULL, log.scale = FALSE, TA = "addVo()", TAsep = ";", 
          line.type = "l", bar.type = "ohlc", theme = chartTheme("black"), 
          layout = NA, major.ticks = "auto", minor.ticks = TRUE, yrange = NULL, 
          plot = TRUE, up.col, dn.col, color.vol = TRUE, multi.col = FALSE, 
          ...) 
{
  x <- try.xts(x, error = "chartSeries requires an xtsible object")
  x <- na.omit(x)
  indexClass(x) <- "POSIXct"
  if (!is.null(subset) & is.character(subset)) {
    if (strsplit(subset, " ")[[1]][1] %in% c("first", "last")) {
      subsetvec <- strsplit(subset, " ")[[1]]
      if (length(subsetvec) < 3) {
        subset.n <- ifelse(length(subsetvec) == 1, 1L, 
                           as.numeric(subsetvec[2]))
      }
      else {
        subset.n <- paste(subsetvec[2:3], collapse = " ")
      }
      sub.index <- index(do.call(subsetvec[1], list(x, 
                                                    subset.n)))
      xsubset <- which(index(x) %in% sub.index)
    }
    else xsubset <- which(index(x) %in% index(x[subset]))
  }
  else xsubset <- 1:NROW(x)
  xdata <- x
  x <- x[xsubset]
  if (is.OHLC(x)) {
    Opens <- as.numeric(Op(x))
    Highs <- as.numeric(Hi(x))
    Lows <- as.numeric(Lo(x))
    Closes <- as.numeric(Cl(x))
  }
  else {
    Lows <- min(x[, 1])
    Highs <- max(x[, 1])
    Closes <- as.numeric(x[, 1])
    type <- "line"
    color.vol <- FALSE
  }
  if (has.Vo(x)) {
    Volumes <- as.numeric(Vo(x))
    show.vol <- TRUE
  }
  else show.vol <- FALSE
  if (is.null(time.scale)) {
    time.scale <- periodicity(x)$scale
  }
  if (is.character(theme)) 
    theme <- chartTheme(theme)
  if (!missing(up.col)) 
    theme$up.col <- up.col
  if (!missing(dn.col)) 
    theme$dn.col <- dn.col
  if (missing(multi.col) | !multi.col) {
    multi.col <- FALSE
    theme$dn.up.col <- theme$up.col
    theme$up.up.col <- theme$up.col
    theme$dn.dn.col <- theme$dn.col
    theme$up.dn.col <- theme$dn.col
  }
  else {
    if (is.character(multi.col)) {
      theme$dn.up.col <- multi.col[1]
      theme$up.up.col <- multi.col[2]
      theme$dn.dn.col <- multi.col[3]
      theme$up.dn.col <- multi.col[4]
    }
    theme$up.col <- theme$up.up.col
    theme$dn.col <- theme$dn.dn.col
    multi.col <- TRUE
  }
  chart.options <- c("auto", "candlesticks", "matchsticks", 
                     "line", "bars")
  chart <- chart.options[pmatch(type, chart.options)]
  if (chart[1] == "auto") {
    chart <- ifelse(NROW(x) > 300, "matchsticks", "candlesticks")
  }
  if (chart[1] == "candlesticks") {
    spacing <- 3
    width <- 3
  }
  else if (chart[1] == "matchsticks" || chart[1] == "line") {
    spacing <- 1
    width <- 1
  }
  else if (chart[1] == "bars") {
    spacing <- 4
    width <- 3
    if (NROW(x) > 60) 
      width <- 1
  }
  ep <- axTicksByTime(x, major.ticks)
  x.labels <- names(ep)
  chob <- new("chob")
  chob@call <- match.call(expand.dots = TRUE)
  if (is.null(name)) 
    name <- as.character(match.call()$x)
  chob@xdata <- xdata
  chob@xsubset <- xsubset
  chob@name <- name
  chob@type <- chart[1]
  chob@xrange <- c(1, NROW(x))
  if (is.OHLC(x)) {
    chob@yrange <- c(min(Lo(x), na.rm = TRUE), max(Hi(x), 
                                                   na.rm = TRUE))
  }
  else chob@yrange <- range(x[, 1], na.rm = TRUE)
  
  if (!is.null(yrange) && length(yrange) == 2) 
    chob@yrange <- yrange
  chob@log.scale <- log.scale
  chob@color.vol <- color.vol
  chob@multi.col <- multi.col
  chob@show.vol <- show.vol
  chob@bar.type <- bar.type
  chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- ep
  chob@x.labels <- x.labels
  chob@colors <- theme
  chob@layout <- layout
  chob@time.scale <- time.scale
  chob@minor.ticks <- minor.ticks
  chob@major.ticks <- major.ticks
  chob@show.grid <- show.grid
  chob@length <- NROW(x)
  chob@passed.args <- as.list(match.call(expand.dots = TRUE)[-1])
  if (!is.null(TA)) {
    thisEnv <- environment()
    if (is.character(TA)) 
      TA <- as.list(strsplit(TA, TAsep)[[1]])
    chob@passed.args$TA <- list()
    for (ta in 1:length(TA)) {
      if (is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text = TA[[ta]]), 
                                          envir = thisEnv)
      }
      else chob@passed.args$TA[[ta]] <- eval(TA[[ta]], 
                                             envir = thisEnv)
    }
    poss.new <- sapply(chob@passed.args$TA, function(x) {
      if (isS4(x) && is(x, "chobTA")) 
        return(x@new)
      stop("improper TA argument/call in chartSeries", 
           call. = FALSE)
    })
    if (length(poss.new) > 0) 
      poss.new <- which(poss.new)
    chob@windows <- length(poss.new) + 1
    chob@passed.args$show.vol <- any(sapply(chob@passed.args$TA, 
                                            function(x) x@name == "chartVo"))
  }
  else chob@windows <- 1
  chob@passed.args$TA <- sapply(chob@passed.args$TA, function(x) {
    eval(x@call)
  })
  if (plot) 
    do.call("chartSeries.chob", list(chob))
  chob@device <- as.numeric(dev.cur())
  write.chob(chob, chob@device)
  invisible(chob)
}
<bytecode: 0x000000001946dc58>
  <environment: namespace:quantmod>