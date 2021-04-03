#' Parse and merge historical stock prices from yahoo.com by using quantmod getSymbols
#' Example:
#'   data <- getQuotesYahoo(
#'             c("^GSPC", "^IXIC", "VBMFX", "^N225", "^GDAXI", "CL=F", "^XAU"), "2021/1/1",
#'               symbolname = c("S&P 500","NASDAQ","Vanguard Total Bond Market Index","NIKKEI 225","DAX","CRUDE OIL","GOLD"))
#'
#' References:
#' https://stackoverflow.com/questions/38240389/merge-xts-objects-with-different-time-intervals
#' https://cran.r-project.org/web/packages/quantmod/quantmod.pdf

# library(quantmod)
getQuotesYahoo <-
  function(symbols = c("^GSPC", "^XAU"),
           startdate = "2021/1/1",
           enddate = Sys.Date(),
           symbolnames = NULL)
  {
    # use for loop for merge to preserve the order of symbolnames vs
    # combine the adjusted close values in one (xts) object via
    # ts <- do.call(merge, eapply(envstockdata, Ad))
    ts <- xts()
    for (s in symbols) { 
      # quantmod getSymbols
      data <- getSymbols(
        s,
        #env = envstockdata,
        src = "yahoo",
        auto.assign = F,
        from = startdate,
        to = enddate
      )
      # merge all the security quotes together at the right dates
      ts <- merge(ts, data[, ncol(data)], all = T)
    }
    
    # optionally use your own symbol names
    if (missing(symbolnames))
      names(ts) <- gsub(".Adjusted", "", names(ts))
    else
      names(ts) <- symbolnames
    
    return(ts)
  }
