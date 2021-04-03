#
# Constants
#

# Yahoo Symbols
YAHOOSYMBOLS <-
  data.frame(
    symbol = c("^GSPC", "^IXIC", "^RUT", "^N225", "^GDAXI", "CL=F", "GC=F"),
    symbolname = c(
      "S&P 500",
      "NASDAQ",
      "RUSSELL 2000",
      "NIKKEI 225",
      "DAX",
      "CRUDE OIL",
      "GOLD"
    )
  )
MAXASSETS <- 7
MINDATAROWS <- 4

# Scaling of financial data
SCALING <- 252

# stock dates
DATASTARTDATE <- "1999-12-31"

#
# Functions
#
# Estimator for fPortfolio/Covariance
muest <- NA
covmatest <- NA
myEstimator <- function (x, spec = NULL, ...)
  list(mu = muest, Sigma = covmatest)
# list(mu = colMeans(x), Sigma = cov(x))
