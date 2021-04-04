---
title: "Help"
author: "Michael Schoenhart"
date: "20. Juni 2015"
output: html_document
---

# Portfolio Optimizer

### Version 0.5.0

Use 'Datafeeds' to load asset prices from file or collect them from yahoo finance.  
Use sliders for selecting and limiting portfolio weights for optimization.  
All charts and tables are interactive and will immediately react to your input.

----

## Black-Litterman

### Implied Returns

The Black-Litterman model uses "equilibrium" returns as a neutral starting point.  
If you "reset returns to adjusted Black-Litterman values" in the "Estimates" tab panel and then optimize for "maximum Sharpe" on the left sidebar panel you will roughly get equal weighting as the result.

----

## Optimization

### List of Available Methods

* Equally Weighted
* Maximum Sharpe
    + use your own estimates for expected returns
    + use your own estimates for covariance
* Minimum Variance
    + use your own estimates for expected returns
    + use your own estimates for covariance
* Equally Risk Contributed
* Most Diversified
* Minimum Tail Dependent
* Minimimum Average Draw Down
* Minimimum Maximum Draw Down
* Maximum-Sharpe Portfolio
    + with weights constraints
    + use your own estimates for expected returns
    + use your own estimates for covariance
* Minimum-Variance
    + with weights constraints
    + use your own estimates for expected returns
    + use your own estimates for covariance

----

## Statistics

### Available Performance and Risk Measures

* Average Return p.a.
* Standard Deviation p.a.%
* Sharpe Ratio (rf=0) p.a.
* Shortfall Probability (1 year)
* Value at Risk 95% Delta-Normal p.a.%
* Value at Risk 95% Cornish-Fisher p.a.%
* Value at Risk 95% Historical p.a.%
* Value at Risk 99% Delta-Normal p.a.%
* Value at Risk 99% Cornish-Fisher p.a.%
* Value at Risk 99% Historical p.a.%
* Positive Returns%
* Maximum Drawdown%
* Skewness
* Kurtosis
* Autocorrelation Lag 1
* Geltner Adj. Average Return p.a.%
* Geltner Adj. Standard Deviation p.a.%
* Black-Litterman Implied Expected Return p.a.%
* Adj. Black-Litterman Implied Expected Return p.a.%

----

[Contact ![](www/smartcube.gif) for further information.](http://www.smartcube.at/contact.html)
