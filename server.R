#' Known Issues and Room for further Improvement:
#' - monte carlo sim: known issues with constraints
#' - preferring xts over zoo https://stackoverflow.com/questions/39777746/sapply-is-successful-for-zoo-object-but-not-xts-object-why
#' - debugging w. assign("r", returnData(), envir = .GlobalEnv)
#' - scraping symbol longnames from yahoo finance
#' - Multiple Plots https://gist.github.com/wch/5436415/

#
# packages
#
library(quantmod)
library(timeDate)
library(timeSeries)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(PerformanceAnalytics)
library(fPortfolio)
library(FRAPO)
library(corrplot)
library(rmarkdown)

#
# external functions for scraping
#
source("getQuotesYahoo.R")

#
# shiny server
#
shinyServer(function(input, output, session) {
  #
  # internal data structure
  #
  rv <-
    reactiveValues() # save reactive data like symbol names and price data
  rv$scaling <- SCALING
  rv$corrmat <- data.frame(NoAssets = NA)
  rv$rettable <- data.frame(NoAssets = NA)
  rv$data <- xts()
  #
  # init first price data
  #
  observe({
    isolate({
      # intitial loading of data
      cat("Loading Financial Data...\n")
      rv$data <-
        # delete negative prices, delete rows with all NAs, and locf
        cleandata(
          getQuotesYahoo(
            YAHOOSYMBOLS$symbol,
            DATASTARTDATE,
            symbolnames = YAHOOSYMBOLS$symbolname
          )
        )
      updatecheckbox()
      calchistval()
    })
  })
  #
  # reactive data
  #
  idxselected <- reactive({
    idx <- c(
      1 %in% input$aselect,
      2 %in% input$aselect,
      3 %in% input$aselect,
      4 %in% input$aselect,
      5 %in% input$aselect,
      6 %in% input$aselect,
      7 %in% input$aselect
    )
    return(idx[1:ncol(rv$data)])
  })
  # normalized weights
  normweights <- reactive({
    w <- c(
      input$slider1[2],
      input$slider2[2],
      input$slider3[2],
      input$slider4[2],
      input$slider5[2],
      input$slider6[2],
      input$slider7[2]
    )
    w <- w[1:ncol(rv$data)]
    w[!idxselected()] <- NA
    return(w / sum(w, na.rm = T))
  })
  # portfolio weights
  portweights <- reactive({
    w <- normweights()
    w <- w[!is.na(w)]
    if (length(w) == 0)
      w <- rep(0, ncol(priceData()))
    return(w)
  })
  # prices
  priceData <- reactive({
    # filter for selection and sample date
    if (length(rv$data) > 0) {
      n <- nrow(rv$data)
      srange <- input$dsamplerange / 100
      idxrows <- seq(floor(n * srange[1]) + 1, floor(n * srange[2]))
      data <- rv$data[idxrows, idxselected(), drop = F]
      if (nrow(data) < MINDATAROWS) {
        # min x returns (eq x+1 raw data) for further calcs
        data <- xts()
      }
    }
    else
      data <- xts()
    return(data)
  })
  # returns
  returnData <- reactive({
    #calc simple returns and portfolio
    if (length(priceData()) > 0) {
      #simple returns
      data <- simpleret(priceData())
      w <- portweights()
      # calc portfolio w simple returns
      Portfolio <- xts(rowSums(t(t(data) * w)), time(data))
      data <- merge(Portfolio, data)
    }
    else
      data <- xts()
    return(data)
  })
  # update stock input label, field values and checkboxgroupinput
  observe({
    symbols <- colnames(rv$data) # unfiltered
    w <- normweights() * 100
    updateTextInput(session,
                    "slider1",
                    label = nameperc(1, symbols[1], w[1], input$slider1[1], input$slider1[2]))
    updateTextInput(session,
                    "slider2",
                    label = nameperc(2, symbols[2], w[2], input$slider2[1], input$slider2[2]))
    updateTextInput(session,
                    "slider3",
                    label = nameperc(3, symbols[3], w[3], input$slider3[1], input$slider3[2]))
    updateTextInput(session,
                    "slider4",
                    label = nameperc(4, symbols[4], w[4], input$slider4[1], input$slider4[2]))
    updateTextInput(session,
                    "slider5",
                    label = nameperc(5, symbols[5], w[5], input$slider5[1], input$slider5[2]))
    updateTextInput(session,
                    "slider6",
                    label = nameperc(6, symbols[6], w[6], input$slider6[1], input$slider6[2]))
    updateTextInput(session,
                    "slider7",
                    label = nameperc(7, symbols[7], w[7], input$slider7[1], input$slider7[2]))
  })
  #
  # Action Buttons
  #
  # Reset Weights
  observeEvent(input$resweightsbutton, {
    equalw <- 100 / ncol(rv$data)
    updateSliderInput(session, "slider1", value = c(0, equalw))
    updateSliderInput(session, "slider2", value = c(0, equalw))
    updateSliderInput(session, "slider3", value = c(0, equalw))
    updateSliderInput(session, "slider4", value = c(0, equalw))
    updateSliderInput(session, "slider5", value = c(0, equalw))
    updateSliderInput(session, "slider6", value = c(0, equalw))
    updateSliderInput(session, "slider7", value = c(0, equalw))
  })
  observeEvent(input$ressamplebutton, {
    updateSliderInput(session, "dsamplerange", value = c(0, 100))
    updateCheckboxGroupInput(session, "aselect", selected = as.character(1:7))
  })
  # Reset Yahoo Symbols
  observeEvent(input$residxbutton, {
    updateTextInput(session, "symb1", value = YAHOOSYMBOLS$symbolname[1])
    updateTextInput(session, "symb2", value = YAHOOSYMBOLS$symbolname[2])
    updateTextInput(session, "symb3", value = YAHOOSYMBOLS$symbolname[3])
    updateTextInput(session, "symb4", value = YAHOOSYMBOLS$symbolname[4])
    updateTextInput(session, "symb5", value = YAHOOSYMBOLS$symbolname[5])
    updateTextInput(session, "symb6", value = YAHOOSYMBOLS$symbolname[6])
    updateTextInput(session, "symb7", value = YAHOOSYMBOLS$symbolname[7])
  })
  # Get Yahoo Prices
  observeEvent(input$getyahoobutton, {
    if (as.Date(input$dates[1]) < as.Date(input$dates[2]))
      # End date must be greater than start date
    {
      getyahooprices()
    }
  })
  # recalc corr matrix if selection changes
  observeEvent(input$aselect, {
    calchistval()
  })
  # Correlations to Historical
  observeEvent(input$corrhistbutton, {
    calchistval(correlations = T,
                returns = F,
                scaling = F)
  })
  # Correlations Stress
  observeEvent(input$corrstressbutton, {
    rv$corrmat[] <- 1
  })
  # Returns to Historical
  observeEvent(input$rethistbutton, {
    calchistval(correlations = F,
                returns = T,
                scaling = F)
  })
  # Returns to Black-Litterman
  observeEvent(input$retblbutton, {
    data <- returnData()[,-1, drop = F] # w/o portfolio column
    w <- portweights()
    rmean <- 100 * ((colMeans(data) + 1) ^ rv$scaling - 1)
    blimpliedret <- 100 *  blimplied(data, w, rv$scaling)
    bladjret <-  blimpliedret * (rmean[1] / blimpliedret[1])
    rv$rettable[, 1] <- bladjret[-1]
  })
  # Action Button for Optimization
  observeEvent(input$optportbutton, {
    if (ncol(returnData()) > 2)
      optimizeportfolio()
  })
  # Select Box for Optimization
  observeEvent(input$optportselbox, {
    if (ncol(returnData()) > 2)
      optimizeportfolio()
  })
  #
  # file handling
  #
  # load data
  observeEvent(input$loadfile$datapath, {
    if (!is.null(input$loadfile)) {
      loadfile(input$loadfile$datapath)
    }
  })
  # save data
  output$savedata <- downloadHandler(
    filename = function()
      paste0(
        "smartAssetAllocation-",
        format(Sys.time(), "%F %H-%M"),
        ".csv"
      ),
    content = function(file)
      write.csv2(data.frame(priceData()), file)
  )
  #
  # output
  #
  # Generate a comparison plot of the data
  output$chartplot <- renderDygraph({
    if (length(returnData()) > 0) {
      # not empty timeseries
      data <- cumprod(1 + returnData()) * 100
      dygraph(data) %>%
        dySeries("Portfolio", strokeWidth = 5) %>%
        dyRangeSelector() %>%
        dyAxis("y", valueRange = c(min(data), max(data) + 50)) %>%
        dyLegend(show = "always", labelsSeparateLines = T) %>%
        dyOptions(
          fillGraph = T,
          fillAlpha = 0.1,
          strokeWidth = 2
        ) %>%
        dyHighlight(
          highlightCircleSize = 4,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut =
            T,
          highlightSeriesOpts = list(strokeWidth = 4)
        )
      #autoplot(data, facets = NULL, xlab="",ylab="Index")+geom_line(size=1)
      #qplot(Index, Value, data = df, geom="line",group = Series, color = Series )#+geom_line(size=1)
      #df <- fortify.zoo(data, melt = T)
      #ggplot(df,aes(x=Index,y=Value,group=Series,color=Series))+geom_line()+xlab("")+ylab("Index")
    }
  })
  output$distplot <- renderPlot({
    if (length(returnData()) > 0) {
      # not empty timeseries
      #hist(returnData()[,1],main="Histogram of Portfolio Returns",xlab="Portfolio Returns")
      #chart.Histogram(returnData(),methods=c("add.normal","add.rug"))
      suppressMessages(x <-
                         melt(
                           data.frame(log(1 + returnData())),
                           variable.name = "Asset",
                           value.name = "Return"
                         ))
      plot1 <-
        ggplot(x, aes(Return, fill = Asset)) + geom_density(alpha = 0.3) + theme(legend.position =
                                                                                   "left")
      plot2 <-
        ggplot(x, aes(reorder(Asset, Return, mean), Return)) + geom_boxplot() + xlab("") +
        coord_flip()
      grid.arrange(plot1, plot2)
    }
  })
  output$ddplot <- renderPlot({
    if (length(returnData()) > 0) {
      # not empty timeseries
      charts.PerformanceSummary(
        returnData(),
        main = "Performance Comparison",
        geometric = T,
        wealth.index =
          T,
        lwd = c(3, 1, 1, 1, 1),
        colorset = tim6equal
      )
    }
  })
  output$efplot <- renderPlot({
    if (length(returnData()) > 0) {
      # not empty timeseries
      #chart.RiskReturnScatter(data,scale=252,geometric=T)
      layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = T),
             heights = c(2, 1),
             widths = c(1, 2))
      #mvplot
      fpo <- NULL
      try(fpo <-
            portfolioFrontier(as.timeSeries(log(1 + returnData()))), silent = T)
      if (!is.null(fpo))
        tailoredFrontierPlot(fpo)
      #pieplot
      w <- portweights()
      pieplot(w, "Portfolio Weights", colnames(priceData()), F)
      #weightsPlot
      fpo <- NULL
      try(fpo <-
            portfolioFrontier(as.timeSeries(log(1 + returnData()[,-1, drop =
                                                                   F]))), silent = T)
      if (!is.null(fpo))
        weightsPlot(fpo)
    }
  })
  output$blplot <- renderPlot({
    if (length(returnData()) > 0) {
      # not empty timeseries
      data <- returnData()[,-1, drop = F] # w/o portfolio column
      w <- portweights()
      rmean <- 100 * ((colMeans(data) + 1) ^ rv$scaling - 1)
      blimpliedret <- 100 *  blimplied(data, w, rv$scaling)
      bladjret <- blimpliedret * (rmean[1] / blimpliedret[1])
      layout(matrix(c(1, 2), 2, 1, byrow = T),
             heights = c(1.3, 1),
             widths = 1)
      col <- seqPalette(length(w), "Blues")
      cex <- 1.2
      barplot(
        rbind(rmean, bladjret[-1]),
        names.arg = colnames(data),
        beside = T,
        col = col[2:3],
        main = "Historical Average Returns vs Black-Litterman Adj. Implied Expected Returns p.a."
      )
      legend(
        "topleft",
        legend = c("Historical Average Return", "BL-Implied Expected Return"),
        bty =
          "n",
        cex = cex,
        fill = col[2:3]
      )
      #pieplot
      pieplot(w, "Portfolio Weights", colnames(priceData()))
    }
  })
  output$perftable <- renderDataTable({
    if (length(returnData()) > 0) {
      # not empty timeseries
      withProgress(message = 'Processing financial data', value = 1, {
        x <-
          to.period(cumprod(1 + returnData()),
                    period = "months",
                    OHLC = F) #calc monthly simple returns
        x <- rbind(x[1,] - 1, simpleret(x))
        
        datatable(
          t(table.CalendarReturns(
            x, digits = 2, geometric = T
          )),
          extensions = c("Buttons", "FixedColumns", "Scroller"),
          options = list(
            searching = F,
            # FixedColumns
            scrollX = T,
            fixedColumns = list(leftColumns = 1),
            # Scroller
            deferRender = T,
            scrollY = 395,
            scroller = T,
            # Buttons
            dom = 'Bfrtip',
            buttons = list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            )
          )
        )
      })
    }
  })#, options = list(pageLength = 20))
  # Generate a summary of the data
  output$statstable <- renderDataTable({
    if (length(returnData()) > 0) {
      # not empty timeseries
      #table.Stats(returnData())
      data <- returnData()
      rmean <- 100 * ((colMeans(data) + 1) ^ rv$scaling - 1)
      rstd <- 100 * colStdevs(data) * sqrt(rv$scaling)
      shortfall <- rmean / rstd
      acorr1 <-
        apply(data, 2, function(x)
          acf(as.numeric(x), lag.max = 1, plot = F)[1][[1]])
      datag <- Return.Geltner(data)[2:nrow(data),]
      rmeang <- 100 * ((colMeans(datag) + 1) ^ rv$scaling - 1)
      rstdg <- 100 * colSds(datag) * sqrt(rv$scaling)
      w <- portweights()
      blimpliedret <-
        100 * blimplied(data[,-1, drop = F], w, rv$scaling)
      bladjret <- blimpliedret * (rmean[1] / blimpliedret[1])
      stats <-
        t(round(
          data.frame(
            rep(nrow(data), ncol(data)),
            rmean,
            rstd,
            shortfall,
            100 * pnorm(shortfall, lower.tail = F),
            100 * t(VaR(
              data, p = 0.95, method = "gaussian"
            )),
            100 * t(VaR(
              data, p = 0.95, method = "modified"
            )),
            100 * t(VaR(
              data, p = 0.95, method = "historical"
            )),
            100 * t(VaR(
              data, p = 0.99, method = "gaussian"
            )),
            100 * t(VaR(
              data, p = 0.99, method = "modified"
            )),
            100 * t(VaR(
              data, p = 0.99, method = "historical"
            )),
            100 * colSums(data > 0) / nrow(data),
            100 *
              t(maxDrawdown(data)),
            colSkewness(data),
            3 + colKurtosis(data),
            acorr1,
            rmeang,
            rstdg,
            blimpliedret,
            bladjret
          ),
          2
        ))
      rownames(stats) <-
        c(
          "Observations",
          "Average Return p.a.%",
          "Standard Deviation p.a.%",
          "Sharpe Ratio (rf=0) p.a.",
          "Shortfall Probability (1 year)%",
          "Value@Risk 95% Delta-Normal p.a.%",
          "Value@Risk 95% Cornish-Fisher p.a.%",
          "Value@Risk 95% Historical p.a.%",
          "Value@Risk 99% Delta-Normal p.a.%",
          "Value@Risk 99% Cornish-Fisher p.a.%",
          "Value@Risk 99% Historical p.a.%",
          "Positive Returns%",
          "Maximum Drawdown%",
          "Skewness",
          "Kurtosis",
          "Autocorrelation Lag 1",
          "Geltner Adj. Average Return p.a.%",
          "Geltner Adj. Standard Deviation p.a.%",
          "Black-Litterman Implied Expected Return p.a.%",
          "Adj. Black-Litterman Implied Expected Return p.a.%"
        )
      
      datatable(
        stats,
        extensions = c("Buttons", "FixedColumns", "Scroller"),
        options = list(
          searching = F,
          # FixedColumns
          scrollX = T,
          fixedColumns = list(leftColumns = 1),
          # Scroller
          deferRender = T,
          scrollY = 662,
          scroller = T,
          # Buttons
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )
          )
        )
      )
    }
  })#, options = list(pageLength = 20))
  #
  # Estimates
  #
  # Correlations
  output$corrplot <- renderPlot({
    if ((length(returnData()) > 0) && (ncol(returnData()) > 2)) {
      # not empty timeseries
      #layout(matrix(c(1,2),2,1,byrow=T), heights = c(1, 1), widths = 1)
      col <-
        colorRampPalette(
          c(
            "#7F0000",
            "red",
            "#FF7F00",
            "yellow",
            "#7FFF7F",
            "cyan",
            "#007FFF",
            "blue",
            "#00007F"
          )
        )
      # correlation table adj
      df <- sapply(hot.to.df(input$corrtable), as.numeric)
      df[df > 1] <- 1
      df[df < (-1)] <- (-1)
      corrplot(df,
               method = "number",
               col = col(100),
               tl.col = "Black")
    }
  })
  # Correlations
  output$corrtable <- renderHotable({
    if ((length(returnData()) > 0) && (ncol(returnData()) > 1))
      data.frame(round(rv$corrmat, 2))
    else
      data.frame(NoAssets = NA)
  }, readOnly = F)
  output$retmat <- renderHotable({
    if ((length(returnData()) > 0) && (ncol(returnData()) > 1)) {
      d <- data.frame(rv$scaling, round(rv$rettable, 2))
      colnames(d) <- c("Scaling", "Mean", "StdDev")
      d
    } else
      data.frame(NoAssets = NA)
  }, readOnly = F)
  # Generate an HTML table view of the data
  output$returnstable <- renderDataTable({
    if (length(returnData()) > 0)
      # not empty timeseries
      datatable(
        data.frame(Date = time(returnData()), round(100 * coredata(returnData(
          
        )), 2)),
        extensions = c("Buttons", "FixedColumns", "Scroller"),
        options = list(
          searching = F,
          # FixedColumns
          scrollX = T,
          fixedColumns = list(leftColumns = 1),
          # Scroller
          deferRender = T,
          scrollY = 340,
          scroller = T,
          # Buttons
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )
          )
        )
      )
  })
  # Generate an HTML table view of the data
  output$rawtable <- renderDataTable({
    if (length(returnData()) > 0)
      # not empty timeseries
      datatable(
        data.frame(Date = time(priceData()), coredata(priceData())),
        extensions = c("Buttons", "FixedColumns", "Scroller"),
        options = list(
          searching = F,
          # FixedColumns
          scrollX = T,
          fixedColumns = list(leftColumns = 1),
          # Scroller
          deferRender = T,
          scrollY = 340,
          scroller = T,
          # Buttons
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )
          )
        )
      )
  })
  #
  # Report
  #
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$reportformat,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath("report.rmd")
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.rmd")
      out <- rmarkdown::render("report.rmd", switch(
        input$reportformat,
        PDF = pdf_document(),
        HTML = html_document(),
        Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  #
  # reactive functions
  #
  # update checkbox
  updatecheckbox <- function() {
    vals <- as.character(1:ncol(rv$data))
    updateCheckboxGroupInput(
      session,
      "aselect",
      choices = vals,
      selected = vals,
      inline = T
    )
    # reset sliders
    updateSliderInput(session, "slider1", value = c(0, 100))
    updateSliderInput(session, "slider2", value = c(0, 100))
    updateSliderInput(session, "slider3", value = c(0, 100))
    updateSliderInput(session, "slider4", value = c(0, 100))
    updateSliderInput(session, "slider5", value = c(0, 100))
    updateSliderInput(session, "slider6", value = c(0, 100))
    updateSliderInput(session, "slider7", value = c(0, 100))
    updateSliderInput(session, "dsamplerange", value = c(0, 100))
  }
  # load file
  loadfile <- function(loadfile) {
    data <- read.csv2(loadfile, header = T, row.names = 1)
    # convert dates
    d <- NULL
    try(d <- as.Date(row.names(data)), silent = T)
    if (is.null(d[1]))
      try(d <- as.Date(row.names(data), "%d.%m.%Y"), silent = T)
    if (!is.null(d[1])) {
      data <- xts(data, order.by = d)
      rv$data <- cleandata(data[, 1:min(MAXASSETS, ncol(data))])
      updatecheckbox()
      calchistval()
    }
  }
  # get prices from yahoo
  getyahooprices <- function() {
    symbols <-
      getsymbols(
        c(
          input$symb1,
          input$symb2,
          input$symb3,
          input$symb4,
          input$symb5,
          input$symb6,
          input$symb7
        )
      )
    updateTextInput(session, "symb1", label = symbols$name[1])
    updateTextInput(session, "symb2", label = symbols$name[2])
    updateTextInput(session, "symb3", label = symbols$name[3])
    updateTextInput(session, "symb4", label = symbols$name[4])
    updateTextInput(session, "symb5", label = symbols$name[5])
    updateTextInput(session, "symb6", label = symbols$name[6])
    updateTextInput(session, "symb7", label = symbols$name[7])
    
    
    rv$data <-
      # delete negative prices, delete rows with all NAs, and locf
      cleandata(getQuotesYahoo(
        c(
          input$symb1,
          input$symb2,
          input$symb3,
          input$symb4,
          input$symb5,
          input$symb6,
          input$symb7
        ),
        input$dates[1],
        input$dates[2]
      ))
    updatecheckbox()
    calchistval()
  }
  cleandata <- function(data) {
    # delete negative prices
    data[data < 0] <- NA
    # delete empty rows, i.e. delete rows with all NAs
    idx <- apply(data, 1, function(x)
      all(is.na(x)))
    data <- data[!idx,]
    # fill NAs with last obs carried forward
    data <-
      na.locf(data, na.rm = F)
    # next obs carried backward
    data <-
      na.locf(data, fromLast = T, na.rm = F)
    return(data)
  }
  calchistval <-
    function(correlations = T,
             returns = T,
             scaling = T) {
      if (ncol(returnData()) > 1) {
        data <- returnData()[,-1, drop = F]
        if (correlations)
          rv$corrmat <- cor(log(1 + data))
        if (scaling) {
          d <- mean(diff(time(data)))
          if (d < 2)
            rv$scaling <- 252 # daily to yearly
          else if (d < 10)
            rv$scaling <- 50 # weekly to yearly
          else if (d < 40)
            # monthly to yearly
            rv$scaling <- 12
          else
            rv$scaling <- 1 # yearly
        }
        if (returns) {
          rmean <- 100 * ((colMeans(data) + 1) ^ rv$scaling - 1)
          rstd <- 100 * colStdevs(data) * sqrt(rv$scaling)
          rv$rettable <- cbind(rmean, rstd)
        }
      }
    }
  optimizeportfolio <- function() {
    method <- as.numeric(input$optportselbox)
    idx <- !is.na(normweights())
    if (method < 9)
      constraints <- NULL
    else {
      #calc constraints
      constraints = ""
      if (idx[1])
        constraints <-
          c(
            constraints,
            paste0("minW[1]=", input$slider1[1] / 100),
            paste0("maxW[1]=", input$slider1[2] /
                     100)
          )
      if (idx[2])
        constraints <-
          c(
            constraints,
            paste0("minW[2]=", input$slider2[1] / 100),
            paste0("maxW[2]=", input$slider2[2] /
                     100)
          )
      if (idx[3])
        constraints <-
          c(
            constraints,
            paste0("minW[3]=", input$slider3[1] / 100),
            paste0("maxW[3]=", input$slider3[2] /
                     100)
          )
      if (idx[4])
        constraints <-
          c(
            constraints,
            paste0("minW[4]=", input$slider4[1] / 100),
            paste0("maxW[4]=", input$slider4[2] /
                     100)
          )
      if (idx[5])
        constraints <-
          c(
            constraints,
            paste0("minW[5]=", input$slider5[1] / 100),
            paste0("maxW[5]=", input$slider5[2] /
                     100)
          )
      if (idx[6])
        constraints <-
          c(
            constraints,
            paste0("minW[6]=", input$slider6[1] / 100),
            paste0("maxW[6]=", input$slider6[2] /
                     100)
          )
      if (idx[7])
        constraints <-
          c(
            constraints,
            paste0("minW[7]=", input$slider7[1] / 100),
            paste0("maxW[7]=", input$slider7[2] /
                     100)
          )
    }
    # Portfolio Specs
    df <- rv$corrmat
    df[df > 1] <- 1
    df[df < (-1)] <- (-1)
    # global vars <<-
    covmatest <<-
      cor2cov(df, rv$rettable[, 2] / sqrt(rv$scaling) / 100)
    muest <<- rv$rettable[, 1] / 100
    portspec <- portfolioSpec()
    setEstimator(portspec) <- "myEstimator"
    w <- NULL
    try(w <-
          portoptweights(method,
                         as.timeSeries(log(1 + returnData()[,-1, drop = F])),
                         as.timeSeries(priceData()),
                         portspec,
                         constraints[-1]),
        silent = T)
    if (!is.null(w)) {
      w <- w * 100
      widx <- rep(NA, length(idx))
      widx[which(idx)] <- w
      updateSliderInput(session, "slider1", value = c(input$slider1[1], ifelse(idx[1], widx[1], input$slider1[2])))
      updateSliderInput(session, "slider2", value = c(input$slider2[1], ifelse(idx[2], widx[2], input$slider2[2])))
      updateSliderInput(session, "slider3", value = c(input$slider3[1], ifelse(idx[3], widx[3], input$slider3[2])))
      updateSliderInput(session, "slider4", value = c(input$slider4[1], ifelse(idx[4], widx[4], input$slider4[2])))
      updateSliderInput(session, "slider5", value = c(input$slider5[1], ifelse(idx[5], widx[5], input$slider5[2])))
      updateSliderInput(session, "slider6", value = c(input$slider6[1], ifelse(idx[6], widx[6], input$slider6[2])))
      updateSliderInput(session, "slider7", value = c(input$slider7[1], ifelse(idx[7], widx[7], input$slider7[2])))
    }
  }
  #   #
  #   # Multiple Plots
  #   #
  #   #https://gist.github.com/wch/5436415/
  #   output$plots <- renderUI({
  #     plotnames<-c("mvplot","weightsplot")
  #     plot_output_list <- lapply(1:length(plotnames), function(i) {
  #       plotOutput(plotnames[i], height = 280, width = 250)
  #     })
  #
  #   # Convert the list to a tagList - this is necessary for the list of items
  #   # to display properly.
  #   do.call(tagList,plot_output_list)
  #   })
}) #  end of shiny server
# ------------------------------------------------------------------------------

#
# static functions
#

#
# Get Symbols Long Names
#
getsymbols <- function(symbols) {
  # depricated by yahoo?
  cond <- any(symbols != "")
  if (cond) {
    url <-
      paste0(
        "https://finance.yahoo.com/quote/quotes.csv?s=",
        paste0(symbols, collapse = "+"),
        "&f=sn"
      )
    symnames <- read.csv(url, header = F, colClasses = "character")
  }
  if (all(symbols != "")) {
    names(symnames) <- c("symbol", "name")
    return(symnames)
  }
  else {
    symnametab <-
      data.frame(
        symbol = rep("", length(symbols)),
        name = rep("", length(symbols)),
        stringsAsFactors =
          F
      )
    if (cond)
      symnametab[symbols != "", ] <- symnames
    return(symnametab)
  }
}
#
# Check Symbols
#
checksymbol <- function(symb) {
  s <- getsymbols(symb)$name
  return((s != "") && (s != "N/A"))
}
#
# Generate Label String
#
nameperc <- function(nr, string, perc, min, max) {
  return(paste0(
    nr,
    " ",
    string,
    " (",
    round(perc, 2),
    "%, min ",
    round(min, 2),
    "%, max ",
    round(max, 2),
    "%)"
  ))
}
#
# stable calc (also for vector) simple return from zoo-object
#
simpleret <- function(z) {
  return(z[-1,] / coredata(z[-nrow(z),]) - 1)
}
#
# Black-Litterman implied returns
#
blimplied <- function(data, w, scaling) {
  lambda <- 1
  blimpliedret <-
    (t(2 * lambda * cov(data) %*% w) + 1) ^ scaling - 1
  return(c(sum(w * blimpliedret), blimpliedret))
}
#
# plot pie
#
pieplot <- function(x, maintext, labeltext, xlabels = T) {
  if (!all(x == 0)) {
    col <- seqPalette(length(x), "Blues")
    cex <- 1.2
    pie(
      x,
      labels = labeltext,
      col = col,
      radius = 1,
      cex = cex,
      main = maintext
    )
    if (xlabels) {
      legend(
        "topleft",
        legend = labeltext,
        bty = "n",
        cex = cex,
        fill = col
      )
      legend(
        "topright",
        legend = paste0(round(x * 100, 2), "%"),
        bty = "n",
        cex = cex,
        fill = col
      )
    }
  }
}
#
# cor2cov
#
cor2cov <- function(cormat, stddevs) {
  d <- diag(stddevs)
  return(d %*% cormat %*% d)
}
#
# estimator
# portfolio optimization weights
#
portoptweights <-
  function(method,
           logreturns,
           prices,
           portspec,
           constraints) {
    switch(
      method,
      #EWP
      "1" = w <- rep(1 / ncol(logreturns), ncol(logreturns)),
      #Max Sharpe Portfolio
      "2" = w <-
        as.numeric(getWeights(
          maxratioPortfolio(logreturns, spec = portspec)
        )),
      #Min Var Portfolio
      "3" = w <-
        as.numeric(getWeights(
          minriskPortfolio(logreturns, spec = portspec)
        )),
      #Equally Risk Contributed
      "4" = w <- 1 / 100 * as.numeric(Weights(PERC(cov(
        logreturns
      )))),
      #Most Diversified
      "5" = w <- 1 / 100 * as.numeric(Weights(PMD(logreturns))),
      #Minimum Tail Dependent
      "6" = w <- 1 / 100 * as.numeric(Weights(PMTD(logreturns))),
      #Minimimum Average Draw Down
      "7" = w <- 1 / 100 * as.numeric(Weights(PAveDD(prices))),
      #Minimimum Maximum Draw Down
      "8" = w <- 1 / 100 * as.numeric(Weights(PMaxDD(prices))),
      #Max Sharpe Portfolio w Weights Constraints
      "9" = w <- as.numeric(getWeights(
        maxratioPortfolio(logreturns, spec = portspec, constraints = constraints)
      )),
      #MinVar Portfolio w Weights Constraints
      "10" = w <- as.numeric(getWeights(
        minriskPortfolio(logreturns, spec = portspec, constraints = constraints)
      ))
    )
    return(w)
  }
