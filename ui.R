#
# packages
#
library(shiny)
library(DT)
library(shinythemes)
# https://community.rstudio.com/t/shinysky-package-is-not-available-for-r-version-3-5-2/27497
# install.packages("devtools")
# install_github("AnalytixWare/ShinySky")
library(shinysky)
library(dygraphs)

#
# shinyUI
#
fluidPage(theme = shinytheme("yeti"),
          sidebarLayout(
            sidebarPanel(
              h2(img(src = "smartcube.gif"), "Portfolio Optimizer"),
              helpText(
                "Select and limit portfolio weights for optimization with sliders.",
                br(),
                "All charts and tables are interactive and will immediately react to your input."
              ),
              a(
                href = "http://www.smartcube.at/contact.html",
                "Use 'help' or contact ",
                img(
                  src = "smartcube.gif",
                  width = 100,
                  height = 25
                ),
                " for further information."
              ),
              br(),
              br(),
              wellPanel(
                h4("Portfolio Optimization Method"),
                selectInput(
                  "optportselbox",
                  "",
                  choices = list(
                    "Equally Weighted" = 1,
                    "Maximum Sharpe" = 2,
                    "Minimum Variance" = 3,
                    "Equally Risk Contributed" = 4,
                    "Most Diversified" = 5,
                    "Minimum Tail Dependent" = 6,
                    "Minimimum Average Draw Down" = 7,
                    "Minimimum Maximum Draw Down" = 8,
                    "Maximum-Sharpe Portfolio w Weights Constraints" =
                      9,
                    "Minimum-Variance w Weights Constraints" =
                      10
                  ),
                  selected = 1
                ),
                actionButton("optportbutton", "Optimize!", styleclass = "primary"),
                actionButton("resweightsbutton", "Reset Weights", styleclass =
                               "warning"),
                actionButton("ressamplebutton", "Reset Sample", styleclass =
                               "warning"),
                hr(),
                checkboxGroupInput(
                  "aselect",
                  label = "Selected Assets",
                  choices = list(
                    "1" = 1,
                    "2" = 2,
                    "3" = 3,
                    "4" = 4,
                    "5" = 5,
                    "6" = 6,
                    "7" = 7
                  ),
                  selected = as.character(1:7),
                  inline = T
                ),
                sliderInput(
                  "dsamplerange",
                  "Data Range %",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                ),
                hr(),
                sliderInput(
                  "slider1",
                  "",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                ),
                sliderInput(
                  "slider2",
                  "",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                ),
                sliderInput(
                  "slider3",
                  "",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                ),
                sliderInput(
                  "slider4",
                  "",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                ),
                sliderInput(
                  "slider5",
                  "",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                ),
                sliderInput(
                  "slider6",
                  "",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                ),
                sliderInput(
                  "slider7",
                  "",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                )
              )
            ),
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Performance",
                  dygraphOutput("chartplot"),
                  br(),
                  dataTableOutput("perftable")
                ),
                tabPanel("Distribution", plotOutput("distplot", height = "800px")),
                tabPanel("Drawdowns", plotOutput("ddplot", height = "800px")),
                tabPanel("Efficient Frontier", plotOutput("efplot", height =
                                                            "800px")),
                tabPanel("Black-Litterman", plotOutput("blplot", height =
                                                         "800px")),
                tabPanel("Stats", dataTableOutput("statstable")),
                tabPanel(
                  "Estimates",
                  h4("Correlations"),
                  plotOutput("corrplot", width = "75%"),
                  br(),
                  actionButton(
                    "corrhistbutton",
                    "Reset Correlations To Historical Values",
                    styleclass =
                      "blank"
                  ),
                  actionButton("corrstressbutton", "Correlation Stress Test", styleclass =
                                 "blank"),
                  hotable("corrtable"),
                  h4("Estimated Returns"),
                  actionButton(
                    "rethistbutton",
                    "Reset Returns To Historical Values",
                    styleclass = "blank"
                  ),
                  actionButton(
                    "retblbutton",
                    "Reset Returns To Adj. Black-Litterman Values",
                    styleclass =
                      "blank"
                  ),
                  hotable("retmat")
                ),
                tabPanel(
                  "Prices",
                  h4("Prices"),
                  dataTableOutput("rawtable"),
                  h4("Returns %"),
                  dataTableOutput("returnstable")
                ),
                tabPanel("Datafeeds",
                         column(
                           6,
                           wellPanel(
                             fileInput(
                               "loadfile",
                               label = h4("Load from File"),
                               multiple = F,
                               accept = ".csv"
                             ),
                             h4("Save To File"),
                             downloadButton("savedata", "Save Data"),
                             hr(),
                             h4("Get Prices from Yahoo"),
                             actionButton("getyahoobutton", "Get Prices", styleclass =
                                            "primary"),
                             actionButton("residxbutton", "Reset Ticker", styleclass =
                                            "warning"),
                             br(),
                             dateRangeInput(
                               "dates",
                               "Date range",
                               start = "2015-01-02",
                               end = as.character(Sys.Date())
                             ),
                             textInput("symb1", "", YAHOOSYMBOLS$symbol[1]),
                             textInput("symb2", "", YAHOOSYMBOLS$symbol[2]),
                             textInput("symb3", "", YAHOOSYMBOLS$symbol[3]),
                             textInput("symb4", "", YAHOOSYMBOLS$symbol[4]),
                             textInput("symb5", "", YAHOOSYMBOLS$symbol[5]),
                             textInput("symb6", "", YAHOOSYMBOLS$symbol[6]),
                             textInput("symb7", "", YAHOOSYMBOLS$symbol[7])
                           )
                         )),
                tabPanel("Report",
                         column(
                           6,
                           wellPanel(
                             radioButtons(
                               "reportformat",
                               "Document format",
                               c("Word", "PDF", "HTML"),
                               inline = TRUE
                             ),
                             downloadButton("downloadReport")
                           )
                         )),
                tabPanel("Help", includeMarkdown("help.md"))
              )
            )
          ))
