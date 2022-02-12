#
#  ECON110 Tools.R        <<-- SAVE THE FILE WITH THIS NAME IN YOUR RStuio.Cloud 
#  
#  Spring 2022, MCC-Longview
#
#  List of functions:
#  
#  1. use.library(p)   -- if library p is not installed it is being installed
#  2. bls.cpi.idx(b,r) -- transforms BLS data set into a data frame into a 
#                         user-friendly format, where CPI is index
#  2. bls.cpi.yoy(b,r) -- transforms BLS data set into a data frame into a 
#                         user-friendly format, where CPI is YoY change

# ------------------------  FUNCTIONS  -----------------------------------------+

# Function which checks if library p is already installed, if not then
# it downloads the library and attaches it to the project file
# p = name of the library
use.library <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# Function which convert the BLS data into a data frame which fits
# to further usage, for example, in plots by ggplot2 library
# b = BLS data set
# r = ID of the specific BLS indicator, example of CPI-All items: CUUR0000SA0
bls.cpi.idx <- function(b,r) {
  # Data formatting
  b       <- b[b$seriesID==r,]            # Narrowing the BLS data set to r (ID)
  b$value <- as.numeric(b$value)          # Converting value(index into numeric
  # Creating new variable date
  b$date  <- as.Date(paste0("1 ",b$periodName," ",b$year), format = "%d %B %Y")
  b       <- b[order(b$date),]            # Ordering data rows by date column
  return(b)
}

# Function which convert the BLS data into a data frame which fits
# to further usage, for example, in plots by ggplot2 library
# b = BLS data set
# r = ID of the specific BLS indicator, example of CPI-All items: CUUR0000SA0
bls.cpi.yoy <- function(b,r) {
  # Data formatting
  b       <- b[b$seriesID==r,]            # Narrowing the BLS data set to r (ID)
  b$value <- as.numeric(b$value)          # Converting value(index into numeric
  # Creating new variable date
  b$date  <- as.Date(paste0("1 ",b$periodName," ",b$year), format = "%d %B %Y")
  b       <- b[order(b$date),]            # Ordering data rows by date column
  # Converting the data set into time series format
  b.ts    <- ts(b$value, start = c(b$year[1],month(b$date[1])), frequency = 12)
  c       <- (b.ts/lag(b.ts,-12)-1)*100   # Calculation of year-on-year change in the data
  print(paste("% year-on-year change of",r))# Printing a text label in the output file
  print(round(c,2))                       # Printing a table with YoY changes in the output file
  #ts.plot(c)                             # Printing a graph with YoY changes in the output file
  #plot(x12(c), forecast=TRUE)            # Printing a graph with YoY chg + forecast
  # data frame
  df <- data.frame(date=as.Date(stats::time(c)),value=as.numeric(c),seriesID=r)
  return(df)
}

# ------------------------  OPERATIONS  ----------------------------------------+

# Checking if libraries being installed already or not
use.library("stats")
use.library("zoo")
use.library("x12")
