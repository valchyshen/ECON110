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
#  3. bls.cpi.yoy(b,r) -- transforms BLS data set into a data frame into a 
#                         user-friendly format, where CPI is YoY change
#  4. bls.cpi.item_name(d,l) -- this function attaches new column to the data
#                         set d, the new column contains the label l  

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
  n       <- cpi.items[cpi.items$item_code==substr(r,9,nchar(r)),]$item_name
  print(paste("% year-on-year change of",r,",",n))    # Printing a text label in the output file
  print(round(c,2))                       # Printing a table with YoY changes in the output file
  #ts.plot(c)                             # Printing a graph with YoY changes in the output file
  #plot(x12(c), forecast=TRUE)            # Printing a graph with YoY chg + forecast
  # data frame
  df <- data.frame(date      = as.Date(stats::time(c)),
                   value     = as.numeric(c),
                   seriesID  = r,
                   item_name = n)
  return(df)
}

# Adding CPI's item names to the historical CPI data set
# d = historical data set of BLS
# l = data set of names
bls.cpi.item_name <- function(d,l) {
  k <- cpi.items[cpi.items$item_code %in% substr(l,9,nchar(l)),c(1,2)]
  n <- nrow(k)
  d$item_name <- c("") # empty column, where names will be stored
  for (i in 1:n) {
    d[d$seriesID==paste0("CUUR0000",k$item_code[i]),]$item_name <- k$item_name[i]
  }
  return(d)
}

#
# US: Corridor system of short-term interest rates
#
ir.corridor.us <- function(d) {
  
  start_d <- as.Date(d) #("1960-01-01")
  
  # 30-yr US Treasury Rate
  ust30 <- 
    fredr(
      series_id = "DGS30",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust30$ind<-"Long-term"
  
  # 10-yr US Treasury Rate
  ust10 <- 
    fredr(
      series_id = "DGS10",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust10$ind<-"10 year"
  
  # 7-yr US Treasury Rate
  ust7 <- 
    fredr(
      series_id = "DGS7",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust7$ind<-" 7 year"
  
  # 5-yr US Treasury Rate
  ust5 <- 
    fredr(
      series_id = "DGS5",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust5$ind<-" 5 year"
  
  # 3-yr US Treasury Rate
  ust3 <- 
    fredr(
      series_id = "DGS3",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust3$ind<-" 3 year"
  
  # 2-yr US Treasury Rate
  ust2 <- 
    fredr(
      series_id = "DGS2",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust2$ind<-" 2 year"
  
  # 1-yr US Treasury Rate
  ust1 <- 
    fredr(
      series_id = "DGS1",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust1$ind<-" 1 year"
  
  # 6-month US Treasury bill
  ust6m <- 
    fredr(
      series_id = "DTB6",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust6m$ind<-" 0.5 year (6m)"
  
  # 3-month US Treasury bill
  ust3m <- 
    fredr(
      series_id = "DTB3",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust3m$ind<-" 0.25 year (3m)"
  
  # 1-month US Treasury bill
  ust1m <- 
    fredr(
      series_id = "DTB1",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust1m$ind<-" 0.08 year (1m)"
  
  # Effective Federal Funds Rate
  fed_er <- fredr(
    series_id = "DFF",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_er$ind<-"Effective Federal Funds Rate"
  
  # Interest rate on excess reserves
  fed_ioer <- fredr(
    series_id = "IOER",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  
  # Interest rate on required reserves
  fed_iorr <- fredr(
    series_id = "IORR",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  
  # Fed Funds Target Rate
  fed_tr <- fredr(
    series_id = "DFEDTAR",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_tr$ind<-"Fed Funds Target Rate"
  
  # Fed Funds Target Range Upper Limit
  fed_tru <- fredr(
    series_id = "DFEDTARU",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_tru$ind<-"Fed Funds Target Range Upper Limit"
  
  # Fed Funds Target Range Lower Limit
  fed_trl <- fredr(
    series_id = "DFEDTARL",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_trl$ind<-"Fed Funds Target Range Lower Limit"
  
  # Secured O/N Financing Rate
  # FRB NY: https://apps.newyorkfed.org/markets/autorates/SOFR
  sofr <- fredr(
    series_id = "SOFR",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  sofr$ind<-"Secured O/N Financing Rate"
  
  df <- data.frame(date=fed_er$date, value=fed_er$value/100, ind=c("EFFR"))
  df <- rbind(df, data.frame(date=fed_ioer$date, value=fed_ioer$value/100, ind=c("IOER")))
  df <- rbind(df, data.frame(date=fed_iorr$date, value=fed_iorr$value/100, ind=c("IORR")))
  df <- rbind(df, data.frame(date=fed_tru$date, value=fed_tru$value/100, ind=c("TR, upper")))
  df <- rbind(df, data.frame(date=fed_trl$date, value=fed_trl$value/100, ind=c("TR, lower")))
  df <- rbind(df, data.frame(date=fed_tr$date, value=fed_tr$value/100, ind=c("FFTR")))
  df <- rbind(df, data.frame(date=sofr$date, value=sofr$value/100, ind=c("SOFR")))
  df <- rbind(df, data.frame(date=ust3m$date, value=ust3m$value/100, ind=c("UST 3M")))
  df <- rbind(df, data.frame(date=ust2$date, value=ust2$value/100, ind=c("UST 2yr")))
  df <- rbind(df, data.frame(date=ust10$date, value=ust10$value/100, ind=c("UST 10yr")))
  
  df <- na.omit(df)
  df <- df[df$date >= (Sys.Date() - 5 * 365),] # last 10 years history 
  
  sofr_mx <- max(df[df$ind=="SOFR",]$value) # max value of SOFR
  effr_mx <- max(df[df$ind=="EFFR",]$value) # max value of EFFR
  yr_st <- year(df$date[1])
  yr_en <- year(df$date[nrow(df)])
  
  h = 4 # Standard heights of the plot
  path_png = "C:/R/Plots/" # Path to the folder where plot's image is saved
  image_save = paste0(path_png, "US Fed int rates system last 5yrs", 
                      format(df$date[nrow(df)],"%Y-%m-%d"), ".png")
  
  g1 <- ggplot() + 
    #ggplot(df, aes(x=date, y=value, color=ind)) + 
    geom_vline(xintercept = 
                 as.Date(df[df$ind=="FFTR", ]$date[nrow(df[df$ind=="FFTR", ])]), size=.1) +
    geom_vline(xintercept = 
                 as.Date(df[df$ind=="SOFR", ]$date[1]), size=.1) + 
    #linetype="dashed", color = "red", size=.5) +
    geom_line(data = df[df$ind=="TR, upper", ], 
              mapping = aes(x=date, y=value), col="black", size=.5, alpha=.7, linetype=1) + #3399CC
    geom_line(data = df[df$ind=="TR, lower", ], 
              mapping = aes(x=date, y=value), col="black", size=.5, alpha=.7, linetype=1) +
    geom_line(data = df[df$ind=="EFFR" | df$ind=="SOFR",], 
              mapping = aes(x=date, y=value, color=ind), size=.7) +
    geom_line(data = df[df$ind=="FFTR",], 
              mapping = aes(x=date, y=value), col="black", size=.5, alpha=.4, linetype=1) + #990000
    labs(x="", y="",
         title = "U.S. Federal Reserve system of interest rate targeting (%)",
         subtitle = paste0("Daily history of past 5-year period from ", format(df$date[1], "%B %d, %Y"),
                           " through ", format(df$date[nrow(df)], "%B %d, %Y")),
         caption = "Notes: EFFR = effective federal funds rate, SOFR = secured overnight financing rate, Source: FRED.") +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = percent,
                       sec.axis = sec_axis(~., name = "", 
                                           labels = percent)) +
    scale_fill_brewer(palette="Spectral") +
    theme(legend.text = element_text(size = 9),
          legend.title = element_blank(), 
          plot.caption = element_text(hjust = 0, size = 9)) +
    ggplot2::annotate("text", x = 
                        as.Date(df[df$ind=="FFTR", ]$date[nrow(df[df$ind=="FFTR", ])]) - 200, 
                      y = 0, 
                      label = paste0("On ", 
                                     format(as.Date(df[df$ind=="FFTR", ]$date[nrow(df[df$ind=="FFTR", ])]), 
                                            "%b %d, %Y"),
                                     " the Fed changed its % rate targeting from \ntargeting a particular rate level to the range targeting"),
                      size = 2, hjust = 1, color="black") +
    ggplot2::annotate("text", x = as.Date(df[df$ind=="SOFR", ]$date[1]) - 30, 
                      y = 0, #df[df$ind=="SOFR", ]$value[1] + .0025, 
                      label = paste0("On ", 
                                     format(as.Date(df[df$ind=="SOFR", ]$date[1]), 
                                            "%b %d, %Y"),
                                     "\nthe Fed launched SOFR"),
                      size = 2, hjust = 1, color="black"); g1
  return(g1)
}

ir.govt.us <- function(d) {
  
  start_d <- as.Date(d) #("1960-01-01")
  
  # 30-yr US Treasury Rate
  ust30 <- 
    fredr(
      series_id = "DGS30",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust30$ind<-"Long-term"
  
  # 10-yr US Treasury Rate
  ust10 <- 
    fredr(
      series_id = "DGS10",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust10$ind<-"10 year"
  
  # 7-yr US Treasury Rate
  ust7 <- 
    fredr(
      series_id = "DGS7",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust7$ind<-" 7 year"
  
  # 5-yr US Treasury Rate
  ust5 <- 
    fredr(
      series_id = "DGS5",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust5$ind<-" 5 year"
  
  # 3-yr US Treasury Rate
  ust3 <- 
    fredr(
      series_id = "DGS3",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust3$ind<-" 3 year"
  
  # 2-yr US Treasury Rate
  ust2 <- 
    fredr(
      series_id = "DGS2",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust2$ind<-" 2 year"
  
  # 1-yr US Treasury Rate
  ust1 <- 
    fredr(
      series_id = "DGS1",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust1$ind<-" 1 year"
  
  # 6-month US Treasury bill
  ust6m <- 
    fredr(
      series_id = "DTB6",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust6m$ind<-" 0.5 year (6m)"
  
  # 3-month US Treasury bill
  ust3m <- 
    fredr(
      series_id = "DTB3",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust3m$ind<-" 0.25 year (3m)"
  
  # 1-month US Treasury bill
  ust1m <- 
    fredr(
      series_id = "DTB1",
      observation_start = start_d,
      observation_end = Sys.Date()
    )
  ust1m$ind<-" 0.08 year (1m)"
  
  # Effective Federal Funds Rate
  fed_er <- fredr(
    series_id = "DFF",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_er$ind<-"Effective Federal Funds Rate"
  
  # Interest rate on excess reserves
  fed_ioer <- fredr(
    series_id = "IOER",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  
  # Interest rate on required reserves
  fed_iorr <- fredr(
    series_id = "IORR",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  
  # Fed Funds Target Rate
  fed_tr <- fredr(
    series_id = "DFEDTAR",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_tr$ind<-"Fed Funds Target Rate"
  
  # Fed Funds Target Range Upper Limit
  fed_tru <- fredr(
    series_id = "DFEDTARU",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_tru$ind<-"Fed Funds Target Range Upper Limit"
  
  # Fed Funds Target Range Lower Limit
  fed_trl <- fredr(
    series_id = "DFEDTARL",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  fed_trl$ind<-"Fed Funds Target Range Lower Limit"
  
  # Secured O/N Financing Rate
  # FRB NY: https://apps.newyorkfed.org/markets/autorates/SOFR
  sofr <- fredr(
    series_id = "SOFR",
    observation_start = start_d,
    observation_end = Sys.Date()
  )
  sofr$ind<-"Secured O/N Financing Rate"
  
  dd <- as.Date("1990-01-01")
  mmr <- na.omit(rbind(ust1m,ust3m,ust6m,ust1,ust2,ust3,ust5,ust7,ust10,ust30))
  mmr <- mmr[mmr$date>=dd,]
  mmr$value <- mmr$value/100
  
  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), 10)
  #image_save = paste0(path_png, "US Trys vs Fed Target Rate.png")
  
  g <- ggplot() + 
    geom_line(data=mmr, mapping = aes(x=date, y=value, color=ind)) +
    geom_line(data=fed_tr[fed_tr$date>=dd,], mapping = aes(x=date, y=value/100), 
              color="black", size=.5, alpha=.7) +
    geom_line(data=fed_tru, mapping = aes(x=date, y=value/100), 
              color="black", size=.5, alpha=.7) +
    geom_line(data=fed_trl, mapping = aes(x=date, y=value/100), 
              color="black", size=.5, alpha=.7) +
    geom_vline(xintercept=as.Date("2008-12-15"), color = "black", size=0.1) +
    geom_vline(xintercept=as.Date("2011-08-05"), color = "red", size=0.1) +
    #scale_x_date(date_breaks = "5 year", date_minor_breaks = "5 year", date_labels = "%Y") + 
    scale_x_date(date_labels = "%Y") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = .1), 
                       sec.axis = sec_axis(~., name = "", 
                                           labels = scales::percent_format(accuracy = .1))) +
    labs(title = "Yields on the US Treasuries & Federal Reserve Target Rate (%)", 
         subtitle = paste0("Daily history from ", format(min(mmr$date), "%B %d, %Y"),
                           " through ", format(max(mmr$date), "%B %d, %Y")),
         x="", y="",
         caption = paste0("Notes: m = month(s). Source: FRED.")) + 
    theme(axis.title.y = element_text(size = 9),
          legend.title = element_blank(), 
          legend.background=element_blank(),
          legend.position = "right", plot.caption = element_text(hjust = 0, size = 8)) +
    guides(color = guide_legend(reverse=T)) +
    scale_color_manual(values = palette) +
    ggplot2::annotate("text", x = as.Date("2008-12-15")-60, y = .08, 
                      label =  "On Dec 15, 2008, the Fed switched\n from the targeting a particular\n % rate level to\n the range targeting", 
                      size = 2, hjust = 1, color = "black") +
    ggplot2::annotate("text", x = as.Date("2011-08-05")+60, y = .08, 
                      label =  "On Aug 5, 2011, S&P cut\n the US credit rating\n from AAA to AA+ with\n a negative outlook", 
                      size = 2, hjust = 0, color = "red") +
    ggplot2::annotate("text", x = as.Date("1998-01-01"), y = .04, 
                      label =  "Fed Funds\n Target Rate", 
                      size = 2, hjust = 1, color = "black") +
    geom_segment(aes(x = as.Date("1998-01-01"), y = .04,
                     xend = as.Date("1999-01-01"), 
                     yend = fed_tr[fed_tr$date==as.Date("1999-01-01"),]$value/100), 
                 color="black", size=0.1) +
    ggplot2::annotate("text", x = as.Date("2020-05-01"), y = .035, 
                      label =  "Fed Funds \nTarget \nRange", 
                      size = 2, hjust = 0, color = "black") +
    geom_segment(aes(x = as.Date("2020-05-01")-30, y = .035,
                     xend = as.Date("2019-05-01"), 
                     yend = fed_tru[fed_tru$date==as.Date("2019-05-01"),]$value/100), 
                 color="black", size=0.1)
  
  return(g)
}

# ------------------------  OPERATIONS  ----------------------------------------+

# Checking if libraries being installed already or not
use.library("stats")
use.library("zoo")
use.library("x12")
use.library("rdbnomics")
use.library("devtools")
use.library("lubridate")
use.library("ggpubr")
use.library("scales")

install_github("mikeasilva/blsAPI")  # Installing R library for BLS

# --- US ECONOMY ---

# Item codes for CPI details: https://download.bls.gov/pub/time.series/cu/cu.item
# These are to be retrieved into the data set called cpi.items
cpi.items <- read.csv("https://download.bls.gov/pub/time.series/cu/cu.item", 
                      header = TRUE, sep = "\t")
