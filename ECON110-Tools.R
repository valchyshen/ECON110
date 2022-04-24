#
#  ECON110-Tools.R        <<-- SAVE THE FILE WITH THIS NAME IN YOUR RStuio.Cloud 
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
#  5. daily.hist(d,v,s)-- it turns a policy rate into daily history
#                         d - dates; v - values as double numbers; 
#                         s - indicator name as string
#  6. ir.corridor.ca   -- chart of the % rate corridor system in Canada
#  7. ir.corridor.mx   -- chart of the % rate corridor system in Mexico
#  8. ir.corridor.tr   -- chart of the % rate corridor system in Turkey
#  9. ir.corridor.us   -- chart of the % rate corridor system in US
# 10. ir.govt.us       -- chart of govt bond yields in USA
# 11. ir.govt.ca       -- chart of govt bond yields in Canada
# 11. ir.govt.mx       -- chart of govt bond yields in Mexico
# 11. ir.govt.tr       -- chart of govt bond yields in Turkey

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
  c       <- (b.ts/stats::lag(b.ts,-12)-1)*100   # Calculation of year-on-year change in the data
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

# Function turns a policy rate into daily history
# d - dates; v - values as double numbers; s - indicator name as string
daily.hist <- function(d,v,s) {
  t <- data.frame(date=as.Date(seq(min(d), Sys.Date(), by="days")))
  u <- data.frame(date=d,value=v)
  u <- na.omit(u)
  r <- left_join(t,u,by=c("date"))
  n <- nrow(r)
  for (i in 1:n) {
    if (is.na(r$value[i])==FALSE) { prev_r <- r$value[i] }
    if (is.na(r$value[i])==TRUE)  { r$value[i] <- prev_r }
  }
  r <- cbind(r,ind=s)
  return(r)
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

#
# Turkey: Corridor system of short-term interest rates
#
ir.corridor.tr <- function(d) {
  
  # Int rates table being initiated
  tcmb.rates <- data.frame()
  
  # CBRT Interest Rates (%) Overnight (O/N) --------------------------------------+
  pageHTML <- getURL("https://tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Main+Menu/Core+Functions/Monetary+Policy/Central+Bank+Interest+Rates/CBRT+INTEREST+RATES",
                     ssl.verifyPeer=FALSE)
  t <- readHTMLTable(pageHTML, header = TRUE, skip.rows = 2)
  n <- t$midTable[1,] # names of columns
  d <- t$midTable[c(2:nrow(t$midTable)),]
  colnames(d) <- n
  head(d);tail(d)
  d$Date <- as.Date(d$Date, format = "%d.%m.%y")
  d$Borrowing <- as.numeric(sub(",",".",d$Borrowing))
  d$Lending <- as.numeric(sub(",",".",d$Lending))
  
  tcmb.rates <- rbind(tcmb.rates,
                      daily.hist(d$Date,d$Borrowing,"O/N Deposit Facility"))
  tcmb.rates <- rbind(tcmb.rates,
                      daily.hist(d$Date,d$Lending,"O/N Lending Facility"))
  
  # 1 Week Repo ------------------------------------------------------------------+
  pageHTML <- getURL("https://tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Main+Menu/Core+Functions/Monetary+Policy/Central+Bank+Interest+Rates/1+Week+Repo",
                     ssl.verifyPeer=FALSE)
  t <- readHTMLTable(pageHTML, header = TRUE)
  #n <- t$midTable[1,] # names of columns
  #d <- t$midTable[c(2:nrow(t$midTable)),]
  #colnames(d) <- n
  d<-t$midTable
  d$DATE <- as.Date(d$DATE, format = "%d.%m.%Y")
  d$Borrowing <- as.numeric(sub("-","",d$Borrowing))
  d$Lending <- as.numeric(sub(",",".",d$Lending))
  head(d);tail(d)
  
  #tcmb.rates <- rbind(tcmb.rates,
  #                    daily.hist(d$DATE,d$Borrowing,"1W Repo (reverse)"))
  tcmb.rates <- rbind(tcmb.rates,
                      daily.hist(d$DATE,d$Lending,"1W Repo"))
  
  # O/N Money-Market Rate --------------------------------------------------------+
  
  # Date: https://borsaistanbul.com/en/sayfa/3278/tlref-data
  
  # Older history
  u <- "https://borsaistanbul.com/files/tlrefindicative.xlsx"
  #tmp <- tempfile()
  tmp <- "tmp.xlsx"
  download.file(u, destfile=tmp, method="curl")
  options(java.parameters = "-Xmx1000m")
  t <- read.xlsx(tmp, 2)
  unlink(tmp)
  t$DATE <- as.Date(t$DATE, origin="1899-12-30")
  tcmb.rates <- rbind(tcmb.rates,
                      data.frame(date=t$DATE,value=t$VALUE,ind="TLREF *"))
  
  # Newer history
  u <- "https://borsaistanbul.com/datum/TLREFORANI_D.zip"
  #tmp <- tempfile()
  tmp <- "TLREFORANI_D.zip"
  download.file(u, destfile=tmp, method="curl")
  unzip(tmp, exdir = ".")
  t <- read.csv(file = "TLREFORANI_D.csv", header = TRUE, sep = ";", skipNul = TRUE, 
                colClasses = "character", fileEncoding = "latin1")
  unlink(tmp)
  colnames(t) <- c("date","name_trk","name_eng","code","isin","value","repo_rate","traded_vlm",
                   "no_of_deals","no_of_active_cntprts","elgbl_trz_vlm","elgbl_no_of_deals",
                   "elgbl_trz_act_cntprts","rate_first15pctl_trdvlm","rate_last15pctl_trdvlm")
  t$date <- as.Date(t$date, format = "%d/%m/%Y")
  t <- na.omit(t)
  head(t);tail(t)
  tcmb.rates <- rbind(tcmb.rates,
                      data.frame(date=t$date,value=t$value,ind="TLREF"))
  tcmb.rates$value <- as.numeric(tcmb.rates$value)
  
  # CHART: O/N Rate and Policy Rates ---------------------------------------------+
  
  g <- 
    ggplot() + 
    geom_line(data = tcmb.rates[tcmb.rates$ind=="TLREF *" | tcmb.rates$ind=="TLREF",], 
              mapping = aes(x=date,y=value/100,color=ind), size=.9) + 
    geom_line(data = tcmb.rates[tcmb.rates$ind=="O/N Deposit Facility"
                                & tcmb.rates$date>=as.Date("2010-01-01"),], 
              mapping = aes(x=date, y=value/100), col="#3399CC", size=.5, alpha=.4, linetype=1) +
    geom_line(data = tcmb.rates[tcmb.rates$ind=="O/N Lending Facility"
                                & tcmb.rates$date>=as.Date("2010-01-01"),], 
              mapping = aes(x=date, y=value/100), col="#3399CC", size=.5, alpha=.4, linetype=1) +
    geom_line(data = tcmb.rates[tcmb.rates$ind=="1W Repo",], 
              mapping = aes(x=date, y=value/100), col="#990000", size=.5, alpha=.4, linetype=1) +
    scale_x_date(date_breaks = "2 year", date_minor_breaks = "2 year", date_labels = "%Y") + 
    scale_y_continuous(labels = percent, sec.axis = sec_axis(~., name = "", labels = percent)) +
    labs(x = "", y = "", title = "Turkey central bank's corridor system & O/N rate (%)",
         subtitle = paste0("Daily history from January 1, 2010 and through ", 
                           format(max(tcmb.rates$date),"%B %d, %Y")),
         caption = "* calculated retrospectively by Borsa Instambul; TLREF = Turkish Lira Overnight Reference Rate. \nSource: Central Bank of Turkey, Borsa Instambul.") +
    theme(axis.title.y = element_text(size = 9),
          legend.title = element_blank(), 
          legend.background=element_blank(),
          legend.position = "right", plot.caption = element_text(hjust = 0, size = 8))
  
  g <- 
    ggplot() + 
    geom_line(data = tcmb.rates[(tcmb.rates$ind=="TLREF *" | tcmb.rates$ind=="TLREF") &
                                  tcmb.rates$date>=as.Date("2018-01-01"),], 
              mapping = aes(x=date,y=value/100,color=ind), size=.9) + 
    geom_line(data = tcmb.rates[tcmb.rates$ind=="O/N Deposit Facility"
                                & tcmb.rates$date>=as.Date("2018-01-01"),], 
              mapping = aes(x=date, y=value/100), col="black", size=.5, alpha=.7, linetype=1) +
    geom_line(data = tcmb.rates[tcmb.rates$ind=="O/N Lending Facility"
                                & tcmb.rates$date>=as.Date("2018-01-01"),], 
              mapping = aes(x=date, y=value/100), col="black", size=.5, alpha=.7, linetype=1) +
    geom_line(data = tcmb.rates[tcmb.rates$ind=="1W Repo" & tcmb.rates$date>=as.Date("2018-01-01"),], 
              mapping = aes(x=date, y=value/100), col="black", size=.5, alpha=.7, linetype=3) +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + 
    scale_y_continuous(labels = percent, sec.axis = sec_axis(~., name = "", labels = percent)) +
    labs(x = "", y = "", title = "Turkey central bank's corridor system & O/N rate (%)",
         subtitle = paste0("Daily history from January 1, 2010 and through ", 
                           format(max(tcmb.rates$date),"%B %d, %Y")),
         caption = "* calculated retrospectively by Borsa Instambul; TLREF = Turkish Lira Overnight Reference Rate. \nSource: Central Bank of Turkey, Borsa Instambul.") +
    theme(axis.title.y = element_text(size = 9),
          legend.title = element_blank(), 
          legend.background=element_blank(),
          legend.position = "right", plot.caption = element_text(hjust = 0, size = 8)) +
    ggplot2::annotate("text", x = as.Date("2021-06-25")+30, 
                      y = tcmb.rates[tcmb.rates$ind=="1W Repo" & 
                                       tcmb.rates$date==as.Date("2021-06-25"),]$value/100+.025, 
                      label =  "O/N target rate \n (dotted line)", 
                      size = 2, hjust = 0, color = "black") +
    geom_segment(aes(x = as.Date("2021-06-25")+30, 
                     y = tcmb.rates[tcmb.rates$ind=="1W Repo" & 
                                      tcmb.rates$date==as.Date("2021-06-25"),]$value/100+.025, 
                     xend = as.Date("2021-06-25"), 
                     yend = tcmb.rates[tcmb.rates$ind=="1W Repo" & 
                                         tcmb.rates$date==as.Date("2021-06-25"),]$value/100), 
                 color="black", size=0.3, arrow = arrow(length = unit(0.15, "cm")))
  
  
  return(g)
}

ir.corridor.ca <- function(d) {
  
  # BoC xml o/n rates
  # https://www.bankofcanada.ca/
  
  d_srt <- "1990-01-01"
  d_end <- Sys.Date()
  
  boc <- "https://www.bankofcanada.ca/valet/observations/"
  
  df <- data.frame()
  
  # Target for the O/N rate: V39079
  
  ind <- "V39079"
  u <- paste0(boc, ind, "/xml?recent_years=25")
  f <- xml2::read_xml(u)
  doc <- xml2::as_list(f)
  d <- lapply(doc$data$observations, 
              function(x) data.frame(Value = x$v[[1]], Indicator = attr(x$v, "s"), Date = attr(x, "d")))
  d <- dplyr::bind_rows(d)
  d$Date <- as.Date(d$Date)
  d$Value <- as.numeric(d$Value)
  head(d)
  
  df <- rbind(df, data.frame(date=d$Date, code=d$Indicator, value=d$Value/100, ind="O/N TR"))
  boc_tr <- data.frame(date=d$Date, Value=d$Value, Label="BoC O/N TR")
  
  # Canadian Overnight Repo Rate Average (CORRA) , Low
  ind <- "V39076"
  u <- paste0(boc, ind, "/xml?recent_years=25")
  f <- xml2::read_xml(u)
  doc <- xml2::as_list(f)
  d <- lapply(doc$data$observations, 
              function(x) data.frame(Value = x$v[[1]], Indicator = attr(x$v, "s"), Date = attr(x, "d")))
  d <- dplyr::bind_rows(d)
  d$Date <- as.Date(d$Date)
  d$Value <- as.numeric(d$Value)
  head(d);tail(d)
  
  df <- rbind(df, data.frame(date=d$Date, code=d$Indicator, value=d$Value/100, ind="CORRA, Low"))
  
  # Canadian Overnight Repo Rate Average (CORRA) , High
  ind <- "V39077"
  u <- paste0(boc, ind, "/xml?recent_years=25")
  f <- xml2::read_xml(u)
  doc <- xml2::as_list(f)
  d <- lapply(doc$data$observations, 
              function(x) data.frame(Value = x$v[[1]], Indicator = attr(x$v, "s"), Date = attr(x, "d")))
  d <- dplyr::bind_rows(d)
  d$Date <- as.Date(d$Date)
  d$Value <- as.numeric(d$Value)
  head(d);tail(d)
  
  df <- rbind(df, data.frame(date=d$Date, code=d$Indicator, value=d$Value/100, ind="CORRA, High"))
  
  # CORRA (AVG.INTWO)
  
  ind <- "AVG.INTWO"
  u <- paste0(boc, ind, "/xml")
  f <- xml2::read_xml(u)
  doc <- xml2::as_list(f)
  d <- lapply(doc$data$observations, 
              function(x) data.frame(Value = x$v[[1]], Indicator = attr(x$v, "s"), Date = attr(x, "d")))
  d <- dplyr::bind_rows(d)
  d$Date <- as.Date(d$Date)
  d$Value <- as.numeric(d$Value)
  head(d)
  
  df <- rbind(df, data.frame(date=d$Date, code=d$Indicator, value=d$Value/100, ind="CORRA"))
  boc_corra <- data.frame(date=d$Date, Value=d$Value, Label="O/N, CORRA")
  
  df <- df[df$date>=as.Date("2010-01-01"),]
  g <- ggplot(df, aes(x=date,y=value,color=ind))+geom_line();g
  
  # Annotations
  d_st <- min(df$date)+365
  ad <- d_st + 30
  hh <- df[df$ind=="CORRA, High",]
  hv <- hh[hh$date==ad,]$value
  hh <- df[df$ind=="CORRA, Low",]
  lv <- hh[hh$date==ad,]$value
  hh <- df[df$ind=="O/N TR",]
  tv <- hh[hh$date==ad,]$value
  
  g <- ggplot() +
    geom_line(data = df[df$ind=="O/N TR",], 
              mapping = aes(x=date, y=value), col="black", size=.5, alpha=1, linetype=3) +
    geom_line(data = df[df$ind=="CORRA, High", ], 
              mapping = aes(x=date, y=value), col="black", size=.5, alpha=.7, linetype=1) +
    geom_line(data = df[df$ind=="CORRA, Low", ], 
              mapping = aes(x=date, y=value), col="black", size=.5, alpha=.7, linetype=1) +
    geom_line(data = df[df$ind=="CORRA",], 
              mapping = aes(x=date, y=value, color=ind), size=.8, alpha=.7) +
    labs(x="", y="",
         title = "Bank of Canada's corridor system of the interest rate targeting (%)",
         subtitle = paste0("History from ", 
                           format(df[df$ind=="CORRA",]$date[1], "%B %d, %Y"),
                           " through ", 
                           format(df[df$ind=="CORRA",]$date[nrow(df[df$ind=="CORRA",])], "%B %d, %Y")),
         caption = "Notes: CORRA = Canadian Overnight Repo Rate Average, TR = target rate. Source: Bank of Canada.") +
    scale_y_continuous(labels = percent, sec.axis = sec_axis(~., name = "", labels = percent)) +
    scale_fill_brewer(palette="Spectral") +
    theme(legend.text = element_text(size = 9),
          legend.title = element_blank(), 
          plot.caption = element_text(hjust = 0, size = 9)) +
    ggplot2::annotate("text", x = ad+30+5, y = tv+.00125, 
                      label =  "O/N target rate", size = 2, hjust = 0, color = "black") +
    geom_segment(aes(x = ad+30, y = tv+.00125, 
                     xend = ad, yend = tv), 
                 color="black", size=0.3, arrow = arrow(length = unit(0.15, "cm"))) +
    ggplot2::annotate("text", x = ad+30+5, y = hv+.00125, 
                      label =  "Upper bound of \nCORRA's operating band", size = 2, hjust = 0, color = "black") +
    geom_segment(aes(x = ad+30, y = hv+.00125, 
                     xend = ad, yend = hv), 
                 color="black", size=0.3, arrow = arrow(length = unit(0.15, "cm"))) +
    ggplot2::annotate("text", x = ad+30+5, y = lv+.00125, 
                      label =  "Lower bound", size = 2, hjust = 0, color = "black") +
    geom_segment(aes(x = ad+30, y = lv+.00125, 
                     xend = ad, yend = lv), 
                 color="black", size=0.3, arrow = arrow(length = unit(0.15, "cm"))); g
  
  return(g)
}

ir.corridor.mx <- function(d) {
  
  cpNote1 <- "TIIE = Interbank Equilibrium Interest Rate"
  
  s <- c("SF61745", "SF331451", "SF43773", "SF44073", "SF45470", "SF45471", "SF45472", "SF45473",
         "SG107", "SG28")
  l <- c("Target Rate",
         "O/N TIIE",
         "Bank funding rate", 
         "Mexibor 3m rate",
         "Cetes 28 days",
         "Cetes 91 days",
         "Cetes 182 days",
         "Cetes 364 days",
         "Primary Balance", "Subcidies & Transfers")
  
  d <- getSeriesData(s, "1950-01-01", Sys.Date())
  
  mx_tr  <- data.frame(d$SF61745, ind=l[1]);               mx_tr$value  <- mx_tr$value/100;  df <- mx_tr
  mx_onr <- data.frame(d$SF331451, ind=l[2]);              mx_onr$value <- mx_onr$value/100; df1 <- mx_onr
  mx_1m  <- data.frame(d$SF45470, ind=" 0.08 year (1m)");  mx_1m$value  <- mx_1m$value/100;  df2 <- mx_1m
  mx_3m  <- data.frame(d$SF45471, ind=" 0.25 year (3m)");  mx_3m$value  <- mx_3m$value/100;  df3 <- mx_3m
  mx_6m  <- data.frame(d$SF45472, ind=" 0.5 year (6m)");   mx_6m$value  <- mx_6m$value/100;  df4 <- mx_6m
  mx_1y  <- data.frame(d$SF45473, ind=" 1 year");          mx_1y$value  <- mx_1y$value/100;  df5 <- mx_1y
  mx_1y  <- mx_1y[mx_1y$value!=0,]
  
  mx_tr <- mx_tr[mx_tr$date>=as.Date("2010-01-01"),]
  mx_onr <- mx_onr[mx_onr$date>=as.Date("2010-01-01"),]
  
  g <- ggplot() +
    geom_line(data = mx_onr, mapping = aes(x=date,y=value,color=ind), 
              size=.8, alpha=1, linetype=1) +
    geom_line(data = mx_tr, mapping = aes(x=date,y=value), 
              col="black", size=.5, alpha=1, linetype=3) +
    labs(x="", y="",
         title = "Banco de Mexico's system of the interest rate targeting (%)",
         subtitle = paste0("History from ", 
                           format(mx_onr$date[1], "%B %d, %Y"),
                           " through ", 
                           format(mx_onr$date[nrow(mx_onr)], "%B %d, %Y")),
         caption = "Notes: TIIE = Interbank Equilibrium Interest Rate. Source: Banco de Mexico.") +
    scale_y_continuous(labels = percent, sec.axis = sec_axis(~., name = "", labels = percent)) +
    scale_fill_brewer(palette="Spectral") +
    theme(legend.text = element_text(size = 9),
          legend.title = element_blank(), 
          plot.caption = element_text(hjust = 0, size = 9)) +
    ggplot2::annotate("text", x = as.Date("2012-01-01")+60, 
                      y = mx_tr[mx_tr$date==as.Date("2012-01-01"),]$value +.005, 
                      label =  "O/N target rate\n (dotted line)", size = 2, hjust = 0, color = "black") +
    geom_segment(aes(x = as.Date("2012-01-01")+30, y = mx_tr[mx_tr$date==as.Date("2013-01-01"),]$value+.005, 
                     xend = as.Date("2012-01-01"), yend = mx_tr[mx_tr$date==as.Date("2013-01-01"),]$value), 
                 color="black", size=0.3, arrow = arrow(length = unit(0.15, "cm")))
  return(g) 
}

#
# Govt bond yields: Canada
#
ir.govt.ca <- function(d) {
  
  # BoC xml o/n rates
  # https://www.bankofcanada.ca/
  
  d_srt <- "1990-01-01"
  d_end <- Sys.Date()
  
  boc <- "https://www.bankofcanada.ca/valet/observations/"
  
  df <- data.frame()
  
  # Target for the O/N rate: V39079
  
  ind <- "V39079"
  u <- paste0(boc, ind, "/xml?recent_years=25")
  f <- xml2::read_xml(u)
  doc <- xml2::as_list(f)
  d <- lapply(doc$data$observations, 
              function(x) data.frame(Value = x$v[[1]], Indicator = attr(x$v, "s"), Date = attr(x, "d")))
  d <- dplyr::bind_rows(d)
  d$Date <- as.Date(d$Date)
  d$Value <- as.numeric(d$Value)
  head(d)
  
  df <- rbind(df, data.frame(date=d$Date, code=d$Indicator, value=d$Value/100, ind="O/N TR"))
  boc_tr <- data.frame(date=d$Date, Value=d$Value, Label="BoC O/N TR")
  
  # BoC Target Rate History ---------------------------------------------------------------------------
  boc_tr <- as.matrix(boc_tr)
  head(boc_tr)
  boc_tr_new <- data.frame(date=as.Date("2009-03-03"), Value=0.5, Label="BoC O/N TR")
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2009-04-21"), Value=1/4, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2009-01-20"), Value=1, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2008-12-09"), Value=1.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2008-10-21"), Value=2.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2008-09-03"), Value=3, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2008-04-22"), Value=3, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2008-03-04"), Value=3.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2008-01-22"), Value=4, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2007-12-04"), Value=4.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2007-07-10"), Value=4.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2006-05-24"), Value=4.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2006-04-25"), Value=4, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2006-03-07"), Value=3.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2006-01-24"), Value=3.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2005-12-06"), Value=3.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2005-10-18"), Value=3, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2005-09-07"), Value=2.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2004-10-19"), Value=2.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2004-09-08"), Value=2.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2004-04-13"), Value=2, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2004-03-02"), Value=2.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2004-01-20"), Value=2.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2003-09-03"), Value=2.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2003-07-15"), Value=3, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2003-04-15"), Value=3.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2003-03-04"), Value=3, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2002-07-16"), Value=2.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2002-06-04"), Value=2.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2002-04-16"), Value=2.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2002-01-15"), Value=2, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-11-27"), Value=2.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-10-23"), Value=2.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-09-17"), Value=3.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-08-28"), Value=4, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-07-17"), Value=4.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-05-29"), Value=4.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-04-17"), Value=5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-03-06"), Value=5.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2001-01-23"), Value=5.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2000-12-05"), Value=6, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2000-05-17"), Value=5.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2000-03-22"), Value=5.5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("2000-02-03"), Value=5.25, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("1999-11-17"), Value=5, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("1999-05-04"), Value=4.75, Label="BoC O/N TR"
  ))
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("1999-03-31"), Value=5, Label="BoC O/N TR"
  )) # prior rate was 5.25
  boc_tr_new <- rbind(boc_tr_new, data.frame(
    date=as.Date("1999-01-01"), Value=5.25, Label="BoC O/N TR"
  )) # prior rate was 5.25
  
  boc_tr_new <- boc_tr_new[order(boc_tr_new$date),]
  boc_tr <- rbind(boc_tr,as.matrix(boc_tr_new))
  boc_tr <- as.data.frame(boc_tr)
  boc_tr$date <- as.Date(boc_tr$date)
  boc_tr$Value <- as.numeric(boc_tr$Value)
  boc_tr <- boc_tr[order(boc_tr$date),]
  dtbl <- data.frame(date=as.Date(seq(boc_tr$date[1], boc_tr$date[nrow(boc_tr)], by="days")))
  boc_tr1 <- left_join(dtbl, boc_tr, by=c("date"))
  n<-nrow(boc_tr1)
  for (i in 1:n) {
    if (is.na(boc_tr1$Value[i])==FALSE) { #if rate is not NA then it has to be remembered for next i
      prev_r <- boc_tr1$Value[i]
      prev_l <- boc_tr1$Label[i]
    }
    if (is.na(boc_tr1$Value[i])==TRUE) { #if rate is NA then it has to be equal to previous rate value
      boc_tr1$Value[i] <- prev_r
      boc_tr1$Label[i] <- prev_l
    }
  }
  #head(boc_tr1)
  boc_tr <- boc_tr1
  
  # Treasury bills yields:
  # https://www.bankofcanada.ca/rates/interest-rates/t-bill-yields/selected-treasury-bill-yields-10-year-lookup/#:~:text=Selected%20Treasury%20Bill%20Yields:%2010-Year%20Lookup%20%20,%20%20V122529%20%207%20more%20rows
  # all data at once link:
  # https://www.bankofcanada.ca/rates/interest-rates/t-bill-yields/selected-treasury-bill-yields-10-year-lookup/?lookupPage=lookup_tbill_yields.php&startRange=2010-12-26&rangeType=dates&dFrom=2010-12-27&dTo=2020-12-25&rangeValue=1&rangeWeeklyValue=1&rangeMonthlyValue=1&series%5B%5D=LOOKUPS_V39063&series%5B%5D=LOOKUPS_V39065&series%5B%5D=LOOKUPS_V39066&series%5B%5D=LOOKUPS_V39067&submit_button=Submit
  # https://www.bankofcanada.ca/valet/observations/group/money_market/xml
  
  # u <- "https://www.bankofcanada.ca/stats/results//p_xml?rangeType=dates&lP=lookup_tbill_yields.php&sR=2010-12-26&se=L_V39063-L_V39065-L_V39066-L_V39067&dF=2010-12-27&dT=2020-12-25"
  u <- "https://www.bankofcanada.ca/valet/observations/group/money_market/xml"
  f <- xml2::read_xml(u)
  doc <- xml2::as_list(f)
  # data's main details
  Chart_Title2 <- doc$data$groupDetail$label[[1]]
  Chart_Sub_Title2 <- doc$data$groupDetail$description[[1]]
  Chart_Source2 <- doc$data$groupDetail$link[[1]]
  
  # Description of the items of the govt bill yields
  df <- data.frame()
  n <- length(doc$data$seriesDetail)
  for( i in 1:n){
    if (substr(doc$data$seriesDetail[i]$series$label,1,2)=="10" |   # 10 year
        substr(doc$data$seriesDetail[i]$series$label,1,2)=="Lo") {   # long-term
      temp <- data.frame(Label = doc$data$seriesDetail[i]$series$label,
                         Desc = doc$data$seriesDetail[i]$series$description,
                         ID = attr(doc$data$seriesDetail[i]$series, "id"))
    }
    if (substr(doc$data$seriesDetail[i]$series$label,1,2)!="10" &   # 10 year
        substr(doc$data$seriesDetail[i]$series$label,1,2)!="Lo") {   # long-term
      temp <- data.frame(Label = paste0(" ", doc$data$seriesDetail[i]$series$label),
                         Desc = doc$data$seriesDetail[i]$series$description,
                         ID = attr(doc$data$seriesDetail[i]$series, "id"))
    }
    rownames(temp) <- c("")
    colnames(temp) <- c("Label","Desc","ID")
    #print(temp)
    df <- rbind(df, temp)
  }
  #head(df)
  Data_Desc2 <- df
  
  # Data on Govt Bill Yields
  df <- data.frame()
  n <- length(doc$data$observations)
  for(i in 1:n){
    #print(paste0(i," ----------------------------------------------------------------"))
    k <- length(doc$data$observations[[i]])
    v <- doc$data$observations[[i]]
    rec <- data.frame()
    for (j in 1:k) {
      temp <- data.frame(value=v[j]$v[[1]], id=attr(v[j]$v, "s"))
      rownames(temp) <- c("")
      colnames(temp) <- c("Value","ID")
      #print(temp)
      rec <- rbind(rec, temp)
    }
    rec <- cbind(rec, date=attr(doc$data$observations[[i]], "d"))
    df <- rbind(df, rec)
  }
  #head(df)
  df$Value <- as.numeric(df$Value)
  df$date <- as.Date(df$date)
  Data_Bills <- df
  Data_Bills$Label <- ""
  Data_Bills[Data_Bills$ID=="AVG.INTWO",]$Label <- "O/N TR"
  Data_Bills[Data_Bills$ID=="CL.CDN.MOST.1DL",]$Label <- " O/N (1 day)"
  Data_Bills[Data_Bills$ID=="TB.CDN.30D.MID",]$Label <- " 0.08 year (1m)"
  Data_Bills[Data_Bills$ID=="TB.CDN.60D.MID",]$Label <- " 0.17 year (2m)"
  Data_Bills[Data_Bills$ID=="TB.CDN.90D.MID",]$Label <- " 0.25 year (3m)"
  Data_Bills[Data_Bills$ID=="TB.CDN.180D.MID",]$Label <- " 0.50 year (6m)"
  Data_Bills[Data_Bills$ID=="TB.CDN.1Y.MID",]$Label <- " 1 year"
  
  #Data_Ext2 <- left_join(Data_Hist_Bills, Data_Desc2, by=c("ID"))
  #head(Data_Ext2)
  
  #Data_Govt_Bills <- Data_Ext2[substr(Data_Ext2$Desc,1,14)=="Treasury Bills",]
  #Data_Govt <- Data_Govt[Data_Govt$Label=="2 year" | Data_Govt$Label=="3 year" | 
  #                         Data_Govt$Label=="5 year" | Data_Govt$Label=="7 year" | Data_Govt$Label=="10 year",]
  #head(Data_Govt_Bills)
  
  u <- "https://www.bankofcanada.ca/valet/observations/group/bond_yields_all/xml"
  f <- xml2::read_xml(u)
  doc <- xml2::as_list(f)
  
  # data's main details
  Chart_Title <- doc$data$groupDetail$label[[1]]
  Chart_Sub_Title <- doc$data$groupDetail$description[[1]]
  Chart_Source <- doc$data$groupDetail$link[[1]]
  
  # description of the items of the govt bond yields
  df <- data.frame()
  n <- length(doc$data$seriesDetail)
  for( i in 1:n){
    if (substr(doc$data$seriesDetail[i]$series$label,1,2)=="10" |   # 10 year
        substr(doc$data$seriesDetail[i]$series$label,1,2)=="Lo") {   # long-term
      temp <- data.frame(Label = doc$data$seriesDetail[i]$series$label,
                         Desc = doc$data$seriesDetail[i]$series$description,
                         ID = attr(doc$data$seriesDetail[i]$series, "id"))
    }
    if (substr(doc$data$seriesDetail[i]$series$label,1,2)!="10" &   # 10 year
        substr(doc$data$seriesDetail[i]$series$label,1,2)!="Lo") {   # long-term
      temp <- data.frame(Label = paste0(" ", doc$data$seriesDetail[i]$series$label),
                         Desc = doc$data$seriesDetail[i]$series$description,
                         ID = attr(doc$data$seriesDetail[i]$series, "id"))
    }
    rownames(temp) <- c("")
    colnames(temp) <- c("Label","Desc","ID")
    #print(temp)
    df <- rbind(df, temp)
  }
  #head(df)
  Data_Desc <- df
  
  # Data on Govt Bond Yield
  df <- data.frame()
  n <- length(doc$data$observations)
  for(i in 1:n){
    #print(paste0(i," ----------------------------------------------------------------"))
    k <- length(doc$data$observations[[i]])
    v <- doc$data$observations[[i]]
    rec <- data.frame()
    for (j in 1:k) {
      temp <- data.frame(value=v[j]$v[[1]], id=attr(v[j]$v, "s"))
      rownames(temp) <- c("")
      colnames(temp) <- c("Value","ID")
      #print(temp)
      rec <- rbind(rec, temp)
    }
    rec <- cbind(rec, date=attr(doc$data$observations[[i]], "d"))
    df <- rbind(df, rec)
  }
  #head(df)
  df$Value <- as.numeric(df$Value)
  df$date <- as.Date(df$date)
  Data_Hist <- df
  
  Data_Ext <- left_join(Data_Hist, Data_Desc, by=c("ID"))
  head(Data_Ext)
  
  Data_Govt <- Data_Ext[Data_Ext$Desc=="Government of Canada benchmark bond yields",]
  #Data_Govt <- Data_Govt[Data_Govt$Label=="2 year" | Data_Govt$Label=="3 year" | 
  #                         Data_Govt$Label=="5 year" | Data_Govt$Label=="7 year" | Data_Govt$Label=="10 year",]
  #head(Data_Govt)
  
  Data_Govt_Yields <- rbind(Data_Bills, Data_Govt[,c(1:4)])
  df <- Data_Govt_Yields
  
  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), 10)
  
  g <- 
    ggplot() +
    #geom_vline(xintercept=as.Date("2020-06-24"), color = "red", size=0.1) +
    geom_line(data = df[df$date>=as.Date("2001-01-02") & df$ID!="AVG.INTWO" & 
                          df$ID!="CL.CDN.MOST.1DL" & df$ID!="TB.CDN.60D.MID",], 
              mapping = aes(x=date, y=Value/100, color=Label)) +
    geom_line(data = boc_tr[boc_tr$date>=as.Date("2001-01-02"),], 
              mapping = aes(x=date, y=Value/100), color="black", size=.5, alpha=.6) +
    scale_color_manual(values = palette) + 
    #labels = c(" 1 month", " 3 month", " 6 month", " 1 year",
    #            " 2 year", " 3 year", " 5 year", " 7 year", "10 year", "Long-term")) +
    scale_x_date(date_breaks = "2 year", date_minor_breaks = "2 year", date_labels = "%Y") + 
    scale_y_continuous(labels = percent, sec.axis = sec_axis(~., name = "", labels = percent)) +
    labs(title = paste("Yields on Treasury Bills and Government of Canada Bonds Yields (%)"), 
         subtitle = paste("History from January 2, 2001", 
                          " through ", format(max(df$date), "%B %d, %Y")),
         x = "", y = "", 
         caption = paste0("Notes: m = month(s). Source: Bank of Canada, Investing.com.")) + 
    theme(axis.title.y = element_text(size = 9),
          legend.title = element_blank(), 
          legend.background=element_blank(),
          legend.position = "right", plot.caption = element_text(hjust = 0, size = 8)) +
    guides(color = guide_legend(reverse=T)) +
    ggplot2::annotate("text", x = as.Date("2007-01-01"), y = .01, #boc_tr[boc_tr$date==boc_tr$date[1],]$Value/100,
                      label =  "BoC's target \nfor overnight rate", 
                      size = 2, hjust = 1, color = "black") +
    geom_segment(aes(x = as.Date("2007-01-01"), y = .01,
                     xend = as.Date("2008-12-15"), 
                     yend = boc_tr[boc_tr$date==as.Date("2008-12-15"),]$Value/100), 
                 color="black", size=0.1)
  
  return(g)
}

ir.govt.mx <- function(d) {
  
  s <- c("SF61745", "SF331451", "SF43773", "SF44073", "SF45470", "SF45471", "SF45472", "SF45473",
         "SG107", "SG28")
  l <- c("Target Rate",
         "O/N TIIE",
         "Bank funding rate", 
         "Mexibor 3m rate",
         "Cetes 28 days",
         "Cetes 91 days",
         "Cetes 182 days",
         "Cetes 364 days",
         "Primary Balance", "Subcidies & Transfers")
  
  d <- getSeriesData(s, "1950-01-01", Sys.Date())
  
  mx_tr  <- data.frame(d$SF61745, ind=l[1]);               mx_tr$value  <- mx_tr$value/100;  df <- mx_tr
  mx_onr <- data.frame(d$SF331451, ind=l[2]);              mx_onr$value <- mx_onr$value/100; df1 <- mx_onr
  mx_1m  <- data.frame(d$SF45470, ind=" 0.08 year (1m)");  mx_1m$value  <- mx_1m$value/100;  df2 <- mx_1m
  mx_3m  <- data.frame(d$SF45471, ind=" 0.25 year (3m)");  mx_3m$value  <- mx_3m$value/100;  df3 <- mx_3m
  mx_6m  <- data.frame(d$SF45472, ind=" 0.5 year (6m)");   mx_6m$value  <- mx_6m$value/100;  df4 <- mx_6m
  mx_1y  <- data.frame(d$SF45473, ind=" 1 year");          mx_1y$value  <- mx_1y$value/100;  df5 <- mx_1y
  mx_1y  <- mx_1y[mx_1y$value!=0,]
  
  #mx_tr <- mx_tr[mx_tr$date>=as.Date("2010-01-01"),]
  #mx_onr <- mx_onr[mx_onr$date>=as.Date("2010-01-01"),]
  
  df <- rbind(mx_1m, mx_3m, mx_6m, mx_1y) #, mx_3y, mx_5y, mx_7y, mx_10y, mx_30y)
  
  # Google Drive Link to csv file with Mexico Govt Bond Yields:
  u <- "https://drive.google.com/uc?export=download&id=1ShSPB_qr1JWR9yWmRo35xwurUHvuOTHU"
  t <- read.csv(u, header = TRUE)
  t$value <- t$value/100
  t <- t[t$value!=0,]
  
  df <- rbind(df, t)
  
  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), 10)
  
  g <- 
    ggplot() +
    geom_line(data = df[df$date>=mx_tr$date[1],], 
              mapping = aes(x=date, y=value, color=ind)) +
    geom_line(data = mx_tr, 
              mapping = aes(x=date, y=value), color="black", size=.5, alpha=.6) +
    scale_color_manual(values = palette) + 
    scale_x_date(date_breaks = "3 year", date_minor_breaks = "3 year", date_labels = "%Y") + 
    scale_y_continuous(labels = percent, sec.axis = sec_axis(~., name = "", labels = percent)) +
    labs(title = paste("Yields on the Government of Mexico Bonds & Banco de México's Target Rate (%)"), 
         subtitle = paste("Daily history from ", format(mx_tr$date[1], "%B %d, %Y"), 
                          " through ", format(mx_tr$date[length(mx_tr$date)], "%B %d, %Y")),
         x = "", y = "", 
         caption = paste0("Notes: BdM = Banco de México, m = month(s). Source: Banco de México, Investing.com.")) + 
    theme(axis.title.y = element_text(size = 9),
          legend.title = element_blank(), 
          legend.background=element_blank(),
          legend.position = "right", plot.caption = element_text(hjust = 0, size = 8)) +
    guides(color = guide_legend(reverse=T)) +
    #annotate("text", x = as.Date("2020-06-24") - 50, y = .05,
    #         label =  "Red vertical line indicates the day (June 24, 2020),\n when Fitch Ratings downgraded Canada's \nsovereign rating from AAA to AA+ \n'citing the effects of Covid-19 on \ngovernment finances'", 
    #         size = 2, hjust = 1, color = "red") +
    ggplot2::annotate("text", x = as.Date("2010-01-01"), y = mx_tr[mx_tr$date==as.Date("2010-01-01"),]$value-0.01, 
                      label =  "BdM's target \nfor overnight rate", 
                      size = 2, hjust = 1, color = "black") +
    geom_segment(aes(x = as.Date("2010-01-01"), y = mx_tr[mx_tr$date==as.Date("2010-01-01"),]$value-0.01,
                     xend = as.Date("2010-07-01"), 
                     yend = mx_tr[mx_tr$date==as.Date("2010-07-01"),]$value), 
                 color="black", size=0.1); g
  return(g)
}

ir.govt.tr <- function(d) {
  
  tcmb.rates <- data.frame()
  
  # 1 Week Repo ------------------------------------------------------------------+
  pageHTML <- getURL("https://tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Main+Menu/Core+Functions/Monetary+Policy/Central+Bank+Interest+Rates/1+Week+Repo",
                     ssl.verifyPeer=FALSE)
  t <- readHTMLTable(pageHTML, header = TRUE)
  #n <- t$midTable[1,] # names of columns
  #d <- t$midTable[c(2:nrow(t$midTable)),]
  #colnames(d) <- n
  d<-t$midTable
  d$DATE <- as.Date(d$DATE, format = "%d.%m.%Y")
  d$Borrowing <- as.numeric(sub("-","",d$Borrowing))
  d$Lending <- as.numeric(sub(",",".",d$Lending))
  head(d);tail(d)
  
  #tcmb.rates <- rbind(tcmb.rates,
  #                    daily.hist(d$DATE,d$Borrowing,"1W Repo (reverse)"))
  tcmb.rates <- rbind(tcmb.rates,
                      daily.hist(d$DATE,d$Lending,"1W Repo"))
  tcmb.rates$value <- tcmb.rates$value/100
  
  # https://drive.google.com/file/d/1MAYh2-V7Wo-vVqnv_SqT3bhBiKqwAxBs/view?usp=sharing
  # Google Drive Link to csv file with Mexico Govt Bond Yields:
  u <- "https://drive.google.com/uc?export=download&id=1MAYh2-V7Wo-vVqnv_SqT3bhBiKqwAxBs"
  t <- read.csv(u, header = TRUE)
  t$value <- t$value/100
  t$date <- as.Date(t$date)
  t <- t[t$value!=0,]
  
  tcmb.rates <- rbind(tcmb.rates, t)
  df <- tcmb.rates[tcmb.rates$date>=as.Date("2010-01-01"),]
  
  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), 10)
  
  g <- 
    ggplot() +
    geom_line(data = df[df$ind!="1W Repo",], 
              mapping = aes(x=date, y=value, color=ind)) +
    geom_line(data = df[df$ind=="1W Repo",], 
              mapping = aes(x=date, y=value), color="black", size=.7, alpha=.6) +
    scale_color_manual(values = palette) + 
    scale_x_date(date_breaks = "3 year", date_minor_breaks = "3 year", date_labels = "%Y") + 
    scale_y_continuous(labels = percent, sec.axis = sec_axis(~., name = "", labels = percent)) +
    labs(title = paste("Yields on the Government of Turkey Bonds & Turkey Central Bank's Target Rate (%)"), 
         subtitle = paste("Daily history from ", format(min(df$date), "%B %d, %Y"), 
                          " through ", format(max(df$date), "%B %d, %Y")),
         x = "", y = "", 
         caption = paste0("Notes: m = month(s). Source: Central Bank of Turkey, Investing.com.")) + 
    theme(axis.title.y = element_text(size = 9),
          legend.title = element_blank(), 
          legend.background=element_blank(),
          legend.position = "right", plot.caption = element_text(hjust = 0, size = 8)) +
    guides(color = guide_legend(reverse=T)) +
    #annotate("text", x = as.Date("2020-06-24") - 50, y = .05,
    #         label =  "Red vertical line indicates the day (June 24, 2020),\n when Fitch Ratings downgraded Canada's \nsovereign rating from AAA to AA+ \n'citing the effects of Covid-19 on \ngovernment finances'", 
    #         size = 2, hjust = 1, color = "red") +
    ggplot2::annotate("text", x = as.Date("2017-01-01"), 
                      y = df[df$ind=="1W Repo" & df$date==as.Date("2017-01-01"),]$value-0.035, 
                      label =  "Turkey central bank's target \nfor overnight rate", 
                      size = 2, hjust = 1, color = "black") +
    geom_segment(aes(x = as.Date("2017-01-01"), 
                     y = df[df$ind=="1W Repo" & df$date==as.Date("2017-01-01"),]$value-0.035,
                     xend = as.Date("2017-07-01"), 
                     yend = df[df$ind=="1W Repo" & df$date==as.Date("2017-07-01"),]$value), 
                 color="black", size=0.1); g
  
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
use.library("RCurl")
use.library("XML")
use.library("xml2")
use.library("dplyr")
use.library("openxlsx")
use.library("siebanxicor")

install_github("mikeasilva/blsAPI")  # Installing R library for BLS

# Installing R library for Turkish central bank data (CBRT)
#install.packages("http://users.metu.edu.tr/etaymaz/files/CBRT_0.1.0.tar.gz",
#                 repos = NULL, type = "source")
install_github("etaymaz/CBRT")
use.library("CBRT")
tcmb.api.key <- "m8AR45yMoO" # for the Python package

# Mexico central bank
bmx_token <- "5424470c741ea53875355570ba530591de4ee1e114b5015fa564ff34d491a5df"
siebanxicor::setToken(bmx_token)

# --- US ECONOMY ---

# Item codes for CPI details: https://download.bls.gov/pub/time.series/cu/cu.item
# These are to be retrieved into the data set called cpi.items
cpi.items <- read.csv("https://download.bls.gov/pub/time.series/cu/cu.item", 
                      header = TRUE, sep = "\t")
