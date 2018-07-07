library(data.table)
library(lubridate)

init <- function() {
  # uncomment the below with your preferred location
  setwd("c:/shared/ExData_plotting1/")
  dirname = "scratch"
  if (!dir.exists(dirname)) {dir.create(dirname)}
  fname <- "pc.zip"
  setwd(dirname)
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  if (!file.exists(fname)) {
    download.file(url, fname, mode="wb")
    unzip(fname)
  }
}

loadData <- function() {
  data_fname <- "household_power_consumption.txt"
  sampleData <- fread(data_fname, nrows=5)
  # because we won't load the first 60k rows, we use this sample to get the column names
  x <- sapply(sampleData, class)
  df <- fread(data_fname, skip=60000, nrows=10000, col.names=names(x), na.strings="?")
}

processData <- function(df) {
  # we only care about those 2 specific days - 1st and 2nd of Feb 2007
  Sys.setenv(TZ="GMT")
  df <- df[, datetime:=dmy_hms(paste(Date, Time, sep=" "))]
  dd <- df[, Date:=dmy(Date)]
  dd[Date <= ymd("20070202") & Date>=ymd("20070201")]
}

graph1 <- function(df) {
  fname = "plot1.png"
  png(fname, width=480, height=480)
  hist(dd$Global_active_power, col="red", xlab="Global Active Power (kilowats)", ylab="Frequency", main="Global Active Power")
  dev.off()
}

init()
df <- loadData()
pd <- processData(df)
graph1(pd)
