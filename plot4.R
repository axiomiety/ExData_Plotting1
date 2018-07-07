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

graph4 <- function(df) {
  fname = "plot4.png"
  png(fname, width=480, height=480)
  par(mfrow=c(2,2))
  # from left to right, top to bottom
  # using `with` allows us to directly reference the data frame's names, which will be used as labels
  with(df, {
    hist(Global_active_power, col="red", xlab="Global Active Power (kilowats)", ylab="Frequency", main="Global Active Power")
    plot(datetime,Voltage,xlab="datetime", ylab="Voltage", type="l")
    plot(datetime, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    lines(datetime, Sub_metering_2, type="l", col="red")
    lines(datetime, Sub_metering_3, type="l", col="blue")
    legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=1, col=c("black","red","blue"))
    plot(datetime, Global_reactive_power, type="l")
  })
  dev.off()
}

init()
df <- loadData()
pd <- processData(df)
graph4(pd)
