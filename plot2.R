library(data.table)

## This function load the data into a data.table
load.data <- function(filename) {
    ## Determine the start and length of the target rows
    dt <- (fread(filename, sep=";", header=TRUE, na.strings="?", colClasses=list(character=1, NULL=2:9)))$Date
    start <- match("1/2/2007", dt)
    end <- max(which(dt == "2/2/2007"))
    len <- end - start + 1
    
    ## Read in the target data and return
    fread(filename, sep=";", header=TRUE, na.strings="?", nrows=len, skip=start-1)
}

## This function create the plot into the PNG file
plot2 <- function(filename) {
    ## Get the data and create the plot file
    dt <- load.data(filename)
    datetime <- strptime(paste(dt[[1]], dt[[2]]), "%d/%m/%Y %T")
    png(filename="plot2.png", width=480, height=480)
    plot(datetime, dt[[3]], type="l", xlab="", ylab="Global Active Power (kilowatts)")
    dev.off()
}

## To make this script run, the data file has to be named
## "household_power_consumption.txt" and placed in the working
## directory. Otherwise, just call plot2() with correct path and
## name of the data file.
plot2("household_power_consumption.txt")
