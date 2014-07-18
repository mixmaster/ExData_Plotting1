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
plot4 <- function(filename) {
    ## Get the data
    dt <- load.data(filename)
    datetime <- strptime(paste(dt[[1]], dt[[2]]), "%d/%m/%Y %T")
    
    ## Prepare the graphic device
    png(filename="plot4.png", width=480, height=480)
    par(mfcol=c(2,2))
    
    ## Global Active Power
    plot(datetime, dt[[3]], type="l", xlab="", ylab="Global Active Power")
    
    ## Energy sub metering
    plot(datetime, dt[[7]], type="l", col="black", xlab="", ylab="Energy sub metering")
    legend("topright", bty="n", lty=1, col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    lines(datetime, dt[[8]], col="red")
    lines(datetime, dt[[9]], col="blue")
    
    ## Voltage
    plot(datetime, dt[[5]], type="l", xlab="datetime", ylab="Voltage")
    
    ## Global Reactive Power
    plot(datetime, dt[[4]], type="l", xlab="datetime", ylab="Global_reactive_power")
    
    ## Close the graphic device
    dev.off()
}

## To make this script run, the data file has to be named
## "household_power_consumption.txt" and placed in the working
## directory. Otherwise, just call plot4() with correct path and
## name of the data file.
plot4("household_power_consumption.txt")
