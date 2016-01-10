initDataFile <- function() {
    if ( !dir.exists( "data" ) ) {
        url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
        download.file( url, destfile = "data.zip" )
        
        dir.create( "data" )
        unzip( "data.zip", exdir = "data" )
        file.remove( "data.zip" )
    }
}

getFilteredData <- function() {
    data <- read.csv( "data/household_power_consumption.txt", sep = ";", 
                      colClasses = c( rep( "character", 2 ), rep( "numeric", 7 ) ),
                      na.strings = c( "?" ) ) 
    data$str_date <- paste( data$Date, data$Time, sep = " " ) 
    data$date <- strptime( data$str_date, format="%d/%m/%Y %H:%M:%S")
    
    filtered <- subset( data, data$date < strptime( "2007-02-03", "%Y-%m-%d" ) & 
                        data$date > strptime( "2007-02-01", "%Y-%m-%d" ) )
    
    filtered
}

createPlot <- function( data ) {
    Sys.setlocale( "LC_TIME", "C" )
    
    plot( data$date, data$Sub_metering_1, type = "n", 
          xlab = "", ylab = "Energy sub metering", main = "" )
    
    lines( data$date, data$Sub_metering_1, col = "black" )
    lines( data$date, data$Sub_metering_2, col = "red" )
    lines( data$date, data$Sub_metering_3, col = "blue" )
    
    legend( "topright", 
            lwd = 2, cex = 1.2,
            col = c( "black", "red", "blue" ),
            legend = c( "Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ) )
}

savePlotToFile <- function( data ) {
    png( "plot3.png", width = 480, height = 480, units = "px" )
    createPlot( data )
    dev.off()
}

initDataFile()
data <- getFilteredData()
savePlotToFile( data )