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
    hist( data$Global_active_power, col = c( "red" ), 
          xlab = "Global Active Power (kilowatts)",
          main = "Global Active Power")
}

savePlotToFile <- function( data ) {
    png( "plot1.png", width = 480, height = 480, units = "px" )
    createPlot( data )
    dev.off()
}

initDataFile()
data <- getFilteredData()
savePlotToFile( data )