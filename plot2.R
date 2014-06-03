plot2 <- function() {
    root <- "exdata-data-household_power_consumption"
        
    ## Read data set
    path <- c(root, "household_power_consumption.txt")
    path <- paste(path, collapse = "/")
    data <- read.csv2(path, header = TRUE, na.strings = '?',
                      colClasses = "character")
    
    # Numeric conversion
    idx <- grep("Date|Time", colnames(data))
    data[, -idx] <- sapply(data[, -idx], as.numeric)
    
    data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
    data <- subset(data, DateTime >= as.POSIXlt("2007-02-01 00:00:00") & 
                         DateTime <  as.POSIXlt("2007-02-03 00:00:00"))
    
    # Set Plot Language for date and times
    Sys.setlocale("LC_TIME", "English")
    
    # plot
    png(filename = "plot2.png", bg = "transparent")
    plot(data$DateTime, data$Global_active_power, type = "l",
         xlab = "",
         ylab = "Global Active Power (kilowatts)")
    dev.off()
}