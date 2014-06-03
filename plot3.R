plot3 <- function() {
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
    png(filename = "plot3.png", bg = "transparent")
    plot(data$DateTime, data$Sub_metering_1, type = "l",
         xlab = "",
         ylab = "Energy sub metering")
    lines(data$DateTime, data$Sub_metering_2, col = "red")
    lines(data$DateTime, data$Sub_metering_3, col = "blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty = c(1, 1, 1), col = c("black", "red", "blue"),
           pt.cex = 1, cex = 1)
    dev.off()
}