plot3 <- function() {
    # Download the data file if it is not available
    if (!file.exists("household_power_consumption.txt")) {
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        path <- file.path(getwd(), "household_power_consumption.zip")
        download.file(url, path, mode = "wb")
        unzip("household_power_consumption.zip")
    }
    
    # Reads dataset
    hpc <- read.csv2("household_power_consumption.txt", stringsAsFactors = FALSE, na.strings = "?")
    
    # Converts 'Date' variable to date
    hpc$Date <- as.Date(hpc$Date, format = "%d/%m/%Y")
    
    # Subset the dataset for the considered period: 2007-02-01 & 2007-02-02
    plot3_data <- subset(hpc, Date >= "2007-02-01" & Date <= "2007-02-02")
    rm(hpc)
    
    # Concatenate 'Date' and 'Time' in a new variable 'Datetime'
    plot3_data$Datetime <- as.POSIXct(paste(plot3_data$Date, plot3_data$Time))
    
    # Plot the graph
    Sys.setlocale(category = "LC_ALL", locale = "english")
    with(plot3_data, plot(Sub_metering_1~Datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
    with(plot3_data, lines(Sub_metering_2~Datetime, col = "red"))
    with(plot3_data, lines(Sub_metering_3~Datetime, col = "blue"))
    legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_1"), lty = 1, lwd = 2)
    
    # Save graph as a PNG file
    dev.copy(png, file = "plot3.png", height = 480, width = 480)
    dev.off()
}