plot4 <- function() {
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
    plot4_data <- subset(hpc, Date >= "2007-02-01" & Date <= "2007-02-02")
    rm(hpc)
    
    # Concatenate 'Date' and 'Time' in a new variable 'Datetime'
    plot4_data$Datetime <- as.POSIXct(paste(plot4_data$Date, plot4_data$Time))
    
    # Sets 2 rows and 2 columns
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    
    # Add 1st graph
    Sys.setlocale(category = "LC_ALL", locale = "english")
    with(plot4_data, plot(Global_active_power~Datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
    
    # Add 2nd graph
    with(plot4_data, plot(Voltage~Datetime, type = "l", ylab = "Voltage", xlab = ""))
    
    #Add 3rd graph
    with(plot4_data, plot(Sub_metering_1~Datetime, type="l", ylab = "Global Active Power (kilowatts)", xlab = ""))
    with(plot4_data, lines(Sub_metering_2~Datetime, col = "red"))
    with(plot4_data, lines(Sub_metering_3~Datetime, col = "blue"))
    with(plot4_data, legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
                            lty = 1, lwd = 2, bty = "n"))
    
    # Add 4th graph
    with(plot4_data, plot(Global_reactive_power~Datetime, type="l", ylab = "Global Reactive Power (kilowatts)", xlab = ""))
    
    # Save graph as a PNG file
    dev.copy(png, file = "plot4.png", height = 480, width = 480)
    dev.off()
}