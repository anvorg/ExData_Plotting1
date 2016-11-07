plot1 <- function() {
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
    plot1_data <- subset(hpc, Date >= "2007-02-01" & Date <= "2007-02-02")
    rm(hpc)
    
    # Converts 'Global_active_power' to numeric
    plot1_data$Global_active_power <- as.numeric(plot1_data$Global_active_power)
    
    # Plot the histogram graph
    hist(plot1_data$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", 
         ylab = "Frequency", col = "red")
    
    # Save graph as a PNG file
    dev.copy(png, file = "plot1.png", height = 480, width = 480)
    dev.off()
}