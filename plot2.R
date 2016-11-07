plot2 <- function() {
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
    plot2_data <- subset(hpc, Date >= "2007-02-01" & Date <= "2007-02-02")
    rm(hpc)
    
    plot2_data$Datetime <- as.POSIXct(paste(plot2_data$Date, plot2_data$Time))
    
    # Plot the graph
    Sys.setlocale(category = "LC_ALL", locale = "english")
    with(plot2_data, plot(Global_active_power~Datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
    
    # Save graph as a PNG file
    dev.copy(png, file = "plot2.png", height = 480, width = 480)
    dev.off()
}