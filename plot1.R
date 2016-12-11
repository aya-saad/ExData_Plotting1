library(lubridate)
# download the data for the project
filedownload <- function(){
  destfile="exdata%2Fdata%2Fhousehold_power_consumption.zip"
  fileURL <-
    "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"   
  if (!file.exists(destfile)) {
    download.file(fileURL ,destfile,method="auto") }
  else { txt <- message("file already exists in the current directory!") }
}


# read file in the data zip file passing the date range from - to
fileRead <- function(from_date = "", to_date = ""){
  if(from_date == "" || to_date == "" ){ stop("missing arguments: must enter the date range")}
  file_zip = "exdata%2Fdata%2Fhousehold_power_consumption.zip"
  if(!file.exists(file_zip)){ stop("Coudn't find zip file in the current directory")}
  file_name <- "household_power_consumption.txt"
  num_of_days <- as.numeric(as.Date(to_date) - as.Date(from_date))
  if(num_of_days < 0) { stop("Number of days is negative") }
  if (num_of_days > 0) x <- c(0:num_of_days)
  else x <- c(0:0)
  
  for(n in x){
    d <- as.Date(from_date) + n
    d1 <- paste(as.numeric(format(as.Date(d), "%d")),as.numeric(format(as.Date(d), "%m")),as.numeric(format(as.Date(d), "%Y")), sep = "/")
    d2 <- paste("^", d1, sep = "")
    if(is.integer(list))
      list <- append(list, grep(d2,readLines(unz(file_zip, file_name))))
    else list <- grep(d2,readLines(unz(file_zip, file_name)))
  }
  if(!is.integer(list)) { stop("No data is collected for the dates given")}
  num_of_rows <- tail(list, 1) - head(list, 1)
  col_names <- read.table(unz(file_zip, file_name), header = TRUE, nrows = 1, sep = ";")
  data <- read.table(unz(file_zip, file_name), header = FALSE, skip = (head(list,1)-1), nrow = num_of_rows+1, sep = ";", col.names = names(col_names))
}

# plot1
plot1 <- function(){
  # download the data of the project
  filedownload() 
  # read the data from just the given dates
  dat <- fileRead("2007-2-1", "2007-2-2")
  # start plotting
  hist(dat$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
  # copy plot to the png file
  dev.copy(png, file = "plot1.png", width = 480, height = 480) 
  # turn-off the device
  dev.off()
  
  closeAllConnections()
  rm(list=ls())
}
