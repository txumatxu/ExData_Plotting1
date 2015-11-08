################################################################################
#
# Function plot1 
#
# This function generates the first of the plots required by the first project
# of the exploratory data analysis course. It draws a histogram with where the 
# values of the Global Active Power are shown.
# 
#
# This function requires: The function doesn't take any input argument, but requires
#                         a copy of the household power consumption file. If the file
#                         is not located in the working directory it checks for the
#                         zip file that contains this information and if the file
#                         is not available it proceeds downloading the file from the
#                         given URL.
#
# This function returns: It creates a file called plot1.png in the working directory.
#                        This plot contains a histogram of Global Active Power.
#
# Execution instructions: Load source and Execute the script from your R console as follows:
#
#                               > source("plot1.R")
#                               > plot1()
#
#
## Author: Jose Maria Echevarria Fernandez
## Date: 20151108
## Related: Coursera,Johns Hopkins University,Exploratory Data Analysis,Data Science

plot1<- function() {
        # Check if required files exist or download them
        if(!file.exists("reducedData.txt")){
                if(!file.exists("household_power_consumption.txt")){
                        if(!file.exists("household_power_consumption.zip")){
                                if(Sys.info()['sysname']=="Darwin"){
                                        #Mac users require to set download method to curl
                                        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip",method = "curl") 
                                        
                                }else{
                                        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip") 
                                }
                               
                        }
                        # unzip the downloaded file
                        unzip("household_power_consumption.zip")
                }
                # A new file is created containing only the required information
                # the required information corresponds to the data of two especific dates.
                
                dataToSave <- grep("^[12]/2/2007|^Date",
                                   readLines("household_power_consumption.txt"),
                                   value = TRUE)
                writeLines(dataToSave,"reducedData.txt")
        }
        # load reduced dataset to data frame
        usedData <- read.csv("reducedData.txt",header = TRUE,sep=";")
        
        # PNG is selected as Graphing Device.
        png(filename = "plot1.png",width = 480, height = 480, units = "px")
        # Histogram is drawn 
        hist(usedData$Global_active_power,col = "red",
             xlab = "Global Active Power (kilowatts)",
             main = "Global Active Power")
        # The graphing device is closed.
        dev.off()
}