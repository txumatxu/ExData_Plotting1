################################################################################
#
# Function plot3
#
# This function generates the third of the plots required by the first project
# of the exploratory data analysis course. It draws a plot where the evolution
# of the three power submetering is shown (in watt-hour of active energy).
# 
# energy sub-metering No. 1 .It corresponds to the kitchen (hot plates are not electric but gas powered).
# energy sub-metering No. 2 . It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# energy sub-metering No. 3 .It corresponds to an electric water-heater and an air-conditioner.
# 
#
# This function requires: The function doesn't take any input argument, but requires
#                         a copy of the household power consumption file. If the file
#                         is not located in the working directory it checks for the
#                         zip file that contains this information and if the file
#                         is not available it proceeds downloading the file from the
#                         given URL.
#                         
#                         The function also requires the library plyr to installed
#                         on the system as it uses the function mutate.
#
# This function returns: It creates a file called plot3.png in the working directory.
#                        This plot contains the graph previously described.
#
# Execution instructions: Load source and Execute the script from your R console as follows:
#
#                               > source("plot3.R")
#                               > plot3()
#
#
## Author: Jose Maria Echevarria Fernandez
## Date: 20151108
## Related: Coursera,Johns Hopkins University,Exploratory Data Analysis,Data Science
plot3<- function() {
        library(plyr)
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
                dataToSave <- grep("^[12]/2/2007|^Date",readLines("household_power_consumption.txt"),value = TRUE)
                writeLines(dataToSave,"reducedData.txt")
        }
        # Locale is set to be English so that the time labels in the plot are
        # shown in English and not dependant on the machines Locale. Locale is
        # restored to it's original value at the end of this script.
        curTimeLocale <- Sys.getlocale("LC_TIME")
        Sys.setlocale("LC_TIME", "English")
        
        # load reduced dataset to data frame
        usedData <- read.csv("reducedData.txt",header = TRUE,sep=";")
        
        # PNG is selected as Graphing Device.
        png(filename = "plot3.png",width = 480, height = 480, units = "px")
        
        # A field called datetime is computed from the existing Date and Time
        usedData<-mutate(usedData,DateTime = paste(Date,Time,sep=" "))
        
        # The plot area is configured but no drawing is done (type="n").
        # strptime is used so that the field datetime is converted from string
        # to a more appropiate POSIXlt
        # The function lines is used to include each of the three lines to be
        # drawn in the plotting area. Each of which uses different colour values.
        plot(strptime(usedData$DateTime,"%d/%m/%Y %H:%M:%S"),usedData$Global_active_power,ylim=c(0, 38),ylab = 'Energy sub metering',xlab="",type="n")
        lines(strptime(usedData$DateTime,"%d/%m/%Y %H:%M:%S"),usedData$Sub_metering_1)
        lines(strptime(usedData$DateTime,"%d/%m/%Y %H:%M:%S"),usedData$Sub_metering_2, col="red")
        lines(strptime(usedData$DateTime,"%d/%m/%Y %H:%M:%S"),usedData$Sub_metering_3, col="blue")
        
        # The legend is added to the top right of the plot
        legend("topright",lwd=1, lty=c(1,1,1), col = c("black","red","blue"),legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        
        # The graphing device is closed.
        dev.off()
        # The systems time locale is restored to previous value.
        Sys.setlocale("LC_TIME", curTimeLocale)
}