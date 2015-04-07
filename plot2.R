plot2<-function(){
  library(dplyr)                                                    ## Include dplyr library
  
  setwd("Exploratory Data Analysis/")                               ## Directory is a sub-directory of working directory
  
  df <- read.table("household_power_consumption.txt",
                   header = TRUE,sep = ";",
                   stringsAsFactors = FALSE,na.strings = "?")       ## Read entire dataset
  
  df <- df%>%mutate(datetime=paste(Date,Time,sep=" "))%>%           ## Merge Date and Time into one Variable 
    select(datetime,Global_active_power:Sub_metering_3)             ## Select required columns   
  
  df$datetime <- strptime(df$datetime,format="%d/%m/%Y %H:%M:%S")   ## Convert datetime column to Date-Time class
  
  df<-df[df$datetime>="2007-02-01 00:00:00" &                       ## Select data from 1st Feb 2007 to 3rd Feb 2007
           df$datetime<="2007-02-03 00:00:00",] 
  
  png(filename = "plot2.png",width = 480,                           ## Set parameters for saving plot as png
      height = 480, units = "px", pointsize = 12,                   ## File is saved as 480x480 pixel png in current Working directory
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"))
  
  plot(df$datetime,df$Global_active_power,type="l",                 ## Line plot for Global Active power vs datetime
       xlab="datetime",ylab="Global Active Power (kilowatts)")      ## This plot is saved as the active graphic device
  
  dev.off()                                                         ## Exit current graphic device
  
  setwd("..")
  message("The png file plot2 was successfully saved")
}