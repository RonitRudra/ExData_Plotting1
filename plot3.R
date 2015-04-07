plot3<-function(){
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
  
  png(filename = "plot3.png",width = 480,                           ## Set parameters for saving plot as png
      height = 480, units = "px", pointsize = 12,                   ## File is saved as 480x480 pixel png in current Working directory
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"))
  
  with(df,plot(datetime,Sub_metering_1,type="l",xlab="",            ## Line plot of Sub Metering 1
               ylab="Energy sub metering"))                   
  with(df,lines(datetime,Sub_metering_2,type="l",col="red"))        ## Overlay plot of Sub Metering 2 in red
  with(df,lines(datetime,Sub_metering_3,type="l",col="blue"))       ## Overlay plot of Sub Metering 2 in blue
  legend("topright",                                                ## Add legend to topright of plot
         legend=c("Sub_Metering_1","Sub_Metering_2","Sub_Metering_3"),
         lty=c(1,1,1),lwd=c(2.5,2.5,2.5),
         col=c("black","red","blue"),cex=.5)
  
  dev.off()                                                         ## Exit current graphic device                         
  
  setwd("..")
  message("The png file plot3 was successfully saved")
}