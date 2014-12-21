#Download the file:
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
location<-"F://Assignment2//Assignment2.zip"
download.file(url,location)

#List the unzipped files:
unzip(zipfile="F://Assignment2//Assignment2.zip", exdir="F://Assignment2")
list.files("F://Assignment2//", recursive=TRUE)

#Assign the tables:
NEI<-readRDS("F://Assignment2//summarySCC_PM25.rds")
SCC<-readRDS("F://Assignment2//Source_Classification_Code.rds")

##########
##Plot 1##
##########

#Group the emission information by year:
emissions1<-aggregate(NEI$Emissions, by=list(NEI$year), FUN=sum)
colnames(emissions1)<-c("Year", "Emissions")

#Create the plot:
png(file="plot1.png", width=480, height=480)
plot(emissions1$Year, emissions1$Emissions, type="l", xlab="Year", ylab="Emissions", col="blue",
     main="Plot of Emissions by Year")
dev.off()

##########
##Plot 2##
##########

#Subset the relevant are and years from the dataset: 
NEI2<-NEI[NEI$fips %in% "24510",]
NEI3<-NEI2[NEI2$year >= "1999" & NEI2$year <="2008",]

#Group the emission information by year:
emissions2<-aggregate(NEI3$Emissions, by=list(NEI3$year), FUN=sum)
colnames(emissions2)<-c("Year","Emissions")

#Create the graph:
png(file="plot2.png", width=480, height=480)
plot(emissions2$Year, emissions2$Emissions, type="l", xlab="Year", ylab="Emissions", 
     col="blue",main="Plot of Baltimore City, MD Emissions by Year")
dev.off()

##########
##Plot 3##
##########

#Group the emission information by year and emission type:
emissions3<-aggregate(NEI3$Emissions, by=list(NEI3$year, NEI3$type), FUN=sum)
colnames(emissions3)<-c("Year","Type", "Emissions")

#Create the graph:
png(file="plot3.png", width=480, height=480)
qplot(emissions3$Year, emissions3$Emissions, group=emissions3$Type,
      color=Type, data=emissions3, geom=c("point","line"), 
      xlab="Year", ylab="Emissions", main="Plot of Baltimore City, MD Emissions Types by Year")+(labs(fill="new"))
dev.off()

##########
##Plot 4##
##########

#Subset the coal SCC's values from the SCC dataset:
NEI4<-NEI[NEI$SCC %in% SCC[grep("Coal",SCC$EI.Sector),1],]

#Group the emission information by year
emissions4<-aggregate(NEI4$Emissions, by=list(NEI4$year), FUN=sum)
colnames(emissions4)<-c("Year", "Emissions")

#Create the graph:
png(file="plot4.png", width=480, height=480)
plot(emissions4$Year, emissions4$Emissions, type="l", xlab="Year", ylab="Emissions", 
     col="blue",main="Plot of U.S. Coal Emissions by Year")
dev.off()

##########
##Plot 5##
##########

#Subset the motor vehicle SCC values from the SCC dataset:
#For the SCC rows only: test<-SCC[grep("Mobile", SCC$EI.Sector),]
NEI5<-NEI[NEI$SCC %in% SCC[grep("Mobile",SCC$EI.Sector),1],]
NEI6<-NEI5[grep("24510",NEI5$fips),]

#Group the emission information by year
emissions5<-aggregate(NEI6$Emissions, by=list(NEI6$year), FUN=sum)
colnames(emissions5)<-c("Year", "Emissions")

#Create the graph:
png(file="plot5.png", width=480, height=480)
plot(emissions5$Year, emissions5$Emissions, type="l", xlab="Year", ylab="Emissions", 
     col="blue",main="Plot of Baltimore City, MD Vehicle Emissions by Year")
dev.off()

##########
##Plot 6##
##########

#emissions5 from plot 5 represents motor vehicle emissions in Baltimore City, Maryland, from 1999 to 2008.
colnames(emissions5)[2]<-"Emissions"
#Subset the information for Los Angeles County, California:
NEI7<-NEI[NEI$SCC %in% SCC[grep("Mobile",SCC$EI.Sector),1],]
NEI8<-NEI7[grep("06037",NEI7$fips),]

#Group the emission information by year
emissions6<-aggregate(NEI8$Emissions, by=list(NEI8$year), FUN=sum)
colnames(emissions6)<-c("Year", "Emissions")

#Create a new table that includes both areas:
plot6table<-rbind(emissions5, emissions6)
area<-c(rep("Baltimore City, Maryland", times=4), rep("Los Angeles County, California",times=4))
plot6table2<-cbind(plot6table, area)
colnames(plot6table2)[3]<-"Location"


#Create the graph:
png(file="plot6.png", width=480, height=480)
qplot(plot6table2$Year, plot6table2$Emissions, data=plot6table2, group=plot6table2$Location, color=Location, 
      geom=c("point","line"),xlab="Year", ylab="Emissions",main="Plot of Annual Vehicle Emissions by Location")
dev.off()

################
##Plot Summary##
################

#Assign the plots to variables:
plot1<-qplot(emissions1$Year, emissions1$Emissions, geom=c("point","line"), xlab="Year", ylab="Emissions",
             main="Plot of Emissions by Year")

plot2<-qplot(emissions2$Year, emissions2$Emissions, xlab="Year", ylab="Emissions", 
             geom=c("point","line"),main="Plot of Baltimore City, MD Emissions by Year")

plot3<-qplot(emissions3$Year, emissions3$Emissions, group=emissions3$Type,
             color=Type, data=emissions3, geom=c("point","line"), 
             xlab="Year", ylab="Emissions", main="Plot of Baltimore City, MD Emissions Types by Year")+(labs(fill="new"))

plot4<-qplot(emissions4$Year, emissions4$Emissions, geom=c("point","line"), xlab="Year", ylab="Emissions", 
             main="Plot of U.S. Coal Emissions by Year")

plot5<-qplot(emissions5$Year, emissions5$Emissions, geom=c("point","line"), xlab="Year", ylab="Emissions",
             main="Plot of Baltimore City, MD Vehicle Emissions by Year")

plot6<-qplot(plot6table2$Year, plot6table2$Emissions, data=plot6table2, group=plot6table2$Location, color=Location, 
             geom=c("point","line"),xlab="Year", ylab="Emissions",main="Plot of Annual Vehicle Emissions by Location")

#Arrange the plots in a grid:
library(gridExtra)
png(file="Plot7 - GGplot Summary Grid.png", width=1200)
grid.arrange(main="Exploratory Data Analysis (Assignment 2): GGpot2 Plot Summary",plot1, plot2, plot3, plot4, plot5, plot6, ncol=3, nrow=2)
dev.off()



