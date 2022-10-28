library(ggplot2)
##R script file for questions on Project 2 for Exploratory Data Analysis
#Import Data into data frames
#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")
#==============================================================================
#Q1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
#make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
#==============================================================================
P2Q1<-aggregate(Emissions~year,data=NEI,FUN=sum,na.rm=TRUE)
plot(P2Q1$year,P2Q1$Emissions,type="o",xlab="Year",ylab="Emissions",main="Total PM2.5 emissions for all sources by year")
#==============================================================================
#==============================================================================
#Q2 - Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (\color{red}{\verb|fips == "24510"|}fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.
#==============================================================================

NEIQ2<-subset(NEI,fips=="24510")
P2Q2<-aggregate(Emissions~year,data=NEIQ2,FUN=sum,na.rm=TRUE)
plot(P2Q2$year,P2Q2$Emissions,type="o",xlab="Year",ylab="Emissions",main="Total PM2.5 emissions for Baltimore City,Maryland from all sources by year")


#==============================================================================
#==============================================================================
#Q3 - Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) 
#variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer 
#this question.
#==============================================================================
P2Q3<-aggregate(Emissions~year+type,data=NEIQ2,FUN=sum,na.rm=TRUE)
p<-ggplot(P2Q3,aes(year,Emissions,color=type))+geom_line()+geom_point()+ggtitle("Total PM2.5 emissions for Baltimore City,Maryland from all sources by type and year")
print(p)
#==============================================================================
#Q4 - Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#==============================================================================
SCCcoal <- SCC[grepl("coal", SCC$Short.Name, ignore.case = T),]
NEIcoal <- NEI[NEI$SCC %in% SCCcoal$SCC,]
P2Q3 <- aggregate(Emissions ~ year + type, NEIcoal, sum)
p<-ggplot(P2Q3,aes(year,Emissions,color=type))+geom_line()+geom_point()+ggtitle(expression("Total US PM2.5 Coal Emission by Type and Year"))
print(p)
#==============================================================================


#Q5 - How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


#==============================================================================
NEIMotor <- subset(NEI, NEI$fips == "24510" & NEI$type == "ON-ROAD")
P2Q5 <- aggregate(Emissions ~ year, NEIMotor, sum)
p<-ggplot(P2Q5 ,aes(year,Emissions))+geom_line()+geom_point()+ggtitle(expression("Total US PM2.5 Emission by motor vehicles in Baltimore across Year"))
print(p)
#==============================================================================


#Q6 - Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in 
#Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips == "06037"). Which city has 
#seen greater changes over time in motor vehicle emissions?
#==============================================================================
NEIMotor2 <- subset(NEI, NEI$fips %in% c("24510","06037") & NEI$type == "ON-ROAD")
P2Q6 <- aggregate(Emissions ~ year+fips, NEIMotor2, sum)
p<-ggplot(P2Q6 ,aes(year,Emissions,col=fips))+geom_line()+geom_point()+ggtitle(expression("Total US PM2.5 Emission by motor vehicles in Baltimore and LA across Year"))
print(p)