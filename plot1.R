# Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008."

total_ems_by_year <- function(){
    library(dplyr) # Let's use Dplyr library with this dataq
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI <- as_tibble(NEI) #convert the dataframe to tibble to work with Dplyr
    by_year <- group_by(NEI,year) #group by year so that we can work with summarise based on each year
    ems_yearly <- by_year %>% summarise(ems=mean(Emissions)) #take the mean of all emission for a given year
    png(filename = "plot1.png") #open up a graphic device 
    with(ems_yearly,plot(year,ems,main = "Avg total PM2.5 emissions by year in USA",ylab = "PM2.5 emissions (tons)",ylim = c(1,8))) #make the plot with some basic annotations
    with(ems_yearly,text(year,ems, round(ems,digits = 2),cex = 0.6,pos = 3,col = "red")) #highlight the individual values with 2-digit rounding
    dev.off() #close the graphics device
}
