# Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (\color{red}{\verb|fips == "24510"|}fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

total_ems_BaltimoreCity <- function(){
    library(dplyr) # Let's use Dplyr library with this dataq
    NEI <- as_tibble(NEI) #convert the dataframe to tibble to work with Dplyr
    NEI_Baltimore <- NEI %>% filter(fips=="24510") #filter out everything else except Batimore City data
    NEI_Baltimore_by_year <- NEI_Baltimore %>% group_by(year) #group Baltimore data by year
    Baltimore_yearly <- NEI_Baltimore_by_year %>% summarise(ems=mean(Emissions)) #calculate the mean per year
    
    png(filename = "plot2.png") #open up a graphic device 
    with(Baltimore_yearly,plot(year,ems,main = "Avg total PM2.5 emissions by year in Baltimore City, Maryland",ylab = "PM2.5 emissions (tons)",cex.main=0.80,cex.lab=0.65,ylim = c(2,11)))
    with(Baltimore_yearly,text(year,ems, round(ems,digits = 2),cex = 0.6,pos = 3,col = "red")) #highlight the individual values with 2-digit rounding
    dev.off() #close the graphics device
    }