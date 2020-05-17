#Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

ems_by_source_year_Baltimore <- function(){
    library(dplyr) # Let's use Dplyr library with this dataq
    library(ggplot2) #Include ggplot2 as well
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI <- as_tibble(NEI) #convert the dataframe to tibble to work with Dplyr
    NEI_Baltimore <- NEI %>% filter(fips=="24510") #filter out everything else except Batimore City data
    NEI_Baltimore$type <- as.factor(NEI_Baltimore$type) #let's convert the type to factoruniq
    NEI_Baltimore <- NEI_Baltimore %>% group_by(year,type) #group Baltimore data by year and type
    NEI_Baltimore <- NEI_Baltimore %>% summarise(ems=mean(Emissions)) #calculate the mean per year and per type as this is the most indicative metrics for visualizing yearly trends
    
    png(filename = "plot3.png") #open up a graphic device 
    #qplot(year,ems,data=NEI_Baltimore,facets = . ~ type,  geom = c("point","smooth"))
    g <- ggplot(NEI_Baltimore,aes(year,ems)) #initiate the ggplot
    g <- g+geom_point()+facet_grid(.~type)+geom_smooth(method = "lm")+ggtitle("PM2.5 emission in Baltimore City by source type") + labs(title="PM2.5 emissions in Baltimore City by type")+labs(y="Mean of total PM2.5 emissions (tons)") #add aesthetics and annotations
    print(g) #print the plot to graphics device
    dev.off() #close the graphics device
}