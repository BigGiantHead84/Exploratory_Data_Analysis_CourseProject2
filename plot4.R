#Question4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

ems_from_coalcomb_by_year<- function(){
    library(dplyr) # Let's use Dplyr library with this dataq
    library(ggplot2) #Include ggplot2 as well
    NEI <- readRDS("summarySCC_PM25.rds") # let's read in the data
    scc <- readRDS("Source_Classification_Code.rds") #let's read in the Source Classsification Code, we're going to need it
    NEI <- as_tibble(NEI) #convert the dataframe to tibble to work with Dplyr
    scc <- as_tibble(scc) #same thing for source classification code
    scc_coal <- scc %>% filter(grepl("Coal",EI.Sector)) %>% select(SCC) #let's filter sources that have a word coal in it since we're only interested of coal combustion sources. Then, let's only select the SCC since we only need that
    scc_coal <- scc_coal %>% mutate_if(is.factor,as.character) #StringAsFactors=False cannot be used with RDS, so let's convert the factors to characters
    NEI_coal <- NEI %>% filter(SCC %in%  as.character(scc_coal[[1]])) #for some strange reason %in% operator doesn't work with tibbles so we need to convert the tibble with one character column into character again with list notation[[]]. But now we have only the sources that include only coal combustion
    NEI_coal_by_year <- NEI_coal %>% group_by(year) #group  data by year
    NEI_coal_mean_ems_by_year <- NEI_coal_by_year %>% summarise(ems=mean(Emissions)) #calculate the mean per year and per type as this is the most indicative metrics for visualizing yearly trends
    
    png(filename = "plot4.png") #open up a graphic device 
    with(NEI_coal_mean_ems_by_year,plot(year,ems,main = "Mean total PM2.5 emissions from coal-combustion related sources by year in USA",ylab = "PM2.5 emissions (tons)",cex.main=0.80,cex.lab=0.65,ylim = c(60,110)))
    with(NEI_coal_mean_ems_by_year,text(year,ems, round(ems,digits = 2),cex = 0.6,pos = 3,col = "red")) #highlight the individual values with 2-digit rounding
    dev.off() #close the graphics device
}