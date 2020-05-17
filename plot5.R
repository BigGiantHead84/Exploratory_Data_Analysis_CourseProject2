#Question 5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

ems_from_motorvehicles_Baltimore<- function(){
    library(dplyr) # Let's use Dplyr library with this dataq
    library(ggplot2) #Include ggplot2 as well
    NEI <- readRDS("summarySCC_PM25.rds") # let's read in the data
    scc <- readRDS("Source_Classification_Code.rds") #let's read in the Source Classsification Code, we're going to need it
    NEI <- as_tibble(NEI) #convert the dataframe to tibble to work with Dplyr
    scc <- as_tibble(scc) #same thing for source classification code
    scc_onroad <- scc %>% filter(grepl("On-Road",EI.Sector)) %>% select(SCC) #NOTE: to filter based on motor vehicle sources I include here sources that includes the word 'On-Road' as per the codebook both light-duty and heavy-duty vehicles that are working either with diesel or with gasoline are included in there
    scc_onroad <- scc_onroad %>% mutate_if(is.factor,as.character) #StringAsFactors=False cannot be used with RDS, so let's convert the factors to characters
    NEI_onroad_baltimore <- NEI %>% filter(SCC %in%  as.character(scc_onroad[[1]]) & fips=="24510") # let's filter out NEI data based on the SCC sources that we have from previous step and also filter rows based on Baltimore City data (fips 24510)
    NEI_onroad_baltimore_by_year <- NEI_onroad_baltimore %>% group_by(year) #group  data by year
    NEI_onroad_baltimore_by_year <- NEI_onroad_baltimore_by_year %>% summarise(ems=mean(Emissions)) #calculate the mean per year and per type as this is the most indicative metrics for visualizing yearly trends
    
    png(filename = "plot5.png") #open up a graphic device 
    with(NEI_onroad_baltimore_by_year,plot(year,ems,main = "Mean total PM2.5 emissions from motor vehicles 1999-2008 in Baltimore City",ylab = "PM2.5 emissions (tons)",cex.main=0.80,cex.lab=0.65,ylim = c(0.2,2)))
    with(NEI_onroad_baltimore_by_year,text(year,ems, round(ems,digits = 2),cex = 0.6,pos = 3,col = "red")) #highlight the individual values with 2-digit rounding
    dev.off() #close the graphics device
}