#Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

ems_from_motorvehicles_BaltimoreLAcounty<- function(){
    library(dplyr) # Let's use Dplyr library with this dataq
    library(ggplot2) #Include ggplot2 as well
    NEI <- readRDS("summarySCC_PM25.rds") # let's read in the data
    scc <- readRDS("Source_Classification_Code.rds") #let's read in the Source Classsification Code, we're going to need it
    NEI <- as_tibble(NEI) #convert the dataframe to tibble to work with Dplyr
    scc <- as_tibble(scc) #same thing for source classification code
    scc_onroad <- scc %>% filter(grepl("On-Road",EI.Sector)) %>% select(SCC) #NOTE: to filter based on motor vehicle sources I include here sources that includes the word 'On-Road' as per the codebook both light-duty and heavy-duty vehicles that are working either with diesel or with gasoline are included in there
    scc_onroad <- scc_onroad %>% mutate_if(is.factor,as.character) #StringAsFactors=False cannot be used with RDS, so let's convert the factors to characters
    NEI_onroad_baltimore <- NEI %>% filter(SCC %in%  as.character(scc_onroad[[1]]) & fips=="24510") # let's filter out NEI data based on the SCC sources that we have from previous step and also filter rows based on Baltimore City data (fips 24510)
    NEI_onroad_LAcounty <- NEI %>% filter(SCC %in%  as.character(scc_onroad[[1]]) & fips=="06037") # let's filter out NEI data based on the SCC sources that we have from previous step and also filter rows based on LA county data (fips 06037)
    NEI_onroad_Baltimore_LAcounty <- bind_rows(NEI_onroad_baltimore,NEI_onroad_LAcounty) #lets add the two tables together
    NEI_onroad_Baltimore_LAcounty <- NEI_onroad_Baltimore_LAcounty %>% mutate(fips=replace(fips,fips=="24510","Baltimore City"))
    NEI_onroad_Baltimore_LAcounty <- NEI_onroad_Baltimore_LAcounty %>% mutate(fips=replace(fips,fips=="06037","LA County"))
    NEI_onroad_Baltimore_LAcounty <- NEI_onroad_Baltimore_LAcounty %>% rename(Location="fips")
    NEI_onroad_Baltimore_LAcounty <- NEI_onroad_Baltimore_LAcounty %>% mutate(Location=factor(Location)) #change fips variable to factor so that we can use it in GGplot for visualization
    #NEI_onroad_baltimore_by_year <- NEI_onroad_baltimore %>% group_by(year) #group  data by year
    #NEI_onroad_baltimore_by_year <- NEI_onroad_baltimore_by_year %>% summarise(ems=mean(Emissions)) #calculate the mean per year and per type as this is the most indicative metrics for visualizing yearly trends
    
    png(filename = "plot6.png") #open up a graphic device 
    g <- ggplot(NEI_onroad_Baltimore_LAcounty,aes(year,Emissions)) #initialize the ggplot
    g <- g+geom_point(color="steelblue",alpha=1/2) + facet_grid(. ~ Location, scales = "free") + geom_smooth(method = "lm") + labs(title="PM2.5 emissions from motorvehicles in Baltimore City and LA county") + labs(y=expression(PM[2.5]*" emissions (in tons)"))
    print(g)
    dev.off() #close the graphics device
}