#' Create plot of Covid Cases by County
#' @export
#' @importFrom dplyr filter
#' @importFrom plotly ggplotly
#' @import ggplot2
PlotLocalChoro<-function(IncludedCounties, 
                         ChosenBase, 
                         TypofPlot,
                         AFBaseLocations,
                         county_df,
                         PlottingCountyData){

    if (TypofPlot == "County") {
        
        BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        #Creating the choropleth dataset so we have all info in one data set and can plot it together
        choropleth <- merge(county_df, 
                            PlottingCountyData, 
                            by = c("county", "State"))
        colnames(choropleth)[7]<-"CountyFIPS"
        choropleth <- choropleth[order(choropleth$order), ]
        choropleth$state_name<-NULL
        choropleth<-data.frame(choropleth$county, 
                               choropleth$State, 
                               choropleth$CountyFIPS, 
                               choropleth$group, 
                               choropleth$lat, 
                               choropleth$long, 
                               rev(choropleth)[,1])
        colnames(choropleth)<-c("County","State","CountyFIPS","group","lat","long","Cases")
        choropleth<-subset(choropleth, CountyFIPS %in% IncludedCounties$FIPS)

        #Plot the data
        PlotCovidLocal<-ggplot(choropleth, 
                               aes(long, lat, group = group)) +
            geom_polygon(aes(fill = Cases)) +
            coord_fixed() +
            theme_minimal() +
            ggtitle("COVID-19 Cases by County (County View)") +
            geom_point(data = BaseStats, 
                       aes(x=Long, y=Lat, group = 1),
                       color = 'red', 
                       size = 5)+
            theme(axis.line = element_blank(), axis.text = element_blank(),
                  axis.ticks = element_blank(), axis.title = element_blank()) +
            scale_fill_viridis("Cases")

        ggplotly(PlotCovidLocal)

    } else  {
        
        BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        #Creating the choropleth dataset so we have all info in one data set and can plot it together
        choropleth <- merge(county_df, 
                            PlottingCountyData, 
                            by = c("county", "State"))
        colnames(choropleth)[7]<-"CountyFIPS"
        choropleth <- choropleth[order(choropleth$order), ]
        choropleth$state_name<-NULL
        choropleth<-data.frame(choropleth$county, 
                               choropleth$State, 
                               choropleth$CountyFIPS, 
                               choropleth$group, 
                               choropleth$lat, 
                               choropleth$long, 
                               rev(choropleth)[,1])
        colnames(choropleth)<-c("County","State","CountyFIPS","group","lat","long","Cases")
        choropleth<-subset(choropleth, State %in% IncludedCounties$State)

        #Plot the data
        PlotCovidLocal<-ggplot(choropleth, 
                               aes(long, lat, group = group)) +
            geom_polygon(aes(fill = log(Cases))) +
            coord_fixed() +
            theme_minimal() +
            ggtitle("COVID-19 Cases by County (State View)") +
            geom_point(data = BaseStats, 
                       aes(x= Long, y= Lat, group = 1),
                       color = 'red', 
                       size = 5)+
            theme(axis.line = element_blank(), 
                  axis.text = element_blank(),
                  axis.ticks = element_blank(), 
                  axis.title = element_blank()) +
            scale_fill_viridis("log(Cases)")

        plotly::ggplotly(PlotCovidLocal)

    }


}
