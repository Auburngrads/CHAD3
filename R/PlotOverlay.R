#' Create charts for projecting local health data
#' @export
#' @importFrom dplyr filter
#' @import ggplot2
PlotOverlay<-function(ChosenBase, IncludedCounties, IncludedHospitals, SocialDistance, DaysProjected, StatisticType){
    if (StatisticType == "Hospitalizations") {
        
      #Establish initial inputs such as base, counties, and filter IHME model
      BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
      IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
      hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
      TTBCounty <- sum(IncludedHospitals$BEDS)
      
      #Get covid cases and hospitalization rates for county
      CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
      CovidCountiesHospRate <- subset(CountyHospRate, FIPS %in% IncludedCounties$FIPS)
      
      #Get past data in daily hospital use
      #This will use a 5 day hospital stay as the average
      HistoricalDataDaily <- CovidCounties[,(5+5):length(CovidCounties)] -
        CovidCounties[,5:(length(CovidCounties)-5)]
      HistoricalDataHosp<-colSums(HistoricalDataDaily*CovidCountiesHospRate$HospRate)
      
      #Create dataframe to hold daily hospitalizations
      HistoricalDates<-seq(as.Date("2020-01-27"), length=length(HistoricalDataHosp), by="1 day")
      HistoricalData<-data.frame(HistoricalDates, HistoricalDataHosp, HistoricalDataHosp*0.75, HistoricalDataHosp*1.25)
      colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      
      currHosp = HistoricalData[nrow(HistoricalData),2]
      
      #Get regional and state populations
      StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
      RegPop <- sum(IncludedCounties$Population)
      StPop <- sum(StPopList$Population)
      
      # Use Population ratio to scale IHME
      PopRatio <- RegPop/StPop
      
      
      # Apply ratio's to IHME data
      IHME_Region <- IHME_State
      IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
      IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
      IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
      IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
      
      DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
      CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
      CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
      CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
      colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
      
      #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
      #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
      ActiveCases<-rev(CovidCounties)[1:7]
      ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
      colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
      SIRinputs<-data.frame(currHosp,sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
      colnames(SIRinputs)<-c("cases","pop","doubling")
      
      
      ####################################################################################
      #Mean Estimate
      
      #Next we use the calculated values, along with estimated values from the Estimated Values. 
      #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
      cases<-SIRinputs$cases
      pop<-SIRinputs$pop
      doubling<-7
      
      #Established Variables at the start for every county or populations
      Ro<-2.5
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      socialdistancing<-SocialDistance
      hospitalizationrate<-14
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-5
      icutime<-4
      ventilatortime<-7
      daysforecasted<-120
      
      
      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                 ventilatortime,daysforecasted,Ro, .5)
      
      MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
      DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
      TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
      
      
      ####################################################################################
      #Lower Estimate
      
      #Next we use the calculated values, along with estimated values from the Estimated Values. 
      #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
      cases<-SIRinputs$cases
      pop<-SIRinputs$pop
      doubling<-10
      
      #Established Variables at the start for every county or populations
      Ro<-2.3
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      
      hospitalizationrate<-11
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-3.5
      icutime<-4
      ventilatortime<-7
      
      
      
      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                 icutime,ventilatortime,daysforecasted,Ro, .5)
      
      DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
      TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
      
      ####################################################################################
      #Upper Estimate
      #Next we use the calculated values, along with estimated values from the Estimated Values. 
      
      #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
      cases<-SIRinputs$cases
      pop<-SIRinputs$pop
      doubling<-6
      
      #Established Variables at the start for every county or populations
      Ro<-2.6
      incubationtime<-5
      latenttime<-2
      recoverydays<-14
      
      hospitalizationrate<-17
      icurate<-6
      ventilatorrate<-3
      hospitaltime<-7
      icutime<-4
      ventilatortime<-7
      
      #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
      #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
      SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                 socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                 icutime,ventilatortime,daysforecasted,Ro, .5)
      
      DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
      TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
      colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Estimate","Upper Estimate")
      colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
      
      DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
      DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`,0)
      DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`,0)
      DailyData<-DailyData[-1,]
      colnames(IHME_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Estimate","Upper Estimate")
      DailyData$ID<-rep("CHIME",nrow(DailyData))
      IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
      HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
      HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
      OverlayData<-rbind(DailyData,IHME_Data)
      OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
      
      OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
      
      OverlayData<-rbind(HistoricalData, OverlayData)
      
      
      
      hospCounty <- subset(HospUtlzCounty, fips %in% IncludedCounties$FIPS)
      #Finds number of hospitals in radius
      TotalBeds<-sum(hospCounty$num_staffed_beds)
      #get historic utilization
      hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
      totalUsedBeds <- sum(hospCounty$bedsUsed)
      baseUtlz <- totalUsedBeds/TotalBeds
      
      
      projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID, linetype = ID)) +
        geom_line(aes(linetype = ID, color = ID)) + 
        geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                    alpha = .2) +
        scale_colour_manual(values=c("tan", "blue", "black","red"))+
        scale_fill_manual(values = c("tan4", "cadetblue", "gray","red"))+
        scale_linetype_manual(values=c("dashed", "solid", "dashed", "solid"))+
        
        geom_hline(aes(yintercept = TotalBeds * (1-baseUtlz),
                       linetype = "Estimated COVID Patient Bed Capacity"),
                   colour = "red") +
        ggtitle("Projected Daily Hospital Bed Utilization")+
        ylab("Daily Beds Needed")+
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = "top",
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        scale_x_date(date_breaks = "2 week")+
        labs(color = "ID")
      
      
      projections <- ggplotly(projections)
      projections <- projections %>% config(displayModeBar = FALSE)
      projections
        
    } else {
        
        #Creating the stats and dataframes determined by the base we choose to look at.
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        TotalBedsCounty <- sum(IncludedHospitals$BEDS)
        
        #Get regional and state populations
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
        RegPop <- sum(IncludedCounties$Population)
        StPop <- sum(StPopList$Population)
        
        # Use Population ratio to scale IHME
        PopRatio <- RegPop/StPop
        
        # Get total hospital bed number across state
        IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
        TotalBedsState <- sum(IncludedHospitalsST$BEDS)
        
        # Calculate bed ratio
        BedProp <- TotalBedsCounty/TotalBedsState
        
        # Apply ratio's to IHME data
        IHME_Region <- IHME_State
        IHME_Region$deaths_mean = round(IHME_State$totdea_mean*PopRatio)
        IHME_Region$deaths_lower = round(IHME_State$totdea_lower*PopRatio)
        IHME_Region$deaths_upper = round(IHME_State$totdea_upper*PopRatio)
        
        IHME_Data<-data.frame(IHME_Region$date,IHME_Region$deaths_mean, IHME_Region$deaths_lower, IHME_Region$deaths_upper)
        
        BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
        #We include State, county, population in those counties, cases, fatalities, doubling rate
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
        CovidDeathHist<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        HistoricalData<-colSums(CovidDeathHist[,5:length(CovidDeathHist)])
        HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalData, HistoricalData, HistoricalData)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        
        DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
        CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
        CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
        colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
        
        #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
        #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
        ActiveCases<-rev(CovidCounties)[1:7]
        ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
        colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
        SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
        colnames(SIRinputs)<-c("cases","pop","doubling")
        
        
        ####################################################################################
        #Mean Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-8
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-SocialDistance
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-120
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                   ventilatortime,daysforecasted,Ro, .5)
        
        MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
        DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
        TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
        
        
        ####################################################################################
        #Lower Estimate
        
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-10
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
       
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
        
        ####################################################################################
        #Upper Estimate
        #Next we use the calculated values, along with estimated values from the Estimated Values. 
        
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-7
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
      
        hospitalizationrate<-5.5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
       
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Fatalities","Lower Estimate","Upper Estimate")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Lower Estimate","Upper Estimate")
        
        DailyData$`Expected Fatalities` <- round(DailyData$`Expected Fatalities`*(.25/5.5),0)
        DailyData$`Lower Estimate` <- round(DailyData$`Lower Estimate`*(.15/4),0)
        DailyData$`Upper Estimate` <- round(DailyData$`Upper Estimate`*(1/8),0)
        DailyData<-DailyData[-1,]
        DailyData$`Expected Fatalities`<-cumsum(DailyData$`Expected Fatalities`)
        DailyData$`Lower Estimate`<-cumsum(DailyData$`Lower Estimate`)
        DailyData$`Upper Estimate`<-cumsum(DailyData$`Upper Estimate`)
        
        colnames(IHME_Data)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        colnames(HistoricalData)<-c("ForecastDate", "Expected Fatalities", "Lower Estimate","Upper Estimate")
        DailyData$ID<-rep("CHIME",nrow(DailyData))
        IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
        HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
        HistoricalData <- dplyr::filter(HistoricalData, ForecastDate >= as.Date("2020-01-27") + 30)
        OverlayData<-rbind(DailyData,IHME_Data)
        OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
        
        OverlayData<- dplyr::filter(OverlayData, ForecastDate >= (Sys.Date()) & ForecastDate <= (Sys.Date() + DaysProjected))
        
        OverlayData<-rbind(HistoricalData, OverlayData)
        
        
        
        
        projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Fatalities`, color = ID, fill = ID, linetype = ID)) +
            geom_line() + 
            scale_colour_manual(values=c("tan", "blue", "black"))+
            scale_fill_manual(values = c("tan4", "cadetblue", "gray"))+
            scale_linetype_manual(values = c("dashed", "dashed", "solid"))+
            geom_ribbon(aes(ymin = `Lower Estimate`, ymax = `Upper Estimate`), 
                        alpha = .2) +
            ggtitle("Projected Fatalities")+
            ylab("Fatalities")+
            theme_bw() + 
            theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
                  axis.title = element_text(face = "bold", size = 11, family = "sans"),
                  axis.text.x = element_text(angle = 60, hjust = 1), 
                  axis.line = element_line(color = "black"),
                  legend.position = "top",
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank()) +
            scale_x_date(date_breaks = "2 week")+
            labs(color = "ID")
        
        
        projections <- ggplotly(projections)
        projections <- projections %>% config(displayModeBar = FALSE)
        projections
    }
        
    }
