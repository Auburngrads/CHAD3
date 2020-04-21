##################
##### Global #####
##################

# Layout
##############################################################################################################################################
# The global introduces all libraries, functions, datasets, and formatting that is necessary to pass through the server.
# First:  The libraries are loaded which have built in functions are used throughout the app.
# Second: We load data from https://github.com/treypujats/COVID19/tree/master/covid19/data ,  usafacts.org , and covid19.healthdata.org
#         The github data has static information on on all air force bases, US counties, and US hospitals. 
#         The usafacts data has dynamic information that is updated daily reporting the number of cases and number of deaths daily.
#         The IHME provides projection data of the pandemic
#         After loading the data, we format headers, establish data tables for printing, and do any static changes to the dataset for the app.
# Third:  Functions are used to execute the tasks in the server. Functions in the global are not dynamic, but they take in dynamic inputs
#         Global functions are used to calculate statistics, make data tables, plot graphs, and create visuals.
##############################################################################################################################################       



# Step One
###################################################################################################################################################
#Loads in all necessary packages for the shiny app




library(stringr)
library(stringi)
library(markdown)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(geosphere)
library(scales)
library(googleVis)
library(usmap)
library(data.table)
library(plyr)
library(jsonlite)
library(splitstackshape)
library(DT)
library(mapproj)
library(viridis)
library(tidyverse)
library(zoo) #used for rollsum function 
library(rmarkdown)
library(rvest)
library(maps)
library(tm)
library(sf)
library(ggrepel)
library(tigris)
library(plotly)



# Step Two
###################################################################################################################################################
#Define Variables and load in data up front if necessary.
#This data updates daily with CovidConfirmedCases and CovidDeaths. These numbers are updated every day.
#The static data (countyinfo, hospitalinfo, AFBaseLocations) is used to for lat and long coordinates to measure distance.
#Hospital Data allows us to determine the bed capacity of all hospitals in the nation
#AFBaseLocations provide names and coordinates of base.
#CountyInfo is used to measure population of a county and coordinates.

#CovidConfirmedCases <- as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"))
CovidConfirmedCases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
#CovidConfirmedCases<-CovidConfirmedCases[colSums(!is.na(CovidConfirmedCases)) > 0]
CovidConfirmedCases<-CovidConfirmedCases[colSums(!is.na(CovidConfirmedCases)) > 0]

CountyInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/CHAD/raw/master/data/countyinfo.rda"))
HospitalInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/CHAD/blob/master/data/hospitalinfo.rda?raw=true"))
#CovidDeaths<-as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
CovidDeaths<-as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
HospUtlzCounty <- read.csv("https://github.com/treypujats/CHAD/raw/master/data/county_hospitals.csv")
CountyHospRate <- read.csv("https://github.com/treypujats/CHAD/raw/master/data/CountyHospRateCalc.csv")
#himd <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/himd.rda?raw=true"))
#cimd <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/cimd.rda?raw=true"))
#AFBaseLocations <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/raw/master/covid19/data/baseinfo.rda"))


#Updated data frames to read in
githubURL <- "https://github.com/treypujats/CHAD/blob/master/data/cimd.RData?raw=true"
load(url(githubURL))

githubURL <- "https://github.com/treypujats/CHAD/blob/master/data/himd.RData?raw=true"
load(url(githubURL))

githubURL <- "https://github.com/treypujats/CHAD/blob/master/data/baseinfo.RData?raw=true"
load(url(githubURL))



######################### ADDED TO MAKE JHU DATA FRAME LOOK LIKE EXISTING DATA FRAMEs#################################
#Updating data frames to ensure they are filled and match the data we reference later in the scripts
#colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
# Keep county fips code and all cases data
CovidConfirmedCases<-CovidConfirmedCases[,c(5, 12:ncol(CovidConfirmedCases))]
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
CovidDeaths<-CovidDeaths[,c(5, 13:ncol(CovidDeaths))]
colnames(CovidDeaths)[1]<-"CountyFIPS"

#Get state infomration based on county fips code
StateInfo<-fips_codes[c(5,1,2,4)]
#combine state and county codes to get CountyFIPS code
StateInfo$county_code <- paste(StateInfo$state_code,StateInfo$county_code, sep="")
#make countyFIPS code a numeric value
StateInfo[, c(3,4)] <- sapply(StateInfo[, c(3,4)], as.numeric)
#names for headers
colnames(StateInfo)[1:4]<-c("County Name", "State","stateFIPS","CountyFIPS")
CovidConfirmedCases<-merge(StateInfo,CovidConfirmedCases,by = "CountyFIPS")
CovidDeaths<-merge(StateInfo,CovidDeaths,by = "CountyFIPS")
#################################END JHU DATA PREP############################################


colnames(CovidDeaths)[1]<-"CountyFIPS"
HospitalInfo$BEDS <- ifelse(HospitalInfo$BEDS < 0, 0, HospitalInfo$BEDS)
CovidConfirmedCases[is.na(CovidConfirmedCases)]<-0
CovidDeaths[is.na(CovidDeaths)]<-0
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"


#Read in IHME data for projecting data in the future
temp <- tempfile()
download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", temp, mode="wb")
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name[2]
IHME_Model <- read.table(unz(temp, csv_file), header = T, sep = ",")
unlink(temp)
IHME_Model$date <- as.Date(IHME_Model$date, format = "%Y-%m-%d")
StateList <- data.frame(state.name, state.abb)
IHME_Model <- merge(IHME_Model, StateList, by.x = names(IHME_Model)[2], by.y = names(StateList)[1])
names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"


#Create list of hospitals, bases, and counties.
BaseList<-sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County
MAJCOMList <- sort(unique(AFBaseLocations$`Major Command`), decreasing = FALSE)
MAJCOMList<-c("All",'Active Duty',MAJCOMList)

#Calculate county case doubling rate for most recent day
CovidConfirmedCases <- dplyr::filter(CovidConfirmedCases, CountyFIPS != 0)
CovidConfirmedCases <- head(CovidConfirmedCases,-1)

currCount = 0

v <- rep(0, as.numeric(ncol(CovidConfirmedCases)))

for (i in 1:nrow(CovidConfirmedCases)){
    
    j = 0
    cases = CovidConfirmedCases[i,ncol(CovidConfirmedCases)]
    
    if (cases != 0){
        
        while (cases/2 < CovidConfirmedCases[i,ncol(CovidConfirmedCases)-j])
        {
            j = j + 1
        }
        
        days = j
    } 
    else{
        days = 0
    }
    
    v[i] <- days
    
}

CovidConfirmedCasesRate <- cbind(CovidConfirmedCases,v)


######################Data Specific to plotting counties and states as choropleth

#Input the Included Counties as factors
# PlottingCountyData<- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
#                               header = TRUE, stringsAsFactors = FALSE)
# PlottingCountyData$county <- tolower(gsub("([A-Za-z]+).*", "\\1", PlottingCountyData$County.Name))
# PlottingCountyData$county <- gsub("^(.*) parish, ..$","\\1", PlottingCountyData$county)
# #Creating state name in addition to state abb
# PlottingCountyData<-PlottingCountyData %>% 
#     mutate(state_name = tolower(state.name[match(State, state.abb)]))
# #Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
# county_df <- map_data("county")
# names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
# county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
# county_df$state_name <- NULL
# #Calling in state data so we can map it correctly
# state_df <- map_data("state", projection = "albers", parameters = c(39, 45))
# colnames(county_df)[6]<-"State"
#Input the Included Counties as factors
PlottingCountyData<- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                              header = TRUE, stringsAsFactors = FALSE)

PlottingCountyData <- PlottingCountyData<-PlottingCountyData[colSums(!is.na(PlottingCountyData)) > 0]

# stopwords = "County"     #Your stop words file
# x  = PlottingCountyData$County.Name        #Company column data
# x  =  removeWords(x,stopwords)     #Remove stopwords
#
# df$company_new <- x     #Add the list as new column and check


# PlottingCountyData$county <- tolower(removeWords(PlottingCountyData$County.Name,"County"))
# PlottingCountyData$county <-gsub(" ", "" ,PlottingCountyData$county)
# PlottingCountyData$county <- gsub("^(.*) parish, ..$","\\1", PlottingCountyData$county)
#
# #Creating state name in addition to state abb
# PlottingCountyData<-PlottingCountyData %>%
#   mutate(state_name = tolower(state.name[match(State, state.abb)]))
PlottingCountyData<-data.frame(PlottingCountyData[,5],rev(PlottingCountyData)[,1])
colnames(PlottingCountyData)<-c("GEOID","Cases")
#Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
county_df<-counties(state = NULL, cb = TRUE, resolution = "5m")




#######################Create National Data table on summary page

NationalDataTable<-CovidConfirmedCases
NationalDataTable$State<-as.factor(NationalDataTable$State)
NationalDataTable<-NationalDataTable[,-c(1,2,4)]
NationalDataTable<-aggregate(.~State, NationalDataTable, sum)
RateofCovidChange<-rev(NationalDataTable)[c(1:7)]
RateofCovidChange<-ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

NationalDeathTable<-CovidDeaths
NationalDeathTable$State<-as.factor(NationalDeathTable$State)
NationalDeathTable<-NationalDeathTable[,-c(1,2,4)]
NationalDeathTable<-aggregate(.~State, NationalDeathTable, sum)
RateofDeathChange<-rev(NationalDeathTable)[c(1:7)]
RateofDeathChange<-ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)

NationalDataTable<-data.frame(NationalDataTable$State, NationalDataTable[,length(NationalDataTable)],RateofCovidChange, NationalDeathTable[,length(NationalDeathTable)], RateofDeathChange)
colnames(NationalDataTable)<-c("State","Total Cases","Average New Cases Per Day", "Total Deaths","Average New Deaths Per Day")
NationalDataTable$`Cases Per 100,000 People`<-c(731545,4903185,3017825,7278717,39512223,5758736,3565287,705749,973764,21477737,10617423,1415872,3155070,1787065,12671821,6732219,2913314,4467673,4648794,6949503,6045680,1344212,9986857,5639632,6137428,2976149,1068778,10488084,762062,1934408,1359711,8882190,2096829,3080156,19453561,11689100,3956971,4217737,12801989,1059361,5148714,884659,6833174,28995881,3205958,8535519,623989,7614893,5822434,1792147,578759)
NationalDataTable$`Cases Per 100,000 People`<-round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 100,000 People`/100000))



# beds <- read.csv('beds.csv')
# pops <- read.csv('pops.csv')
# source('acme_support.R')
##########################################################################################################
##########################################################################################################
##########################################################################################################
############################################################################################################################################







#######################################################
############### Helper Functions ######################
#######################################################







###################################################################################################################################################
# Statistics for Local Health Page -------------------------------------------------------------------------------------------------------------------------------------



##########################################################################################################
##########################################################################################################
##########################################################################################################
# Create Charts for plotting lines showing trends among the virus  ------------------------------------------------------------------------------------------------------------------



#Create charts for Local Health Tab


#This function creates the dataframe for plotting daily cases, deaths, estimated hospitalizations in selected region





    




# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------





# Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------





# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------




# Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------


# Output Projections ---------------------------------------------------------------------------------------------------------------------------------------------------------------
AFrow = nrow(AFBaseLocations)
ForecastDataTable <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                                                                        "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                        "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                        "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date"))

ForecastDataTableCases <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),c("Installation","MAJCOM","State","Available Beds","Cases Per 100,000", "Cases Per 10,000", "New Cases",
                                                                             "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                                                                             "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                                                                             "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                                                                             "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date"))


for (i in 2:AFrow){
  #Create Number of current cases and cases per 100,000 in a local area
  MyCounties<-GetCounties(AFBaseLocations$Base[i],60)
  CovidDataCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  NewCases<-sum(rev(CovidDataCounties)[,1]-rev(CovidDataCounties)[,2])
  NewHospitalizations<-round(NewCases*.2)
  TotalPop<-CalculateCounties(MyCounties)
  TotalCases<-CalculateCovid(MyCounties)
  CasesPer100000<-round(TotalCases/TotalPop*100000)
  CasesPer10000<-round(TotalCases/TotalPop*10000)
  HospitalizationsPer100000<-round(CasesPer100000*.2)
  HospitalizationsPer10000<-round(HospitalizationsPer100000/10)
  
  
  #Create a datatable with just the forecasted values for every installation
  #Creating the stats and dataframes determined by the base we choose to look at.
  #IHME_Model is the initial import data table from global.R
  #BaseState<-AFBaseLocations$State[i] #dplyr::filter(AFBaseLocations, Base == baseinput)
  #IncludedHospitals<-GetHospitals() 
  #GetHospitals
  HospitalInfo$DistanceMiles = himd[,as.character(AFBaseLocations$Base[i])]
  MyHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= 60))
  MyHospitals<-dplyr::filter(MyHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
  
  IHME_State <- dplyr::filter(IHME_Model, State == AFBaseLocations$State[i])
  TotalBedsCounty <- sum(MyHospitals$BEDS)
  
  
  hospCounty <- subset(HospUtlzCounty, fips %in% MyCounties$FIPS)
  #Finds number of hospitals in radius
  TotalBeds<-sum(hospCounty$num_staffed_beds)
  #get historic utilization
  hospCounty$bedsUsed <- hospCounty$bed_utilization * hospCounty$num_staffed_beds
  totalUsedBeds <- sum(hospCounty$bedsUsed)
  baseUtlz <- totalUsedBeds/TotalBeds
  
  #Get regional and state populations
  #MyCounties <- GetCounties()
  #GetCounties
  CountyInfo$DistanceMiles = cimd[,as.character(AFBaseLocations$Base[i])]
  MyCounties<-dplyr::filter(CountyInfo, DistanceMiles <= 60)
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
  HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
  HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
  HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21) #, HistoricalData*.15, HistoricalData*.27)
  colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations") #, "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
  
  StPopList <- dplyr::filter(CountyInfo, State == AFBaseLocations$State[i])
  RegPop <- sum(MyCounties$Population)
  StPop <- sum(StPopList$Population)
  
  # Use Population ratio to scale IHME
  PopRatio <- RegPop/StPop
  
  # Get total hospital bed number across state
  IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == AFBaseLocations$State[i])
  TotalBedsState <- sum(IncludedHospitalsST$BEDS)
  
  # Calculate bed ratio
  BedProp <- TotalBedsCounty/TotalBedsState
  
  # Apply ratio's to IHME data
  IHME_Region <- IHME_State
  IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
  #IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
  #IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
  IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean) #, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
  colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations") #, "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
  IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date())
  
  IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)
  IHME_Region <- dplyr::arrange(IHME_Region,ForecastDate)
  
  DeathCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
  CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% MyCounties$FIPS)
  CountyDataTable<-cbind(MyCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
  CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
  colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
  
  ####################################################################################
  #Mean Estimate
  
  #Next we use the calculated values, along with estimated values from the Estimated Values. 
  #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
  #CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS) 
  ActiveCases<-rev(CovidCounties)[1:7]
  ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1],MyCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
  colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
  SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`)) 
  colnames(SIRinputs)<-c("cases","pop","doubling")
  
  #Established Variables at the start for every county or populations 
  cases<-SIRinputs$cases
  pop<-SIRinputs$pop
  
  if(nrow(IHME_Region) == 0 || pop == 0){
    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0,
                        0,0,0,0,0,0)
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000", "New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0,
                             0,0,0,0,0,0)
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000", "New Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")    
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
  }else{ 
    incubationtime<-5
    latenttime<-2
    doubling<-8 
    recoverydays<-14
    socialdistancing<-15
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    Ro<-2.5
    
    daysforecasted<-60
    SEIARProj<-SEIAR_Model_Run(cases,pop,incubationtime,latenttime,doubling,recoverydays,socialdistancing,hospitalizationrate,
                               icurate,ventilatorrate,hospitaltime,icutime,ventilatortime,daysforecasted,Ro,.5)
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate","Expected Hospitalizations")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate > Sys.Date())
    ########################################################################################
    SevDayVal<-round(DailyData$`Expected Hospitalizations`[7])
    FourteenDayVal<-round(DailyData$`Expected Hospitalizations`[14])
    ThirtyDayVal<-round(DailyData$`Expected Hospitalizations`[21])
    SixtyDayVal<-round(DailyData$`Expected Hospitalizations`[30])
    PeakSevDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:7]))
    PeakFourteenDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:14]))
    PeakThirtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:21]))
    PeakSixtyDayVal<-round(max(DailyData$`Expected Hospitalizations`[1:30]))
    PeakDateSevDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:7])
    PeakDateFourteenDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:14])
    PeakDateThirtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:21])
    PeakDateSixtyDayVal<-which.max(DailyData$`Expected Hospitalizations`[1:30])
    PeakDateSevDayVal<-format(DailyData$ForecastDate[PeakDateSevDayVal], format="%b-%d")
    PeakDateFourteenDayVal<-format(DailyData$ForecastDate[PeakDateFourteenDayVal], format="%b-%d")
    PeakDateThirtyDayVal<-format(DailyData$ForecastDate[PeakDateThirtyDayVal], format="%b-%d")
    PeakDateSixtyDayVal<-format(DailyData$ForecastDate[PeakDateSixtyDayVal], format="%b-%d")
    
    
    #BEGIN IHME CALCS
    I1 = round(IHME_Region$`Expected Hospitalizations`[7])
    I2 = round(IHME_Region$`Expected Hospitalizations`[14])
    I3 = round(IHME_Region$`Expected Hospitalizations`[21])
    I4 = round(IHME_Region$`Expected Hospitalizations`[30])
    
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:7])
    Peak<-IHME_Region[PeakDate,2]
    PI1<-round(Peak)
    PID1<-IHME_Region[PeakDate,1]
    PID1<-format(PID1, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:14])
    Peak<-IHME_Region[PeakDate,2]
    PI2<-round(Peak)
    PID2<-IHME_Region[PeakDate,1]
    PID2<-format(PID2, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:21])
    Peak<-IHME_Region[PeakDate,2]
    PI3<-round(Peak)
    PID3<-IHME_Region[PeakDate,1]
    PID3<-format(PID3, format="%b-%d")
    PeakDate<-which.max(IHME_Region$`Expected Hospitalizations`[1:30])
    Peak<-IHME_Region[PeakDate,2]
    PI4<-round(Peak)
    PID4<-IHME_Region[PeakDate,1]
    PID4<-format(PID4, format="%b-%d")
    
    NewDF <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), HospitalizationsPer100000, HospitalizationsPer10000, NewHospitalizations,
                        I1,PI1,PID1,SevDayVal,PeakSevDayVal,PeakDateSevDayVal,
                        I2,PI2,PID2,FourteenDayVal,PeakFourteenDayVal,PeakDateFourteenDayVal,
                        I3,PI3,PID3,ThirtyDayVal,PeakThirtyDayVal,PeakDateThirtyDayVal,
                        I4,PI4,PID4,SixtyDayVal,PeakSixtyDayVal,PeakDateSixtyDayVal) 
    names(NewDF) <- c("Installation","MAJCOM","State","Available Beds", "Hopitalization Per 100,000", "Hopitalization Per 10,000","New Hospitalizations",
                      "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                      "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                      "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                      "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")
    ForecastDataTable <- rbind(ForecastDataTable,NewDF)
    
    NewDFCases <- data.frame(AFBaseLocations$Base[i],AFBaseLocations$`Major Command`[i],AFBaseLocations$State[i],round(TotalBedsCounty*(1-baseUtlz)), CasesPer100000, CasesPer10000, NewCases,
                             I1/.2,PI1/.2,PID1,SevDayVal/.2,PeakSevDayVal/.2,PeakDateSevDayVal,
                             I2/.2,PI2/.2,PID2,FourteenDayVal/.2,PeakFourteenDayVal/.2,PeakDateFourteenDayVal,
                             I3/.2,PI3/.2,PID3,ThirtyDayVal/.2,PeakThirtyDayVal/.2,PeakDateThirtyDayVal,
                             I4/.2,PI4/.2,PID4,SixtyDayVal/.2,PeakSixtyDayVal/.2,PeakDateSixtyDayVal) 
    names(NewDFCases) <- c("Installation","MAJCOM","State","Available Beds", "Cases Per 100,000", "Cases Per 10,000","New Cases",
                           "7D IHME Forecast","7D IHME Peak","7D IHME Peak Date","7D SEIAR Forecast","7D SEIAR Peak","7D SEIAR Peak Date",
                           "14D IHME Forecast","14D IHME Peak","14D IHME Peak Date","14D SEIAR Forecast","14D SEIAR Peak","14D SEIAR Peak Date",
                           "21D IHME Forecast","21D IHME Peak","21D IHME Peak Date","21D SEIAR Forecast","21D SEIAR Peak","21D SEIAR Peak Date",
                           "30D IHME Forecast","30D IHME Peak","30D IHME Peak Date","30D SEIAR Forecast","30D SEIAR Peak","30D SEIAR Peak Date")
    ForecastDataTableCases <- rbind(ForecastDataTableCases,NewDFCases)
  }
}

ForecastDataTable$Installation<-as.character(ForecastDataTable$Installation)
ForecastDataTable<-ForecastDataTable %>% arrange(ForecastDataTable$Installation)

ForecastDataTableCases$Installation<-as.character(ForecastDataTableCases$Installation)
ForecastDataTableCases<-ForecastDataTableCases %>% arrange(ForecastDataTableCases$Installation)


#Create Top 15 Bases Report###################################################################################
TruncatedReport<-ForecastDataTable[order(ForecastDataTable$`Hopitalization Per 100,000`, decreasing = TRUE),]
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "ANG")
TruncatedReport<-TruncatedReport %>% filter(MAJCOM != "AFRC")
TruncatedReport<-TruncatedReport[,c(1,7,20:25)]
colnames(TruncatedReport)<-c("Installation","New Hospitalizations", "30 Day IHME (Hosp)","30 Day IHME Peak (Hosp)", "30 Day IHME Date (Hosp)", "30 Day CHIME (Hosp)", "30 Day CHIME Peak (Hosp)", "30 Day CHIME Date (Hosp)")

TruncatedReport2<-ForecastDataTableCases[order(ForecastDataTableCases$`Cases Per 100,000`, decreasing = TRUE),]
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "ANG")
TruncatedReport2<-TruncatedReport2 %>% filter(MAJCOM != "AFRC")
TruncatedReport2<-TruncatedReport2[c(1:15),c(1,2,3,4,5,6,7,20:25)]
colnames(TruncatedReport2)<-c("Installation","MAJCOM","State", "Availab Beds", "Cases Per 100,000", "Cases Per 10,000", "Cases Today", "30 Day IHME (Cases)","30 Day IHME Peak (Cases)", "30 Day IHME Date (Cases)", "30 Day CHIME (Cases)", "30 Day CHIME Peak (Cases)", "30 Day CHIME Date (Cases)")

Top15Report<-join(TruncatedReport2, TruncatedReport, by = "Installation")
Top15Report<-Top15Report[,c(1,2,3,5,6,7,14,4,8,9,10,11,12,13,15,16,17,18,19,20)]
rm(TruncatedReport)
rm(TruncatedReport2)
##############################################################################################################




######################## Summary Tab Heat Map
HeatMapForecast<-merge(AFBaseLocations, ForecastDataTable, by.x = "Base", by.y = "Installation")
HeatMapForecast<-data.frame(HeatMapForecast$Base, HeatMapForecast$Location, HeatMapForecast$State.x, HeatMapForecast$`Major Command`, HeatMapForecast$Lat, HeatMapForecast$Long,HeatMapForecast$`Available Beds`,HeatMapForecast$`Hopitalization Per 100,000`,HeatMapForecast$`Hopitalization Per 10,000`,HeatMapForecast$`New Hospitalizations`,HeatMapForecast$`New Hospitalizations` ,HeatMapForecast$`7D SEIAR Forecast`, HeatMapForecast$`7D IHME Forecast`,HeatMapForecast$`14D SEIAR Forecast`,  HeatMapForecast$`14D IHME Forecast`,  HeatMapForecast$`21D SEIAR Forecast`, HeatMapForecast$`21D IHME Forecast`, HeatMapForecast$`30D SEIAR Forecast`, HeatMapForecast$`30D IHME Forecast`)
colnames(HeatMapForecast)<-c("Base","City","State","MAJCOM","Lat","Long","Beds","Hospitalizations Per 100,000","Hospitalizations Per 10,000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME","Fourteen.IHME","Fourteen.CHIME","Twenty-One.IHME","Twenty-One.CHIME", "Thirty.IHME","Thirty.CHIME")
HeatMapForecast<-reshape(HeatMapForecast, direction='long', 
                         varying=c('Today.CHIME','Today.IHME','Seven.IHME', 'Seven.CHIME', 'Fourteen.IHME', 'Fourteen.CHIME','Twenty-One.IHME','Twenty-One.CHIME','Thirty.IHME','Thirty.CHIME'), 
                         timevar='Days',
                         times=c('Today','Seven', 'Fourteen',"Twenty-One","Thirty"),
                         v.names=c('CHIME', 'IHME'),
                         idvar=c('Base','City','State','MAJCOM','Lat','Long','Beds',"Hospitalizations Per 100,000","Hospitalizations Per 10,000"))
HeatMapForecast<-transform(HeatMapForecast,IHMEID=ifelse((Beds)>=IHME,"Under Capacity","Over Capacity"))
HeatMapForecast<-transform(HeatMapForecast,CHIMEID=ifelse((Beds)>=CHIME,"Under Capacity","Over Capacity"))


HeatMapForecastCases<-merge(AFBaseLocations, ForecastDataTableCases, by.x = "Base", by.y = "Installation")
HeatMapForecastCases<-data.frame(HeatMapForecastCases$Base, HeatMapForecastCases$Location, HeatMapForecastCases$State.x, HeatMapForecastCases$`Major Command`, HeatMapForecastCases$Lat, HeatMapForecastCases$Long,HeatMapForecastCases$`Available Beds`,HeatMapForecastCases$`Cases Per 100,000`,HeatMapForecastCases$`Cases Per 10,000`,HeatMapForecastCases$`New Cases`,HeatMapForecastCases$`New Cases` ,HeatMapForecastCases$`7D SEIAR Forecast`, HeatMapForecastCases$`7D IHME Forecast`,HeatMapForecastCases$`14D SEIAR Forecast`,  HeatMapForecastCases$`14D IHME Forecast`,  HeatMapForecastCases$`21D SEIAR Forecast`, HeatMapForecastCases$`21D IHME Forecast`, HeatMapForecastCases$`30D SEIAR Forecast`, HeatMapForecastCases$`30D IHME Forecast`)
colnames(HeatMapForecastCases)<-c("Base","City","State","MAJCOM","Lat","Long","Beds","Cases Per 100,000","Cases_Per_10000","Today.CHIME","Today.IHME", "Seven.IHME","Seven.CHIME","Fourteen.IHME","Fourteen.CHIME","Twenty-One.IHME","Twenty-One.CHIME","Thirty.IHME","Thirty.CHIME")
HeatMapForecastCases<-reshape(HeatMapForecastCases, direction='long', 
                              varying=c('Today.CHIME','Today.IHME','Seven.IHME', 'Seven.CHIME', 'Fourteen.IHME', 'Fourteen.CHIME','Twenty-One.IHME','Twenty-One.CHIME','Thirty.IHME','Thirty.CHIME'), 
                              timevar='Days',
                              times=c('Today','Seven', 'Fourteen',"Twenty-One","Thirty"),
                              v.names=c('CHIME', 'IHME'),
                              idvar=c('Base','City','State','MAJCOM','Lat','Long','Beds',"Cases Per 100,000","Cases_Per_10000"))
HeatMapForecastCases<-transform(HeatMapForecastCases,IHMEID=ifelse((Cases_Per_10000*10000*.05)>=IHME,"Under 5% Population","Over 5% Population"))
HeatMapForecastCases<-transform(HeatMapForecastCases,CHIMEID=ifelse((Cases_Per_10000*10000*.05)>=CHIME,"Under 5% Population","Over 5% Population"))



####################################################################




# Identify Info Pages
# Inputs
InfoLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/InputsInfo.md")
CalcLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/CalcInfo.md")
SourceLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/SourceInfo.md")
OverviewLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/OverviewInfo.md")
ProjLink <- includeMarkdown("https://github.com/treypujats/CHAD/raw/master/ProjInfo.md")


