#' Get total confirmed cases in the selected region
#' @export
CalculateCovid<-function(IncludedCounties, CovidConfirmedCases){
  
    CovidCounties = subset(CovidConfirmedCases, 
                           CountyFIPS %in% IncludedCounties$FIPS)
    
    return(sum(rev(CovidCounties)[,1]))
    
}
