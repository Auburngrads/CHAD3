#' @export
CalculateDeaths<-function(IncludedCounties, CovidDeaths){
  
    #Get total deaths in the selected region
    CovidCountiesDeath<-subset(CovidDeaths, 
                               CountyFIPS %in% IncludedCounties$FIPS)
    
    return(sum(rev(CovidCountiesDeath)[,1]))
}
