#' Get counties within a specified radius 
#' @export
#' @importFrom dplyr filter
GetCounties<-function(base, radius, CountyInfo, cimd){
    
    #Find counties in radius
    CountyInfo$DistanceMiles = cimd[,as.character(base)]
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius)
    IncludedCounties
    
}
