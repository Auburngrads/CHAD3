#' @return  
#' @export
#' @importFrom dplyr filter
GetCounties<-function(base,radius){
    
    #Find counties in radius
    CountyInfo$DistanceMiles = cimd[,as.character(base)]
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius)
    IncludedCounties
    
}
