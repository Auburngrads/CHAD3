#' #Get the total population in the selected region
#' @export
CalculateCounties<-function(IncludedCounties){
  
    return(sum(IncludedCounties$Population))
  
}
