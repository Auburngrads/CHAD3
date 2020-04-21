#' @export
#' @import magrittr
#' @importFrom plotly layout toRGB plot_geo add_markers
GetHeatMap<-function(MAJCOMChoice,
                     ModelChoice,
                     ForecastChoice,
                     Stat){
  
  HeatMap <- HeatMapForecast
  Banner <- "Projected Daily New Hospitalizations"

  
  if (Stat == "Cases") {
    
      HeatMap <- HeatMapForecastCases
      Banner <- "Projected Daily New Cases"
    
  } 
  
  HeatMapout <- HeatMap %>% filter(MAJCOM == MAJCOMChoice & Days == ForecastChoice)

  HeatMapout = switch(MAJCOMChoice, 
                      "All"         = HeatMap %>% filter(Days == ForecastChoice),
                      "Active Duty" = HeatMap %>% filter((!MAJCOM %in% c("ANG","AFRC")) & (Days == ForecastChoice)))

  HeatMap = HeatMapout 
  
     g <- list(scope = 'usa',
               projection = list(type = 'albers usa'),
               showland = TRUE,
               landcolor = plotly::toRGB("gray85"),
               subunitwidth = 1,
               countrywidth = 1,
               subunitcolor = plotly::toRGB("white"),
               countrycolor = plotly::toRGB("white"))


    fig <- plotly::plot_geo(HeatMap, 
                            locationmode = 'USA-states', 
                            sizes = c(20, 400))
    fig <- fig %>% plotly::add_markers(x = ~Long, 
                                       y = ~Lat, 
                                       size = `if`(ModelChoice=="IHME", ~IHME, ~CHIME), 
                                       color = `if`(ModelChoice=="IHME", ~IHMEID, ~CHIMEID), 
                                       colors = c("red","#228B22"), 
                                       hoverinfo = "text",
                                       text = ~paste(HeatMap$Base, "<br />", `if`(ModelChoice=="IHME", HeatMap$IHME, HeatMap$CHIME)))
    fig <- fig %>% plotly::layout(title = Banner, geo = g, showlegend=TRUE)
    fig <- fig %>% plotly::layout(legend = list(orientation = "h",   # show entries horizontally
                                                xanchor = "center",  # use center of legend as anchor
                                                x = 0.5,
                                                y = 0.95))
    
    fig
    
}
