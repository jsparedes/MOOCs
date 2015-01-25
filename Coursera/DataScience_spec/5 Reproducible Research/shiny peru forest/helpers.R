# Dataset which show total hectares lost or gained in the selected range of years
aggregate_by_year <- function(dt, year_min, year_max, cdtypes) {

    dt %>% filter(year >= year_min, year <= year_max, min_percent_canopy_density %in% cdtypes) %>%
           group_by(year) %>% summarise_each(funs(sum), loss_ha, gain_ha)

}

# Dataset which show canopy density percent in each year
aggregate_by_cd <- function(dt, year_min, year_max, cdtypes) {
  
    dt %>% filter(year >= year_min, year <= year_max, min_percent_canopy_density %in% cdtypes) %>%
      group_by(year, min_percent_canopy_density) %>% summarise_each(funs(sum), loss_ha, gain_ha)

}

# To select between Hectares lost or gained
compute_indicator = function(data, indicador) {
  data %>% mutate(res = {
    if(indicador == 'both') {
      loss_ha - gain_ha
    } else if(indicador == 'losses') {
      loss_ha
    } else {
      gain_ha
    } 
  }) %>% select(region, res)
}


# Function to plot Stacked Area Chart with NDV3 (rCharts)
plot_impact_by_year <- function(dt, dom_id, yAxisLabel, desc = FALSE) {

    impactPlot <- nPlot(
        loss_ha ~ year, group = 'min_percent_canopy_density',
        data = dt,
        type = 'stackedAreaChart', dom = dom_id, width = 650
    )
    impactPlot$chart(margin = list(left = 100))
    impactPlot$yAxis(axisLabel = yAxisLabel, width = 80)
    impactPlot$xAxis(axisLabel = "Year", width = 70)
    
    return(impactPlot)
}


# Function to plot Linear Chart with NDV3 (rCharts)
plot_events_by_year <- function(dt, dom = "HaByYear", yAxisLabel = "ha") {
  
    HaByYear <- nPlot(
      loss_ha ~ year,
        data = dt,
        type = "lineChart", dom = dom, width = 650
    )
        
    HaByYear$chart(margin = list(left = 100))
    HaByYear$yAxis( axisLabel = yAxisLabel, width = 80)
    HaByYear$xAxis( axisLabel = "Year", width = 70)
    HaByYear
}