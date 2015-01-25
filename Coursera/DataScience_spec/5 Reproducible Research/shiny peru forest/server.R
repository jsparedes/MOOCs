library(shiny)
library(rCharts)
require(googleVis)
library(RCurl)
library(dplyr)
library(markdown) # To include .md files (markdown)

# Load helper functions
source("helpers.R", local = TRUE)


# Load data
# Because of the Spanish alphabet symbols and the use of 
# googleVis package, the database contains a small alterations
# (check last part of this file) to get a correct regions plot
# of my country.
dt = read.csv('peru_forest.csv')
dt2 = dt
levels(dt2$region) = c("Amazonas", "Ancash", "Apurímac", "Arequipa", 
                      "Ayacucho", "Cajamarca", "Cusco", "El Callao",
                      "Huánuco", "Huancavelica", "Ica", "Junín",
                      "La Libertad", "Lambayeque", "Lima", "Loreto",
                      "Madre de Dios", "Moquegua", "Pasco", "Piura",
                      "Puno", "San Martín", "Tacna", "Tumbes", "Ucayali")

cdtypes = unique(dt$min_percent_canopy_density)


# Shiny server 
shinyServer(function(input, output, session) {
    
    # Define and initialize reactive values
    values = reactiveValues()
    values$cdtypes = cdtypes
    
    # Create event type checkbox
    output$cdtypeControls = renderUI({
        checkboxGroupInput('cdtypes', 'Canopy density (percentage):', cdtypes, selected=values$cdtypes)
    })
    
    # Add observers on clear and select all buttons
    observe({
        if(input$clear_all == 0) return()
        values$cdtypes = 75 # At least one option is used
    })
    
    observe({
        if(input$select_all == 0) return()
        values$cdtypes = cdtypes
    })

    # Preparing datasets
    
    # Prepare dataset for time series
    dt.agg.year = reactive({
        aggregate_by_year(dt, input$range[1], input$range[2], input$cdtypes)
    })
    
    dt.agg.cd = reactive({
      aggregate_by_cd(dt, input$range[1], input$range[2], cdtypes)
    })
    
    # Prepare dataset for maps
    myData = reactive({
      
      if (length(input$cdtypes)==0) {
        input$cdtypes = values$cdtypes
      }
      
      dt %>% mutate(res = {
        if(input$indicatorForest == "losses") {
          loss_ha
        } else {
          gain_ha
        } 
      }) %>% select(region, res, year, min_percent_canopy_density) %>%
        filter(year >= input$range[1], year <= input$range[2], min_percent_canopy_density %in% as.integer(input$cdtypes) ) %>%
        group_by(region) %>% summarise_each(funs(sum), res)
    })
    
   
    
    # Forest impact by region
    output$ImpactByRegion = renderGvis({

      G = gvisGeoChart(myData(),
                   locationvar="region", colorvar="res",
                   options=list(region="PE", displayMode="regions", 
                                resolution="provinces",
                                width=400, height=300,
                                colorAxis="{colors:['#808080', '#ffff00', '#ffcc00', '#cca300', '#997a00', '#4c3d00']}"

                    )
      )
      
      myData2 = myData()
      names(myData2) = c("Regions", "Hectares")
      
      Tab = gvisTable(myData2, option = list(width=285, height=320, 
                                             page='enable'
                                             )
      )
      
      GT = gvisMerge(G, Tab, horizontal = TRUE) 
      
    })
    
    
    # Hectares by year (Lost or gained)
    output$HaByYear = renderChart({
       plot_events_by_year(dt.agg.year())
    })
    
    # Lost hectares by canopy density (percentage)
    output$cdcontribution = renderChart({
        plot_impact_by_year(
            dt = dt.agg.cd() %>% select(min_percent_canopy_density, loss_ha),
            dom = "cdcontribution",
            yAxisLabel = "ha",
            desc = TRUE
        )
    })
    
    # Render data table and create download handler
    output$table = renderDataTable(      
        {dt2}, options = list(orderClasses = TRUE, pageLength = 10)
    )

    # Function to download the dataset from DataTables(Shiny)
    output$downloadData <- downloadHandler(
        filename = 'data.csv',
        content = function(file) {
            write.csv(dataTable(), file, row.names=FALSE)
        }
    )
})

## Mentioned small alterations in region names:
#
#   According googleVis || Real Names
#   ==================================
#   Apur\u00edmac       || Apurímac
#   Cuzco               || Cusco
#   El Callao           || Callao
#   Hu\u00e1nuco        || Huánuco
#   Jun\u00edn          || Junín
#   San Mart\u00edn     || San Martín