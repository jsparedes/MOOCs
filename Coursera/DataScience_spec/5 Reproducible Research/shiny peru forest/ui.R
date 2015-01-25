# Inputs:
#    - range: range[1], range[2]
#    - clear_all
#    - select_all
#    - indicatorForest: c("Losses ha" = "losses", "Gain ha" = "gain") # To do plot by region
#    - input$cdtypes: represent canopy density percentage types 


shinyUI(
    navbarPage("Peru Forest Explorer",
        tabPanel("Visualizations",
                sidebarPanel(
                    sliderInput("range", 
                        "Range:", min = 2000, max = 2012, 
                        value = c(2003, 2005),
                        sep = ""
                    ),
                    uiOutput("cdtypeControls"),
                    actionButton(inputId = "clear_all", label = "Clear selection", icon = icon("check-square")),
                    actionButton(inputId = "select_all", label = "Select all", icon = icon("check-square-o"))
                ),
  
                mainPanel(
                    tabsetPanel(
                        
                        # Data by region
                        tabPanel(p(icon("map-marker"), "By region"),
                            column(12,
                                wellPanel(
                                    radioButtons(
                                        "indicatorForest",
                                        "Forest impact:",
                                        c("Losses (ha)" = "losses", "Gain (ha)" = "gain"))
                                )
                            ),
                            column(7,
                                htmlOutput("ImpactByRegion")
                            )
                        ),
                        
                        # Time series data
                        tabPanel(p(icon("line-chart"), "By year"),
                                 h4('Hectares of forest lost by year', align = "center"),
                                 showOutput("HaByYear", "nvd3"),
                                 h4('Lost hectares by canopy density', align = "center"),
                                 showOutput("cdcontribution", "nvd3")
                        ),
                        
 
                        # Data 
                        tabPanel(p(icon("table"), "Data"),
                            dataTableOutput(outputId="table"),
                            downloadButton('downloadData', 'Download')
                        )
                    )
                )
            
        ),
        
        tabPanel("About", mainPanel(includeMarkdown("include.md"))
        ),
        
        tabPanel("Help", mainPanel(includeMarkdown("instructions.md"))
        )
    )
)
