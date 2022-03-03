gc()
print(getwd())
setwd(getwd())
#setwd("/home/i7mintu/Documents/PROJECTS/R-ShinyProj/shiny_eg_001_municipality_analysis")

library(shiny)
library(leaflet)
#library(arrow, warn.conflicts = FALSE)  # Add CSV speedups to script


options(run.main=FALSE)
source("bknd_muni_subsidios_prepare_df.r", encoding = "UTF-8")

df_muni <- loadSubsidiesXOrganismos( "./TA_Subsidios_beneficios.csv" , "organismos360_updated.csv"  )

df_muni <- leafletDfPrepwork( df_muni )

# Define UI for the APP ----
ui <- bootstrapPage(
  # App title ----
  titlePanel("Chile Subsidies Nov 2021 Analysis"),
  leafletOutput(outputId = "leafletMap" , width = "100%", height = "800px"), # height= 100% doesn't pass through
  
  modalDialog(
    dataTableOutput(outputId = "dataTbl1") ,   
    title = "This is a Modal",
    footer = modalButton("Dismiss"),
    size = "xl",
    easyClose = FALSE,
    fade = TRUE
  ),
  
  dataTableOutput(outputId = "dataTbl1"),
  
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$dataTbl1 <- renderDataTable({
    filtered_muni <- df_muni %>% 
      filter(  ORG_NOMBRE == "Municipalidad de Osorno"  )
  })
  
  output$leafletMap <- renderLeaflet({
      leaflet(data = df_muni ) %>%
      setView(lat = -36.82699, lng = -73.04977, zoom = 3) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addTiles() %>%
      addCircleMarkers(~LON, ~LAT, popup =  ~MSG_SUMMARY,  label=~MSG_SUMMARY )
  })
}

shinyApp(
          ui = ui, server = server , 
          options=list(port=4050, launch.browser=FALSE ) 
         )

