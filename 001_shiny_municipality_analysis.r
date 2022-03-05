#gc()
print(getwd())
setwd(getwd())
#setwd("/home/i7mintu/Documents/PROJECTS/R-ShinyProj/shiny_eg_001_municipality_analysis")

library(shiny)
library(leaflet)
library(dplyr, warn.conflicts = FALSE )
#library(arrow, warn.conflicts = FALSE)  # Add CSV speedups to script


#options(run.main=FALSE)
#source("bknd_muni_subsidios_prepare_df.r", encoding = "UTF-8")


df_muni_full <- readRDS(file = "df_muni_full.Rds")
df_muni_summary <- readRDS(file = "df_muni_summary.Rds")
df_muni <- readRDS(file = "df_muni_leaf.Rds")
  ## Available in order to include CSV files from Dec, Jan, Feb ... etc
  #df_muni_summary <- loadSubsidiesXOrganismos( "./TA_Subsidios_beneficios.csv" , "organismos360_updated.csv"  )
  #df_muni <- leafletDfPrepwork( df_muni_summary )




# Define UI for the APP ----
ui <- bootstrapPage(
  # App title ----
  h1("Chile Subsidies Nov 2021 Analysis"),
  actionButton(inputId = "buttonCount_n", label = "Relative to #Entries" ),
  actionButton(inputId = "buttonTotalMonto", label = "Relative to #Money" ),
  
  leafletOutput(outputId = "leafletMap" , width = "100%", height = "800px"), # height= 100% doesn't pass through
 
  dataTableOutput(outputId = "dataTbl2"),
)

radiusChosen <- 0

# Define server logic required to draw a histogram ----
server <- function(input, output) {
 
  radiusColSrcName <- function(radiusChosen){
   return(  
     switch( radiusChosen, 
            ~CIRCLE_RADIUS_MONTO_TOTAL, 
            ~CIRCLE_RADIUS_COUNT_N 
            )
   )
  }
   
  changeRadiusVectorSrc <- function( radiusChosen)
  {
    leafletProxy("leafletMap", data = df_muni) %>%
      clearShapes() %>%
      addCircleMarkers(
        ~LON, ~LAT, 
        popup = ~MSG_SUMMARY,  
        label = ~MSG_SUMMARY , 
        layerId = ~ORG_NOMBRE ,
        radius = radiusColSrcName(radiusChosen),
        #radius = switch( radiusChosen, ~CIRCLE_RADIUS_MONTO_TOTAL, ~CIRCLE_RADIUS_COUNT_N ),    # works !!
        #radius = if( radiusChosen > 0) {  ~CIRCLE_RADIUS_MONTO_TOTAL} else { ~CIRCLE_RADIUS_COUNT_N  } , # also works!!
        stroke = FALSE, fillOpacity = 0.5,
      )
  }

  observeEvent(input$buttonTotalMonto, {
    changeRadiusVectorSrc(1)
  }, ignoreNULL = TRUE)

  observeEvent(input$buttonCount_n, {
    changeRadiusVectorSrc(2)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$leafletMap_marker_click, {
    click <- input$leafletMap_marker_click
    print(paste0("clicked ::: ", click ) )
    summary_selected <- df_muni %>% filter( ORG_NOMBRE == click[1]  )
    filtered_full <- df_muni_full %>% filter(organismo_codigo == as.character( summary_selected$organismo_codigo ) )
    
    output$dataTbl1 = renderDataTable({  filtered_full    }) 
    
    showModal(modalDialog(
      {
        p( )
        dataTableOutput(outputId = "dataTbl1") 
      },   
      title = as.character(summary_selected$MSG_SUMMARY), #click[1],
      footer = modalButton("Dismiss"),
      size = "xl" ,
      easyClose = TRUE,
      fade = TRUE
    ))
  })
  
  output$leafletMap <- renderLeaflet({
      leaflet(data = df_muni ) %>%
      setView(lat = -36.82699, lng = -73.04977, zoom = 10) %>% 
      addTiles() %>%
      addMiniMap( position = "bottomleft", zoomAnimation = TRUE , zoomLevelOffset = -4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~LON, ~LAT, 
        popup = ~MSG_SUMMARY,  
        label = ~MSG_SUMMARY , 
        layerId = ~ORG_NOMBRE ,
        radius = radiusColSrcName(1),      #radius = ~CIRCLE_RADIUS_MONTO_TOTAL,
        stroke = FALSE, fillOpacity = 0.5,
        )
  })
  
}

shinyApp(
          ui = ui, server = server , 
          options=list(port=4050, launch.browser=FALSE ) 
         )

