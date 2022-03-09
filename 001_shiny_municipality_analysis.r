#gc()
print(getwd())
setwd(getwd())
#setwd("/home/i7mintu/Documents/PROJECTS/R-ShinyProj/shiny_eg_001_municipality_analysis")

library(shiny)
library(leaflet)
library(dplyr, warn.conflicts = FALSE )
library(stringr)
#library(arrow, warn.conflicts = FALSE)  # Add CSV speedups to script


#options(run.main=FALSE)
#source("bknd_muni_subsidios_prepare_df.r", encoding = "UTF-8")


df_muni_full <- readRDS(file = "df_muni_full.Rds")
df_muni_summary <- readRDS(file = "df_muni_summary.Rds")
df_muni_leaf <- readRDS(file = "df_muni_leaf.Rds")
  ## Available in order to include CSV files from Dec, Jan, Feb ... etc
  #df_muni_summary <- loadSubsidiesXOrganismos( "./TA_Subsidios_beneficios.csv" , "organismos360_updated.csv"  )
  #df_muni_leaf <- leafletDfPrepwork( df_muni_summary )

chile_comunas_shp_joined <- readRDS(file = "chile_comunas_shp_joined.Rds")
chile_comunas_shp_joined <- chile_comunas_shp_joined %>% sf::st_transform('+proj=longlat +datum=WGS84')

iconQuestionMarkPng <- makeIcon(
  iconUrl = "question-circle-o.png",
  iconWidth = 24, iconHeight = 24
)


# Define UI for the APP ----
ui <- bootstrapPage(
  # App title ----
  h1("Chile Subsidies Nov 2021 Analysis"),
  #actionButton(inputId = "buttonCount_n", label = "Relative to #Entries" ),
  #actionButton(inputId = "buttonTotalMonto", label = "Relative to #Money" ),
 radioButtons("inRadioEntriesOrMoney", "Data points relative to:",
               inline=TRUE,
               choiceNames= c("Number of Entries", "Amount of Money"),
               choiceValues= c(1,2),
               ) ,
  checkboxGroupInput("chkboxGrpPtSubsets", "Filter by:",
                     choiceNames =  list("Municipalities", "Universities", "Subsecretary Offices", "Government Offices"),
                     choiceValues = list("MUNICIPAL", "UNIVERSIDAD", "SUBSEC", "GOBIERNO"),
                     inline=TRUE,
                     selected="as.character(0)"
                     ),
  # actionButton(inputId = "buttonFilterOnlyMuni", label = "Show only by MUNICIPALITY\'s" ),
  # actionButton(inputId = "buttonFilterOnlySubsec", label = "Show only Office Subsecretaries" ),
  # actionButton(inputId = "buttonFilterOnlyGov", label = "Show only Office Government" ),
  # actionButton(inputId = "buttonFilterOnlyUniversity", label = "Show only by Universities" ),
  # actionButton(inputId = "buttonFilterEverthing", label = "Show ALL" ),
  actionButton(inputId = "buttonInsertRegionNuble", label = "Insert Region BioBio Layer" ),
  
  leafletOutput(outputId = "leafletMap" , width = "100%", height = "800px"), # height= 100% doesn't pass through
 
  dataTableOutput(outputId = "dataTbl2"),
)

radiusChosen <- 0

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
 
  radiusColSrcName <- function(radiusChosen){
   return(  
     switch( radiusChosen, 
            ~CIRCLE_RADIUS_MONTO_TOTAL, 
            ~CIRCLE_RADIUS_COUNT_N 
            )
   )
  }
   
  changeRadiusVectorSrc <- function( radiusChosen , filterByStr )
  {
    filtered_df <- df_muni_leaf %>% filter( str_detect( str_to_upper(ORG_NOMBRE) , filterByStr ) )
    
    NUM_ROWS_FOUND_NAs <- nrow( filtered_df %>% filter( is.na(TOTAL_MONTO) ) )  
    NUM_ROWS_FOUND_NOT_NAs <- nrow( filtered_df %>% filter( !is.na(TOTAL_MONTO) )  )
    leafletProxy("leafletMap", data = filtered_df  ) %>%  clearMarkers() 
    
    if( NUM_ROWS_FOUND_NAs > 0  ) {
      
    leafletProxy("leafletMap", data = filtered_df  ) %>%
      #clearMarkers() %>% 
      addMarkers( ~LON, ~LAT,
                  icon = iconQuestionMarkPng,
                  popup = ~MSG_SUMMARY,
                  label = ~MSG_SUMMARY ,
                  layerId = ~ORG_NOMBRE ,
                  data = filtered_df %>% filter( is.na(TOTAL_MONTO)  ),
                  )
    }
    if( NUM_ROWS_FOUND_NOT_NAs > 0  ) {
    leafletProxy("leafletMap", data = filtered_df  ) %>%
      addCircleMarkers(~LON, ~LAT,
                       popup = ~MSG_SUMMARY,
                       label = ~MSG_SUMMARY ,
                       layerId = ~ORG_NOMBRE ,
                       radius = radiusColSrcName(radiusChosen),
                       #radius = switch( radiusChosen, ~CIRCLE_RADIUS_MONTO_TOTAL, ~CIRCLE_RADIUS_COUNT_N ),    # works !!
                       #radius = if( radiusChosen > 0) {  ~CIRCLE_RADIUS_MONTO_TOTAL} else { ~CIRCLE_RADIUS_COUNT_N  } , # also works!!
                       stroke = FALSE, fillOpacity = 0.5,
                       data = filtered_df %>% filter( !is.na(TOTAL_MONTO)  ),
                       )
    }
  }

  observeEvent(input$buttonInsertRegionNuble, {
    filtered_data <- chile_comunas_shp_joined %>%  filter( REGION == "Región Del Biobío") 
    leafletProxy("leafletMap", data = filtered_data ) %>%
      addPolygons( color = "#444444", weight = 1, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = 0.5,
                   fillColor = ~colorQuantile("YlOrRd", TOTAL_MONTO_PER_COMUNA)(TOTAL_MONTO_PER_COMUNA),
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE),
                   label = ~str_c(Comuna, " - CLP $ " , format( TOTAL_MONTO_PER_COMUNA, 
                                                                scientific = FALSE, 
                                                                big.mark = ","
                   )
                   ),
      )
  }, ignoreNULL = TRUE)
  
  observe({
    radiusChosen <- input$inRadioEntriesOrMoney
    print(paste0("radioButton chosen: ", radiusChosen ))
    changeRadiusVectorSrc(as.numeric(radiusChosen) , "")
    # Can also set the label and select items
    #updateRadioButtons(session, "inRadioEntriesOrMoney",
    #                   label = paste("radioButtons label", x),
    #                   choices = x,
    #                   selected = x
    #)
  })
  
  # observe({  #OLD logic when dealing with CheckBoxGroup 
  #   chkboxGroupSelectionsStr <- paste(input$chkboxGrpPtSubsets, collapse = "|")
  #   if( str_detect(chkboxGroupSelectionsStr, "ALL") ){
  #     print(paste0("You chose: ALL" , chkboxGroupSelectionsStr))
  #     updateCheckboxGroupInput(session, 
  #                              "chkboxGroupSelectionsStr",
  #                              selected = "ALL"
  #     )
  #     changeRadiusVectorSrc(radiusChosen, "")
  #   } else {
  #     print(paste0("You chose: ", chkboxGroupSelectionsStr))
  #     changeRadiusVectorSrc(radiusChosen , chkboxGroupSelectionsStr )
  #   }
  #   
  # })
  
  observe({  #Simplest form of CheckBoxGroupInput logic,  since none selected passes as "" empty string
    chkboxGroupSelectionsStr <- paste(input$chkboxGrpPtSubsets, collapse = "|")
    print(paste0("You chose: ", chkboxGroupSelectionsStr))
    changeRadiusVectorSrc(radiusChosen , chkboxGroupSelectionsStr )
  })

  # observeEvent(input$buttonFilterEverthing, {
  #   changeRadiusVectorSrc(2, "")
  # }, ignoreNULL = TRUE)

  observeEvent(input$leafletMap_marker_click, {
    click <- input$leafletMap_marker_click
    print(paste0("clicked ::: ", click ) )
    summary_selected <- df_muni_leaf %>% filter( ORG_NOMBRE == click[1]  )
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
      leaflet(data = df_muni_leaf ) %>%
      setView(lat = -36.82699, lng = -73.04977, zoom = 10) %>% 
      addTiles() %>%
      addMiniMap( position = "bottomleft", zoomAnimation = TRUE , zoomLevelOffset = -4) %>%
      addProviderTiles(providers$CartoDB.Positron)  %>%
      addMapPane("layerTop", zIndex=420) %>% addMapPane("layer2",zIndex=410) %>%
      addMarkers(
          ~LON, ~LAT,
          #icon = if( is.na( ~TOTAL_MONTO ) ) { leafletIcon } else { NULL } ,
          icon = iconQuestionMarkPng,
          popup = ~MSG_SUMMARY,
          label = ~MSG_SUMMARY ,
          layerId = ~ORG_NOMBRE ,
          data = df_muni_leaf %>% filter( is.na(TOTAL_MONTO)  ),
          options = pathOptions(pane = "layerTop"),
          ) %>%
      addCircleMarkers(
        ~LON, ~LAT,
        popup = ~MSG_SUMMARY,
        label = ~MSG_SUMMARY ,
        layerId = ~ORG_NOMBRE ,
        radius = radiusColSrcName(1),      #radius = ~CIRCLE_RADIUS_MONTO_TOTAL,
        stroke = FALSE, fillOpacity = 0.5,
        data = df_muni_leaf %>% filter( !is.na(TOTAL_MONTO) ),
        options = pathOptions(pane = "layerTop"),
      )

      #### tried to remove addMarkers() and addCircleMarkers in order to just run function -->   changeRadiusVectorSrc(1)  --- did NOT work
  })
  
}

shinyApp(
          ui = ui, server = server , 
          options=list(port=4050, launch.browser=FALSE ) 
         )

