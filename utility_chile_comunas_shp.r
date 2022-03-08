ptm <- proc.time()  #Starting PROCESS time counter
print(getwd())
setwd(getwd())
#setwd("/home/i7mintu/Documents/PROJECTS/R-ShinyProj/shiny_eg_001_municipality_analysis")

# Example Source: https://stackoverflow.com/questions/31873151/how-rotate-map-in-r?noredirect=1&lq=1
####################################################################################################

library(ggplot2)
library(tmap)
library(tmaptools)
library(dplyr)
library(leaflet)
library(sf)
library(shiny)
library(stringr)


# Rotate an sf geom around a center point. If no center is
# specified then it rotates around the center of the geom.
# This is technically an affine transformation: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations-1
st_ellide_rotate = function(x, degrees, center_coords=NULL){
  if(degrees < -360 | degrees > 360) stop('Degrees must be in the range -360 to 360')
  x = sf::st_combine(x)
  if(is.null(center_coords)){
    center_coords = sf::st_centroid(x)
  }
  radians = degrees * pi/180
  transform_matrix = matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2, 2)
  
  return((x-center_coords) * transform_matrix + center_coords)
}


df_muni <- readRDS(file = "df_muni_leaf.Rds")


if( !file.exists("chile_comunas_shp_joined.Rds") ) {
  chile_comunas_shp <- st_read("chile_comunas_maps/comunas.shp")  # size 64MB
  df_muni_comuna_summary <- readRDS(file="df_muni_comuna_summary.Rds")
  
  chile_comunas_shp_joined <- chile_comunas_shp %>% 
    left_join(df_muni_comuna_summary, 
              by = c( "Comuna" = "COMUNA" )
              )
  
  saveRDS(chile_comunas_shp_joined, file = "chile_comunas_shp_joined.Rds")

} else {
  chile_comunas_shp_joined <- readRDS(file = "chile_comunas_shp_joined.Rds")
}

#chile_comunas_shp_joined <- readRDS(file = "chile_comunas_shp_joined.Rds")

chile_comunas_shp_joined <- chile_comunas_shp_joined %>% sf::st_transform('+proj=longlat +datum=WGS84')
# proj=longlat converts shp file coords to be compatible with Leaflet input structure,


proc.time() - ptm
print(paste0("loading shp/RDS file \n"))  # not worth it, probably better to use DB

### Don't need to rotate, since working on LeafLetMap
#tmComunas <- tm_shape( chile_comunas_shp ) + tm_polygons() 
#comunas_rotated = chile_comunas_shp %>%  st_ellide_rotate(-90)


ui <- bootstrapPage(
  actionButton(inputId = "buttonInsertTmap", label = "InsertTmap Layer" ),
  actionButton(inputId = "buttonInsertRegionNuble", label = "InsertTmap Region BioBio Layer" ),
  leafletOutput(outputId = "leafletMap" , width = "100%", height = "800px"), # height= 100% doesn't pass through
)

  #tmap_mode("plot")
  tmap_options(check.and.fix = TRUE)
  #tm_map <- tm_shape(chile_comunas_shp) +  tm_polygons("Comuna") 
  
  #Howto filter out Bio-Bio region---  use levels(chile_comunas_shp$Region)  to identify uniques
  #tm_map <- tm_shape( chile_comunas_shp ) + tm_polygons()   # Full map takes ::: 64.153 s to create PDF file
  #tm_map <- tm_shape( chile_comunas_shp %>% filter(Region == "Región del Bío-Bío") ) +
  #          tm_polygons()   # Region BioBio takes ::: 2.705 seconds to load
  
  # tm_map <- tm_shape( chile_comunas_shp_joined ) + #%>% filter( REGION == "Región del Bío-Bío" ) ) +
  #           tm_polygons( "TOTAL_MONTO_PER_COMUNA" ,
  #                        alpha= 0.55,
  #                        n= 100, 
  #                        style="quantile",
  #                        border.col = "white", 
  #                        lwd = 0.5, lty = "dashed" , 
  #                        legend.show = FALSE, 
  #                        palette= c("green", "red")
  #                        )
  # Region BioBio takes ::: 2.705 seconds to load

server <- function(input, output) {
  #pdf("img_plot.pdf")
  
  

  
  # leafMap <- leaflet(data = df_muni ) %>%
  #   setView(lat = -36.82699, lng = -73.04977, zoom = 3) %>%
  #   addTiles() %>%
  #   addCircleMarkers(~LON, ~LAT, popup =  ~MSG_SUMMARY,  label=~MSG_SUMMARY )
  # leafMap
  # 
  
  #ggplot() + geom_sf(data = chile_comunas_shp) #, aes(fill=pcp_ECCC))      
  #   user  system elapsed 
  #63.976   0.428  64.739 
  
  #dev.off()
  #system('pdfopen img.pdf')
  
  # proc.time() - ptm
  # print(paste0("finished plotting to PDF file \n"))

  observeEvent(input$buttonInsertRegionNuble, {
    filtered_data <- chile_comunas_shp_joined %>%  filter( REGION == "Región Del Biobío") 
    leafletProxy("leafletMap", data = filtered_data ) %>%
      # setView(lat = -36.82699, lng = -73.04977, zoom = 10) %>% 
      # addTiles() %>%
      # addProviderTiles(providers$CartoDB.Positron)  %>%
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
  
  observeEvent(input$buttonInsertTmap, {
    print(paste0("this was clicked,  buttonInsertTmap"))
    
    # leafletProxy("leafletMap", data = df_muni ) %>%
    #   setView(lat = -36.82699, lng = -73.04977, zoom = 10) %>% 
    #   addTiles() %>%
    #   addMiniMap( position = "bottomleft", zoomAnimation = TRUE , zoomLevelOffset = -4) %>%
    #   addProviderTiles(providers$CartoDB.Positron)  %>%
    #   # addMarkers(
    #   #   ~LON, ~LAT,
    #   #   #icon = if( is.na( ~TOTAL_MONTO ) ) { leafletIcon } else { NULL } ,
    #   #   icon = iconQuestionMarkPng,
    #   #   popup = ~MSG_SUMMARY,
    #   #   label = ~MSG_SUMMARY ,
    #   #   layerId = ~ORG_NOMBRE ,
    #   #   data = df_muni %>% filter( is.na(TOTAL_MONTO)  ),
    #   # ) %>%
    #   addCircleMarkers(
    #     ~LON, ~LAT,
    #     popup = ~MSG_SUMMARY,
    #     label = ~MSG_SUMMARY ,
    #     layerId = ~ORG_NOMBRE ,
    #     radius = 20, #radiusColSrcName(1),      #radius = ~CIRCLE_RADIUS_MONTO_TOTAL,
    #     stroke = FALSE, fillOpacity = 0.5,
    #     data = df_muni %>% filter( !is.na(TOTAL_MONTO) ),
    #   ) 
  }, ignoreNULL = TRUE)
  
  # output$leafletMap <- renderLeaflet({
  #   tmap_leaflet(tm_map, in.shiny=TRUE, options = pathOptions(pane = "layer2")) %>%
  #     setView(lat = -36.82699, lng = -73.04977, zoom = 10) %>% 
  #     addTiles() %>%
  #     addMiniMap( position = "bottomleft", zoomAnimation = TRUE , zoomLevelOffset = -4) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addMapPane("layerTop", zIndex=420) %>% addMapPane("layer2",zIndex=410)
  # 
  # })

  output$leafletMap <- renderLeaflet({
    leaflet(data = df_muni ) %>%
      setView(lat = -36.82699, lng = -73.04977, zoom = 10) %>% 
      addTiles() %>%
      addMiniMap( position = "bottomleft", zoomAnimation = TRUE , zoomLevelOffset = -4) %>%
      addProviderTiles(providers$CartoDB.Positron)  %>%
      addMapPane("layerTop", zIndex=420) %>% addMapPane("layer2",zIndex=410) %>%
      # addMarkers(
      #   ~LON, ~LAT,
      #   #icon = if( is.na( ~TOTAL_MONTO ) ) { leafletIcon } else { NULL } ,
      #   icon = iconQuestionMarkPng,
      #   popup = ~MSG_SUMMARY,
      #   label = ~MSG_SUMMARY ,
      #   layerId = ~ORG_NOMBRE ,
      #   data = df_muni %>% filter( is.na(TOTAL_MONTO)  ),
      # ) %>%
      addCircleMarkers(
        ~LON, ~LAT,
        popup = ~MSG_SUMMARY,
        label = ~MSG_SUMMARY ,
        layerId = ~ORG_NOMBRE ,
        radius = 5, #radius = radiusColSrcName(1),      #radius = ~CIRCLE_RADIUS_MONTO_TOTAL,
        stroke = FALSE, fillOpacity = 0.5,
        data = df_muni %>% filter( !is.na(TOTAL_MONTO) ),
        options = pathOptions(pane = "layerTop"),
      )
    
    #### tried to remove addMarkers() and addCircleMarkers in order to just run function -->   changeRadiusVectorSrc(1)  --- did NOT work
  })
    
  # output$leafletMap <- renderLeaflet({
  #   leaflet( data = df_muni  ) %>%   ##########   WATCH out when COPY/pasting from leafletProxy() function ---  the ID is autoidentified
  #     setView(lat = -36.82699, lng = -73.04977, zoom = 10) %>% 
  #     addTiles() %>%
  #     addMiniMap( position = "bottomleft", zoomAnimation = TRUE , zoomLevelOffset = -4) %>%
  #     addProviderTiles(providers$CartoDB.Positron)  %>%
  #     # addMarkers(
  #     #   ~LON, ~LAT,
  #     #   #icon = if( is.na( ~TOTAL_MONTO ) ) { leafletIcon } else { NULL } ,
  #     #   icon = iconQuestionMarkPng,
  #     #   popup = ~MSG_SUMMARY,
  #     #   label = ~MSG_SUMMARY ,
  #     #   layerId = ~ORG_NOMBRE ,
  #     #   data = df_muni %>% filter( is.na(TOTAL_MONTO)  ),
  #     # ) %>%
  #     addCircleMarkers(
  #       ~LON, ~LAT,
  #       popup = ~MSG_SUMMARY,
  #       label = ~MSG_SUMMARY ,
  #       layerId = ~ORG_NOMBRE ,
  #       radius = 5, #radiusColSrcName(1),      #radius = ~CIRCLE_RADIUS_MONTO_TOTAL,
  #       stroke = FALSE, fillOpacity = 0.5,
  #       data = df_muni %>% filter( !is.na(TOTAL_MONTO) ),
  #       options = pathOptions(pane = "layerTop"),
  #     ) 
  # })
}
 

shinyApp(
  ui = ui, server = server , 
  options=list(port=4050, launch.browser=FALSE ) 
)