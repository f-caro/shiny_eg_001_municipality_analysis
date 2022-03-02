library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(leaflet)
library(sf)

#DPLYR ver of read.csv() # 
df_muni_subsidios <- read.csv("./TA_Subsidios_beneficios.csv", 
                         sep=";" , 
                         #nrows=10,
                         encoding="latin1" ,
)

df_org_upd <- read.csv("organismos360_updated.csv")

df_org_upd_filtered <- 
  df_org_upd %>% 
  filter( .$addrChk == TRUE , .$lat_lon != "" ) %>% 
  select( Codigo_org , lat_lon )

df_muni_subsidios_upd <- df_muni_subsidios %>% 
  inner_join(df_org_upd_filtered, by = c( "organismo_codigo" = "Codigo_org")) %>%  
  group_by( organismo_codigo) %>% 
  summarise( ORG_NOMBRE = unique(organismo_nombre) , 
             TOTAL_MONTO =  sum( monto_global), 
             LAT_LON = unique(lat_lon) 
            ) %>% 
  mutate( LAT = as.numeric( str_split(LAT_LON, "," )[[1]][1] )  , 
          LON = as.numeric( str_split(LAT_LON, "," )[[2]][1] ) 
        )

df_muni_subsidios_upd <- df_muni_subsidios_upd %>% 
  rowwise() %>% 
  mutate( LAT = as.numeric( str_split(LAT_LON, "," )[[1]][1] )  ,
          LON = as.numeric( str_split(LAT_LON, "," )[[1]][2] )
        )


leafMap <- leaflet(data = df_muni_subsidios_upd ) %>%
  setView(lat = -36.82699, lng = -73.04977, zoom = 3) %>%
  addTiles() %>%
  addMarkers(~LON, ~LAT, popup = ~ORG_NOMBRE)
leafMap

