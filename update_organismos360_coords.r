library(dplyr)
library(sf)
library(stringr)
library(stringi)
library(jsonlite)

#toponimos_shp <- st_read("chile_toponimos/Toponimos.shp")
#chile_comunas_shp <- st_read("chile_comunas_maps/comunas.shp")
organismos360 <- read.csv("./Organismos_360.csv", 
                         sep=";" , 
                         #nrows=10,
                         encoding="latin1" ,
)

#shp_coord <- organismos360 %>% left_join( toponimos_shp , by= c( "idOrg" = "cod_comuna"), keep = TRUE) 
#View(shp_coord %>% select( Organismo,  Padre_org,  Region.x,  Municipalidad, direccion , Nombre, Comuna) )
#proved there is nothing in common between official Topological name src and Gov transparency files.


#need to get coords from street address.  
#NB. get GEOCODE from street address.   
#REVERSE GEOCODE means get street address/surrounding point information from lat+lon values 
#https://nominatim.openstreetmap.org/search?q=Esmeralda+N+145+Corral.+Regi%C3%B3n+de+Los+R%C3%ADos.&format=json&polygon=1&addressdetails=1
#JSON response:
#[{"place_id":254250092,"licence":"Data © OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright","osm_type":"way","osm_id":786053680,"boundingbox":["-39.8892","-39.8870483","-73.4283132","-73.4270759"],"lat":"-39.8880754","lon":"-73.4273033","display_name":"Esmeralda, La Aguada, Corral, Provincia de Valdivia, Los Ríos Region, Chile","class":"highway","type":"residential","importance":0.7200000000000001,"address":{"road":"Esmeralda","village":"La Aguada","town":"Corral","county":"Provincia de Valdivia","state":"Los Ríos Region","country":"Chile","country_code":"cl"}}]

#resp_test <- jsonlite::fromJSON("https://nominatim.openstreetmap.org/search?q=Esmeralda+N+145+Corral.+Regi%C3%B3n+de+Los+R%C3%ADos.&format=json&polygon=1&addressdetails=1")
#class(resp_test) #[1] "data.frame"
#resp_test$lat #[1] "-39.8880754"
#resp_test$lon #[1] "-73.4273033"


#writing response lat+lon to columns
#dataf <- dataf %>% add_column(lat = resp_test$lat,  lon=resp_test$lon )
#dataf <- dataf %>% add_column(C = if_else(.$A == .$B, TRUE, FALSE))

#need functions:
chk_if_street_address <- function( dataStr ){
  
  dataStr <- str_trim(dataStr)

  if( is.na( dataStr ) )   {  return( FALSE )  }          #chk if Empty
  if( nchar(dataStr) <= 3 ) {  return(FALSE) }             #chk if low Char count  <--- nchar because len() is not same as C/Python/PHP etc
  if( length( str_split(dataStr, " ")[[1]] )  < 2 )  { return(FALSE) }    #chk if low Word count
  #Steps to chk for Phone Number
  dataStr <- str_replace_all( dataStr, "[ ()]", "")         #replace all " ", "(" ,")" chars
  #print(c(dataStr , str_detect( dataStr , '\\d{6,}')) )
  if( str_detect( dataStr , '\\d{6,}') ) { return(FALSE) } #chk for 6 to 11 digit numbers
  
  return( TRUE )   #indicates Address Str found
}

get_coords_from_nominantum <- function( addrStr  ){
  
  addrStr <- str_replace_all(addrStr, "#", "")  
  addrStr <- str_replace_all(addrStr, " ", "+")  
  httpStr <- str_c("https://nominatim.openstreetmap.org/search?q=", addrStr, "&format=json&polygon=1&addressdetails=1" )
  
  Sys.sleep(ceiling( runif(1, min=0, max=2 ) ) )  # 1 second delay before it fetches the Json string
  
  print( str_c( 'busy with : ', httpStr) )    # NB ::: print() expects a vector,  so combine as if a column
  try(
    resp_json <- jsonlite::fromJSON( httpStr )  # Response Payload will return DataFrame with  .$lat  and .$lon  columns 
  )  # suppose to skip, when nominatum returns a website instead of JSON.
  
    #TEST coord str## resp_json <- data.frame( lat =  runif(1, min=0, max=100) , lon= runif(1, min=0, max=100) )   #Testing Coords output
  print( str_c( "found lat - ", resp_json$lat[1] , ",  lon - " , resp_json$lon[1] ) )
  coordStr <- ifelse(is.null(resp_json$lat) | is.null(resp_json$lon) 
                     , ""
                     , str_c(resp_json$lat[1] , "," , resp_json$lon[1] ) 
                     )
  return(coordStr)
}
  
# TODO:  test functions:
# chk_if_street_address( "EDIFICIO CONVENCIONES SN  IQUIQUE" )  # should return 4
# N.B -- take note that functions act VECTORS or elements.  
#chk_if_street_address() was tested as receiving a str element
# BUT DPLYR %>% mutate(addrChk = chk_if_street_address( direccion )  )
# passes a VECTOR,  hence the function fails to work
#  DPLYR  needs   %>%  rowwise()  call, in order for chk_if_street_address()  to process, line by line
# the following works:
# organismos360 %>% rowwise() %>% mutate(addrChk = chk_if_street_address( direccion )  ) %>% select(direccion, addrChk)


run_address_chk <- function( df ) {
  
  df_addr_chk <- df %>% 
    rowwise() %>% 
    mutate(addrChk = chk_if_street_address( stri_trans_general(direccion, "Latin-ASCII")  )  )
  
  return(df_addr_chk)
}


run_coords_fetch <- function( df ) {
  
  org_updated <- run_address_chk( df )  # first creates addrChk column
  
  org_updated <- org_updated %>% 
    rowwise() %>% 
    #print(org_updated$addChk) %>%
    #filter( addrChk == TRUE ) %>%
    #mutate( lat_lon =  get_coords_from_nominantum(direccion) )
    mutate( lat_lon =  ifelse( addrChk == TRUE,  
                               get_coords_from_nominantum( 
                                 stri_trans_general( str_c(direccion, "+", Municipalidad ), 
                                                     "Latin-ASCII")  
                                                    ) 
                               , "" ) 
            )
  return(org_updated)
  
}

write.csv( run_coords_fetch(organismos360) , "organismos360_updated.csv"  )

