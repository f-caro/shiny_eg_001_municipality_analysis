#!/usr/bin/Rscript

library(dplyr, warn.conflicts = FALSE)
library(leaflet)
library(sf)
library(stringr)
library(stringi)


#Monetary col needs to be multiplied by certain factors relative to $Tipo.Unidad.monetaria
factorAmountType <- function( dataStr ){
  #unique( df_muni_subsidios$Tipo.Unidad.monetaria )
  #[1] Miles de pesos Pesos          Dólares        UF             Euros          UTM  
  dataStr <- str_trim(dataStr)                        #Remove empty spaces
  dataStr <- stri_trans_general(dataStr, "Latin-ASCII") 
  #if( is.na( dataStr ) )  {  return( 1 )  }          #chk if Empty
  if( str_detect( dataStr , 'Pesos')[1] )  { return( 1 ) }  # factor 1
  if( str_detect( dataStr , 'Miles de pesos')[1] ) { return( 1000 ) }  # factor 1000
  if( str_detect( dataStr , 'Dolares')[1] ) { return( 812.15 ) }             # USD vs CLP rate
  if( str_detect( dataStr , 'Euros')[1] ) { return( 941.38 ) }               # EUR vs CLP rate 
  if( str_detect( dataStr , 'UF')[1] ) { return( 30392.22 ) }                # Unidad de Fomento, Chilean fin measure relative to various factors
  if( str_detect( dataStr , 'UTM')[1] ) { return( 53476 ) }                  # Unidades Tributarias Mensuales, another Chilean fin measure dependent on various factors
  
  return( 1 )   #indicates Address Str found
}


loadSubsidiesXOrganismos <- function( govSubCsvLocation , orgCsvLocation ){
  #STEP 1.1:  load Government Subsidies supplied CSV file
  #DPLYR ver of read.csv() # 
  df_muni_subsidios <- read.csv( govSubCsvLocation, #"./TA_Subsidios_beneficios.csv", 
                                sep=";" , 
                                #nrows=10,
                                encoding="latin1" ,
                                )
  #STEP 1.2:  load Government Organizations table, updated with LAT_LON coords
  df_org_upd <- read.csv( orgCsvLocation ) #"organismos360_updated.csv")
  
  #STEP 2.1:  factor multiply Money col relative to TypeOfMonetaryUnit
  df_muni_subsidios <- df_muni_subsidios %>% 
    rowwise() %>% 
    mutate( NewAmount = 
              format( 
                factorAmountType( Tipo.Unidad.monetaria ) * monto_global , 
                scientific = FALSE, big.mark = ","
                ) 
            ) 
    ### visual inspection --> %>% select( monto_global , Tipo.Unidad.monetaria , NewAmount    ) 
  
  #STEP 2.2:  Filter Government Organization df to include only lat_lon non-empty entries
  df_org_upd_filtered <- 
    df_org_upd %>% 
    filter( .$addrChk == TRUE , .$lat_lon != "" ) %>% 
    select( Codigo_org , lat_lon )
  
  #STEP 2.3: Combine Gov Subsidies with Gov Org df's
  df_muni_subsidios_upd <- df_muni_subsidios %>% 
    inner_join(df_org_upd_filtered, by = c( "organismo_codigo" = "Codigo_org")) %>%  
    group_by( organismo_codigo) %>% 
    summarise( ORG_NOMBRE = unique(organismo_nombre) ,
               COUNT_N = n(), 
               TOTAL_MONTO =  sum( monto_global), 
               LAT_LON = unique(lat_lon)
              )
  
  return( df_muni_subsidios_upd )
}

leafletDfPrepwork <- function ( df_muni ){
  #STEP 2.4: Gov Subsidies df prep work for Leaflet Map columns
  df_muni <- df_muni %>% 
    rowwise() %>% 
    mutate( LAT = as.numeric( str_split(LAT_LON, "," )[[1]][1] ) ,
            LON = as.numeric( str_split(LAT_LON, "," )[[1]][2] ) ,
            MSG_SUMMARY = str_c( ORG_NOMBRE , "::: Entries: " , COUNT_N, " - Total: ",  
                                 format( TOTAL_MONTO, 
                                         scientific = FALSE, 
                                         big.mark = ","
                                         ) 
                                 )
          )
   return ( df_muni )
}


main_bknd_muni_subsidios_prepare_df <- function() {
  df_muni <- loadSubsidiesXOrganismos( "./TA_Subsidios_beneficios.csv" , "organismos360_updated.csv"  )
  
  df_muni <- leafletDfPrepwork( df_muni )
  
  #STEP 3.0: Leaflet preview output
  leafMap <- leaflet(data = df_muni ) %>%
    setView(lat = -36.82699, lng = -73.04977, zoom = 3) %>%
    addTiles() %>%
    addCircleMarkers(~LON, ~LAT, popup =  ~MSG_SUMMARY,  label=~MSG_SUMMARY )
  leafMap

}


# runs only when script is run by itself & in Interactive Mode
if (getOption('run.main', default=TRUE)) {
  main_bknd_muni_subsidios_prepare_df()
  ## in parent script, so that main() does not execute:  
  ## options(run.main=FALSE)
  ## source('bknd_muni_subsidios_prepare_df.r')
}
