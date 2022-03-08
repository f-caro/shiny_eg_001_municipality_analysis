#!/usr/bin/Rscript
ptm <- proc.time()
print(getwd())
setwd(getwd())

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


processSubsidiesXOrganismos <- function( govSubCsvLocation , orgCsvLocation , dismissNAs = FALSE){
  #STEP 1.1:  load Government Subsidies supplied CSV file
  df_muni_subsidios <- loadFullSubsidies( govSubCsvLocation )
    
  #STEP 1.2:  load Government Organizations table, updated with LAT_LON coords
  df_org_upd <- loadOrgDataUpdatedWithCoords( orgCsvLocation )
  
  #STEP 2.1:  factor multiply Money col relative to TypeOfMonetaryUnit
  df_muni_subsidios <- fixTipoUnidadMonetariaCol( df_muni_subsidios )
  
  #STEP 2.2:  Filter Government Organization df to include only lat_lon non-empty entries
  df_org_upd_filtered <- filterOrgDataByValidCoords(df_org_upd)

  #STEP 2.3: Combine Gov Subsidies with Gov Org df's
  df_muni_subsidios_upd <- innerJoinSubsidiesWithOrgData( df_muni_subsidios, df_org_upd_filtered , removeNAs = dismissNAs  )

  return( df_muni_subsidios_upd )
}

loadFullSubsidies <- function( govSubCsvLocation){ 
  ##STEP 1.1:  load Government Subsidies supplied CSV file
  df <- read.csv( govSubCsvLocation, #"./TA_Subsidios_beneficios.csv", 
                  sep=";" , 
                  #nrows=10,
                  encoding="latin1" ,
                )
  return(df)
}

loadOrgDataUpdatedWithCoords <- function ( orgCsvLocation ){ 
  ##STEP 1.2:  load Government Organizations table, updated with LAT_LON coords
  df_org_upd <- read.csv( orgCsvLocation ) #"organismos360_updated.csv")
  return( df_org_upd )
}

fixTipoUnidadMonetariaCol <- function ( df_full ){
  #STEP 2.1:  factor multiply Money col relative to TypeOfMonetaryUnit
  df_muni_subsidios <- df_full %>% 
    rowwise() %>% 
    mutate( NewAmount = 
              format( 
                factorAmountType( Tipo.Unidad.monetaria ) * monto_global , 
                scientific = FALSE, big.mark = ","
              ) 
    ) 
  ### visual inspection --> %>% select( monto_global , Tipo.Unidad.monetaria , NewAmount    ) 
  return(df_muni_subsidios)
}
  
filterOrgDataByValidCoords <- function( df_org )
{
  #STEP 2.2:  Filter Government Organization df to include only lat_lon non-empty entries
  df_org_upd_filtered <- 
    df_org %>% 
    filter( .$addrChk == TRUE , .$lat_lon != "" ) %>% 
    select( Codigo_org , lat_lon )
  return(df_org_upd_filtered)
}

innerJoinSubsidiesWithOrgData <- function ( df_subsidies, df_org , removeNAs = false , ...  )
{
  #STEP 2.3: Combine Gov Subsidies with Gov Org df's
  df_muni_subsidios_upd <- df_subsidies %>% 
    inner_join(df_org, by = c( "organismo_codigo" = "Codigo_org")) %>%  
    group_by( organismo_codigo) %>% 
    summarise( ORG_NOMBRE = unique(organismo_nombre) ,
               COUNT_N = n(), 
               TOTAL_MONTO =  sum( monto_global , na.rm= removeNAs ),
               LAT_LON = unique(lat_lon),
               ...
    )
  return(df_muni_subsidios_upd)
}

leafletDfPrepwork <- function ( df_muni ){
  #STEP 2.4: Gov Subsidies df prep work for Leaflet Map column
  MAX_TOTAL_MONTO <- max( df_muni$TOTAL_MONTO, na.rm=TRUE )
  MAX_COUNT_N <- max(df_muni$COUNT_N, na.rm=TRUE )
  df_muni <- df_muni %>% 
    rowwise() %>% 
    mutate( LAT = as.numeric( str_split(LAT_LON, "," )[[1]][1] ) ,
            LON = as.numeric( str_split(LAT_LON, "," )[[1]][2] ) ,
            CIRCLE_RADIUS_MONTO_TOTAL = ceiling( 100 * TOTAL_MONTO / MAX_TOTAL_MONTO ) , 
            CIRCLE_RADIUS_COUNT_N = ceiling( 100 * COUNT_N / MAX_COUNT_N) , 
            MSG_SUMMARY = str_c( ORG_NOMBRE , "::: Entries: " , COUNT_N, " - Total: ",  
                                 format( TOTAL_MONTO, 
                                         scientific = FALSE, 
                                         big.mark = ","
                                         ) 
                                 )
          )
   return ( df_muni )
}


prepMuniComunaSummary <- function(govSubCsvLocation , orgCsvLocation )
{
  
  df_muni_fixed <- fixTipoUnidadMonetariaCol( loadFullSubsidies( govSubCsvLocation) ) #"TA_Subsidios_beneficios.csv" ) )
  df_muni_org <- loadOrgDataUpdatedWithCoords( orgCsvLocation ) #"organismos360_updated.csv")
  
  df_muni_sum_per_org  <- df_muni_fixed %>% 
    group_by( organismo_nombre ) %>% 
    summarise(ORG_COD = unique(organismo_codigo) , 
              ORG_NOMBRE = unique(organismo_nombre) , 
              COUNT_ENTRIES_PER_ORG = n() , 
              TOTAL_MONTO_PER_ORG = sum(monto_global, na.rm=TRUE) 
    )
  
  df_muni_sum_per_org_joined_muni_orgs <- df_muni_sum_per_org %>% 
    left_join(df_muni_org, by = c( "ORG_COD" = "Codigo_org" )) 

  df_muni_comunas_summary <- df_muni_sum_per_org_joined_muni_orgs %>%  
    group_by( Municipalidad ) %>% 
    summarise( COMUNA = unique( str_to_title(Municipalidad) ),
               REGION = unique( str_to_title(Region) ), 
               COUNT_ORGS_PER_COMUNA = n(), 
               COUNT_NUM_ENTRIES_PER_COMUNA = sum(COUNT_ENTRIES_PER_ORG),
               TOTAL_MONTO_PER_COMUNA =  sum( TOTAL_MONTO_PER_ORG , na.rm= TRUE ),
    )
  
  return( df_muni_comunas_summary )
}



main_leaflet_visuals <- function( df ) {
  
  #STEP 3.0: Leaflet preview output
  leafMap <- leaflet(data = df ) %>%
    setView(lat = -36.82699, lng = -73.04977, zoom = 3) %>%
    addTiles() %>%
    addCircleMarkers(~LON, ~LAT, popup =  ~MSG_SUMMARY,  label=~MSG_SUMMARY )
  leafMap

}

main_recalc_all_df_from_csv <- function ()
{
  ptm <- proc.time()
  print(paste0("started function::: main_recalc_df_muni_df_summary_df_muni_leaf_RDSfiles"))
  df_muni_full <- loadFullSubsidies( "TA_Subsidios_beneficios.csv" )
  saveRDS(df_muni_full, file = "df_muni_full.Rds")
    
  df_muni_summary <- processSubsidiesXOrganismos( "TA_Subsidios_beneficios.csv" , 
                                               "organismos360_updated.csv" , 
                                               dismissNAs=FALSE  )
  saveRDS(df_muni_summary, file = "df_muni_summary.Rds")  
  
  df_muni_summary_dismissNAs <- processSubsidiesXOrganismos( "TA_Subsidios_beneficios.csv" , 
                                                             "organismos360_updated.csv" , 
                                                             dismissNAs=TRUE  )
  saveRDS(df_muni_summary_dismissNAs, file = "df_muni_summary_dismissNAs.Rds")
    
  df_muni_leaf <- leafletDfPrepwork( df_muni_summary )
  saveRDS(df_muni_leaf, file = "df_muni_leaf.Rds")
  
  
  df_muni_comuna_summary <- prepMuniComunaSummary("TA_Subsidios_beneficios.csv" , 
                                                        "organismos360_updated.csv" )
  saveRDS(df_muni_comuna_summary, file = "df_muni_comuna_summary.Rds")
  
  #measure Time taken to process this script
  print(paste0("ended function::: main_recalc_df_muni_df_summary_df_muni_leaf_RDSfiles"))
  proc.time() - ptm
}



# runs only when script is run by itself & in Interactive Mode
#if (getOption('run.main', default=TRUE)) {
  ### lets not run for now::::   main_bknd_muni_subsidios_prepare_df( df_muni_leaf  )
  ## in parent script, so that main() does not execute:  
  ## options(run.main=FALSE)
  ## source('bknd_muni_subsidios_prepare_df.r')
#}

#measure Time taken to process this script
proc.time() - ptm
