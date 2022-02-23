library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(leaflet)
library(sf)

#DPLYR ver of read.csv() # 
csv_muni_subsidios <- read.csv("./TA_Subsidios_beneficios.csv", 
                         sep=";" , 
                         #nrows=10,
                         encoding="latin1" ,
)

csv_org_upd <- read.csv("organismos360_updated.csv")