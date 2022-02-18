library(shiny)
#library(arrow, warn.conflicts = FALSE)  # Add CSV speedups to script
library(dplyr, warn.conflicts = FALSE)
library(tmap)
library(tmaptools)
library(sf)


#map of chile - comunas
chile_comunas_shp <- st_read("chile_comunas_maps/comunas.shp")
tmComunas <- tm_shape( chile_comunas_shp ) + tm_polygons() 


#DPLYR ver of read.csv() # 
csv_muni_old <- read.csv("./TA_Subsidios_beneficios.csv", 
                         sep=";" , 
                         #nrows=10,
                         encoding="latin1" ,
)

select_input_startup <- unique(csv_muni_old$organismo_nombre)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Municipality Analysis"),
  
  #error in ShapeFile processing in Shiny --- trying to plot it instead
  #tmapOutput("tmap_chile_comunas"),
  plotOutput(outputId = "tm_chile_comunas",
             width = "100%",
             height = "400px",
             click = NULL,
             dblclick = NULL,
             hover = NULL,
             brush = NULL,
             inline = FALSE
             ),
  
  #not working properly#  navbarMenu(title="Hello navbarMenu"),

  # Input: select for the number of organismo_nombre ----
  selectInput(inputId = "select_input_organismo_nombre",
              label = "List of Organizations",
              choices = select_input_startup ,
              #selected = select_input_startup[0]
              ),
  

  sliderInput(inputId = "bins",
              label="slider Input box",
              min=1,
              max=50,
              value=25
                    ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot"),

  
  dataTableOutput(outputId = "dataTbl1")
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })

  
  output$dataTbl1 <- renderDataTable({
    #DPLYR ver of read.csv() # 
    filtered_muni <- csv_muni_old %>% 
                    filter(  
                      organismo_nombre == input$select_input_organismo_nombre)

    
    
    #having issue with Arrow library and Latin characters...  
    #arrow lib, for optimal CSV loadin, should be done at start of script
    #data <- read_delim_arrow("./TA_Subsidios_beneficios.csv", delim=";" ) 
    #Don't know howto pass UTF8 parameters to read_delim_arrow()
    #csv_muni_ref <- data %>% head(10) %>% collect()
    #csv_muni <- data.frame(csv_muni_ref)
  })
  
  # error with shp file -leads to ::: Warning: Error in +.tmap: argument "e2" is missing, with no default
  #output$tmap_chile_comunas <- renderTmap({ 
  #  tm_shape( chile_comunas_shp ) 
  #  + tm_polygons() 
  #  })
  output$tm_chile_comunas <- renderPlot({ tmComunas })
}

shinyApp(ui = ui, server = server)

