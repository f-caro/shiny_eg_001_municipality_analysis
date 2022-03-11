library(dplyr)
library(shiny)
#library(gapminder)
library(plotly)
library(DT)
library(gt)
library(kableExtra)
library(stringr)
library(reactable)


df_muni_full <- readRDS(file = "df_muni_full.Rds")

ui <- fluidPage(
  tags$h1("R Table Showcase"),
  
  selectInput(
    inputId = "selectOrganismo",
    label = "Select country:",
    choices = unique(df_muni_full$organismo_nombre),
    #selected = ""
  ),
  h2("ReactTable"),
  reactableOutput("reactTable"),
  
  h2("ye ol dataTbl1"),
  #dataTableOutput(outputId = "dataTbl1") ,
  
  # GT table
  tags$h2("GT Table"),
  #gt_output(outputId = "gtTable"),
  
  # # Kable Extra table
  # tags$h2("Kable Extra Table"),
  # tableOutput(outputId = "kableExtraTable"),
  # 
  # # DT table
  # tags$h2("DT Table"),
  # DTOutput(outputId = "dtTable"),
  # 
  # # Plotly table
  # tags$h2("Plotly Table"),
  # plotlyOutput(outputId = "plotlyTable")
)

server <- function(input, output, session) {
  data <- reactive(
    df_muni_full %>%
      filter(organismo_nombre == input$selectOrganismo) 
      #%>%
      #select(year, lifeExp)
  )
  
  output$reactTable <- renderReactable({
    reactable( data(), 
               #filterable = TRUE, #rather ugly, since it goes to each column
               #minRows = 10, 
               showPageSizeOptions = TRUE, pageSizeOptions = c(10, 25, 100, 1000), defaultPageSize = 10 , 
               highlight = TRUE,
               searchable = TRUE,   #much better, since it auto searches in all columns
               defaultExpanded = TRUE,
               details = colDef(
                 name = "Mas Info",
                 details = JS("function(rowInfo) {
                                console.log(rowInfo);
                                return '<div> <b>Numero de Acto: </b>' + rowInfo.original.numero_acto + '</div>' +
                                '<div><b>Año: </b>' + rowInfo.original.anyo + '   <b>Mes: </b>' + rowInfo.original.Mes + '</div>' +
                                '<div><b>Fecha de Acto: </b>' + rowInfo.original.fecha_acto + '</div>' +
                                '<div><b>Requisitos: </b>' + rowInfo.original.requisitos + '</div>' +
                                '<div><b>Periodo Inicio: </b>' + rowInfo.original.periodo_inicio + '  <b>Fin : </b>' + rowInfo.original.periodo_fin + '</div>' +
                                '<div><b>Criterio de Evaluacion: </b>' + rowInfo.original.criterio_evaluacion + '</div>' +
                                '<div><b>Plazos Asociados: </b>' + rowInfo.original.plazos_asociados + '</div>' +
                                '<div><b>Objetivo: </b>' + rowInfo.original.objetivo + '</div>' +
                                '<div><b>Tipo Acto: </b>' + rowInfo.original[\"Tipo.Acto\"] + '</div>' +
                                '<div><b>Denominacion de Acto: </b>' + rowInfo.original.denominacion_acto + '</div>' +
                                '<div><b>Numero Acto: </b>' + rowInfo.original.numero_acto + '</div>'  +
                                '<div><b>Fecha Acto: </b>' + rowInfo.original.fecha_acto + '</div>' +
                                '<div><b>Numero de Beneficiarios: </b>' + rowInfo.original.numero_beneficiarios + '</div>' +
                                '<div><b>Razones de Exclusion: </b>' + rowInfo.original.razones_exclusion + '</div>' +
                                '<div><b>Nombre de Beneficiario: </b>' + rowInfo.original.nombrebene + '</div>'
                              }"),
                 html = TRUE,
                 width = 60,
               ),
                 columns = list(
                   organismo_nombre = colDef(name = "Organismo"), #, filterable = FALSE),
                   organismo_codigo = colDef(name = "Codigo Interno de Organismo", show = FALSE),
                   fecha_publicacion_ta = colDef(name = "Fecha Publicado"),
                   anyo = colDef(name = "Año", show=FALSE),
                   Mes = colDef(name = "Mes", show=FALSE),
                   tipo_subsidiobeneficio = colDef(name = "Tipo Subsidio/Beneficio"),
                   unidad_organo = colDef(name = "Unidad Organismo"),
                   requisitos = colDef(name = "Requisitos", show = FALSE),
                   monto_global = colDef(name = "Monto por Unidad"),
                   Tipo.Unidad.monetaria = colDef(name = "Tipo Unidad"),
                   periodo_inicio = colDef(name = "Periodo Inicio", show=FALSE),
                   periodo_fin = colDef(name = "Periodo Fin", show=FALSE),
                   criterio_evaluacion = colDef(name = "Criterio Evaluacion", show = FALSE),
                   plazos_asociados = colDef(name = "Plazos Asociados", show=FALSE),
                   objetivo = colDef(name = "Objetivo", show = FALSE),
                   Tipo.Acto = colDef(name = "Tipo de Acto", show=FALSE),
                   denominacion_acto = colDef(name = "Denominacion Acto", show=FALSE),
                   numero_acto = colDef(name = "Numero Acto", show=FALSE),
                   fecha_acto = colDef(name = "Fecha Acto", show=FALSE),
                   enlace_texto = colDef(name = "Enlace Documento", 
                      cell = function(value) {
                         url <- str_extract( value , regex("(http.*)(?=target)") )
                         filename <- str_extract( as.character(url) , regex('(?!.*\\/).*') )
                         htmltools::tags$a(href = url, target = "_blank", filename  ) 
                   }),
                   numero_beneficiarios = colDef(name = "Numero de Beneficiarios", show=FALSE),
                   razones_exclusion = colDef(name = "Razones Exclusion", show=FALSE),
                   nombrebene = colDef(name = "Nombre de Beneficiario", show=FALSE),
                   enlace_masinfo = colDef(name = "Enlace Sitio", 
                    cell = function(value) {
                     url <- str_extract( value , regex("(http.*)(?=target)") )
                     htmltools::tags$a(href = url, target = "_blank", "mas Info")
                   }),
                   activado = colDef(name = "Activado/Inactivo", cell = JS("
                      function(cellInfo) {
                        // Render as an X mark or check mark
                        return cellInfo.value === '0' ? '\u274c No' : '\u2714\ufe0f Si'
                      }
                    "))
                 ),
               theme = reactableTheme(
                 rowSelectedStyle = list(backgroundColor = "#eee", 
                                         boxShadow = "inset 2px 0 0 0 #ffa62d"
                                         ) 
                 )
              )
  })
  
  # GT table
  output$gtTable <- render_gt({
    data() %>%
      gt() %>%
      tab_header(title = paste("Life expectancy in", input$selectOrganismo, "over time")) 
      #%>%
      # cols_label(year = "Year", lifeExp = "Life expectancy (years)") %>%
      # tab_source_note(source_note = "Source: df_muni_full dataset") %>%
      # tab_style(
      #   style = list(cell_fill(color = "#F4F4F4")),
      #   locations = cells_body(columns = year)
      # )
  })
  
  
  
  output$dataTbl1 <- renderDataTable({   
    df_muni_full %>% filter(organismo_nombre == input$selectOrganismo)
    }) 
  
  # # Kable Extra table
  # output$kableExtraTable <- reactive({
  #   data() %>%
  #     kbl(caption = paste("Life expectancy in", input$selectOrganismo, "over time")) %>%
  #     kable_material() %>%
  #     kable_styling(bootstrap_options = c("striped", "hover"))
  # })
  # 
  # # DT table
  # output$dtTable <- renderDT({
  #   datatable(
  #     data = data(),
  #     colnames = c("Year", "Life expectancy (years)"),
  #     caption = paste("Table shows life expectancy in", input$selectOrganismo, "over time"),
  #     filter = "top"
  #   )
  # })
  # 
  # # Plotly table
  # output$plotlyTable <- renderPlotly({
  #   plot_ly(
  #     type = "table",
  #     columnwidth = c(100, 100),
  #     columnorder = c(0, 1),
  #     header = list(
  #       values = c("Year", "Life expectancy (years)"),
  #       align = c("center", "center"),
  #       line = list(width = 1, color = "#000000"),
  #       fill = list(color = c("#0099f9", "#0099F9")),
  #       font = list(size = 14, color = "#FFFFFF"),
  #       height = 40
  #     ),
  #     cells = list(
  #       values = rbind(data()$year, data()$lifeExp),
  #       align = c("center", "center"),
  #       line = list(width = 1, color = "#000000"),
  #       font = list(size = 12, color = "#000000")
  #     )
  #   )
  # })
}

shinyApp(ui, server, 
         options=list(port=4050, launch.browser=FALSE ) 
         
         )