## app.R ##
library(readxl)
library(tidyverse)
library(dplyr)
library(shiny)
library(DT)
library(leaflet)
library(sf)
library(bslib)
library(rmapshaper)
library(plotly)

# Cargar datos
IRS_2017 <- read_excel("data/IRS_30_database.xlsx", sheet = "IRS_30")

# Seleccionar y renombrar columnas
IRS_2017 <- IRS_2017 %>% select(2, 3, 5, 9, 10, 11, 12, 13, 14, 24, 25, 35)
colnames(IRS_2017) <- c("ID", "Partido", "Poblacion", "Densidad", "Gas", "Cobertura Salud", 
                        "NSE", "Anegamiento", "Agua", "Amenaza", "Vulnerabilidad", "Riesgo")

# Formatear población
DATAIRS <- IRS_2017
DATAIRS$Poblacion <- format(round(DATAIRS$Poblacion, 0), nsmall = 0)

# Cargar shapefiles
map <- read_sf("data/Shape/IRS_30_originales.shp") %>% st_transform(4326)
mapartidos <- read_sf("data/Shape/partidos_concesion2019.shp") %>% st_transform(4326)

# Unir datos con shapefiles
mapaunido <- map %>% left_join(DATAIRS, by = c("ID_rd" = "ID"))

# Preparar datos de partidos
mapartidos$NOMBREBAJO <- tolower(mapartidos$NOMBRE)
DATAIRS$Partido <- tolower(DATAIRS$Partido)

prueba <- DATAIRS %>% group_by(Partido) %>% summarise(Casos = n(), Riesgos = sum(Riesgo))
prueba <- mutate(prueba, valor = Riesgos / Casos)
prueba$valor <- format(round(prueba$valor, 0), nsmall = 0)

mapartidos <- mapartidos %>% left_join(prueba, by = c("NOMBREBAJO" = "Partido"))
mapartidos$valor <- as.numeric(mapartidos$valor)

# Simplificar shapefiles
mapaunido <- ms_simplify(mapaunido, keep = 0.01, keep_shapes = TRUE)
mapartidos <- ms_simplify(mapartidos, keep = 0.01, keep_shapes = TRUE)

# Paletas de colores
palnumeric <- colorFactor("magma", mapaunido$Riesgo, na.color = "#808080", reverse = TRUE)
palnumeric2 <- colorFactor("magma", mapaunido$Riesgo, na.color = "#808080", reverse = TRUE)

# Popups y etiquetas
popup <- paste0("<b>ID: </b>", mapaunido$ID_rd, "<br><b>Partido: </b>", mapaunido$Partido,
                "<br><b>Población del radio: </b>", mapaunido$Poblacion,
                "<br><b>Nivel de riesgo: </b>", mapaunido$Riesgo)

label1 <- paste0("Nivel de Riesgo: ", mapaunido$Riesgo)

popup2 <- paste0("<b>ID: </b>", mapartidos$FeatId1, "<br><b>Partido: </b>", mapartidos$NOMBREBAJO,
                 "<br><b>Corona a la que pertenece: </b>", mapartidos$CORONA,
                 "<br><b>Riesgo medio del partido: </b>", mapartidos$valor)

label2 <- paste0("Riesgo promedio: ", mapartidos$valor)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel(p("", style = "")),
  navbarPage(
    title = "IRS",
    theme = bs_theme(bootswatch = "flatly"),
    tabPanel(
      title = "Inicio",
      icon = icon("comment-dots"),
      imageOutput("home_img"),
      br(), hr(),
      h4(style = "text-align: center", strong("Índice de Riesgo Sanitario")),
      p(style = "text-align: center; font-size = 25px", "Dirección de Medio Ambiente y Calidad")
    ),
    tabPanel(
      title = "Tablas",
      icon = icon("table"),
      selectInput("selection1", label = "Seleccionar una tabla",
                  choices = list("2016", "2017", "2022"), selected = "2017"),
      DT::dataTableOutput("TABLA")
    ),
    tabPanel(
      title = "Mapas",
      icon = icon("map"),
      fluidRow(
        column(2, selectInput("selection2", label = "Seleccionar un año (Radios)",
                              choices = list("2016", "2017", "2022"), selected = "2017")),
        column(3, selectInput("selection3", label = "Seleccionar un año (Partidos)",
                              choices = list("2016", "2017", "2022"), selected = "2017"))
      ),
      splitLayout(cellWidths = c("50%", "50%"), style = "border: 1px solid silver;",
                  leafletOutput(outputId = "map1"), leafletOutput(outputId = "map2"))
    ),
    tabPanel(
      title = "Gráficos",
      icon = icon("chart-pie"),
      selectInput("selection4", label = "Seleccionar un gráfico",
                  choices = list("2016", "2017", "2022"), selected = "2017"),
      plotlyOutput("graph")
    )
  )
)

# Funciones auxiliares
configurar_tabla <- function(data) {
  datatable(
    data,
    style = "default",
    extensions = c('Buttons', 'FixedColumns', 'Select'),
    rownames = FALSE,
    options = list(
      select = TRUE,
      dom = "Bfrtip",
      buttons = list(
        list(extend = "copy", text = "Copiar selección", exportOptions = list(modifier = list(selected = TRUE))),
        list(extend = "copy", text = "Copiar todo", exportOptions = list(modifier = list(selected = FALSE))),
        list(extend = "csv", text = "CSV", filename = "IRS_2017"),
        list(extend = "excel", text = "Excel", filename = "IRS_2017"),
        list(extend = "pdf", text = "PDF"),
        list(extend = "print", text = "Imprimir"),
        list(extend = "colvis", text = "Ocultar/Mostrar columnas"),
        list(extend = "collection", text = "Mostrar más", action = DT::JS("function ( e, dt, node, config ) {
          dt.page.len(50);
          dt.ajax.reload();
        }")),
        list(extend = "collection", text = "Mostrar menos", action = DT::JS("function ( e, dt, node, config ) {
          dt.page.len(15);
          dt.ajax.reload();
        }"))
      ),
      language = list(
        search = "Buscar",
        info = 'Mostrando páginas _START_ a _END_. Total: _TOTAL_ páginas',
        paginate = list('next' = "Siguiente", 'previous' = "Anterior")
      ),
      pageLength = 15,
      lengthMenu = c(5, 10, 15, 20),
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 0, rightColumns = 1),
      searching = TRUE,
      deferRender = TRUE,
      scrollY = 600,
      order = list(1, 'asc'),
      autoWidth = FALSE,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  ) %>%
    formatStyle(
      'Riesgo',
      backgroundColor = styleEqual(c(1, 2, 3, 4, 5), c("#b1ed96", "#cbfab7", "#fff4d4", "#ffeae8", "#eacecc"))
    )
}

configurar_mapa <- function(data, pal, popup, label, titulo_leyenda, columna_color) {
  leaflet(data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      color = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      opacity = 1.0,
      fillOpacity = 0.5,
      fillColor = ~pal(data[[columna_color]]),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      ),
      label = label,
      labelOptions = labelOptions(direction = "auto"),
      popup = popup
    ) %>%
    setView(-58.381944, -34.599722, zoom = 9) %>%
    addLegend(
      position = "topright",
      pal = pal,
      values = ~data[[columna_color]],
      title = titulo_leyenda
    )
}

configurar_grafico <- function(data) {
  plot_ly(
    data = data,
    x = ~CORONA,
    y = ~conteo,
    color = ~valor,
    colors = c("lightgreen", "#F9E79F", "#FA8072"),
    type = "bar",
    hoverinfo = "text",
    text = ~paste("Corona: ", CORONA, "<br>Riesgo: ", valor, "<br>Conteo: ", conteo),
    textposition = "none"
  ) %>%
    layout(
      title = "Distribución de riesgo según corona metropolitana",
      xaxis = list(
        title = "", 
        tickvals = c("primera", "Segunda", "Tercera"),
        ticktext = c("1ra Corona", "2da Corona", "3ra Corona")
      ),
      yaxis = list(title = "Conteo de radios"),
      showlegend = TRUE,
      legend = list(title = list(text = "<b>Riesgo</b>"), orientation = "h", x = 0, y = -0.2)
    )
}

# Servidor
server <- function(input, output, session) {
  # Imagen de portada
  output$home_img <- renderImage({
    list(src = "www/Portada.jpg",
         style = "display: block; margin-left: auto; margin-right: auto;",
         width = 700,
         height = 350)
  }, deleteFile = F)
  
  # Datos reactivos para la tabla
  my_data <- reactive({
    if(input$selection1 == "2016")
      shiny::validate(need(input$selection1 != "2016", "No existe tabla para mostrar"))
    if(input$selection1 == "2017")
      return(DATAIRS)
    if(input$selection1 == "2022")
      shiny::validate(need(input$selection1 != "2022", "No existe tabla para mostrar"))
  })
  
  # Tabla
  output$TABLA <- DT::renderDataTable({
    configurar_tabla(my_data())
  })
  
  # Datos reactivos para los mapas
  MAPA <- reactive({
    if(input$selection2 == "2016")
      shiny::validate(need(input$selection2 != "2016", "No existe mapa para mostrar"))
    if(input$selection2 == "2017")
      return(mapaunido)
    if(input$selection2 == "2022")
      shiny::validate(need(input$selection2 != "2022", "No existe mapa para mostrar"))
  })
  
  MAPARTIDO <- reactive({
    if(input$selection3 == "2016")
      shiny::validate(need(input$selection3 != "2016", "No existe mapa para mostrar"))
    if(input$selection3 == "2017")
      return(mapartidos)
    if(input$selection3 == "2022")
      shiny::validate(need(input$selection3 != "2022", "No existe mapa para mostrar"))
  })
  
  # Mapa 1: Radios censales
  output$map1 <- renderLeaflet({
    progress <- Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Cargando mapa: ', detail = 'Puede demorar unos segundos...')
    for (i in 1:15) progress$set(value = i); Sys.sleep(0.1)
    
    configurar_mapa(
      data = MAPA(),
      pal = palnumeric,
      popup = popup,
      label = label1,
      titulo_leyenda = "Riesgo (Radios)",
      columna_color = "Riesgo"
    )
  })
  
  # Mapa 2: Partidos
  output$map2 <- renderLeaflet({
    progress <- Progress$new(session, min = 1, max = 5)
    on.exit(progress$close())
    progress$set(message = 'Cargando mapa: ', detail = 'Puede demorar unos segundos...')
    for (i in 1:5) progress$set(value = i); Sys.sleep(0.1)
    
    configurar_mapa(
      data = MAPARTIDO(),
      pal = palnumeric2,
      popup = popup2,
      label = label2,
      titulo_leyenda = "Riesgo (Partidos)",
      columna_color = "valor"
    )
  })
  
  # Gráfico
  output$graph <- renderPlotly({
    mapartidos2 <- st_drop_geometry(mapartidos)
    mapartidos2 <- mapartidos2 %>% filter(!is.na(CORONA) & !is.na(valor))
    mapartidos2 <- arrange(mapartidos2, valor)
    mapartidos2$valor <- as.factor(mapartidos2$valor)
    
    datos_agrupados <- mapartidos2 %>%
      group_by(CORONA, valor) %>%
      summarise(conteo = n(), .groups = 'drop')
    
    configurar_grafico(datos_agrupados)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)



