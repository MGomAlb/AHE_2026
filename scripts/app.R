# INCLUDE LIBRARIES
library(shiny)
library(leaflet)
library(tidyverse)
library(fs)
library(scales) # used to rescale the size of map points
library(RColorBrewer) # used for color palettes
library(leaflegend) # used for leaflet captions
library(shinyjs) # used for the clear button and to get info from HTML elements
library(mapview)   # used to add mapview functions for map downloads
library(DT) # Tables
library(htmltools) # HTML elements

# Function to apply multiple different filters to a data frame
conditional <- function(condition, success) {
  if (condition) success else TRUE
}

# READING DATA 
# Ceates a unique index with all CSV filenames containing the data for the maps. Pattern for valid filenames: "rasgo" + "NN" + "_" + ChapterName + ".cvs"
mi_indice <- data_frame(Fichero = list.files(path = "../data", pattern = ".csv$"))
mi_indice <- mi_indice %>% mutate(Fenómeno = str_remove(Fichero, "[rR]asgo\\d*\\_")) %>% mutate(Fenómeno = str_remove(Fenómeno, ".csv$"))
choices_capitulos <<- unique(mi_indice$Fenómeno)

# DEFINING Shiny's WEB User Interface

ui <- fluidPage(
  # JavaScript code to get the size of the "map" div as it appears to the user
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = document.getElementById("mapa").clientWidth;
                        dimension[1] = document.getElementById("mapa").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = document.getElementById("mapa").clientWidth;
                        dimension[1] = document.getElementById("mapa").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),  
  # Slider stile
  tags$head(tags$style(type='text/css', ".slider-animate-container  { margin-top: 10px !important; font-size: 20pt !important; text-align: center;}")),
  
  fluidRow(
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
      
      position = "right",  # Menu to the right side of the APP
      
      
      ## Sidebar panel for inputs
      sidebarPanel(
        
        # enables functions to reset HTML form
        useShinyjs(), 
        id = "side-panel",
        width = 3,
        
        # Title and instructions for users
        h4(strong("Uso del mapa")),
        
        helpText(p(id="Ayuda_1", "Seleccione dos rasgos para cartografiarlos
                 de forma simultánea. Puede escribir en el desplegable 
                 para encontrar más fácilmente los rasgos. Puede también seleccionar qué variantes mostrar para cada rasgo."),
                 p(id="Ayuda_2", "Especifique los rangos cronológicos y tipológicos para filtrar 
                 los datos.")),
        
        # selection button: list of first phenomena to display
        selectizeInput(inputId = "fenómeno1",
                       label = "",
                       choices = c('Elegir rasgo'='', choices_capitulos),
                       selected = NULL
        ),
        # selection button: list of second phenomena to display
        selectizeInput(inputId = "fenómeno2",
                       label = "",
                       choices = c('Elegir rasgo'='', choices_capitulos),
                       selected = NULL
        ),
        # checkbox to select variants for all phenomena selected
        hidden(checkboxGroupInput(inputId = "Variantes",
                                  label = "Variantes"
        )),
        
        # radio button to choose the type of count the user wants: examples or documents
        radioButtons(inputId = "recuento",
                     label = "Tipo de recuento",
                     choices = c("Número de ejemplos" = "ejemplos", 
                                 "Número de documentos" = "documentos")
        ),
        
        # blank separation line
        br(),
        
        # slider for selecting chronological range
        sliderInput(inputId = "año",
                    label = "Rango cronológico por años",
                    min = 1200,
                    max = 1600,
                    value = c(1200, 1300), # default slider position
                    step = 25, # minimal selectable range
                    animate = animationOptions(interval = 1000),
                    ticks = TRUE,
                    sep = ".",
                    dragRange = TRUE),
        
        # slider for Latitude (hidden)
        hidden(sliderInput(inputId = "latitud",
                           label = "Latitud [Meridional ◄ ► Septentrional]",
                           min = 35,
                           max = 50,
                           value = c(35, 50), # default slider position
                           step = 0.1, # minimal selectable range
                           ticks = FALSE,
                           sep = ".",
                           dragRange = TRUE)
        ),
        # slider for Longitude (hidden)
        hidden(sliderInput(inputId = "longitud",
                           label = "Longitud [Occidental ◄ ► Oriental]",
                           min = -10,
                           max = 17,
                           value = c(-10, 17), # default slider position
                           step = 0.1, # minimal selectable range
                           ticks = FALSE,
                           sep = ".",
                           dragRange = TRUE)
        ),
        
        # Option to filter out documents by type
        checkboxInput(inputId = "cancilleresco", 
                      label = "Excluir documentos cancillerescos", FALSE),
        
        # Option to filter documents by their status regarding location
        checkboxInput(inputId = "conjetural", 
                      label = "Excluir poblaciones sin una localización segura (conjeturales)", FALSE),
        
        # Option to show or hide map legends
        checkboxInput(inputId = "leyenda", 
                      label = "Mostrar leyendas", TRUE),
        
        
        # blank separation line
        br(),
        
        # Reset options button
        actionButton(inputId = "limpiar",
                     label = "Limpiar configuración"),
        
        # Button to reset connection to the Shiny server       
        actionButton(inputId = "stopSession",
                     label = "Desconectar aplicación")
        
      ), # end sidebarPanel() definition
      
      
      ## Main panel for displaying outputs
      mainPanel(
        
        # enables Javascript functions
        useShinyjs(), 
        width = 9,
        
        # MAIN MAP DISPLAY
        leafletOutput("mapa", 
                      #width = "650px", 
                      height = "1000px"),
        
        # blank separation line
        br(),
        
        # Download map button
        downloadButton( outputId = "descarga", label = "Descargar mapa", icon("download") ),
        
      ), # end mainPanel()
    ) # end sidebarLayout()
  ), # end fluidRow()
  
  fluidRow(
    column(12,
           
           # horizontal separation line
           hr(),
           hidden(h1(id="title_tabla1", "Tabla de ejemplos")),
           # horizontal separation line
           br(),
           # First table title
           hidden(tabPanel(id="panel_tabla1", "Tabla de ejemplos", DT::dataTableOutput("mytable1"))),
           # blank separation line
           br(),
           # Download button for first table
           hidden(downloadButton( outputId = "descarga_tabla1", label = "Descargar tabla de ejemplos", icon("download") )),
           # blank separation lines
           br(),
           br(),
           # horizontal separation line
           hr(),
           # Second table title
           hidden(h1(id="title_tabla2", "Tabla de datos cartográficos")),
           # blank separation line
           br(),
           hidden(tabPanel(id="panel_tabla2", "Tabla de datos cartográficos", DT::dataTableOutput("mytable2"))),
           # blank separation line
           br(),
           # Download button for second table
           hidden(downloadButton( outputId = "descarga_tabla2", label = "Descargar tabla cartográfica", icon("download") )),
           # blank separation lines
           br(),
           br(),
           
    )
  ),
  
) # end fluidPage()

# SERVER SIDE

server <- function(input, output, session) {
  
  # Observe event session$onSessionEnded(stopApp): Did user pressed stopSession button?
  observeEvent(input$stopSession, {
    stopApp()
  })
  
  # Observe and analyze application URL to get chapter name
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['cap']])) {
      updateSelectInput(session, "fenómeno1", selected = query[['cap']])
      disable('fenómeno1')
      hide('fenómeno2')
      hide('Ayuda_1')
      hide('limpiar')
    }
  })  
  
  # Empty data frames for all phenomena
  fenómenos1 <- data_frame(ID=character(), Fecha=integer(), Siglo=character(), Provincia=character(), Población=character(), Cont_anterior=character(), Forma=character(), Cont_posterior=character(), Ámbito=character(), Latitud=character(), Longitud=character(), Fenómeno=character(), Variante=character())
  fenómenos2 <- data_frame(ID=character(), Fecha=integer(), Siglo=character(), Provincia=character(), Población=character(), Cont_anterior=character(), Forma=character(), Cont_posterior=character(), Ámbito=character(), Latitud=character(), Longitud=character(), Fenómeno=character(), Variante=character())
  fenómenos <- data_frame(ID=character(), Fecha=integer(), Siglo=character(), Provincia=character(), Población=character(), Cont_anterior=character(), Forma=character(), Cont_posterior=character(), Ámbito=character(), Latitud=character(), Longitud=character(), Fenómeno=character(), Variante=character())
  # Apply color palette to Variants
  paleta <- colorFactor(viridis_pal(option = "C")(6), domain = fenómenos$Variante)
  
  # Observe user's first phenomenon selection and load and filter its corresponding CSV file
  observe({
    if(length(mi_indice[mi_indice$Fenómeno==input$fenómeno1,]$Fichero) > 0) {
      csv_file1 = mi_indice[mi_indice$Fenómeno==input$fenómeno1,]$Fichero
      csv_file1 = paste0("../data/", csv_file1)
      concordancias1 <- map_dfr(csv_file1, read_delim, delim = ";", show_col_types = FALSE)
      fenómenos1 <<- concordancias1 %>%
        mutate(Fenómeno = input$fenómeno1) %>%
        filter(Población != "NA") %>% 
        mutate(Variante = str_c(Fenómeno, " ► ", Variante))
      
      fenómenos <<- rbind(fenómenos1, fenómenos2)
      
      # Re-apply color palette to Variants
      paleta <<- colorFactor(viridis_pal(option = "C")(6), domain = fenómenos$Variante)
    }    
  })
  
  # Observe user's second phenomenon selection and load and filter its corresponding CSV file
  observe({
    if(length(mi_indice[mi_indice$Fenómeno==input$fenómeno2,]$Fichero) > 0) {
      csv_file2 = mi_indice[mi_indice$Fenómeno==input$fenómeno2,]$Fichero
      csv_file2 = paste0("../data/", csv_file2)
      concordancias2 <- map_dfr(csv_file2, read_delim, delim = ";", show_col_types = FALSE)
      fenómenos2 <<- concordancias2 %>%
        mutate(Fenómeno = input$fenómeno2) %>%
        filter(Población != "NA") %>% 
        mutate(Variante = str_c(Fenómeno, " ► ", Variante))
      
      fenómenos <<- rbind(fenómenos1, fenómenos2)
      
      # Re-apply color palette to Variants
      paleta <<- colorFactor(viridis_pal(option = "C")(6), domain = fenómenos$Variante)
    }    
  })
  
  # Filtering data based on input (store the selected and filtered variants in variable "datos")
  datos <- reactive({
    tabla1 <- fenómenos %>%
      filter(Fecha >= input$año[1]) %>% # first year range filter
      filter(Fecha <= input$año[2]) %>% # last year range filter
      # filter(Latitud >= input$latitud[1]) %>% # uncomment to use Latitude filter
      # filter(Latitud <= input$latitud[2]) %>% # uncomment to use Latitude filter
      # filter(Longitud >= input$longitud[1]) %>% # uncomment to use Longitude filter
      # filter(Longitud <= input$longitud[2]) %>% # uncomment to use Longitude filter
      
      # Possible combinations depending on which selection list is empty or not
      filter(
        conditional(input$fenómeno1 == "" & input$fenómeno2 == "", Fenómeno == 'XXXX'),
        conditional(input$fenómeno1 != "" & input$fenómeno2 == "", Fenómeno == input$fenómeno1),
        conditional(input$fenómeno1 != "" & input$fenómeno2 != "", Fenómeno %in% c(input$fenómeno1, input$fenómeno2)),
        conditional(input$fenómeno1 == "" & input$fenómeno2 != "", Fenómeno == input$fenómeno2)
      ) %>%
      # Only keep selected Variants
      filter(
        conditional(input$fenómeno1 != "" | input$fenómeno2 != "", Variante %in% c(input$Variantes))
      ) %>%
      # Filter by document type
      filter(
        conditional(input$cancilleresco, Ámbito != 'cancilleresco'),
      ) %>%
      # Filter by location status
      filter(
        conditional(input$conjetural, Población != 's.l.'),
      ) %>%
      group_by(ID, Latitud, Longitud, Provincia, Población, Variante) %>% 
      count() %>% 
      ungroup() %>% 
      group_by(Provincia, Población, Variante) %>% 
      mutate(documentos = n()) %>% 
      mutate(ejemplos = sum(n)) %>% 
      select(-c(n, ID)) %>%
      ungroup() %>%
      unique() 
    
    # prepare table so that it can be mapped by type of count
    if  (nrow(tabla1) != 0) {
      tabla1 %>%
        pivot_longer(cols = documentos:ejemplos, names_to = "tipo", values_to = "Valores") %>% 
        filter(tipo == input$recuento)
    }
    
  })
  
  # Filtering data based on input (store unfiltered variants in variable "datos2")
  datos2 <- reactive({
    tabla1 <- fenómenos %>%
      # Possible combinations depending on which selection list is empty or not
      filter(
        conditional(input$fenómeno1 == "" & input$fenómeno2 == "", Fenómeno == 'XXXX'),
        conditional(input$fenómeno1 != "" & input$fenómeno2 == "", Fenómeno == input$fenómeno1),
        conditional(input$fenómeno1 != "" & input$fenómeno2 != "", Fenómeno %in% c(input$fenómeno1, input$fenómeno2)),
        conditional(input$fenómeno1 == "" & input$fenómeno2 != "", Fenómeno == input$fenómeno2)
      ) %>%
      # Filter by document type
      filter(
        conditional(input$cancilleresco, Ámbito != 'cancilleresco'),
      ) %>%
      # Filter by location status
      filter(
        conditional(input$conjetural, Población != 's.l.'),
      )
  })
  
  # Data for the Table "Tabla de ejemplos" (store in "tabla2" without grouping or calculating)
  datos_tabla <- reactive({
    tabla2 <- fenómenos %>%
      filter(Fecha >= input$año[1]) %>% # first year range filter
      filter(Fecha <= input$año[2]) %>% # last year range filter
      # filter(Latitud >= input$latitud[1]) %>% # uncomment to use Latitude filter
      # filter(Latitud <= input$latitud[2]) %>% # uncomment to use Latitude filter
      # filter(Longitud >= input$longitud[1]) %>% # uncomment to use Longitude filter
      # filter(Longitud <= input$longitud[2]) %>% # uncomment to use Longitude filter
      
      # Possible combinations depending on which selection list is empty or not
      filter( # combinaciones dependiendo de qué botón de selección está vacío o no
        conditional(input$fenómeno1 == "" & input$fenómeno2 == "", Fenómeno == 'XXXX'),
        conditional(input$fenómeno1 != "" & input$fenómeno2 == "", Fenómeno == input$fenómeno1),
        conditional(input$fenómeno1 != "" & input$fenómeno2 != "", Fenómeno %in% c(input$fenómeno1, input$fenómeno2)),
        conditional(input$fenómeno1 == "" & input$fenómeno2 != "", Fenómeno == input$fenómeno2)
      ) %>%
      # Only keep selected Variants
      filter(
        conditional(input$fenómeno1 != "", Variante %in% c(input$Variantes))
      ) %>%
      # Filter by document type
      filter(
        conditional(input$cancilleresco, Ámbito != 'cancilleresco'),
      ) %>%
      # Filter by location status
      filter(
        conditional(input$conjetural, Población != 's.l.'),
      )
  })
  
  # Reset input choices
  observeEvent(input$limpiar, {
    reset("side-panel")
    hide('Variantes')
    disable('descarga')
    disable('descarga_tabla1')
    disable('descarga_tabla2')
    disable('leyenda')
    disable('cancilleresco')
    disable('conjetural')
  })
  
  # Show variants for first phenomenon
  observeEvent(input$fenómeno1, {
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(),
                             inputId = "Variantes",
                             choices = c(sort(unique(datos2()$Variante))),
                             selected = c(sort(unique(datos2()$Variante)))
    )
  })
  
  # Show variants for second phenomenon
  observeEvent(input$fenómeno2, {
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(),
                             inputId = "Variantes",
                             choices = c(sort(unique(datos2()$Variante))),
                             selected = c(sort(unique(datos()$Variante)))
    )
  })
  
  # Remove choice for second phenomenon from firsr phenomenon list
  observeEvent(input$fenómeno1, {
    choices_capitulos2 <- choices_capitulos[! choices_capitulos %in% input$fenómeno1]
    updateSelectInput(session = getDefaultReactiveDomain(),
                      inputId = "fenómeno2",
                      choices = c('Elegir rasgo'='', choices_capitulos2),
                      selected = NULL
    )
  })
  
  ### MAP ####
  
  # Load simplified geojson files  for Spain provinces, Portugal, Andorra, and other countries
  provincias_geojson <- readLines("../provincias/provincias_simples.geojson") %>% paste(collapse = "\n") 
  portugal_geojson <- readLines("../provincias/portugal_simples.geojson") %>% paste(collapse = "\n") 
  andorra_geojson <- readLines("../provincias/andorra_simples.geojson") %>% paste(collapse = "\n") 
  paises_geojson <- readLines("../paises/paises_simplificado.geojson") %>% paste(collapse = "\n") 
  
  # Create a leaflet map (options: no attribution, no zoom controls) and save it as a reactive function
  
  mapa_inicio <- reactive({
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 8, attributionControl=FALSE, zoomControl = FALSE)) %>%
      setView(lng = -3.0, lat = 40.0, zoom = 7)  %>%
      # Esri.WorldGrayCanvas Tiles
      addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(opacity = 0.5)) %>% # mapa gris con división de países
      # Apply geojson data and allow user to display regional divisions or not
      addGeoJSON(provincias_geojson,
                 group = "Divisiones provincias",
                 color = "#666666",
                 weight = 0.5,
                 smoothFactor = 0.8,
                 opacity = 0.6,
                 fill = TRUE,
                 fillColor = "white",
                 fillOpacity = 1) %>%
      addGeoJSON(portugal_geojson,
                 group = "Divisiones provincias",
                 color = "#666666",
                 weight = 0.5,
                 smoothFactor = 0.8,
                 opacity = 0.6,
                 fill = TRUE,
                 fillColor = "white",
                 fillOpacity = 0.2) %>%
      addGeoJSON(andorra_geojson,
                 group = "Divisiones provincias",
                 color = "#666666",
                 weight = 0.5,
                 smoothFactor = 0.8,
                 opacity = 0.4,
                 fill = TRUE,
                 fillColor = "white",
                 fillOpacity = 0.7) %>%
      addLayersControl(overlayGroups = "Divisiones provincias",
                       options = layersControlOptions(collapsed = TRUE))
  })  
  
  # Render Map stored in function "mapa_inicio()"
  output$mapa <- renderLeaflet({
    mapa_inicio()
  })
  
  # Function to add circles for each phenomenon
  mis_fenomenos <- function(mapa){
    
    # Validation to determine whether the data() function returns NULL when there is no data for the specified dates. 
    # We clear the circles and legends and disable the download buttons (otherwise, clicking them would cause an error because there is no data) and show the legend.
    # If there is data, we make sure that the download and legend buttons are enabled. validate(FALSE) prevents the function from continuing and causing an error
    
    if  (is_null(datos())) {
      clearMarkers(mapa)
      clearControls(mapa)
      disable('descarga')
      disable('descarga_tabla1')
      disable('descarga_tabla2')
      disable('leyenda')
      disable('cancilleresco')
      disable('conjetural')
      hide('title_tabla1')
      hide('title_tabla2')
      hide('panel_tabla1')
      hide('panel_tabla2')
      hide('descarga_tabla1')
      hide('descarga_tabla2')
      validate(FALSE)
    }
    else{
      show('Variantes')
      enable('descarga')
      enable('descarga_tabla1')
      enable('descarga_tabla2')
      enable('leyenda')
      enable('cancilleresco')
      enable('conjetural')
      show('title_tabla1')
      show('title_tabla2')
      show('panel_tabla1')
      show('panel_tabla2')
      show('descarga_tabla1')
      show('descarga_tabla2')
      
    }
    
    # To ensure that the province divisions layer always remains below the circles, we place the circles in a MapPane with a very high zIndex.
    # First, create the MapPane with zIndex 410, then use options = pathOptions(pane = "circles_above") in AddCircles.
    
    if (input$fenómeno1 != "" | input$fenómeno2 != "")
    {
      clearMarkers(mapa) %>%
        addMapPane("circulos_arriba", zIndex = 410) %>%
        addCircleMarkers(data = datos(), options = pathOptions(pane = "circulos_arriba"),
                         lng = ~Longitud, 
                         lat = ~Latitud,
                         radius = ~rescale(Valores, c(6, 30)), # in pixels for addCircleMarkers
                         color = ~paleta(Variante),
                         fillOpacity = 0.7,
                         label = ~lapply(paste0("<h4> <span style='color: #0071b3'>Población: <span style='color: black'>", Población, " (<i>", Provincia, "</i>)</span><br>", "<span style='color: #0071b3'>Ocurrencias:</span> <span style='color: black'>", Valores, "</span><br><span style='color: #0071b3'>Variante:</span> <span style='color: black'>", Variante, "</span>"), htmltools::HTML),
                         stroke = FALSE)
    }
    else {
      clearMarkers(mapa)
    }
  }
  
  # Function to add legends to the map
  mis_leyendas <- function(mapa) {
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$leyenda) {
      clearControls(mapa)
      mapa  %>%  addLegend("bottomright", pal = paleta, values = datos()$Variante, title = "Variantes")   %>%  addLegend("bottomleft", pal = paleta, values = str_c(input$año[1], " ↔ " , input$año[2]), title = "Arco cronológico")
    }
    else {
      clearControls(mapa)
    }
    
  }
  
  # Apply functions "mis_fenomenos()" and "mis_leyendas()" to Map
  observe({
    leafletProxy("mapa") %>% mis_fenomenos() %>% mis_leyendas()
  })
  
  # Create the map that the user is currently viewing to download it: store in "mapa_descarga" (updating the coordinates and zoom by monitoring the mapa_center and mapa_zoom events)
  # and applying mis_fenomenos and mis_leyendas functions to the mapa_inicio.
  observeEvent({
    input$mapa_zoom
    input$mapa_center
  }, {
    mapa_descarga <<- reactive({
      mapa_inicio() %>% mis_fenomenos() %>% mis_leyendas() %>% setView(lng = input$mapa_center$lng, lat = input$mapa_center$lat, zoom = input$mapa_zoom) 
    })
  })
  
  # Download map function
  output$descarga <- downloadHandler(
    filename = function() {paste0("AHE_", Sys.time(), ".png")}, content = function(file) {
      mapshot( x = mapa_descarga(), file = file,
               vwidth = input$dimension[1], vheight = input$dimension[2],
               selfcontained = FALSE
      ) # end of mapshot()
    } # end of content() function
  ) # end of downloadHandler() function
  
  # Download first table function
  output$descarga_tabla1 <- downloadHandler(
    filename = function(){"tabla_ejemplos.csv"}, 
    content = function(fname){
      write.csv(datos_tabla(), fname)
    }
  )
  
  # Download second table function
  output$descarga_tabla2 <- downloadHandler(
    filename = function(){"tabla_datos.csv"}, 
    content = function(fname){
      write.csv(datos(), fname)
    }
  )
  
  # Create and display First Table including CODEA or OSTA links to the source document
  output$mytable1 <- DT::renderDataTable({
    datos_filtrados <- datos_tabla()[-grep('^(source|Latitud|Longitud|Fenómeno)$', colnames(fenómenos))] %>%
      mutate(ID = case_when(grepl("^CODEA", ID) ~ paste0("<a target='_blank' href='https://corpuscodea.es/corpus/documento.php?documento=", ID, "&busqueda=", Forma, "'>", ID, "</a>"),
                            grepl("^OSTA", ID) ~ ID,
                            TRUE ~ ID)
      )
    
    mi.tabla = DT::datatable(datos_filtrados, escape = FALSE, # allows text cell interpreted as HTML
                             options = list(language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                            autoWidth = FALSE, # smart width handling
                                            searching = TRUE, # search box above table
                                            ordering = TRUE, # whether columns can be sorted
                                            lengthChange = TRUE, # ability to change number rows shown on page in table
                                            paging = TRUE, # whether to do pagination
                                            info = TRUE, # notes whether or not table is filtered
                                            columnDefs = list(list(className = 'dt-center', targets = c(1))),
                                            pageLength = 5,
                                            lengthMenu = c(5, 10, 15, 20, 100))) 
    mi.tabla <- formatStyle(mi.tabla,
                            columns = c(1,7,9), #specify columns to format
                            backgroundColor = "#1f456e",
                            color = "#ffffff",
                            fontFamily = "Arial",
                            fontSize = "12px",
                            fontWeight = "bold",
                            textAlign = "center",
                            verticalAlign = "middle",
                            borderBottomColor = "#ffffff",
                            borderBottomStyle = "solid",
                            borderBottomWidth = "1px",
                            borderCollapse = "collapse",
                            borderRightColor = "#ffffff",
                            borderRightStyle = "solid",
                            borderRightWidth = "1px",
                            paddingBottom = "2.6px",
                            paddingLeft = "2.2px",
                            paddingRight = "2.2px",
                            paddingTop = "2.6px",
                            wordWrap = "break-word")
    mi.tabla <- formatStyle(mi.tabla,
                            columns = c(1), #specify columns to format
                            backgroundColor = "white",
                            color = "black",
                            borderLeftColor = "black",
                            borderLeftStyle = "solid")
    
    mi.tabla <- formatStyle(mi.tabla,
                            columns = c(2,3,4,5,6,8,10), #blank columns means row labels
                            backgroundColor = '#e6e6e5',
                            color = '#1f456e',
                            fontFamily = "Arial",
                            fontSize = "12px",
                            fontWeight = 'bold',
                            lineHeight = "normal",
                            textAlign = "left",
                            verticalAlign = "middle",
                            borderBottomColor = "#ffffff",
                            borderBottomStyle = "solid",
                            borderBottomWidth = "1px",
                            borderCollapse = "collapse",
                            borderRightColor = "#ffffff",
                            borderRightStyle = "solid",
                            borderRightWidth = "1px",
                            paddingBottom = "2.6px",
                            paddingLeft = "5.2px",
                            paddingRight = "5.2px",
                            paddingTop = "2.6px",
                            wordWrap = "break-word")
    
    mi.tabla
    
  })
  
  # Create and display Second Table
  output$mytable2 <- DT::renderDataTable({
    datos_filtrados2 <- datos()[-grep('^tipo$', colnames(datos()))]
    mi.tabla2 = DT::datatable(datos_filtrados2, options = list(language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                               autoWidth = FALSE, # smart width handling
                                                               searching = TRUE, # search box above table
                                                               ordering = TRUE, # whether columns can be sorted
                                                               lengthChange = TRUE, # ability to change number rows shown on page in table
                                                               paging = TRUE, # whether to do pagination
                                                               info = TRUE, # notes whether or not table is filtered
                                                               columnDefs = list(list(className = 'dt-center', targets = c(4,5,6)), list(className = 'dt-left', targets = c(1,2,3))),
                                                               pageLength = 5,
                                                               lengthMenu = c(5, 10, 15, 20, 100))) 
    if  (!is_null(datos())) {
      
      mi.tabla2 <- formatStyle(mi.tabla2,
                               columns = c(4,5,6), # specify columns to format
                               backgroundColor = "#1f456e",
                               color = "#ffffff",
                               fontFamily = "Arial",
                               fontSize = "12px",
                               fontWeight = "bold",
                               textAlign = "center",
                               verticalAlign = "middle",
                               borderBottomColor = "#ffffff",
                               borderBottomStyle = "solid",
                               borderBottomWidth = "1px",
                               borderCollapse = "collapse",
                               borderRightColor = "#ffffff",
                               borderRightStyle = "solid",
                               borderRightWidth = "1px",
                               paddingBottom = "2.6px",
                               paddingLeft = "5.2px",
                               paddingRight = "5.2px",
                               paddingTop = "2.6px",
                               wordWrap = "break-word")
      
      mi.tabla2 <- formatStyle(mi.tabla2,
                               columns = c(1,2,3), #blank columns means row labels
                               backgroundColor = '#e6e6e5',
                               color = '#1f456e',
                               fontFamily = "Arial",
                               fontSize = "12px",
                               fontWeight = 'bold',
                               lineHeight = "normal",
                               textAlign = "left",
                               verticalAlign = "middle",
                               borderBottomColor = "#ffffff",
                               borderBottomStyle = "solid",
                               borderBottomWidth = "1px",
                               borderCollapse = "collapse",
                               borderRightColor = "#ffffff",
                               borderRightStyle = "solid",
                               borderRightWidth = "1px",
                               paddingBottom = "2.6px",
                               paddingLeft = "5.2px",
                               paddingRight = "5.2px",
                               paddingTop = "2.6px",
                               wordWrap = "break-word")
      
      mi.tabla2
    }
    
  })
  
}

# SHINYAPP
shinyApp(ui = ui, server = server)

