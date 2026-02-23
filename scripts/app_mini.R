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
      
      position = "right",  # si queremos mover el menú lateral a la derecha
      
      
      ## Sidebar panel for inputs ----
      # en nuestro caso, panel lateral en el que configuraremos las características del mapa
      ## Sidebar panel for inputs
      sidebarPanel(
        
        # enables functions to reset HTML form
        useShinyjs(), 
        id = "side-panel",
        width = 3,
        
        # selection button: list of first phenomena to display (hidden)
        hidden(selectizeInput(inputId = "fenómeno1", label = "", choices = c('Elegir fenómeno'='', choices_capitulos), selected = NULL)),
        # checkbox to select variants for all phenomena selected (hidden)
        hidden(checkboxGroupInput(inputId = "Variantes", label = "Variantes")),
        
        # radio button to choose the type of count the user wants: examples or documents (hidden)
        hidden(radioButtons(inputId = "recuento",
                            label = "Tipo de recuento",
                            choices = c("Número de ejemplos" = "ejemplos", 
                                        "Número de documentos" = "documentos"))),
        
        # slider for selecting chronological range
        sliderInput(inputId = "año",
                    label = "",
                    min = 1200,
                    max = 1600,
                    value = c(1200, 1600), # default slider position
                    step = 25, # minimal selectable range
                    animate = animationOptions(interval = 1000),
                    ticks = TRUE,
                    sep = ".",
                    dragRange = TRUE),
        
        # Option to show or hide map legends
        checkboxInput(inputId = "leyenda", 
                      label = "Mostrar leyendas", TRUE),
        
        # Reset options button (hidden)
        hidden(actionButton(inputId = "limpiar",
                            label = "Limpiar configuración")),
        
        # Button to reset connection to the Shiny server       
        actionButton(inputId = "stopSession",
                     label = "Desconectar aplicación")
        
      ), # end sidebarPanel() definition
      
      
      ## Main panel for displaying outputs ----
      mainPanel(
        
        
        # MAIN MAP DISPLAY
        leafletOutput("mapa", 
                      #width = "650px", 
                      height = "250px"),
        
        useShinyjs(),
        # Download map button (hidden)
        hidden(downloadButton( outputId = "descarga", label = "Descargar mapa", icon("download") ))
        
      ), # end mainPanel()
    ) # end sidebarLayout()
  ), # end fluidRow()
) # end fluidPage()

# SERVER SIDE

server <- function(input, output, session) {
  
  # Observe event session$onSessionEnded(stopApp): Did user pressed stopSession button?
  observeEvent(input$stopSession, {
    stopApp()
  })
  
  # hide and disable controls
  hide('limpiar')
  disable('fenómeno1')
  hide('Variantes')
  
  # Observe and analyze application URL to get chapter name
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['cap']])) {
      updateSelectInput(session, "fenómeno1", selected = query[['cap']])
    }
  })  
  
  # Empty data frame for all phenomena
  fenómenos <- data_frame(ID=character(), Fecha=integer(), Siglo=character(), Provincia=character(), Población=character(), Cont_anterior=character(), Forma=character(), Cont_posterior=character(), Ámbito=character(), Latitud=character(), Longitud=character(), Fenómeno=character(), Variante=character())
  # Apply color palette to Variants
  paleta <- colorFactor(viridis_pal(option = "C")(2), domain = fenómenos$Variante)
  
  # load and filter CSV file
  observe({
    if(length(mi_indice[mi_indice$Fenómeno==input$fenómeno1,]$Fichero) > 0) {
      csv_file1 = mi_indice[mi_indice$Fenómeno==input$fenómeno1,]$Fichero
      csv_file1 = paste0("../data/", csv_file1)
      concordancias1 <- map_dfr(csv_file1, read_delim, delim = ";", show_col_types = FALSE)
      fenómenos <<- concordancias1 %>%
        mutate(Fenómeno = input$fenómeno1) %>%
        filter(Población != "NA") %>% 
        mutate(Variante = str_c(Fenómeno, " ► ", Variante))
      
      # Re-apply color palette to Variants
      paleta <<- colorFactor(viridis_pal(option = "C")(2), domain = fenómenos$Variante)
    }    
  })
  
  # Unique list of chapters
  choices_capitulos <- unique(fenómenos$Fenómeno)
  # Re-apply color palette to Variants
  paleta <<- colorFactor(viridis_pal(option = "C")(2), domain = fenómenos$Variante)
  
  # Filtering data (store filtered variants in variable "datos")
  datos <- reactive({
    tabla1 <- fenómenos %>%
      filter(Fecha >= input$año[1]) %>% # first year range filter
      filter(Fecha <= input$año[2]) %>% # last year range filter
      # Possible combinations
      filter(
        conditional(input$fenómeno1 == "", Fenómeno == 'XXXX'),
        conditional(input$fenómeno1 != "", Fenómeno == input$fenómeno1),
      ) %>%
      # Only keep selected Variants
      filter(
        conditional(input$fenómeno1 != "", Variante %in% c(input$Variantes))
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
      tabla1 %>% # preparamos la tabla para que se pueda cartografiar por tipo de recuento
        pivot_longer(cols = documentos:ejemplos, 
                     names_to = "tipo", values_to = "Valores") %>% 
        filter(tipo == input$recuento)
    }
    
  })
  
  # Filtering data(store unfiltered variants in variable "datos2")
  datos2 <- reactive({
    tabla1 <- fenómenos %>%
      filter(
        conditional(input$fenómeno1 == "", Fenómeno == 'XXXX'),
        conditional(input$fenómeno1 != "", Fenómeno == input$fenómeno1),
      ) 
  })
  
  # Reset input choices
  observeEvent(input$limpiar, {
    reset("side-panel")
    hide('Variantes')
    disable('fenómeno1')
    disable('descarga')
    disable('leyenda')
  })
  
  # Show variants
  observeEvent(input$fenómeno1, {
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(),
                             inputId = "Variantes",
                             choices = c(sort(unique(datos2()$Variante))),
                             selected = c(sort(unique(datos2()$Variante)))
    )
  })  
  
  # MAPA ####
  
  # Load simplified geojson files  for Spain provinces, Portugal, and Andorra
  provincias_geojson <- readLines("../provincias/provincias_simples.geojson") %>% paste(collapse = "\n") 
  portugal_geojson <- readLines("../provincias/portugal_simples.geojson") %>% paste(collapse = "\n") 
  andorra_geojson <- readLines("../provincias/andorra_simples.geojson") %>% paste(collapse = "\n") 
  
  # Create a leaflet map (options: no attribution, no zoom controls) and save it as a reactive function
  mapa_inicio <- reactive({
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 8, attributionControl=FALSE, zoomControl = FALSE)) %>%
      setView(lng = -3.0, lat = 40.0, zoom = 5)  %>%
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
                 fillOpacity = 0.7)
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
      clearShapes(mapa)
      clearControls(mapa)
      disable('descarga')
      disable('leyenda')
      validate(FALSE)
    }
    else{
      show('Variantes')
      enable('descarga')
      enable('leyenda')
    }
    
    # To ensure that the province divisions layer always remains below the circles, we place the circles in a MapPane with a very high zIndex.
    # First, create the MapPane with zIndex 410, then use options = pathOptions(pane = "circles_above") in AddCircles.
    
    if (input$fenómeno1 != "")
    {
      clearShapes(mapa) %>%
        addMapPane("circulos_arriba", zIndex = 410) %>% 
        addCircles(data = datos(), options = pathOptions(pane = "circulos_arriba"),
                   lng = ~Longitud, 
                   lat = ~Latitud,
                   radius = ~rescale(Valores, c(12000, 48000)), # en metros para addCircles
                   color = ~paleta(Variante),
                   fillOpacity = 0.7,
                   label = ~Valores,
                   stroke = FALSE)
      
    }
    else {
      clearShapes(mapa)
    }
  }
  
  # Function to add legends to the map
  mis_leyendas <- function(mapa) {
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$leyenda) {
      clearControls(mapa)
      mapa %>%  addLegend("bottomright", pal = paleta, values = datos()$Variante, title = "Variantes")
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
  
}

# SHINYAPP
shinyApp(ui = ui, server = server)
