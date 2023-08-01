library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(nycflights13)
library(histoslider)
library(rlang)
library(htmltools)
library(leaflet)
library(tidyr)

parametros <- list(
  color = "#236478",
  round_digits = 1,
  font_family = "Raleway",
  font_family_code = "Source Code Pro",
  tabla_datos = "estaciones_datos",
  tabla_estaciones = "estaciones",
  paleta = c("#730000","#E60000","#FFAA00","#FFD37F","#FFFF00","#FFFFFF",
             "#8CCDEF","#00BFFF","#1D90FF","#4169E1","#0000FF"),
  etiquetas = c("Sequía excepcional", "Sequía extrema", "Sequía severa",
                "Sequía moderada", "Anormalmente seco","Normal",
                "Anormalmente húmedo","Moderadamente húmedo","Severamente húmedo",
                "Extramademente húmedo", "Excepcionalmente húmedo")
)
theme_odes <-  bs_theme(
  version = 5,
  primary = parametros$color,
  base_font = font_google(parametros$font_family),
  code_font = font_google(parametros$font_family_code)
)

# Data prep
data.inicial <- read.csv("Biodiversidad-Topografia.csv")
data.inicial$Longitud <- round(data.inicial$Longitud,2)
data.inicial$Latitud <- round(data.inicial$Latitud,2)
biodiversidad <- tibble(data.inicial[,c("Sitio","Riqueza","Shannon","Simpson")])
Sitio <- tibble(data.inicial[,c("Sitio","Longitud","Latitud")])
Topografica <- tibble(data.inicial[c("Sitio","Elevación","Pendiente.Porcentaje","Exposición")])
PRIMARY <- "#68B47D"
biodiversidad <- biodiversidad %>%
  left_join(Sitio) %>%
  left_join(Topografica)

# load("BiodiversidadDatos_Unidos.RData")
# load("TOMST_Aconcagua.RData")
# 
# censores_final<-censores_final %>%
#   mutate(ID_Sensor = as.character(ID_Sensor),
#          Sitio =as.character(Sitio),
#          ID_Sensor = if_else(ID_Sensor =="94238449" & Sitio=="18","94238459", ID_Sensor),
#          ID_Sensor = as.numeric(ID_Sensor))
# 
# censores_finall <- censores_final %>%
#   rename(serial_number = ID_Sensor) %>%
#   dplyr::select(., serial_number, Elevation, Latitude, Categoria) %>%
#   distinct(.)
# 
# air_temp <- tms_bind %>%
#   dplyr::filter(., sensor_name=="TMS_T1" | sensor_name=="TMS_T2" | sensor_name=="TMS_T3") %>%
#   filter(., Profundidad=="15")
# 
# air_temp_15 <- air_temp %>%
#   dplyr::filter(., height=="air 15 cm") %>%
#   drop_na(value) %>%
#   dplyr::filter(., value> -60) %>% # para remover un par de valores erroneos
#   dplyr::filter(., value < 60) %>%
#   group_by(serial_number, datetime2) %>%
#   summarise_at("value", list(minT=min, maxT=max)) %>%
#   mutate(daily_T= (maxT+minT)/2,
#          serial_number=as.numeric(serial_number)) %>%
#   left_join(.,censores_finall, by="serial_number")
# 
# head(air_temp_15)
# 
# # y la temperatura del suelo:
#   
#   soil_temp_8 <- air_temp %>%
#   dplyr::filter(., height=="soil 8 cm") %>%
#   drop_na(value) %>%
#   dplyr::filter(., value> -60) %>%
#   dplyr::filter(., value < 60) %>%
#   group_by(serial_number, datetime2) %>%
#   summarise_at("value", list(minT=min, maxT=max)) %>%
#   mutate(daily_T= (maxT+minT)/2,
#          serial_number=as.numeric(serial_number)) %>%
#   left_join(.,censores_finall, by="serial_number")
# 
# 
# 
# # Mi recomendación es evitar sensores con "Profundidad" que no sean "15" para la aplicación. Para el contenido del agua del suelo ("VWC"), he usado el siguiente código:
#   
#   vwc_s <- tms_bind %>%
#   dplyr::filter(., sensor_name=="VWC_moisture") %>%
#   dplyr::filter(., Profundidad=="15")
# 
# soil_vwc <- vwc_s %>%
#   group_by(serial_number, datetime2) %>%
#   summarise_at("value", list(dailyVWC=mean)) %>%  
#   mutate(serial_number=as.numeric(serial_number)) %>%
#   left_join(.,censores_finall, by="serial_number") %>%
#   mutate(Categoria= if_else(Categoria=="Nativo","Forest",Categoria),
#          Categoria= if_else(Categoria=="Agricola_s/riego","Agr. -H20",Categoria),
#          Categoria= if_else(Categoria=="Agricola_c/riego","Agr. +H20",Categoria))
# 
# 
# 
# d0 <- unique(air_temp[,c("Sitio","serial_number","lat_wgs84","lon_wgs84")])
# d0$serial_number <- as.numeric(d0$serial_number)
# d0$Sitio <- as.numeric(d0$Sitio)
# 
# sensores <- air_temp_15 %>%
#   left_join(soil_temp_8, by=c("serial_number","datetime2","Elevation","Latitude","Categoria")) %>%
#   left_join(soil_vwc, by=c("serial_number","datetime2","Elevation","Latitude")) %>%
#   left_join(d0, by="serial_number")
# 
# head(sensores)
# sensores <- sensores %>% group_by(Categoria.x, Sitio, datetime2, Elevation, lat_wgs84, lon_wgs84) %>%
#   summarise(Temp.aire.15 = mean(daily_T.x, na.rm = T),
#             Temp.suelo.8 = mean(daily_T.y, na.rm = T),
#             Humedad.suelo=mean(dailyVWC, na.rm = T))
# names(sensores) <- c("Categoria", "Sitio", "Fecha", "Elevacion", "Latitud", "Longitud",
#                       "Temp.aire.15", "Temp.suelo.8", "Humedad.suelo")


# save(file = "sensores.RData",sensores)
load("sensores.RData")

head(sensores)
# interfaz de usuario
ui <-page_navbar(
  title  = tags$span(
    class = "title",
    tags$a(
      tags$img(src = "horizontal_SB_blanco.png", height = "30px", style = "margin-top: -5px"),
      href = "https://odes-chile.org/"
    ),
    "Sensores"
  ),
  id = "nav",
  lang = "es",
  theme = theme_odes,
  fillable = TRUE,
  fillable_mobile = TRUE,
  # sidebar -----------------------------------------------------------------
  sidebar = sidebar(
    width = 400,
  #   selectInput("unidad", tags$small("Sitio"), opt_unidad),
  #   selectInput("macrozona", tags$small("Macrozona"), opt_macrozona, multiple = FALSE), # selected = "zona central",
  #   selectInput("variable", tags$small("Variable"), opt_variable, selected = "pre"),
  #   sliderTextInput("fecha", tags$small("Fecha"), opt_fecha, selected = c(tail(opt_fecha, 12 * 10)[1], tail(opt_fecha, 1))),
  #   
  #   conditionalPanel(
  #     "input.showchart",
  #     # "hchart va en 2do contitaion panel",
  #     highchartOutput("chart", width = "100%", height = "250px"),
  #     div(
  #       style="display:inline-block;float:right",
  #       downloadButton("descargar_datos_mini", "Descargar datos", class = "btn-primary btn-sm")
  #     )
  #     # tags$br(),
  #   ),
  #   conditionalPanel(
  #     "false",
  #     checkboxInput("showchart", "Mostrar información histórica"),
  #   ),
  #   # actionButton("guidess", "Guide")
  # )
  accordion(
    open = c("Categoría", "Rango de fechas"),
    accordion_panel(
      "Categoría", icon = icon("sun-plant-wilt"),
      uiOutput("categoria_reset"),
      checkboxGroupInput(
        "categoria", NULL,
        choices = sort(unique(sensores$Categoria)),
        inline = FALSE,
        selected = sort(unique(sensores$Categoria))
      )
    ),
    accordion_panel(
      "Sitio", icon = icon("location-dot"),
      uiOutput("sitio_reset"),
      checkboxGroupInput(
        "sitio", NULL,
        choices = sort(unique(sensores$Sitio)),
        inline = TRUE,
        selected = sort(unique(sensores$Sitio))
      )
    ),
    accordion_panel(
        "Rango de fechas", icon = icon("calendar-days"),
      dateRangeInput(
        inputId = "fechas",
        label = NULL,
        language = "es",
        start = min(sensores$Fecha, na.rm = T),#"2018-04-15",
        end =  max(sensores$Fecha, na.rm = T),#"2018-04-16",
        min = min(sensores$Fecha, na.rm = T),#"2018-04-10",
        max = max(sensores$Fecha, na.rm = T)),#"2018-04-20"),
      # sliderInput("fecha", "Fecha:",
                  # min = min(sensores$datetime2, na.rm = T), max = max(sensores$datetime2, na.rm = T), 
                  # value = seq(min(sensores$datetime2, na.rm = T), max(sensores$datetime2, na.rm = T),1)),
   

    ),
    accordion_panel(
      "Topografía", icon = icon("mountain"),
      input_histoslider(
        "topografica_elevacion", "Elevación (msnm)",
        biodiversidad$Elevación, height = 150,
        options = list(     handleLabelFormat = "0d",
                            selectedColor = PRIMARY)
      ),
      input_histoslider(
        "topografica_pendiente", "Pendiente (%)",
        biodiversidad$Pendiente.Porcentaje, height = 150,
        breaks=seq(0,45,5),
        options = list(selectedColor = PRIMARY)
      ),
      input_histoslider(
        "topografica_exposición", "Exposición",
        biodiversidad$Exposición, height = 150,
        breaks=seq(0,360,45),
        options = list(selectedColor = PRIMARY)
      )
    )
  ))
  
  ,
  # mapa --------------------------------------------------------------------
  bslib::nav_panel(
    title = "Aplicación",
    icon  = icon("map-location-dot"),
    tags$head(
      tags$link(href = "Isotip_gradiente_azul.png", rel = "icon"),
      tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-CYG993XQRT", async = ""),
      tags$script(src = "js/ga.js"),
      includeCSS("www/css/styles.css"),
    ),
    fixedRow(
    # fluidRow(
      # column(width = 8,
      column(width = 10,
    plotOutput("plot",click = "plot_click")),#, height = "300px", click = "plot_click"),
    # )),
    fluidRow(
      column(width = 4,
             leafletOutput("mapa")),# width="40%", height="30%")),
      column(width = 6,
             tableOutput("data"))
  # ))),
  ))),
  bslib::nav_panel(
    title = "Ayuda",
    icon  = icon("question"),
    layout_column_wrap(
      width = 1,
      navset_card_tab(
        # height = 450,
        # full_screen = TRUE,
        # title = "HTML Widgets",
        nav_panel(
          "Aplicación",
          includeMarkdown("md/ayuda.md")
        ),
        nav_panel(
          "Indicadores",
          includeMarkdown("md/indicadores.md")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # 
  # biodiversidad <- reactive({
  #   biodiversidad[ is.element(biodiversidad()["Sitio"], sitio=updateSelectInput), ]
  # })
  
  
  
  output$plot <- renderPlot({
    # sitios.i <- unique(input$sitio)
    # riqueza.i <- summary(input$diversidad_riqueza)
    # shannon.i <- summary(input$diversidad_shannon)
    # simpson.i <- summary(input$diversidad_simpson)
    # elevacion.i <- summary(input$topografica_elevacion)
    # pendiente.i <- summary(input$topografica_pendiente)
    # exposicion.i <- summary(input$topografica_exposición)
    # 
    # biodiversidad.i <- subset(biodiversidad, is.element(Sitio, sitios.i) & 
    #                             Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
    #                             Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
    #                             Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
    #                             Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
    #                             Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
    #                             Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    # 
    # ggplot(biodiversidad.i, aes(Elevación, Riqueza)) + geom_point()+
    #   xlim (0, 2500)+
    #   ylim(0,12.5)+
    #   labs(title("Riqueza taxonómica vs elevación"))+
    #   ylab('Riqueza taxonómica') +
    #   xlab('Elevación (msnm)')
    
    categoria.i <- unique(input$categoria)
    sitios.i <- unique(input$sitio)
    fechas.i <- input$fechas
    
    sensores.i <- subset(sensores, is.element(Sitio, sitios.i) & is.element(Categoria, categoria.i))
    sensores.i <- sensores.i %>% group_by(Categoria, Fecha) %>% 
      summarise(Temp.aire.15 = mean(Temp.aire.15, na.rm = T),
                Temp.suelo.8 = mean(Temp.suelo.8, na.rm = T),
                Humedad.suelo = mean(Humedad.suelo, na.rm = T))
    
    ggplot(sensores.i, aes(x=Fecha, y=Temp.aire.15, color=Categoria))+geom_line(linetype = 2)+geom_line(aes(y=Temp.suelo.8))+
      xlim(fechas.i)+
      theme_minimal()+
      ylab('Temperatura (\u00baC)')+
      xlab('Fecha')+
      ggtitle("Temperatura del aire a 15 cm y de suelo a 8cm") +
      ylim(summary(c(sensores$Temp.aire.15,sensores$Temp.suelo.8))[c("Min.","Max.")])
    
  }, res = 96, height = 400)
  
  
  output$mapa <-  renderLeaflet({
    # sitios.i <- unique(input$sitio)
    # riqueza.i <- summary(input$diversidad_riqueza)
    # shannon.i <- summary(input$diversidad_shannon)
    # simpson.i <- summary(input$diversidad_simpson)
    # elevacion.i <- summary(input$topografica_elevacion)
    # pendiente.i <- summary(input$topografica_pendiente)
    # exposicion.i <- summary(input$topografica_exposición)
    # 
    # biodiversidad.i <- subset(biodiversidad, is.element(Sitio, sitios.i) & 
    #                             Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
    #                             Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
    #                             Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
    #                             Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
    #                             Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
    #                             Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    # 
    # 
    
    categoria.i <- unique(input$categoria)
    sitios.i <- unique(input$sitio)
    fechas.i <- input$fechas
    
    sensores.i <- subset(sensores, is.element(Sitio, sitios.i) & is.element(Categoria, categoria.i))
    
    leaflet() %>% addTiles() %>%
    addCircles(data = sensores.i, lat = ~Latitud, lng = ~Longitud)#, color=~Categoria)
  
  })
  
  
  output$data <- renderTable({
    # req(input$plot_click)
    nearPoints(sensores[,c("Categoria","Sitio","Fecha","Temp.aire.15", "Temp.suelo.8","Humedad.suelo")], input$plot_click)
  })
}


shinyApp(ui, server)

  