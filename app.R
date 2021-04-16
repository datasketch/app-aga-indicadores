library(shinypanels)
library(hgchmagic)
library(dsmodules)
library(shinydisconnect)
library(shinycustomloader)
library(shinybusy)



ui <- panelsPage(styles = "/* estílos radiobotones */
                             #mediciones label input {
                               display: none;
                             }
                             #mediciones div.radio label span {
                               border: 2px solid #8790a0;
                               border-radius: 6px;
                               color: #737773;
                               cursor: pointer;
                               display: block;
                               font-size: 0.83rem; /* 82 estaba propongo 84 */
                               font-weight: 400;
                               margin: 10px 0;
                               padding: 15px;
                               width: 97%;
                             }
                             #mediciones label input:checked + span {
                               background-color: #87ad5d;
                               background-color: #d7ddd1;
                               //border-color: #d51317;
                               //color: #ffffff;
                             }
                             .btn,
                             .dropdown-action-trigger {
                               //background: #cfbe8d !important;     
                               background: #b5ac9d !important;     
                               background: #b5ac9dc9 !important;     
                               //color: #6d7179;
                               color: #494444;
                             }
                             .style_section {
                               color: #804e49;
                               color: #737773;
                             }
                             .load-container {
                               //left: 11%;
                               top: 4%;
                               //width: 79%;
                             }
                             #ss-connect-dialog a::before {
                               background: #d7ddd1 !important;     
                               color: #6d7179;
                               font-weight: 600 !important;
                             }",
                 disconnectMessage(text = "La sesión a finalizado. Para iniciar de nuevo refresque la página.",
                                   refresh = "REFRESCAR",
                                   background = "#ffffff",
                                   colour = "#d8277c",
                                   size = 14,
                                   overlayColour = "#2a2e30",
                                   overlayOpacity = 0.85,
                                   refreshColour = "#ffffff",
                                   css = "box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important; color: #000 !important; font-size: 1.1rem !important; padding: 4.8em 3.5em !important;"),
                 busy_start_up(loader = tags$img(src = "img/loading_gris.gif", width = 100),
                               mode = "auto",
                               color = "#435b69",
                               background = "#FFF"),
                 panel(title = "Mediciones",
                       width = 310,
                       # color = "#804e49",
                       color = "#b5ac9b",
                       body = radioButtons("mediciones", "", c("m0", "m1", "m2") %>% setNames(c("Entidades que participan en el cumplimiento de recomendaciones AGA y su completitud",
                                                                                                "Compromisos y su completitud",
                                                                                                "Cronograma de actividades por compromiso"))),
                       footer = downloadButton("descargar_data", "Descargar datos", class = "btn")),
                 panel(title = "Visualizaciones",
                       title_plugin = downloadImageUI("descargar_viz", 
                                                      class = "btn",
                                                      dropdownLabel = "Descargar visualización",
                                                      formats = c("html", "png", "jpeg", "pdf"), 
                                                      display = "dropdown", 
                                                      dropdownWidth = 239),
                       can_collapse = FALSE,
                       # color = "#804e49",
                       color = "#b5ac9b",
                       body = list(conditionalPanel("input.mediciones != 'm1'", 
                                                    div(style = "display: flex; width: 34rem;", 
                                                        uiOutput("select_text", style = "margin-right: 20px;"),
                                                        selectizeInput("select", "", ""))),
                                   uiOutput("viz_titulo"),
                                   withLoader(highchartOutput("viz", height = "80vh"), type = "image", loader = "img/loading_gris.gif"))))




# halar de google
source("global.R")
# b0 <- read_csv("data/data.csv")
a0 <- read_csv("data/Plantilla seguimiento compromisos IV Plan - Hoja 1.csv") %>% clean_data()

server <- function (input, output, session) {
  
  # selector
  observe({
    if (input$mediciones == "m0") updateSelectizeInput(session, "select", choices = c("Todas", unique(a0$`entidades responsables`)))
    if (input$mediciones == "m2") updateSelectizeInput(session, "select", choices = unique(a0$nombre_compromiso))
  })
  
  # texto selector
  output$select_text <- renderUI({
    div(class = "style_section",
        list(m0 = "Seleccione una entidad para ver sus compromisos y actividades",
             m2 = "Compromiso:")[[input$mediciones]])
  })
  
  # visualización
  viz <- reactive({
    if (input$mediciones == "m0") {
      dt <- a0
      # if (input$select != "Todas") dt <- dt[dt$`entidades responsables` %in% input$select, ]
      dt <- dt[, c("entidades responsables", "completitud", "expectativa")] %>% 
        setNames(c("Entidad", "Realidad", "Expectativa")) %>%
        pivot_longer(c(Realidad, Expectativa), "percentage_of", values_to = "Percentage")
      
      hgch_bar_CatCatNum(dt[, c(2, 1, 3)],
                         # palette_colors = c("#b5ac9b", "#87ad5d"),
                         # palette_colors = c("#87ad5d", "#b5ac9b"),
                         # palette_colors = c("#698f3f", "#b5ac9b"),
                         palette_colors = c("#c6d5c7", "#7a897b"),
                         sort = TRUE,
                         hor_title = "",
                         ver_title = "Entidad",
                         agg = "mean",
                         # order = c("Realidad", "Expectativa"),
                         # color_by = names(tb)[1],
                         format_sample_num = "123.",
                         label_wrap = 40,
                         orientation = "hor",
                         dataLabels_show = TRUE,
                         # dataLabels_size = 13,
                         dataLabels_text_outline = FALSE,
                         style = list(suffix = " %")) %>%
        hc_yAxis(min = 0, max = 100, title = "") %>%
        hc_tooltip(useHTML = TRUE,
                   formatter = JS("function () {return '<b>' + this.series.name + '<br/> Entidad: </b>' + this.x + '<br/><b>Completitud: </b>' + this.point.y + '%'}"),
                   style = list(fontSize = "13px"))
    } else if (input$mediciones == "m1") {
      dt <- a0[, c("nombre_compromiso", "completitud")] %>%
        setNames(c("Compromiso", "Completado"))
      
      hgch_bubbles_CatNum(dt,
                          # palette_colors = c("#ff0000", "#ffcc00", "#ffff00", "#d6ff00", "#bbff00", "#0dff00"),
                          palette_colors = c("#7a897b", "#c6d5c7"),
                          agg = "mean",
                          color_by = "Completado",
                          label_wrap = 70,
                          dataLabels_size = 13,
                          dataLabels_text_outline = FALSE,
                          style = list(suffix = " %")) %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE,
                                                       format = "{point.name}"))) %>%
        hc_tooltip(useHTML = TRUE, 
                   formatter = JS("function () {return '<b> Compromiso: </b>' + this.point.name + '<br/><b> Completitud: </b>' + this.point.value + '%';}"),
                   style = list(fontSize = "13px"))
    } else {
      # set.seed(1234)
      # size <- 10
      # library(lubridate)
      # df <- tibble(start = today() + months(sample(10:20, size = size)),
      #              end = start + months(sample(1:3, size = size, replace = TRUE)),
      #              category = rep(1:3, length.out = size) - 1,
      #              progress = round(runif(size), 1)) %>%
      #   # needs to convert to highchart timestamp format
      #   mutate(start = datetime_to_timestamp(start),
      #          end = datetime_to_timestamp(end))
      # hchart(df, type = "xrange", 
      #        mapping = hcaes(x = start, x2 = end, y = category, 
      #                        partialFill = progress),
      #        dataLabels = list(enabled = TRUE)) %>% 
      #   hc_xAxis(type = "datetime") %>% 
      #   hc_yAxis(categories = c("Protyping", "Dev", "Testing"))
      req(input$select)
      dt <- a0[a0$nombre_compromiso %in% input$select, c("fecha_inicio", "fecha_fin", "actividad", "completitud")] 
      dt <- dt[!duplicated(dt), ] 
      dt <- dt %>%
        mutate(fecha_inicio = datetime_to_timestamp(fecha_inicio),
               fecha_fin = datetime_to_timestamp(fecha_fin),
               y = 0:(nrow(dt) - 1),
               cm = completitud,
               completitud = round(completitud / 100, 1))
      hchart(dt,
             type = "xrange",
             mapping = hcaes(x = fecha_inicio, x2 = fecha_fin, y = y, partialFill = completitud),
             dataLabels = list(align = "left", color = "contrast", enabled = TRUE, style = list(textOutline = "none"))) %>%
        hc_tooltip(useHTML = TRUE, 
                   # dateTimeLabelsFormat = "%Y %B",
                   # headerFormat = "{point.x}",
                   pointFormat = paste0("<br/><b> Compromiso: </b>", "input$select", "<br/><b> Actividad: </b> {point.actividad} <br/><b> Completitud: </b> {point.cm}% <br/>"),
                   style = list(fontSize = "13px")) %>%
        hc_colors("#c6d5c7") %>% #d7ddd1
        hc_xAxis(title = "", type = "datetime") %>%
        hc_yAxis(title = "", categories = gsub("\\\n", "<br/>", stringr::str_wrap(dt$actividad, 50)))
    }
  })
  
  # rendering viz
  output$viz <- renderHighchart({
    viz()
  })
  
  # descargar datos
  output$descargar_data <- downloadHandler(filename = function () "Plantilla seguimiento compromisos IV Plan.csv",
                                           content = function (file) write_csv(a0, file))
  
  # descarga de gráficas
  downloadImageServer("descargar_viz", element = reactive(viz()), lib = "highcharter", formats = c("html", "png", "jpeg", "pdf"))
  
}



shinyApp(ui, server)