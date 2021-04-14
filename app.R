library(shinypanels)
library(hgchmagic)
library(shinydisconnect)
library(shinybusy)
library(yaml)



m0 <- read_yaml("conf/meta.yaml")
ui <- panelsPage(styles = "/* estílos radiobotones */
                             #mediciones label input {
                               display: none;
                             }
                             #mediciones div.radio label span {
                               border: 2px solid #8790a0;
                               border-radius: 6px;
                               //color: #87ad5d;
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
                             .btn {
                               background: #cfbe8d !important;     
                                color: #6d7179;
                             }
                             #ss-connect-dialog a::before {
                               background: #d7ddd1 !important;     
                               color: #6d7179;
                               font-weight: 600 !important;
                             }",
                 disconnectMessage(text = "La sesión a finalizado. Para iniciar de nuevo refresque la página.",
                                   refresh = "REFRESCAR",
                                   background = "#ffffff",
                                   # colour = "#d8277c",
                                   size = 14,
                                   overlayColour = "#2a2e30",
                                   overlayOpacity = 0.85,
                                   refreshColour = "#ffffff",
                                   css = "box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important; font-size: 1.1rem !important; padding: 4.8em 3.5em !important;"),
                 # busy_start_up(loader = tags$img(src = "img/loading_gris.gif", width = 100),
                 #               mode = "auto",
                 #               color = "#435b69",
                 #               background = "#FFF"),
                 panel(title = "Mediciones",
                       width = 310,
                       color = "#804e49",
                       body = radioButtons("mediciones", "", c("m0", "m1", "m2") %>% setNames(names(m0))),
                       footer = downloadButton("descargar_data", "Descargar datos", class = "btn")),
                 panel(title = "Visualizaciones",
                       can_collapse = FALSE,
                       color = "#804e49",
                       # body = conditionalPanel("input.mediciones == 'm0'", output)))
                       body = highchartOutput("viz", height = "87vh")))



# halar de google
a0 <- read_csv("data/data.csv")

server <- function (input, output, session) {
  
  # visualización
  output$viz <- renderHighchart({
    if (input$mediciones == "m0") {
      dt <- a0[, c("entidad", "completitud")] %>% setNames(c("Entidad", "Completitud"))
      hgch_bar_CatNum(dt,
                      palette_colors = c("#87ad5d"),
                      sort = TRUE,
                      hor_title = "Completitud",
                      ver_title = "Entidad",
                      agg = "mean",
                      
                      # color_by = names(tb)[1],
                      # format_sample_num = "123.0",
                      label_wrap = 60,
                      orientation = "hor",
                      dataLabels_show = TRUE,
                      # dataLabels_size = ifelse(vz_tp == "treemap", 14, 11),
                      dataLabels_size = 13,
                      dataLabels_text_outline = FALSE,
                      style = list(suffix = " %"))
      # tooltip = "<b> {point.series}: </b><br/> point.y ")
    } else if (input$mediciones == "m1") {
      dt <- a0[, c("compromiso", "completitud")]
      hgch_bubbles_CatNum(dt,
                          palette_colors = c("#ff0000", "#ffcc00", "#ffff00", "#d6ff00", "#bbff00", "#0dff00"),
                          agg = "mean",
                          
                          color_by = "completitud",
                          # format_sample_num = "123.0",
                          style = list(suffix = " %"))
    } else {
      set.seed(1234)
      size <- 10
      library(lubridate)
      df <- tibble(start = today() + months(sample(10:20, size = size)),
                   end = start + months(sample(1:3, size = size, replace = TRUE)),
                   category = rep(1:3, length.out = size) - 1,
                   progress = round(runif(size), 1)) %>%
        # needs to convert to highchart timestamp format
        mutate(start = datetime_to_timestamp(start),
               end = datetime_to_timestamp(end))
      hchart(df, type = "xrange", 
             mapping = hcaes(x = start, x2 = end, y = category, partialFill = progress),
             dataLabels = list(enabled = TRUE)) %>% 
        hc_xAxis(type = "datetime") %>% 
        hc_yAxis(categories = c("Protyping", "Dev", "Testing"))
    }
  })
  
}



shinyApp(ui, server)