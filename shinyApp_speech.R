
library(shiny)
library(shinythemes)
library(sparkline)
library(timevis)
library(DT)
library(shinycssloaders)
library(fontawesome)
library(xlsx)
library(quanteda)

shinyAppUI = fluidPage(
  # define theme ####
  theme = shinytheme("cerulean"),
  # use custom css #### 
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  
  titlePanel("Descarga de sesiones - Parlamento Uruguayo",
             windowTitle = "speech App"),
  
  textAreaInput("url", "Ingrese URL", "",width = '800px' ),
  hr(), 
  actionButton("boton", "Buscar",class="btn btn-primary"),
  downloadButton('descargar', 'Descargar datos (.xlsx)',class = "btn btn-primary"), 
  hr(), 
  hr(), 
    mainPanel(width = 10, 
              tabsetPanel(tabPanel(div("Tabla"),
                                   wellPanel(h3(""),
                                             h5(""),
                                             fluidRow(column(10, htmlOutput("resumen")
                                             ),))),  tabPanel(div("Nubes de palabras"),
                                                              wellPanel(h3(""),
                                                                        h5(""),
                                                                        fluidRow(column(10, imageOutput("nube")
                                                                        ),))))
              
              
              
              ))


shinyAppServer <- function(input, output) {
  
  d <- reactive({
    speech::speech_build(input$url) 
  })
  
  observeEvent(input$boton,{ output$resumen <- renderText({
    d() %>%
      as.data.frame()%>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width = F)
    
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste('Sesion-', as.data.frame(d())[1,3],'-',as.data.frame(d())[1,4],'.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(as.data.frame(d()), con)
    }
  )
  
  })
  
#})

  
  
  output$nube <- renderImage({
    salida <- tempfile(fileext='.png')
    png(salida, width=800, height=800)
    d()%>%
      as.data.frame()%>%
      quanteda::corpus(.,text_field = "speech") %>%
      quanteda::dfm(.,stem = FALSE,
                    tolower = TRUE,
                    remove = c(stopwords("spanish"),"señor", "señora","legislador","legisladora"), 
                    remove_punct = TRUE, 
                    remove_numbers = TRUE, 
                    verbose = FALSE)%>%
                  dfm_remove(min_nchar=3)  %>%
      textplot_wordcloud(., min.count = 3,max_words = 150,random.order = FALSE,
                         rot.per = .50, colors = RColorBrewer::brewer.pal(8,"Dark2"))
    dev.off()
    
    # Return a list
    list(src = salida,
         alt = "Texto")
  }, deleteFile = TRUE)
  
}

shinyApp(ui = shinyAppUI, server = shinyAppServer)

                                             