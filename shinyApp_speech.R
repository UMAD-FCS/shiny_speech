
library(shiny)
library(shinythemes)
library(sparkline)
library(timevis)
library(DT)
library(shinycssloaders)
library(xlsx)
library(quanteda)
library(kableExtra)
library(wordcloud2)
library(dplyr)
library(bslib)
library(bsplus)


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
  h6("* Es posible ingresar más de una URL, separando con coma (,)"),
  hr(),
  div(checkboxInput(
    inputId = "compila",
    label = "Compilar", FALSE,width = "13%") %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(title = "Compilar", content = "Se debe marcar la opción Compilar si se quiere agrupar todas las intervenciones de cada uno de los legisladores en un documento o conjunto de documentos.", placement = "bottom")),use_bs_popover()),
  br(),
  actionButton("boton", "Buscar",class="btn btn-primary"),
  downloadButton('descargar', 'Descargar datos (.xlsx)',class = "btn btn-primary"), 
  hr(), 
  hr(), 
  mainPanel(width = 12, 
            tabsetPanel(tabPanel(div("Tabla"),
                                 wellPanel(h3(""),
                                           h5(""),
                                           fluidRow(column(12, DTOutput("resumen")%>% withSpinner(color="#36a1dc") ),))),  tabPanel(div("Nubes de palabras"),
                                                            wellPanel(h3(""),
                                                                      h5(""),
                                                                      fluidRow(column(12, wordcloud2Output("nube",width = "100%")
                                                                      ),))))
            
            
            
  ))


shinyAppServer <- function(input, output) {
  
  

  
  
  d <- eventReactive(input$boton,{
    
    
    urls <- unlist(strsplit(input$url, ","))
    
    urls=c(paste0(urls, "_", seq_along(urls)))
    
    speech::speech_build(urls,compiler = input$compila) 
    
  })
  
  

    output$resumen <- renderDT({
    
      
      datatable(
        d(),
        rownames = TRUE,
        #extensions = 'Buttons',
        options = list(
          pageLength = 50,
          dom = 'Bfrtip'))
    
  })
    
    
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste('Sesion-', as.data.frame(d())[1,3],'-',as.data.frame(d())[1,4],'.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(as.data.frame(d()), con)
    }
  )
  
  output$nube <- renderWordcloud2({
    
      
      d()%>%
          as.data.frame()%>%
          quanteda::corpus(.,text_field = "speech") %>%
          quanteda::dfm(.,stem = FALSE,
                        tolower = TRUE,
                        remove = c(stopwords("spanish"),"señor", "señora","legislador","legisladora"),
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        verbose = FALSE)%>%
          dfm_remove(min_nchar=3)%>%
      dfm_trim(min_termfreq = 2)%>%
      quanteda.textstats::textstat_frequency()%>%
           as.data.frame()%>%select(feature ,frequency)%>%
      dplyr::rename(word=feature,freq=frequency)%>%
      wordcloud2(size=0.9,color= rev(RColorBrewer::brewer.pal(9,"Blues")),backgroundColor = "grey")
      
      
    
  })
  
  
  
  
  
  
  
  
  
  
  
  # output$nube <- renderImage({
  #   salida <- tempfile(fileext='.png')
  #   png(salida, width=500, height=500)
  #   d()%>%
  #     as.data.frame()%>%
  #     quanteda::corpus(.,text_field = "speech") %>%
  #     quanteda::dfm(.,stem = FALSE,
  #                   tolower = TRUE,
  #                   remove = c(stopwords("spanish"),"señor", "señora","legislador","legisladora"), 
  #                   remove_punct = TRUE, 
  #                   remove_numbers = TRUE, 
  #                   verbose = FALSE)%>%
  #     dfm_remove(min_nchar=3)  %>%
  #     textplot_wordcloud(., min.count = 3,max_words = 150,random.order = FALSE,
  #                        rot.per = .50, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  #   dev.off()
  #   
  #   # Return a list
  #   list(src = salida,
  #        alt = "Texto")
  # }, deleteFile = TRUE)
  
}

shinyApp(ui = shinyAppUI, server = shinyAppServer)

