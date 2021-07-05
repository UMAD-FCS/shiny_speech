
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sparkline)
library(timevis)
library(DT)
library(rJava)
library(shinycssloaders)
library(xlsx)
library(xlsxjars)
library(quanteda)
library(kableExtra)
library(wordcloud2)
library(dplyr)
library(bslib)
library(bsplus)
library(puy)
library(speech)

library(tabulizer)
library(tabulizerjars)
library(lubridate)
library(magrittr)
library(pdftools)
library(purrr)
library(stringr)
library(tibble)
library(tm)
library(tidyr)
library(shinyjs)
library(Rcpp)
library(curl)
library(htmlwidgets)
library(jsonlite)
library(htmltools)



shinyAppUI = fluidPage(

  
  navbarPage(
  # define theme ####
  theme = shinytheme("flatly"),
  title = "SPEECH App",
  collapsible=TRUE,
  windowTitle = "SPEECH App",

  
  tabPanel(shiny::icon(name="home"), br(),
           
           mainPanel(width = 12,
                     br(),
                     
                     div(class = "list-group",
                         a(href = "#",
                           class = "list-group-item list-group-item-action active",
                           "")),
                     br(),
                     img(src = "logo.png",
                         style = "float: left;",
                         height = "120"),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     
                     includeMarkdown("proyecto.Rmd"),
                     br(),
                     br(),
                     
                     div(class = "list-group",
                         a(href = "#",
                           class = "list-group-item list-group-item-action active",
                           "")),
                     br(),
                     br(),
                     includeMarkdown("proyecto2.Rmd"),
                     br(),
                     br(),
                     
           )),
  
  tabPanel("Descarga sesiones",
             
  h2("Descarga de sesiones - Parlamento Uruguayo"),
  br(),
  br(),
  tabsetPanel(
    type = "pills",
  
  tabPanel("Descarga con URL",
           br(),
           br(),
           br(),
           
           
           div(textAreaInput("url", "Ingrese URL", "",width = '800px' )%>%
             shinyInput_label_embed(
               shiny_iconlink() %>%
                 bs_embed_popover(title = "Ubicación URL", html="true", 
                                  content = paste0("<a href='https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion'>  Ir a Diarios de sesiones del Parlamento </a>"," ","<img src='url.png' height='150' />"), placement = "bottom")),use_bs_popover())
             ,
  h6("* Es posible ingresar más de una URL, separando con coma (,)"),
  hr(),
  div(checkboxInput(
    inputId = "compila",
    label = "Compilar", FALSE,width = "13%") %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(title = "Compilar", content = "Se debe marcar la opción Compilar si se quiere agrupar todas las intervenciones de cada uno de los/las legisladores/as en un documento o conjunto de documentos.", placement = "bottom")),use_bs_popover()),
  br(),
  actionButton("boton", "Procesar",class="btn btn-primary"),
  downloadButton('descargar', 'Descargar datos (.xlsx)',class = "btn btn-primary"), 
  hr(), 
  hr(), 
  mainPanel(width = 12, 
            tabsetPanel(tabPanel(div("Tabla"),
                                 wellPanel(h3(""),
                                           h5(""),
                                           fluidRow(column(12, DTOutput("resumen")%>% withSpinner(color="#2a3a4a") ),))),  tabPanel(div("Nubes de palabras"),
                                                            wellPanel(h3(""),
                                                                      h5(""),
                                                                      fluidRow(column(12, wordcloud2Output("nube",width = "100%")
                                                                      ),))))
            
            
            
  )),
            
  tabPanel("Descarga con PDF",
           br(),
           br(),
           br(),
  
  fileInput('file_input', 'Subir archivo ( . pdf )', accept = c('.pdf')),
           
  h6("* Es posible subir un único archivo"),
  hr(),
  div(checkboxInput(
    inputId = "compila2",
    label = "Compilar", FALSE,width = "13%") %>%
      shinyInput_label_embed(
        shiny_iconlink() %>%
          bs_embed_popover(title = "Compilar", content = "Se debe marcar la opción Compilar si se quiere agrupar todas las intervenciones de cada uno de los/las legisladores/as en un documento o conjunto de documentos.", placement = "bottom")),use_bs_popover()),
  br(),
  actionButton("boton2", "Procesar",class="btn btn-primary"),
  downloadButton('descargar2', 'Descargar datos (.xlsx)',class = "btn btn-primary"), 
  hr(), 
  hr(), 
  mainPanel(width = 12, 
            tabsetPanel(tabPanel(div("Tabla"),
                                 wellPanel(h3(""),
                                           h5(""),
                                           fluidRow(column(12, DTOutput("resumen2")%>% withSpinner(color="#2a3a4a") ),))),  tabPanel(div("Nubes de palabras"),
                                                                                                                                    wellPanel(h3(""),
                                                                                                                                              h5(""),
                                                                                                                                              fluidRow(column(12, wordcloud2Output("nube2",width = "100%")
                                                                                                                                              ),))))
            
            
            
  )
  
  
  ))),
  tabPanel("Tutorial", br(),
           
           mainPanel(width = 12,
                     br(),
                     
                     div(class = "list-group",
                         a(href = "#",
                           class = "list-group-item list-group-item-action active",
                           "")),
                     br(),
                     br(),
                     includeMarkdown("tutorial.Rmd"),
                     br(),
                     br(),
                     
                     div(class = "list-group",
                         a(href = "#",
                           class = "list-group-item list-group-item-action active",
                           "")),
                     br(),
                     br(),
                  
                     
           ))
  
  ))


shinyAppServer <- function(input, output) {
  

  d <- eventReactive(input$boton,{


    urls <- unlist(strsplit(input$url, ","))

   # urls=c(paste0(urls, "_", seq_along(urls)))

    d = speech::speech_build(urls,compiler = input$compila)
    
    d <- puy::add_party(speech = d)

  })


  d2 <- eventReactive(input$boton2,{
    
    
    #urls <- unlist(strsplit(input$url, ","))
    
    # urls=c(paste0(urls, "_", seq_along(urls)))
    
    d2 = speech::speech_build(input$file_input$datapath,compiler = input$compila2)
    
    d2 <- puy::add_party(speech = d2)
    
  })

    output$resumen <- renderDT({ 
  DT::datatable(d()%>% relocate(id, .after = last_col()),rownames = TRUE, options = list(pageLength = 50,
                                                dom = 'Bfrtip',
                                                scrollX = T,
                                                columnDefs = list(list(
    targets = 5,
    render = JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 150 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
      "}")
  ))), callback = JS('table.page(3).draw(false);'))
    
  
  })  
    
    output$resumen2 <- renderDT({ 
      DT::datatable(d2()%>% relocate(id, .after = last_col()),rownames = TRUE, options = list(pageLength = 50,
                                                                                         dom = 'Bfrtip',
                                                                                         scrollX = T,
                                                                                         columnDefs = list(list(
                                                                                           targets = 5,
                                                                                           render = JS(
                                                                                             "function(data, type, row, meta) {",
                                                                                             "return type === 'display' && data.length > 150 ?",
                                                                                             "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
                                                                                             "}")
                                                                                         ))), callback = JS('table.page(3).draw(false);'))
      
      
    })  
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste('Sesion-', as.data.frame(d())[1,3],'-',as.data.frame(d())[1,4],'.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(as.data.frame(d()), con)
    }
  )
  
  output$descargar2 <- downloadHandler(
    filename = function() {
      paste('Sesion-', as.data.frame(d2())[1,3],'-',as.data.frame(d2())[1,4],'.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(as.data.frame(d2()), con)
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
  
  output$nube2 <- renderWordcloud2({
    
    
    d2()%>%
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

  
}

shinyApp(ui = shinyAppUI, server = shinyAppServer)

