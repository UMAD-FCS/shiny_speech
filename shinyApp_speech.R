options(shiny.sanitize.errors = FALSE)
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(rhandsontable))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(png))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(bsplus))
suppressPackageStartupMessages(library(speech))
suppressPackageStartupMessages(library(puy))
library(DT)
suppressPackageStartupMessages(library(wordcloud2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(quanteda))
suppressPackageStartupMessages(library(quanteda.textstats))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(tidyverse))
library(htmlwidgets)
library(curl)
library(stringr)
library(lubridate)
library(purrr)
library(pdftools)
library(tibble)
library(utils)
library(kableExtra)
library(knitr)
library(shinyFiles)
library(tabulizer)
library(tabulizerjars)
library(magrittr)
library(utf8)
library(rJava)
library(tm)
library(htmltools)
library(stringi)


shinyAppUI = fluidPage(
  navbarPage(
    # define theme ####
    theme = shinytheme("flatly"),
    #useShinyjs(),
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
                        #h6("* Es posible ingresar más de una URL, separando con coma (,)"),
                        hr(),
                        div(checkboxInput(
                          inputId = "compila",
                          label = "Compilar", FALSE ,width = "13%") %>%
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
                                                                 fluidRow(column(12, DTOutput("resumen")
                                                                                 #%>% withSpinner(color="#2a3a4a",hide.ui = FALSE) 
                                                                 ),))),  tabPanel(div("Nubes de palabras"),
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
                                                                 fluidRow(column(12, DTOutput("resumen2")#%>% withSpinner(color="#2a3a4a",hide.ui = FALSE) 
                                                                 ),))),  tabPanel(div("Nubes de palabras"),
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




shinyAppServer <- shinyServer(function(input, output, session) {
  
  
  library(tabulizer)
  library(tabulizerjars)
  

  pdf <- reactiveValues(pdf_folder = NA,
                        pdfPath = "NA",
                        pdfUrl = "NA")
  
  tables <- reactiveValues(df=NA)
  safe_GET <- safely(GET)
  assign_extension <- function(response){
    if(is.null(response$result)){
      print("no response")
      # response$result$status_code
      "no response"
    } else{
      if(response$result$status_code==200){
        con_type <- response$result$headers$`content-type`
        if(grepl("pdf", con_type)){
          ext <- "pdf"
        } else if(grepl("zip", con_type)){
          ext <- "zip"
        } else if(grepl("html", con_type)){
          ext <- "html"
        } else {
          ext <- "other"
        }
        ext
      } else {
        print("bad response")
        response$result$status_code
        # stop()
      }
    }
  }

  
  observeEvent(input$url, {
    req(input$url)
    #showModal(waitingModal())
    url <- input$url
    pdf$pdf_folder <- "pdf_folder"
    
    
    x <- safe_GET(url)
    
    ext <- assign_extension(x)
    
    
    
    if(ext!='pdf') {
      print("bad extension")
      showNotification("URL incorrecta!", duration=3, type=c("warning"))
      removeModal()
      req(FALSE)
    }
    
    # if(!file.exists(pdf$pdf_folder)){
    suppressWarnings(dir.create("pdf_folder"))
    # }
    
    temp <- tempfile(fileext = ".pdf", tmpdir = "pdf_folder")
    
    download.file(url, temp, mode = "wb", quiet=TRUE)
    
    addResourcePath("pdf_folder", pdf$pdf_folder)
    
    pdf$pdfPath <- temp
    removeModal()
  })
  
  

  observeEvent(input$boton, {
    
    library(puy)
    a <- speech::speech_build(file = pdf$pdfPath, compiler = input$compila)
    a$speech=enc2native(a$speech)
    
    library(puy)
    aa <- puy::add_party(speech = a)
    aa=as.data.frame(aa)

    
    output$resumen <- renderDT({
      
      # validate(need(nrow(aa) > 0, 
      #               'No se pudo recuperar el documento'))
      
      #mutate(aa, date = format(date, "%Y-%m-%d"))%>%
      
      
      
      DT::datatable(aa %>%
                      dplyr::relocate(id, .after = last_col()),rownames = TRUE, options = list(pageLength = 50,
                                                                                        
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
      
      
      # aa %>%  
      #   dplyr::relocate(id, .after = last_col())%>%
      #   DT::datatable(rownames = TRUE, options = list(pageLength = 50,
      #                                                 
      #                                                 dom = 'Bfrtip',
      #                                                 scrollX = T,
      #                                                 columnDefs = list(list(
      #                                                   targets = 5,
      #                                                   render = JS(
      #                                                     "function(data, type, row, meta) {",
      #                                                     "return type === 'display' && data.length > 150 ?",
      #                                                     "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
      #                                                     "}")
      #                                                 ))), callback = JS('table.page(3).draw(false);'))
      
      
    })
    
    
    output$nube <- renderWordcloud2({
      
      # validate(need(nrow(aa) > 0, 
      #               'No se pudo recuperar el documento'))
      aa %>%
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
    
    output$descargar <- downloadHandler(
      filename = function() {
        paste('Sesion-', aa[1,3],'-',aa[1,4],'.xlsx', sep='')
      },
      content = function(con) {
        xlsx::write.xlsx(aa, con)
      }
    )
    
    
  })
  
  
  observeEvent(input$file_input, {
    if (is.null(input$file_input)){
      return(NULL)
    }
    
    
    pdf$pdfUrl <- input$file_input$datapath



  })
  
  
  observeEvent(input$boton2, {
    
    library(puy)
    library(speech)
    
    d <- speech::speech_build(pdf$pdfUrl,compiler = input$compila2)
    
      
      d <- puy::add_party(speech = d)
      
      dd = as.data.frame(d)
      

    
    output$resumen2 <- renderDT({
      
      
      DT::datatable(dd %>%
                      relocate(id, .after = last_col()),rownames = TRUE, options = list(pageLength = 50,
                                                                                        
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
    
    output$nube2 <- renderWordcloud2({
      
      validate(need(nrow(dd) > 0, 
                    'No se pudo recuperar el documento'))
      
      dd %>%
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
    
    output$descargar2 <- downloadHandler(
      filename = function() {
        paste('Sesion-', dd[1,3],'-',dd[1,4],'.xlsx', sep='')
      },
      content = function(con) {
        xlsx::write.xlsx(dd, con)
      }
    )
    
    
    
    
    
  })
  
  
  
})


shinyApp(ui = shinyAppUI, server = shinyAppServer)


