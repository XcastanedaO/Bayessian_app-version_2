library(shiny)
library(invgamma)
library(ggplot2)
library(plotly)
library(gridExtra)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(readxl)
library(readr)
library(stringr)

# Recurso donde se encuentre almacenado funciones auxiliares para graficar densidades
source("funciones_aux_graph.R")

shinyServer(function(input, output) {
  useShinyjs()
  
  #Separación de escenario
  observeEvent(input$esce_datos,{
    if(input$esce_datos== "sim_datos"){
      
      output$panel_esc<- renderUI({
        if(input$modelo_conj != "Binomial"){
          numericInput("input_numObservaciones",
                       "Ingrese la cantidad de observaciones",
                       value = 1)
        }
      })
      
      output$parametros_datos <- renderUI({
        if(input$modelo_conj == "Poisson"){
          fluidRow(
            column(12,
              h5("Información muestral"),
              numericInput("theta",HTML("Ingrese θ"),
                value = 10,
                min = 0)
              )
          )
        }else if(input$modelo_conj == "Binomial"){

        }else if(input$modelo_conj == "Normal"){
          if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris dependientes: media condicionada"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris independientes"){}
        }
      })
      
    }else if(input$esce_datos == "ing_datos"){
      
      output$panel_esc<- renderUI({
        fluidRow(
          column(12,
                 fileInput("datos", label = "Seleccione el archivo",
                           accept = c(".csv",".xlsx",".txt"),
                           buttonLabel = "Importar", placeholder = NULL),
                 selectInput("nom_var", "Variable", choices = c("No hay variables disponibles"))
          )
        )
      })
      
      # output$parametros_datos <- renderUI({
      #   if(input$modelo_conj == "Poisson"){}else if(input$modelo_conj == "Binomial"){}else if(input$modelo_conj == "Normal"){}
      # })
      
    }
  })
  
  #Entradas para distribución apriori
  output$parametros_Apriori <- renderUI({
    if(input$modelo_conj == "Normal"){
      if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
      }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
      }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris dependientes: media condicionada"){
      }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris independientes"){}
    }else if(input$modelo_conj == "Poisson"){
      fluidRow(
        column(6,
               h5("Parámetros de distribución apriori"),
               numericInput("Alpha_pois",HTML("Ingrese  &alpha;"),
                            value = 1,
                            min = 0),
               numericInput("Beta_pois",
                            HTML("Ingrese  &beta;"),
                            value = 2,
                            min=0)
        )
      )
    }else if(input$modelo_conj == "Binomial"){}
  })
  
  #Leer y guardar datos
  Base_datos <- eventReactive(input$datos,{
    req(input$datos)
    if(grepl(".csv",input$datos$name) | grepl(".txt",input$datos$name) | grepl(".xlsx",input$datos$name)){
      
      #Leer datos
      if(grepl(".xlsx",input$datos$name)){
        Base_datos <- read_excel(input$datos$datapath,col_names = TRUE)
      }else if(grepl(".csv",input$datos$name) | grepl(".txt",input$datos$name)){
        #Definir delimitador
        archivo <- readLines(input$datos$datapath,warn=FALSE)
        delimitador <- sort(table(stringr::str_extract(archivo, ",|;|\t")), decreasing = TRUE)[1]
        
        #Identificar el número de columnas
        num_columnas <- count.fields(input$datos$datapath, sep = names(delimitador))
        max_columnas <- max(num_columnas)
        tipo_columnas <- paste(rep("n",max_columnas), collapse = "")
        
        Base_datos <- read_delim(input$datos$datapath,
                                 delim = names(delimitador),
                                 col_names = TRUE,
                                 col_types = tipo_columnas)
      }

      #Verificar encabezado
      if (any(grepl("^[0-9]+$", names(Base_datos)))) {
        #El archivo no tiene encabezado
        registro_1 <- colnames(Base_datos) 
        names(Base_datos) <- paste0("Columna ", 1:ncol(Base_datos))
        Base_datos <- rbind(Base_datos,as.numeric(registro_1))
      }
      Base_datos
    }else{
      showModal(modalDialog(title = "Advertencia", "Formato inválido"))
    }
  })
  
  #Listar columnas disponibles cuando se haya leído un marco de datos
  observeEvent(Base_datos(),{
    choices <- c(names(Base_datos()))
    updateSelectInput(inputId = "nom_var", choices = choices)
  })
  
  #Guardar variable de interés cuando se haya seleccionado y oprimido el botón generador
  Variable <- eventReactive(input$boton_graf,{
    input$nom_var
  })
  
  #Graficar
  observeEvent(input$boton_graf,{
    if(input$esce_datos== "sim_datos"){

      output$distPlot<- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          pois_vero = fx_pois(nobs=input$input_numObservaciones, theta_m=input$theta, alpha_0=input$Alpha_pois,
                              beta_0=input$Beta_pois)
          ggplotly(pois_vero[[1]])
        }else if(input$modelo_conj == "Binomial"){
        }else if(input$modelo_conj == "Normal"){
          if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris dependientes: media condicionada"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris independientes"){}
        }
      })
      
      output$distPlot2 <- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          pois_ap = fx_pois(nobs=input$input_numObservaciones, theta_m=input$theta, alpha_0=input$Alpha_pois,
                            beta_0=input$Beta_pois)
          ggplotly(pois_ap[[2]])
        }else if(input$modelo_conj == "Binomial"){
        }else if(input$modelo_conj == "Normal"){
          if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris dependientes: media condicionada"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris independientes"){}
        }
      })
      
      output$distPlot3 <- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          pois_pos = fx_pois(nobs=input$input_numObservaciones, theta_m=input$theta, alpha_0=input$Alpha_pois,
                             beta_0=input$Beta_pois)
          ggplotly(pois_pos[[3]])
        }else if(input$modelo_conj == "Binomial"){
        }else if(input$modelo_conj == "Normal"){
          if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris dependientes: media condicionada"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris independientes"){}
        }
      })
      
    }else if(input$esce_datos == "ing_datos"){
      cant_datos = length(Base_datos()[[Variable()]])
      
      output$distPlot<- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          par_theta = mean(Base_datos()[[Variable()]])
          
          pois_vero = fx_pois(nobs=cant_datos,theta_m=par_theta,alpha_0=input$Alpha_pois,
                              beta_0=input$Beta_pois)
          ggplotly(pois_vero[[1]])
          
        }else if(input$modelo_conj == "Binomial"){
        }else if(input$modelo_conj == "Normal"){
          if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$independence_priors == "Aprioris dependientes: media condicionada"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$independence_priors == "Aprioris independientes"){}
        }
      })
      
      output$distPlot2 <- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          par_theta = mean(Base_datos()[[Variable()]])
          pois_ap = fx_pois(nobs=cant_datos, theta_m=par_theta, alpha_0=input$Alpha_pois,
                            beta_0=input$Beta_pois)
          ggplotly(pois_ap[[2]])
        }else if(input$modelo_conj == "Binomial"){
        }else if(input$modelo_conj == "Normal"){
          if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris dependientes: media condicionada"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris independientes"){}
        }
      })
      
      output$distPlot3 <- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          par_theta = mean(Base_datos()[[Variable()]])
          pois_pos = fx_pois(nobs=cant_datos, theta_m=par_theta, alpha_0=input$Alpha_pois,
                             beta_0=input$Beta_pois)
          ggplotly(pois_pos[[3]])
        }else if(input$modelo_conj == "Binomial"){
        }else if(input$modelo_conj == "Normal"){
          if(input$esce_normal_med == "Conocida" & input$esce_normal_var == "Desconocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Conocida"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris dependientes: media condicionada"){
          }else if(input$esce_normal_med == "Desconocida" & input$esce_normal_var == "Desconocida" & input$inde_aprioris == "Aprioris independientes"){}
        }
      })
    }
  })
})




