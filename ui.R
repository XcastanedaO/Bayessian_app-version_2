options(shiny.maxRequestSize = 10 * 1024^2)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Modelos conjugados"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("esce_datos", label = NULL,
                         choices = list("Simular datos" = "sim_datos" , "Ingresar datos" = "ing_datos"),
                         selected = "sim_datos",
                         inline = TRUE
      ),
      selectInput(inputId = "modelo_conj",
                        "Escoge modelo conjugado",
                         selected = "",
                         choices = c("","Normal","Poisson","Binomial")
      ),
      conditionalPanel(condition = "input.modelo_conj=='Normal'",
                       selectInput("esce_normal_med",
                                   "Media: escoge el escenario",
                                   selected = "",
                                   choices = c("","Conocida","Desconocida")
                       ),
                       
                       selectInput(inputId = "esce_normal_var",
                                   label = "Varianza: escoge el escenario",                            
                                   selected = "",
                                   choices = c("","Conocida","Desconocida")
                       )
      ),
      
      conditionalPanel(condition = "input.modelo_conj=='Normal' & input.esce_normal_med=='Desconocida' & input.esce_normal_var=='Desconocida'",
                       selectInput(inputId= "inde_aprioris",
                                   "Independencia de aprioris: escoge el escenario",
                                   selected = "",
                                   choices = c("","Aprioris dependientes: media condicionada","Aprioris independientes")
                       )
      ),
      
      uiOutput("panel_esc"),

      div(actionButton("boton_graf", "Generar Gráfico"), align = "center"),
      h5("Integrantes:"),
      tags$ul(
        tags$li(tags$a(href="mailto:yyocampon@unal.edu.co", "Yeison Yovany Ocampo Naranjo")),
        tags$li(tags$a(href="mailto:xcastaneda@unal.edu.co", "	Ximena Castaneda Ochoa")),
        tags$li(tags$a(href="mailto:yalcaraz@unal.edu.co", "Yojan Andres Alcaraz Perez"))
      ),
      h5("Profesores:"),
      tags$ul(
        tags$li(tags$a(href="mailto:iscramirezgu@unal.edu.co", "Isabel Cristina Ramirez Guevara")),
        tags$li(tags$a(href="mailto:cmlopera@unal.edu.co", "Carlos Mario Lopera Gomez"))
      ),
      h5("Correspondencia:"),
      tags$ul(
        tags$li(tags$a(href="mailto:iscramirezgu@unal.edu.co", "iscramirezgu@unal.edu.co"))
      ),
      img(src="Imagenes/Logo_unal.png", height = 120, width = 160,hspace="20"),
      img(src="Imagenes/Logo_esc_estadistica.png", height = 130, width = 130)
    ),
    
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel(title = "Gráfico",
                           fluidRow(
                             column(4,
                                uiOutput("parametros_datos")),
                             column(8,
                                uiOutput("parametros_Apriori")),
                             column(12,
                                plotlyOutput("distPlot")),
                             column(6,
                                conditionalPanel(
                                condition = "input.boton_graf > 0",
                                style = "display: none;",
                                withSpinner(plotlyOutput("distPlot2")))
                             ),
                             column(6,
                                conditionalPanel(
                                condition = "input.boton_graf > 0",
                                style = "display: none;",
                                withSpinner(plotlyOutput("distPlot3")))
                             )
                           )
                  ), 
                  tabPanel(title = "Teoría",
                           tags$iframe(style = " height: 400px; width: 100%;",
                                       src="Explicacion_modelos.pdf")
                  )
      )
    )
  )
))