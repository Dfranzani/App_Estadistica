library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(Cairo)
options(shiny.usecairo = T)
library(kableExtra)
library(shinyBS)

# Encabezado general

encabezado = dashboardHeader(title = "Estadística", userOutput("user"))

# Panel lateral general: filtro de temas

panelLateral = dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(
    id = "sidebarID",
    menuItem("Teorema Central del Límite", tabName = "DM1"),
    menuItem("IC: Intervalos de confianza", tabName = "IC"),
    menuItem("Pruebas de Hipótesis", tabName = "PH"),
    menuItem("Regresión Lineal", tabName = "RL")
  )
)

# Paneles: cuerpo de cada panel del Panel lateral
# Un cuerpo se puede volver a dividir en otro tipo de Layout

simulador = actionButton(inputId = "go_DM1", label = "Simular")

cuerpo_DM1 = sidebarLayout(
  sidebarPanel(
    width = 3,
    selectInput(inputId = "distribucionDM1", label = "Distribución", choices = c("Normal", "Exponencial", "Binomial", "Poisson")),
    conditionalPanel(
      condition = "input.distribucionDM1 == 'Normal'",
      sliderInput(inputId = "mediaDM1_normal", label = "Media poblacional", min = 0, max = 20, value = 12, step = 0.2),
      sliderInput(inputId = "varianzaDM1_normal", label = "Varianza poblacional", min = 0, max = 20, value = 2, step = 0.1),
    ),
    conditionalPanel(
      condition = "input.distribucionDM1 == 'Binomial'",
      sliderInput(inputId = "nDM1_binomial", label = "Ensayos", min = 1, max = 100, value = 12, step = 1),
      sliderInput(inputId = "pDM1_binomial", label = "Probabilidad de éxito", min = 0, max = 1, value = 0.4, step = 0.05),
    ),
    conditionalPanel(
      condition = "input.distribucionDM1 == 'Exponencial'",
      sliderInput(inputId = "tasaDM1_exponencial", label = "Tasa", min = 0, max = 20, value = 5, step = 0.1)
    ),
    conditionalPanel(
      condition = "input.distribucionDM1 == 'Poisson'",
      sliderInput(inputId = "tasaDM1_poisson", label = "Tasa", min = 0, max = 20, value = 8, step = 0.1)
    ),
    sliderInput(inputId = "nDM1", label = "Tamaño muestral", min = 100, max = 1000, value = 200, step = 50),
    sliderInput(inputId = "simulacionesDM1", label = "Cantidad de simulaciones", min = 100, max = 7000, value = 1300, step = 100),
    simulador
  ),
  mainPanel(
    width = 9,
    plotOutput("Histograma_promedios"), plotOutput("fdp_fmp_simulaciones")
  )
)

simulador2 = actionButton(inputId = "go_IC", label = "Simular")

cuerpo_IC = sidebarLayout(
  sidebarPanel(
    width = 3,
    fluidRow(
      column(width = 4, radioButtons(inputId = "MediasIC", label = "Medias", choices = c("Una", "Dos"))),
      column(width = 8, radioButtons(inputId = "VarianzasIC", label = "Varianzas", choices = c("Conocidas", "Desconocidas")))
    ),
    conditionalPanel(
      condition = "input.VarianzasIC == 'Desconocidas' && input.MediasIC == 'Dos'",
      radioButtons(inputId = "VarianzasICMedias", label = "Varianzas desconocidas", choices = c("Iguales", "Distintas"))
    ),
    sliderInput(inputId = "ConfianzaIC", label = "% Confianza", min = 10, max = 99, value = 90, step = 1),
    radioButtons(inputId = "tipoIC", label = "Tipo de IC", choices = c("Bilateral", "Acotado por la derecha", "Acotado por la izquierda")),
    conditionalPanel(
      condition = "input.MediasIC == 'Una'",
      sliderInput(inputId = "muIC", label = "Media poblacional", min = -20, max = 20, value = 0, step = 0.1),
      sliderInput(inputId = "sigma2IC", label = "Varianza poblacional", min = 5, max = 30, value = 10, step = 0.1),
      sliderInput(inputId = "nIC", label = "Tamaño muestral", min = 10, max = 200, value = 120, step = 10)
    ),
    conditionalPanel(
      condition = "input.MediasIC == 'Dos'",
      fluidRow(
        column(width = 6, sliderInput(inputId = "muICX", label = "Media poblacional X", min = -10, max = 10, value = 0, step = 0.5)),
        column(width = 6, sliderInput(inputId = "muICY", label = "Media poblacional Y", min = -10, max = 10, value = 7, step = 0.5))
      ),
      fluidRow(
        column(width = 6, sliderInput(inputId = "varianzaUnaMediaX", label = "Varianza poblacional X", min = 10, max = 25, value = 12, step = 0.5)),
        column(width = 6, sliderInput(inputId = "varianzaUnaMediaY", label = "Varianza poblacional Y", min = 10, max = 25, value = 16, step = 0.5))
      ),
      fluidRow(
        column(width = 6, sliderInput(inputId = "nICX", label = "Tamaño muestral X", min = 50, max = 150, value = 120, step = 10)),
        column(width = 6, sliderInput(inputId = "nICY", label = "Tamaño muestral Y", min = 50, max = 150, value = 80, step = 10))
      ),
    ),
    sliderInput(inputId = "simulacionesIC", label = "Cantidad de simulaciones", min = 10, max = 100, value = 60, step = 1),
    simulador2
  ),
  mainPanel(
    width = 9,
    fluidRow(
      column(width = 6, plotOutput("plot_ic", click = "plot_click", height = "800px")),
      column(width = 6, plotOutput("histogramaIC"), plotOutput("histograma_medias_IC"))
    )
  )
)

simulador3 = actionButton(inputId = "go_PH", label = "Simular")

cuerpo_PH = sidebarLayout(
  sidebarPanel(
    width = 3,
    radioButtons(inputId = "mediasPH", label = "Medias", choices = c("Una")),#, "Dos")),
    radioButtons(inputId = "varianazasPH", label = "Varianzas", choices = c("Conocidas")),#, "Desconocidas")),
    radioButtons(inputId = "tipoPH", label = "Tipo de prueba", choices = c("Bilateral", "Unilateral derecha", "Unilateral izquierda")),
    sliderInput(inputId = "mu0PH", label = "Media bajo Hipótesis nula", min = -2, max = 2, value = 0, step = 0.02),
    sliderInput(inputId = "sigma2PH", label = "Varianza poblacional", min = 1, max = 50, value = 10, step = 0.5),
    sliderInput(inputId = "mediaMuestraPH", label = "Media muestral", min = -2, max = 2, value = 0.6, step = 0.02),
    sliderInput(inputId = "nPH", label = "Tamaño muestral", min = 10, max = 200, value = 120, step = 10),
    sliderInput(inputId = "confianzaPH", label = "% Confianza", min = 10, max = 99, value = 87, step = 1),
    simulador3
  ),
  mainPanel(
    width = 9,
    fluidRow(
      column(width = 12, plotOutput("plot_ph"), plotOutput("plot_ic_ph"))
    )#,
    # fluidRow(
    #   column(width = 6, withMathJax(includeMarkdown("Regresion+Lineal/Prueba.md")))
    # )
  )
)

simulador4 = actionButton(inputId = "go_RL", label = "Simular")
cuerpo_RL = sidebarLayout(
  sidebarPanel(
    width = 2,
    radioButtons(inputId = "tipoRL", label = "Tipo de regresión", choices = c("Simple")),
    # radioButtons(inputId = "tipoRL", label = "Tipo de regresión", choices = c("Simple", "Múltiple")),
    conditionalPanel(
      condition = "input.tipoRL == 'Simple'",
      sliderInput(inputId = "nPoblacionalRLS", label = "Tamaño poblacional", min = 600, max = 800, value = 700, step = 10),
      sliderInput(inputId = "nMuestralRLS", label = "Tamaño muestral",  min = 100, max = 550, value = 240, step = 10)
    ),
    checkboxGroupInput(inputId = "violacionSupuestos", label = "Violación de supuestos",
                       choices = c("Linealidad", "Normalidad", "Homocedasticidad", "Independencia")),
    simulador4
  ),
  mainPanel(
    width = 10,
    fluidRow(
      column(width = 6, plotOutput("plot_rl", height = "500px"), br(), tableOutput("resumen_rl"), tableOutput("resumen_rl_metricas")),
      column(width = 6, plotOutput("plot_supuestos", height = "500px"), br(), tableOutput("resumen_supuestos"))
    )
  )
)

hover_load_BS_package = bsPopover(id = "Histograma_promedios", trigger = "hover", placement = "left",
                                   content = "La función Aproximada corresponde a la función de densidad que generan los promedios muestrales simulados, la cual, se compara con la función de densidad Teórica.",
                                   title = "")

# Asignando los cuerpos por filtro
hoja_DM = tabItem(tabName = "DM1", fluidPage(cuerpo_DM1), hover_load_BS_package)
hoja_IC = tabItem(tabName = "IC", fluidPage(cuerpo_IC))
hoja_PH = tabItem(tabName = "PH", fluidPage(cuerpo_PH))
hoja_RL = tabItem(tabName = "RL", fluidPage(cuerpo_RL))

cuerpo = dashboardBody(
  tabItems(hoja_DM, hoja_IC, hoja_PH, hoja_RL)
)

# Despliegue general
ui = dashboardPage(header = encabezado, sidebar = panelLateral, body = cuerpo, skin = "black", title = "Hola")


