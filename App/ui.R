library(shiny)
library(shinydashboard)
library(Cairo)
options(shiny.usecairo = T)

# Encabezado general

encabezado = dashboardHeader(title = "Estadística")

# Panel lateral general: filtro de temas

panelLateral = dashboardSidebar(
  # useShinyjs(),
  collapsed = FALSE,
  sidebarMenu(
    menuItem(
      "Distribuciones muestrales", startExpanded = TRUE, tabName = "DM",
      menuSubItem("TCL", tabName = "DM1")
    ),
    menuItem("IC: Intervalos de confianza", tabName = "IC"),
    menuItem("Pruebas de Hipótesis", tabName = "PH")#,
    # menuItem(
    #   "Regresión Lineal", tabName = "RL", startExpanded = TRUE,
    #   menuSubItem("Simple", tabName = "RL1"),
    #   menuSubItem("Múltiple", tabName = "RL2")
    # )
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
    sliderInput(inputId = "mu0PH", label = "Media bajo Hipótesis nula", min = -20, max = 20, value = 0, step = 0.1),
    sliderInput(inputId = "sigma2PH", label = "Varianza poblacional", min = 1, max = 20, value = 6, step = 0.1),
    sliderInput(inputId = "mediaMuestraPH", label = "Media muestral", min = -20, max = 20, value = 2, step = 0.1),
    sliderInput(inputId = "nPH", label = "Tamaño muestral", min = 10, max = 200, value = 120, step = 10),
    sliderInput(inputId = "confianzaPH", label = "% Confianza", min = 10, max = 99, value = 95, step = 1),
    simulador3
  ),
  mainPanel(
    width = 9,
    fluidRow(
      column(width = 6, plotOutput("plot_ph")),
      column(width = 6, plotOutput("plot_ic_ph"))
    )
  )
)

# Asignando los cuerpos por filtro
hoja_DM = tabItem(tabName = "DM1", fluidPage(cuerpo_DM1))
hoja_IC = tabItem(tabName = "IC", fluidPage(cuerpo_IC))
hoja_PH = tabItem(tabName = "PH", fluidPage(cuerpo_PH))

cuerpo = dashboardBody(
  tabItems(hoja_DM, hoja_IC, hoja_PH)
)

# Despliegue general
ui = dashboardPage(encabezado, panelLateral, cuerpo, skin = "black")