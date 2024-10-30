library(shiny)
library(shinydashboard)
# library(shinyjs)

# Encabezado general

encabezado = dashboardHeader(title = "Estadística")

# Panel lateral general: filtro de temas

panelLateral = dashboardSidebar(
  # useShinyjs(),
  collapsed = FALSE,
  sidebarMenu(
    menuItem(
      "Distribuciones muestrales", tabName = "DM", startExpanded = TRUE,
      menuSubItem("TCL", tabName = "DM1")
    )#,
    # menuItem(
    #   "Intervalos de confianza", tabName = "IC", startExpanded = TRUE,
    #   menuSubItem("Para una media", tabName = "IC1"),
    #   menuSubItem("Para dos medias", tabName = "IC2"),
    #   menuSubItem("Para varianzas", tabName = "IC3")
    # ),
    # menuItem(
    #   "Pruebas de Hipótesis", tabName = "PH", startExpanded = TRUE,
    #   menuSubItem("Para una media", tabName = "PH1"),
    #   menuSubItem("Para dos medias", tabName = "PH2"),
    #   menuSubItem("Para varianzas", tabName = "PH3")
    # ),
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

cuerpo_DM2 = fluidPage()

# Asignando los cuerpos por filtro
hoja_DM1 = tabItem(tabName = "DM1", fluidPage(cuerpo_DM1))
# hoja_DM2 = tabItem(tabName = "DM2", cuerpo_DM2)

cuerpo = dashboardBody(
  tabItems(hoja_DM1)
)

# Despliegue general
ui = dashboardPage(encabezado, panelLateral, cuerpo, skin = "black")