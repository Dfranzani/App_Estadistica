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
    simulador, br(), br(),
    a("Nota: para mayor información consulte el siguiente documento.", href = "https://dfranzani.github.io/Estadistica_I/distribuciones-muestrales.html#teorema-del-l%C3%ADmite-central")
  ),
  mainPanel(
    width = 9,
    plotOutput("Histograma_promedios"), plotOutput("fdp_fmp_simulaciones")
  )
)