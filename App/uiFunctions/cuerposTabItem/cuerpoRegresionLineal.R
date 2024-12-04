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
    simulador4, br(), br(),
    a("Nota: para mayor información consulte el siguiente documento.", href = "https://dfranzani.github.io/Estadistica_II/regresi%C3%B3n-lineal.html#regresi%C3%B3n-lineal-simple")
  ),
  mainPanel(
    width = 10,
    fluidRow(
      column(width = 6, plotOutput("plot_rl", height = "500px"), br(), tableOutput("resumen_rl"), tableOutput("resumen_rl_metricas")),
      column(width = 6, plotOutput("plot_supuestos", height = "500px"), br(), tableOutput("resumen_supuestos"))
    )
  )
)