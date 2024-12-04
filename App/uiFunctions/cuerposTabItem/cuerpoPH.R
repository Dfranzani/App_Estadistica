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
    simulador3, br(), br(),
    a("Nota: para mayor información consulte el siguiente documento.", href = "https://dfranzani.github.io/Estadistica_II/PH.html")
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