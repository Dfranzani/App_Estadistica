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
    simulador2, br(), br(),
    a("Nota: para mayor información consulte el siguiente documento.", href = "https://dfranzani.github.io/Estadistica_II/intervalos-de-confianza.html")
  ),
  mainPanel(
    width = 9,
    fluidRow(
      column(width = 6, plotOutput("plot_ic", click = "plot_click", height = "800px")),
      column(width = 6, plotOutput("histogramaIC"), plotOutput("histograma_medias_IC"))
    )
  )
)