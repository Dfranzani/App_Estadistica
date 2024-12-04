encabezado = dashboardHeader(title = "Estadística",
                             tags$li(class = "dropdown",
                                     tags$a(href = "https://github.com/Dfranzani/App_Estadistica/discussions",
                                            icon("comments"), "Comentarios y sugerencias", target = "_blank")),
                             shinydashboardPlus::userOutput("user"))