library(shiny)
library(shinydashboard)
# library(shinydashboardPlus1)
# library(Cairo)
# options(shiny.usecairo = T)
library(ragg)
options(shiny.useragg = T)
library(kableExtra)
library(shinyBS)

# Encabezado general

source("uiFunctions/header.R")

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

source("uiFunctions/cuerposTabItem/cuerpoTCL.R")
source("uiFunctions/cuerposTabItem/cuerpoIC.R")
source("uiFunctions/cuerposTabItem/cuerpoPH.R")
source("uiFunctions/cuerposTabItem/cuerpoRegresionLineal.R")

# hover_load_BS_package: TCL tabItem
hover_load_BS_package = bsPopover(
  id = "Histograma_promedios", trigger = "hover", placement = "left",
  content = paste("La función Aproximada corresponde a la función de densidad que generan los promedios muestrales simulados,",
  "la cual, se compara con la función de densidad Teórica."), title = ""
)

# Asignando los cuerpos por filtro
hoja_DM = tabItem(tabName = "DM1", fluidPage(cuerpo_DM1), hover_load_BS_package)
hoja_IC = tabItem(tabName = "IC", fluidPage(cuerpo_IC))
hoja_PH = tabItem(tabName = "PH", fluidPage(cuerpo_PH))
hoja_RL = tabItem(tabName = "RL", fluidPage(cuerpo_RL))

cuerpo = dashboardBody(
  tabItems(hoja_DM, hoja_IC, hoja_PH, hoja_RL)
)

# Despliegue general
ui = dashboardPage(header = encabezado, sidebar = panelLateral, body = cuerpo, skin = "black")


