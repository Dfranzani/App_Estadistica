library(shiny)
library(shinydashboard)
# library(shinydashboardPlus)
library(Cairo)
options(shiny.usecairo = T)
library(kableExtra)
library(shinyBS)

function(input, output, session) {
  
  # Configuración
  source("config/config.R", local = TRUE)
  
  # Header
  source("serverFunctions/header.R", local = TRUE)
  
  ### Distribuciones muestrales: TCL
  source("serverFunctions/TCL.R", local = TRUE)
  
  ### Intervalos de confianza
  source("serverFunctions/IC.R", local = TRUE)
  
  ### Pruebas de hipótesis
  source("serverFunctions/PH.R", local = TRUE)
  
  ### Regresión Lineal (Simple)
  source("serverFunctions/RegresionLineal.R", local = TRUE)
  
}



