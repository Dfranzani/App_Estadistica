library(shiny)
library(shinydashboard)
# library(shinydashboardPlus)
library(Cairo)
options(shiny.usecairo = T)
library(kableExtra)
library(shinyBS)

function(input, output, session) {
  
  ### CUSTOM URL FOR MENUITEM IN SIDEBARMENU
  ### NOT WORK IN GITHUB PAGES!
  
  # observeEvent(getQueryString(session)$tab, {
  #   currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
  #   if(is.null(input$sidebarID) || !is.null(currentQueryString) && currentQueryString != input$sidebarID){
  #     freezeReactiveValue(input, "sidebarID")
  #     updateTabItems(session, "sidebarID", selected = currentQueryString)
  #   }
  # }, priority = 1)
  # 
  # observeEvent(input$sidebarID, {
  #   currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
  #   pushQueryString <- paste0("?tab=", input$sidebarID)
  #   if(is.null(currentQueryString) || currentQueryString != input$sidebarID){
  #     freezeReactiveValue(input, "sidebarID")
  #     updateQueryString(pushQueryString, mode = "push", session)
  #   }
  # }, priority = 0)
  
  ### END CUSTOM URL FOR MENUITEM IN SIDEBARMENU
  
  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = "Daniel Franzani",
      image = "https://raw.githubusercontent.com/Dfranzani/App_Estadistica/refs/heads/main/App/Logo/Profile.jpeg",
      title = "Ciencia de Datos",
      subtitle = "",
      fluidRow(
        shinydashboardPlus::dashboardUserItem(width = 6,shinydashboardPlus::socialButton(href = "https://github.com/Dfranzani", icon = icon("github-square"))),
        shinydashboardPlus::dashboardUserItem(width = 6, shinydashboardPlus::socialButton(href = "https://dfranzani.github.io/website/principal/home.html", icon = icon("blog")))
      )
    )
  })
  
  ### Distribuciones muestrales
  
  # observeEvent(input$distribucionDM1,{
  #   output$Histograma_promedios = renderPlot({
  #     plot(1, 1, col = "white", ylab = "", xlab = "", axes = F)
  #   })
  #   
  #   output$fdp_fmp_simulaciones = renderPlot({
  #     plot(1, 1, col = "white", ylab = "", xlab = "", axes = F)
  #   })
  # })
  
  histTCL = function(valores, media, desviacion, simulaciones){
    medias = unlist(lapply(X = valores, FUN = mean))
    factor = max(abs(media - c(min(medias), max(medias))))
    lim_inf = media - factor; lim_sup = media + factor
    grilla = seq(from = lim_inf, to = lim_sup, by = 0.001)
    teoricos = dnorm(x = grilla, mean = media, sd = desviacion)
    
    densidad_medias = density(medias)
    hist(x = medias, main = "Distribución de las medias", ylab = "Función de densidad",
         xlab = "Valores de la media en las simulaciones", freq = FALSE, las = 1, breaks = round(1 + log2(simulaciones)),
         xlim = c(lim_inf, lim_sup), ylim = c(0, max(densidad_medias$y, teoricos)*1.1))
    lines(densidad_medias , col = "darkred")
    lines(x = grilla, y = teoricos, col = "darkblue")
    legend(
      "topright", title = "Distribución", bty = "n",
      legend = c("Aproximada ", paste0("Teórica - TCL: N(", media, ", ", round(desviacion^2,5), ")")),
      lty = 1, col = c("darkred", "darkblue")
    )
  }
  
  densidad_masa = function(x, valores, tipo, graph = "p"){
    plot(x, valores, main = paste("Función de ", tipo, " de probabilidad de \n los datos muestrales simulados"), bty = "n",
         xlab = "Valores de X", ylab = "", las = 1, type = graph, pch = 16)
  }
  
  plot_simulaciones = function(valores, dominio){
    largo = length(valores[[1]])
    cantidad = 4
    datos = data.frame(
      "valores" = unlist(valores[1:cantidad]),
      "dominio" = rep(1:cantidad, rep(largo, cantidad))
    )
    boxplot(
      datos$valores ~ datos$dominio, frame = F,
      main = paste("Primeras ", cantidad, " simulaciones"),
      xlab = "Simulación", ylab = "Valores simulados", xaxt = "n", yaxt = "n",
      las = 1, pch = 16, ylim = c(min(datos$valores), max(datos$valores)*1.4)
    )
    axis(side = 1, at = 1:cantidad, labels = paste0("S", 1:cantidad))
    axis(side = 2, at = seq(from = min(datos$valores), to = max(datos$valores), length.out = 5),
         labels = round(seq(from = min(datos$valores), to = max(datos$valores), length.out = 5)), las = 1)
    legend(
      "top", 
      legend = paste0(rep("S", cantidad), 1:cantidad, rep(": ", cantidad),
                      unlist(lapply(X = valores[1: cantidad], FUN = function(x){return(round(mean(x), 4))}))),
      bty = "n", title = "Media de la simulación", ncol = 2)
  }

  observeEvent(list(input$go_DM1, input$distribucionDM1),{
    
    n = input$nDM1
    simulaciones = input$simulacionesDM1
    
    # set.seed(2024) # Bind cache
    
    if(input$distribucionDM1 == "Normal"){
      media = input$mediaDM1_normal
      sigma = sqrt(input$varianzaDM1_normal)
      desviacion = sigma/sqrt(n)
      valores = lapply(X = as.list(1:simulaciones), FUN = function(sim){
        return(rnorm(n = n, mean = media, sd = sigma))
      })
      
      output$Histograma_promedios = renderPlot({
        histTCL(valores, media, desviacion, simulaciones)
      })
      
      output$fdp_fmp_simulaciones = renderPlot({
        medias = unlist(lapply(X = valores, FUN = mean))
        factor = max(abs(media - c(min(medias), max(medias))))
        lim_inf = media - factor; lim_sup = media + factor
        grilla = seq(from = lim_inf, to = lim_sup, by = 0.001)
        teoricos = dnorm(x = grilla, mean = media, sd = desviacion)
        
        par(mfrow = c(1,2))
        plot_simulaciones(valores)
        densidad_masa(x = grilla, valores = teoricos, tipo = "densidad", graph = "l")
      })
      
    } else if(input$distribucionDM1 == "Exponencial"){
      tasa = input$tasaDM1_exponencial
      media = tasa
      desviacion = sqrt(tasa^2/n)
      
      valores = lapply(X = as.list(1:simulaciones), FUN = function(sim){
        return(rexp(n = n, rate = 1/tasa))
      })
      
      output$Histograma_promedios = renderPlot({
        histTCL(valores, media, desviacion, simulaciones)
      })
      
      output$fdp_fmp_simulaciones = renderPlot({
        teoricos = dexp(seq(from = 0, to = 30, by = 0.001), rate = 1/tasa)
        par(mfrow = c(1,2))
        plot_simulaciones(valores)
        densidad_masa(x = seq(from = 0, to = 30, by = 0.001), valores = teoricos, tipo = "densidad", graph = "l")
      })
      
    } else if(input$distribucionDM1 == "Binomial") {
      ensayos = input$nDM1_binomial
      p = input$pDM1_binomial
      media = ensayos*p
      desviacion = sqrt(ensayos*p*(1-p)/n)
      
      valores = lapply(X = as.list(1:simulaciones), FUN = function(sim){
        return(rbinom(n = n, size = ensayos, prob = p))
      })

      output$Histograma_promedios = renderPlot({
        histTCL(valores, media, desviacion, simulaciones)
      })
      
      output$fdp_fmp_simulaciones = renderPlot({
        teoricos = dbinom(x = 0:ensayos, size = ensayos, prob = p)
        par(mfrow = c(1,2))
        plot_simulaciones(valores)
        densidad_masa(x = 0:ensayos, valores = teoricos,, tipo = "masa")
      })
      
    } else if(input$distribucionDM1 == "Poisson"){
      tasa = input$tasaDM1_poisson
      media = tasa
      desviacion = sqrt(tasa/n)
      
      valores = lapply(X = as.list(1:simulaciones), FUN = function(sim){
        return(rpois(n = n, lambda = tasa))
      })
      
      output$Histograma_promedios = renderPlot({
        histTCL(valores, media, desviacion, simulaciones)
      })
      
      output$fdp_fmp_simulaciones = renderPlot({
        teoricos = dpois(x = 0:30, lambda = tasa)
        par(mfrow = c(1,2))
        plot_simulaciones(valores)
        densidad_masa(x = 0:30, valores = teoricos, tipo = "masa")
      })
    }
  })
  
  
  ### Intervalos de confianza
  
  hist_muestra_IC = function(x, y = NULL, nombre_media = nombre_media, control_lanzamiento = FALSE, colores){
    
    color = ifelse("red" %in% colores, "red", "grey")
    
    if(nombre_media == "Una") {
      titulo = ifelse(control_lanzamiento, "Distribución de los datos de \n la muestra del último IC",
                      paste("Distribución de los datos de la muestra \n del IC número", punto_seleccionado()))
      hist(x, main = titulo, xlab = "Valores de la muestra", ylab = "Frecuencia", las = 1, col = color)
    } else {
      titulo = ifelse(control_lanzamiento, "Distribuciones de los datos \n de las muestras del último IC",
                      paste("Distribuciones de los datos de las muestras \n del IC número", punto_seleccionado()))
      par(mfrow = c(2,1))
      h1 = hist(x)
      h2 = hist(y)
      par(mar = c(0,5,3,3))
      plot(h1, col = color, xlim = c(min(h1$breaks, h2$breaks), max(h1$breaks, h2$breaks)), ylim = c(0, max(h1$counts, h2$counts)),
           las = 1, ylab = "Frecuencias de X", xaxt = "n", main = titulo)
      par(mar = c(5,5,0,3))
      plot(h2, col = color, xlim = c(min(h1$breaks, h2$breaks), max(h1$breaks, h2$breaks)), ylim = c(max(h1$counts, h2$counts), 0),
           las = 1, main = "", ylab = "Frecuencias de Y", xlab = "Valores de la muestra")
    }
  }
  
  hist_medias_IC = function(x, nombre_media = nombre_media){
    if(nombre_media == "Una") {
      hist(x, main = "Distribución de las medias muestrales", xlab = "Valores de la muestra",
           ylab = "Frecuencia", las = 1)
    } else {
      hist(x, main = "Distribución de las diferencias \n de las medias muestrales (X-Y)", xlab = "Valores de la muestra",
           ylab = "Frecuencia", las = 1)
    }
  }
  
  simulaciones_muestras = function(nombre_media = nombre_media){
    muestras = lapply(X = as.list(1:input$simulacionesIC), FUN = function(sim){
      if(nombre_media == "Una") {
        data1 = rnorm(n = input$nIC, mean = input$muIC, sd = sqrt(input$sigma2IC))
      } else {
        data1 = rnorm(n = input$nICX, mean = input$muICX, sd = sqrt(input$varianzaUnaMediaX))
        data2 = rnorm(n = input$nICY, mean = input$muICY, sd = sqrt(input$varianzaUnaMediaY))
        data1 = list("data1" = data1, "data2" = data2)
      }
      return(data1)
    })
    return(muestras)
  }
  
  calculo_IC = function(data1, data2 = NULL, tipo, varianzas_conocidas, tipo_varianzas = FALSE, extremos,
                        confianza, varianza = NA, varianzaX = NA, varianzaY = NA, tamanos_muestrales = NA, media = media){
   
    if(varianzas_conocidas == FALSE){
      limites = t.test(x = data1, y = data2, conf.level = confianza, alternative = tipo, var.equal = tipo_varianzas)$conf.int[extremos]  
    } else {
      if (is.null(data2)) {
        if(tipo == "two.sided") {
          limites = mean(data1) + c(-1,1)*qnorm(1 - (1- confianza)/2)*sqrt(varianza/tamanos_muestrales[1])
        } else if(tipo == "less") {
          limites = mean(data1) + qnorm(confianza)*sqrt(varianza/tamanos_muestrales[1])
        } else {
          limites = mean(data1) - qnorm(confianza)*sqrt(varianza/tamanos_muestrales[1])
        }
      } else {
        if(tipo == "two.sided") {
          limites = c(mean(data1) - mean(data2) - qnorm(1 - (1- confianza)/2)*sqrt(varianzaX/tamanos_muestrales[1] + varianzaY/tamanos_muestrales[2]),
                      mean(data1) - mean(data2) + qnorm(1 - (1 - confianza)/2)*sqrt(varianzaX/tamanos_muestrales[1] + varianzaY/tamanos_muestrales[2]))
        } else if(tipo == "less") {
          limites = mean(data1) - mean(data2) + qnorm(confianza)*sqrt(varianzaX/tamanos_muestrales[1] + varianzaY/tamanos_muestrales[2])
        } else {
          limites = mean(data1) - mean(data2) - qnorm(confianza)*sqrt(varianzaX/tamanos_muestrales[1] + varianzaY/tamanos_muestrales[2])
        }
      }
    }
    
    if(tipo == "greater") {
      limites = c(limites, limites[1] + 0.5*sqrt(varianza))
      colores = c(ifelse(limites[1] > media, "red", "black"), "white")
    } else if(tipo == "less") {
      limites = c(limites[1] - 0.5*sqrt(varianza), limites)
      colores = c("white", ifelse(limites[2] < media, "red", "black"))
    } else {
      colores = rep(ifelse(limites[1] < media & limites[2] > media, "black", "red"), 2)
    }
    return(c(limites, colores))
  }
  
  grafico_IC = function(x, y, colores, limites_x, simulaciones, media, nombre_media, tipoIC){
    plot(
      x = x, y = 1:simulaciones, col = colores[,1], pch = 16, las = 1, bty = "n",
      ylim = c(1, simulaciones + 5), xlim = limites_x, xaxt = "n", yaxt = "n",
      xlab = "Valores del IC", ylab = "Número de simulación",
      main = ifelse(nombre_media == "Una",
                    "IC para la media proveniente \n de una distribución normal",
                    "IC para la diferencia de medias (X-Y) \n provenientes de distribuciones normales")
    )
    axis(side = 1, at = c(limites_x[1], media, limites_x[2]), labels = c(limites_x[1], media, limites_x[2]), xlim = limites_x)
    axis(side = 2, at = round(seq(from = 1, to = simulaciones, length.out = 10), 0), las = 1)
    points(x = y, y = 1:simulaciones, pch = 16, col = colores[,2])
    if(tipoIC == "less"){
      arrows(x1 = x, x0 = y, y1 = 1:simulaciones, y0 = 1:simulaciones, length = 0.07, col = colores[,2])
      rojos = round(sum(colores[,2] == "red")/length(colores[,2])*100, digits = 2)
      negros = 100 - rojos
    } else if(tipoIC == "greater") {
      arrows(x0 = x, x1 = y, y0 = 1:simulaciones, y1 = 1:simulaciones, length = 0.07, col = colores[,1])
      rojos = round(sum(colores[,1] == "red")/length(colores[,2])*100, digits = 2)
      negros = 100 - rojos
    } else {
      segments(x0 = x, x1 = y, y0 = 1:simulaciones, y1 = 1:simulaciones, col = colores[,1])
      rojos = round(sum(colores[,1] == "red")/length(colores[,1])*100, digits = 2)
      negros = 100 - rojos
    }
    rojos = paste(rojos, "%")
    negros = paste(negros, "%")
    # abline(v = media, col = "black", lty = 2)
    segments(x0 = media, y0 = 0, x1 = media, y1 = simulaciones+1, lty = 2)
    legend("topleft", legend = c(negros, rojos), lty = 1, col = c("black", "red"), title = "Proporción de IC", bty = "n")
  }
  
  IC = function(tipo_varianzas, tipoIC, extremos, confianza, varianza, varianzaX = NA, varianzaY = NA,
                tamanos_muestrales, media = media, nombre_media = nombre_media){
    muestras_simuladas = simulaciones_muestras(nombre_media = nombre_media)
    ics = lapply(X = muestras_simuladas, FUN = function(muestra){
      if(input$VarianzasIC != "Conocidas") {
        if(input$MediasIC == "Una") {
          ic = calculo_IC(data1 = muestra, tipo = tipoIC, varianzas_conocidas = FALSE, varianza = varianza,
                          extremos = extremos, confianza = confianza, media = media)
        } else {
          ic = calculo_IC(data1 = muestra$data1, data2 = muestra$data2, tipo = tipoIC, varianzas_conocidas = FALSE,
                          tipo_varianzas = tipo_varianzas, extremos = extremos, confianza = confianza, media = media,
                          varianza = varianza,)
        }
      } else {
        if(input$MediasIC == "Una") {
          ic = calculo_IC(data1 = muestra, tipo = tipoIC, varianzas_conocidas = TRUE, tipo_varianzas = tipo_varianzas,
                          extremos = extremos, confianza = confianza, varianza = varianza, tamanos_muestrales = tamanos_muestrales,
                          media = media)
        } else {
          ic = calculo_IC(data1 = muestra$data1, data2 = muestra$data2, tipo = tipoIC, varianzas_conocidas = TRUE,
                          tipo_varianzas = tipo_varianzas, extremos = extremos, confianza = confianza, varianza = varianza,
                          varianzaX = varianzaX, varianzaY = varianzaY, tamanos_muestrales = tamanos_muestrales, media = media)
        }
      }
      return(ic)
    })
    ics = as.data.frame(matrix(unlist(ics), ncol = 4, byrow = TRUE))
    ics[,1] = as.numeric(ics[,1])
    ics[,2] = as.numeric(ics[,2])
    return(list(ics, muestras_simuladas))
  }
  
  muestras = NULL
  ic = NULL
  nombre_media_global = NULL
  simulaciones_global = NULL
  
  observeEvent(list(input$go_IC),{
    confianza = input$ConfianzaIC/100
    simulaciones = input$simulacionesIC
    simulaciones_global <<- simulaciones
    media = ifelse(input$MediasIC == "Una", input$muIC, input$muICX - input$muICY)
    tipoIC = switch(input$tipoIC, "Bilateral" = "two.sided", "Acotado por la derecha" = "less", "Acotado por la izquierda" = "greater")
    extremos = switch(tipoIC, "two.sided" = 1:2, "less" = 2, "greater" = 1)
    varianza = ifelse(input$MediasIC == "Una", input$sigma2IC, (input$varianzaUnaMediaX + input$varianzaUnaMediaY)/2)
    tipo_varianzas = switch(input$VarianzasICMedias, "Iguales" = TRUE, "Distintas" = FALSE)
    nombre_media = ifelse(input$MediasIC == "Una", "Una", "Dos")
    nombre_media_global <<- nombre_media
    varianzaX = input$varianzaUnaMediaX
    varianzaY = input$varianzaUnaMediaY
    tamanos_muestrales = c(input$nIC, input$nICX, input$nICY)
    
    ic_y_muestras = IC(tipo_varianzas = tipo_varianzas, tipoIC = tipoIC, extremos = extremos, confianza = confianza,
                       varianza = varianza, varianzaX = varianzaX, varianzaY = varianzaY,
                       tamanos_muestrales = tamanos_muestrales, media = media, nombre_media = nombre_media)
    ic <<- ic_y_muestras[[1]]
    muestras <<- ic_y_muestras[[2]]
    
    dif_maxima = max(abs(c(media - c(ic[,1], ic[,2]))))
    maximo = round(media + 1.5*dif_maxima, digits = 4)
    minimo = round(media - 1.5*dif_maxima, digits = 4)
    
    output$plot_ic = renderPlot({
      grafico_IC(x = ic[,1], y = ic[,2], colores = ic[,3:4], limites_x = c(minimo, maximo),
                 simulaciones = simulaciones, media = media, nombre_media = nombre_media, tipoIC = tipoIC)
    }, height = 800)
    
    punto_seleccionado(NULL)
    output$histogramaIC = renderPlot({
      if(nombre_media == "Una"){
        hist_muestra_IC(x = muestras[[simulaciones]], nombre_media = nombre_media,
                        control_lanzamiento = TRUE, colores = ic[simulaciones,3:4])
      } else {
        hist_muestra_IC(x = muestras[[simulaciones]]$data1, y = muestras[[simulaciones]]$data2,
                        nombre_media = nombre_media, control_lanzamiento = TRUE, colores = ic[simulaciones,3:4])
      }
    })
    
    output$histograma_medias_IC = renderPlot({
      if(nombre_media == "Una"){
        hist_medias_IC(x = unlist(lapply(X = muestras, FUN = mean)), nombre_media = nombre_media)
      } else {
        hist_medias_IC(
          x = unlist(lapply(X = muestras, FUN = function(muestra){
            return(mean(muestra$data1) - mean(muestra$data2))
          })),
          nombre_media = nombre_media)
      }
    })
  })
  
  punto_seleccionado = reactiveVal(NULL)
  
  observeEvent(input$plot_click, {
    click_y = input$plot_click$y
    punto_seleccionado(round(click_y))
    if(punto_seleccionado() >= 1 & punto_seleccionado() <= simulaciones_global){
      output$histogramaIC = renderPlot({
        if(nombre_media_global == "Una"){
          hist_muestra_IC(x = muestras[[punto_seleccionado()]], nombre_media = nombre_media_global,
                          control_lanzamiento = FALSE, colores = ic[punto_seleccionado(), 3:4])
        } else {
          hist_muestra_IC(x = muestras[[punto_seleccionado()]]$data1, y = muestras[[punto_seleccionado()]]$data2,
                          nombre_media = nombre_media_global, control_lanzamiento = FALSE, colores = ic[punto_seleccionado(), 3:4])
        }
      })
    } else {
      output$histogramaIC = renderPlot({
        if(nombre_media_global == "Una"){
          hist_muestra_IC(x = muestras[[simulaciones_global]], nombre_media = nombre_media_global,
                          control_lanzamiento = TRUE, colores = ic[simulaciones_global, 3:4])
        } else {
          hist_muestra_IC(x = muestras[[simulaciones_global]]$data1, y = muestras[[simulaciones_global]]$data2,
                           nombre_media = nombre_media_global, control_lanzamiento = TRUE, colores = ic[simulaciones_global, 3:4])
        }
      })
    }
  })
  
 
  addPopover(session, id = "plot_ic", trigger = "hover", placement = "left",
             content = "Haga click en un IC (o cerca) para ver la distribución de los datos muestrales asociados.",
             title = "")
  
  ### Pruebas de hipótesis
  
  grafico_estadisticoPrueba = function(ic, mediaMuestra, varianza, mu0, tipoPH, nMuestra, est_prueba, valor_critico, confianza){
    
    max = max(abs(c(0 - c(est_prueba, valor_critico, 4))))
    limites = c(0 - max * 1.2, 0 + max * 1.2)
    
    # print(c(limites, est_prueba, valor_critico, max))
    
    valores_x = seq(from = limites[1], to = limites[2], by = 0.01)
    valores_x = c(valores_x, est_prueba, valor_critico, -est_prueba, -valor_critico)
    valores_x = valores_x[order(valores_x)]
    
    plot(x = valores_x, y = dnorm(x = valores_x),
         main = "Distribución del estadístico de prueba", xlab = "Valores de Z", ylab = "Densidad",
         las = 1, xaxt = "n", type = "l", bty = "n", xlim = limites, ylim = c(0, 0.5))
    axis(side = 1, at = c(limites[1], 0, limites[2]),
         labels = round(c(limites[1], 0, limites[2]), 2), cex.axis = 1, las = 1)
    
    if(tipoPH == "two.sided") {
      axis(side = 1, at = c(-abs(est_prueba), abs(est_prueba)), labels = round(c(-abs(est_prueba), abs(est_prueba)), 2),
           col.axis = "red", cex.axis = 1, las = 1)
      axis(side = 1, at = c(-abs(valor_critico), abs(valor_critico)), labels = round(c(-abs(valor_critico), abs(valor_critico)), 2),
           col.axis = "blue", cex.axis = 1, las = 1)
      polygon(x = c(-abs(valor_critico), valores_x[valores_x <= -abs(valor_critico)]),
              y = c(min(dnorm(x = valores_x[valores_x <= -abs(valor_critico)])), dnorm(x = valores_x[valores_x <= -abs(valor_critico)])),
              lty = 2, density = 5, col = "blue")
      polygon(x = c(-abs(est_prueba), valores_x[valores_x <= -abs(est_prueba)]),
              y = c(min(dnorm(x = valores_x[valores_x <= -abs(est_prueba)])), dnorm(x = valores_x[valores_x <= -abs(est_prueba)])),
              lty = 2, density = 5, col = "red", angle = 135)
      polygon(x = c(abs(valor_critico), valores_x[valores_x >= abs(valor_critico)]),
              y = c(min(dnorm(x = valores_x[valores_x >= abs(valor_critico)])), dnorm(x = valores_x[valores_x >= abs(valor_critico)])),
              lty = 2, density = 5, col = "blue")
      polygon(x = c(abs(est_prueba), valores_x[valores_x >= abs(est_prueba)]),
              y = c(min(dnorm(x = valores_x[valores_x >= abs(est_prueba)])), dnorm(x = valores_x[valores_x >= abs(est_prueba)])),
              lty = 2, density = 5, col = "red", angle = 135)
      valor_p = 2 - 2*pnorm(abs(est_prueba))
    } else if(tipoPH == "less") {
      axis(side = 1, at = c(est_prueba), labels = round(c(est_prueba), 2),
           col.axis = "red", cex.axis = 1, las = 1)
      axis(side = 1, at = c(valor_critico), labels = round(c(valor_critico), 2),
           col.axis = "blue", cex.axis = 1, las = 1)
      polygon(x = c(valor_critico, valores_x[valores_x <= valor_critico]),
              y = c(min(dnorm(x = valores_x[valores_x <= valor_critico])), dnorm(x = valores_x[valores_x <= valor_critico])),
              lty = 2, density = 5, col = "blue")
      polygon(x = c(est_prueba, valores_x[valores_x <= est_prueba]),
              y = c(min(dnorm(x = valores_x[valores_x <= est_prueba])), dnorm(x = valores_x[valores_x <= est_prueba])),
              lty = 2, density = 5, col = "red", angle = 135)
      valor_p = pnorm(est_prueba)
    } else {
      axis(side = 1, at = c(est_prueba), labels = round(c(est_prueba), 2),
           col.axis = "red", cex.axis = 1, las = 1)
      axis(side = 1, at = c(valor_critico), labels = round(c(valor_critico), 2),
           col.axis = "blue", cex.axis = 1, las = 1)
      polygon(x = c(valor_critico, valores_x[valores_x >= valor_critico]),
              y = c(min(dnorm(x = valores_x[valores_x >= valor_critico])), dnorm(x = valores_x[valores_x >= valor_critico])),
              lty = 2, density = 5, col = "blue")
      polygon(x = c(est_prueba, valores_x[valores_x >= est_prueba]),
              y = c(min(dnorm(x = valores_x[valores_x >= est_prueba])), dnorm(x = valores_x[valores_x >= est_prueba])),
              lty = 2, density = 5, col = "red", angle = 135)
      valor_p = 1 - pnorm(est_prueba)
    }
    
    valor_p2 = ifelse(valor_p < 0.0001, "<1e-4", round(valor_p, digits = 4))
    significancia = format((1 - confianza))
    legend("topright", legend = paste(c("Valor-p:", "Significancia:"), c(valor_p2, significancia)), bty = "n",
           lty = 2, col = c("red", "blue"))
    legend("topleft", legend = ifelse(valor_p  <= 1 - confianza, "Se rechaza", "No se rechaza"), bty = "n",
           title = "Estado:", text.font = 4)
  }
  
  grafico_IC_PH = function(ic, mediaMuestra, varianza, mu0, tipoPH, nMuestra, est_prueba, valor_critico, confianza){
    
    max = max(abs(c(0 - c(est_prueba, valor_critico, 4))))
    limites = c(0 - max * 1.2, 0 + max * 1.2)
    
    valores_x = seq(from = limites[1], to = limites[2], by = 0.01)
    valores_x = c(valores_x, est_prueba, valor_critico, -est_prueba, -valor_critico)
    valores_x = valores_x[order(valores_x)]
    valores_y = dnorm(x = valores_x)
    
    transformacion = function(x.barra){
      return((x.barra - mu0)/sqrt(varianza/nMuestra))
    }
    
    transformacion_inversa = function(Z0){
      return(mu0 + Z0*sqrt(varianza/nMuestra))
    }
    
    plot(x = valores_x, y = valores_y,
         main = "Distribución de la media muestral \n e intervalo de confianza", xlab = "Valores del promedio", ylab = "Densidad",
         las = 1, xaxt = "n", type = "l", bty = "n", xlim = limites, ylim = c(0, 0.5))
    axis(side = 1, at = c(limites[1], 0, limites[2]),
         labels = round(transformacion_inversa(c(limites[1], 0, limites[2])), 4), cex.axis = 1, las = 1)
    segments(x0 = 0, x1 = 0, y0 = 0, y1 = max(valores_y), col = "black", lty = 2)
    
    ic = as.numeric(ic[1:2])
    ic2 = transformacion(as.numeric(ic[1:2]))
    max_y = max(valores_y)*0.5
    
    if(tipoPH == "two.sided") {
      segments(x0 = ic2[1], x1 = ic2[2], y0 = max_y, y1 = max_y, col = "red", lty = 2)
      legend("topleft", legend = paste0("(", round(ic[1], 4), ", ", round(ic[2], 4), ")"),
             bty = "n", title = "IC", lty = 2, col = "red")
    } else if(tipoPH == "less") {
      arrows(x1 = limites[1], x0 = ic2[2], y0 = max_y, y1 = max_y, length = 0.1, col = "red", lty = 2)
      legend("topleft", legend = paste0("(-Inf, ", round(ic[2], 4), ")"),
             bty = "n", title = "IC", lty = 2, col = "red")
    } else {
      arrows(x0 = ic2[1], x1 = limites[2], y0 = max_y, y1 = max_y, length = 0.1, col = "red", lty = 2)
      legend("topleft", legend = paste0("(", round(ic[1], 4), ", Inf)"),
             bty = "n", title = "IC", lty = 2, col = "red")
    }
    legend("topright", legend = mu0, bty = "n", title = "mu_0", lty = 2)
  }
  
  valor_critico = function(confianza, tipo, mu0, varianza, nMuestra, mediaMuestra){
    if(tipo == "two.sided") {
      valor_critico = qnorm(1 - (1 - confianza)/2)
    } else if(tipo == "less") {
      valor_critico = qnorm(1 - confianza)
    } else {
      valor_critico = qnorm(confianza)
    }
    estadistico_prueba = (mediaMuestra - mu0)/(sqrt(varianza/nMuestra))
    return(c(estadistico_prueba, valor_critico))
  }
  
  observeEvent(list(input$go_PH), {
    confianza = input$confianzaPH/100
    mu0 = input$mu0PH
    mediaMuestra = input$mediaMuestraPH
    tipoPH = switch(input$tipoPH, "Bilateral" = "two.sided", "Unilateral derecha" = "greater", "Unilateral izquierda" = "less")
    extremos = switch(tipoPH, "two.sided" = 1:2, "less" = 2, "greater" = 1)
    varianza = input$sigma2PH
    nombre_media = "Una"
    nMuestra = input$nPH
    
    # Los argumentos data debe ser el promedio n veces para que calce con el cálculo del IC de la función "calculo_IC"
    ic = calculo_IC(data1 = rep(mediaMuestra, nMuestra), tipo = tipoPH, varianzas_conocidas = TRUE, varianza = varianza,
                    extremos = extremos, confianza = confianza, media = mediaMuestra, tamanos_muestrales = nMuestra)
    
    valor_critico = valor_critico(confianza = confianza, tipo = tipoPH, mu0 = mu0, varianza = varianza,
    nMuestra = nMuestra, mediaMuestra = mediaMuestra)
      
    output$plot_ph = renderPlot({
      grafico_estadisticoPrueba(ic = ic, media = mediaMuestra, varianza = varianza, mu0 = mu0, tipoPH = tipoPH, nMuestra = nMuestra,
                    est_prueba = valor_critico[1], valor_critico = valor_critico[2], confianza = confianza)
    }, height = 400)
    
    output$plot_ic_ph = renderPlot({
      grafico_IC_PH(ic = ic, media = mediaMuestra, varianza = varianza, mu0 = mu0, tipoPH = tipoPH, nMuestra = nMuestra,
                    est_prueba = valor_critico[1], valor_critico = valor_critico[2], confianza = confianza)
    }, height = 400)
    
  })
  
  addPopover(session, id = "plot_ic_ph", trigger = "hover", placement = "left",
             content = "Se rechaza la hipótesis nula cuando la línea roja punteada (IC) intersecta la línea vertical negra punteada (media bajo hipótesis nula).",
             title = "")
  
  ### Regresión Lineal (Simple)
  
  regresion = function(x, y){
    modelo = lm(y ~ x)
    return(modelo)
  }
  
  grafico_RL = function(x, y, modelo){
    betas = modelo$coefficients
    confidence_values = predict.lm(modelo, interval = "confidence")
    prediction_values = predict.lm(modelo, newdata = data.frame("x" = x, "y" = y), interval = "prediction")
    y_confidence = confidence_values[order(confidence_values[,1]), 2:3]
    y_prediction = prediction_values[order(prediction_values[,1]), 2:3]
    plot(x = x, y = y, las = 1, bty = "n",
         main = "Regresión Lineal Simple", xlab = "Valores de X", ylab = "Valores de Y",
         ylim = c(min(c(y, y_confidence, y_prediction)), max(c(y, y_confidence, y_prediction))))
    segments(x0 = min(x), x1 = max(x), y0 = betas[1] + betas[2]*min(x), y1 = betas[1] + betas[2]*max(x), col = "red", lty = 1)
    lines(x = x[order(x)], y = y_confidence[,1], col = "blue", lty = 2)
    lines(x = x[order(x)], y = y_confidence[,2], col = "blue", lty = 2)
    lines(x = x[order(x)], y = y_prediction[,1], col = "darkgreen" ,lty = 2)
    lines(x = x[order(x)], y = y_prediction[,2], col = "darkgreen", lty = 2)
    legend("topleft", legend = c("Recta de regresión ajustada", "IC del 95% para la media", "IC del 95% para la predicción"),
           lty = c(1, 2, 2), col = c("red", "blue", "darkgreen"), bty = "n")
  }
  
  grafico_supuestos = function(x, y, modelo){
    par(mfrow = c(2,2), bty = "n", las = 1)
    plot(x = modelo$fitted.values, y = residuals(modelo),
         main = "Linealidad", xlab = "Valores ajustados", ylab = "Residuos")
    acf(residuals(modelo),
        main = "Independenica \n (Función de Autocorrelación: ACF)", xlab = "Lag", ylab = "ACF")
    plot(x = modelo$fitted.values, y = sqrt(abs(rstandard(modelo))),
         main = "Homocedasticidad", xlab = "Valores ajustados", ylab = "Raíz de los residuos estandarizados")
    qqnorm(modelo$residuals, main = "Normalidad \n (Cuantil - Cuantil)", xlab = "Cuantiles teóricos", ylab = "Cuantiles observados")
    qqline(modelo$residuals)
  }
  
  violacion_supuestos = function(x, y){
    
    aux_y = y
    aux_x = x
    
    if(!is.null(input$violacionSupuestos)){
      
      if ("Linealidad" %in% input$violacionSupuestos){
        y = y + (x/70)^3
      }
      
      if ("Independencia" %in% input$violacionSupuestos ){
        y = apply(X = as.matrix(cbind(seq(from = 0, to = 45, length.out = length(y)), y)), MARGIN = 1, FUN = function(fila){
          recorrido =  sin(fila[1])*200 + fila[2]
          return(recorrido)
        })
      } 
      
      if ("Homocedasticidad" %in% input$violacionSupuestos){
        incrementos = cumsum(seq(from = 0.8, to = 1.2, length.out = length(y)))
        y = abs(y*incrementos - max(y)/2)
      }
      
      if ("Normalidad" %in% input$violacionSupuestos){
        n_min = y[order(y)][8]
        n_max = y[order(y)][(length(y)-8)]
        y[y <= n_min] = y[y <= n_min] - y[y == n_max]*0.5
        y[y >= n_max] = y[y >= n_max] + y[y == n_max]*0.5
      } 
    }
    return(y)
  }
  
  observeEvent(list(input$go_RL), {
    
    set.seed(2024)
    desviacion_simulacion = 100
    x_poblacion = seq(from = 1, to = input$nPoblacionalRLS, by = 1)
    y_poblacion = apply(X = as.matrix(x_poblacion + 100), MARGIN = 1, FUN = function(media){
      return(rnorm(n = 1, mean = media, sd = desviacion_simulacion))
    })
    
    n_muestra = sample(x = 1:input$nPoblacionalRLS, size = input$nMuestralRLS, replace = FALSE)
    x_muestral = x_poblacion[n_muestra]
    y_muestral = y_poblacion[n_muestra]
    y_muestral = violacion_supuestos(x = x_muestral, y = y_muestral)
    RL = regresion(x = x_muestral, y = y_muestral)
    
    output$plot_rl = renderPlot({
      grafico_RL(x = x_muestral, y = y_muestral, modelo = RL)
    })
    
    output$plot_supuestos = renderPlot({
      grafico_supuestos(x = x_muestral, y = y_muestral, modelo = RL)
    })
    
    output$resumen_rl = function(){
      summary_RL = round(summary(RL)$coefficients, digits = 4)
      summary_RL = ifelse(summary_RL == 0, "<2e-16", summary_RL)
      rownames(summary_RL) <- c("β<sub>0</sub>", "β<sub>1</sub>")
      kableExtra::kbl(summary_RL, escape = FALSE, booktabs = T, align = "c",
            caption = "Resumen del modelo de regresión lineal",
            col.names = c("Estimación", "Error estándar", "Estadístico de prueba t", "Valor-p")) |>
        kableExtra::kable_styling(full_width = TRUE, bootstrap_options = c("condensed", "striped"))
    }
    
    output$resumen_rl_metricas = function(){
      summary_RL = summary(RL)
      metrics = round(unlist(summary_RL[c("fstatistic", "sigma", "r.squared", "adj.r.squared")])[c(1,4:6)], digits = 4)
      kableExtra::kbl(t(as.matrix(metrics)), escape = FALSE, booktabs = T, align = "c",
        col.names = c("Estadístico F<sub>0</sub>", "Error estándar residual", "R<sup>2</sup>", "R<sup>2</sup> ajustado")) |>
        kableExtra::kable_styling(full_width = TRUE, bootstrap_options = c("condensed", "striped"))
    }

    output$resumen_supuestos = function(){
      
      tests = list(
        lmtest::bptest(formula = formula(RL), data = data.frame("x" = x_muestral, "y" = y_muestral)),
        lmtest::dwtest(formula = formula(RL), data = data.frame("x" = x_muestral, "y" = y_muestral), alternative = "two.sided"),
        shapiro.test(x = rstandard(RL))
      ) |> lapply(FUN = function(test){
        valor_p = round(test$p.value, digits = 4)
        statistic = round(test$statistic, digits = 4)
        valor_p = ifelse(valor_p < 0.0001, "<1e-4", valor_p)
        statistic = ifelse(statistic < 0.0001, "<1e-4", statistic)
        return(c(statistic, valor_p))
      }) |> unlist() |> matrix(ncol = 2, byrow = TRUE) |> data.frame() |> setNames(c("Estadístico", "Valor-p"))
      
      
      tests$Prueba = c("Breusch-Pagan", "Durbin-Watson", "Shapiro-Wilk")
      tests$Supuesto = c("Homocedasticidad", "Independencia", "Normalidad")
      tests = tests[, c("Supuesto", "Prueba", "Estadístico", "Valor-p")]
      
      kableExtra::kbl(tests, escape = FALSE, booktabs = T, align = "c",
        caption = "Prueba de hipótesis de los supuestos del modelo de regresión lineal") |>
        kableExtra::kable_styling(full_width = TRUE, bootstrap_options = c("condensed", "striped"))
    }

  })
  
  addPopover(session = session, id = "resumen_supuestos", trigger = "hover", placement = "bottom",
             content = "Es posible que la violación de un determinado supuesto influya en otro. Revise el caso de violación del supuesto de homocedasticidad únicamente.",
             title = "") 
  
# End general function  
}






























