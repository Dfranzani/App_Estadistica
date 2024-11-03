library(shiny)

function(input, output, session) {
  
  
  ### Distribuciones muestrales
  
  observeEvent(input$distribucionDM1,{
    output$Histograma_promedios = renderPlot({
      plot(1, 1, col = "white", ylab = "", xlab = "", axes = F)
    })
    
    output$fdp_fmp_simulaciones = renderPlot({
      plot(1, 1, col = "white", ylab = "", xlab = "", axes = F)
    })
  })
  
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
    plot(x, valores, main = paste("Función de ", tipo, " probabilidad"),
         xlab = "Valores de X", ylab = "", las = 1, type = graph, pch = 16)
  }
  
  plot_simulaciones = function(valores, dominio){
    largo = length(valores[[1]])
    cantidad = 4
    datos = data.frame(
      "valores" = unlist(valores[1:cantidad]),
      "dominio" = rep(1:cantidad, rep(largo, cantidad))
    )
    plot(
      datos$dominio, datos$valores,
      main = paste("Primeras ", cantidad, " simulaciones"),
      xlab = "Simulación", ylab = "Valores de X simulados", xaxt = "n",
      las = 1, pch = 16, ylim = c(min(datos$valores), max(datos$valores)*1.4)
    )
    axis(side = 1, at = 1:cantidad, labels = paste0("S", 1:cantidad))
    legend(
      "top", 
      legend = paste0(rep("S", cantidad), 1:cantidad, rep(": ", cantidad),
                      unlist(lapply(X = valores[1: cantidad], FUN = function(x){return(round(mean(x), 4))}))),
      bty = "n", title = "Media de la simulación", ncol = 2)
  }

  observeEvent(input$go_DM1,{
    
    n = input$nDM1
    simulaciones = input$simulacionesDM1
    
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
      plot(h1, col = color, xlim = c(min(x, y), max(x,y)), ylim = c(0, max(h1$counts, h2$counts)),
           las = 1, ylab = "Frecuencias de X", xaxt = "n", main = titulo)
      par(mar = c(5,5,0,3))
      plot(h2, col = color, xlim = c(min(x, y), max(x,y)), ylim = c(max(h1$counts, h2$counts), 0),
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
      ylim = c(-10, simulaciones), xlim = limites_x, xaxt = "n", yaxt = "n",
      xlab = "Valores del IC", ylab = "Número de simulación",
      main = ifelse(nombre_media == "Una",
                    "IC para la media proveniente \n de una distribución normal",
                    "IC para la diferencia de medias (X-Y) \n provenientes de distribuciones normales")
    )
    axis(side = 1, at = c(limites_x[1], media, limites_x[2]), labels = c(limites_x[1], media, limites_x[2]), xlim = limites_x)
    axis(side = 2, at = round(seq(from = 1, to = simulaciones, length.out = 10), 0))
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
    legend("bottomleft", legend = c(negros, rojos), lty = 1, col = c("black", "red"), title = "Proporción de IC", bty = "n")
    legend("bottomright", legend = c("Haga click en un IC (o cerca) \n para ver la distribución de los \n datos de la muestra. \n \n"), bty = "n")
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
  
  observeEvent(input$go_IC,{
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
                          control_lanzamiento = FALSE, colores = ic[punto_seleccionado(),3:4])
        } else {
          hist_muestra_IC(x = muestras[[punto_seleccionado()]]$data1, y = muestras[[punto_seleccionado()]]$data2,
                          nombre_media = nombre_media_global, control_lanzamiento = FALSE, colores = ic[punto_seleccionado(),3:4])
        }
      })
    } else {
      output$histogramaIC = renderPlot({
        if(nombre_media_global == "Una"){
          hist_muestra_IC(x = muestras[[simulaciones_global]], nombre_media = nombre_media_global,
                          control_lanzamiento = TRUE, colores = ic[simulaciones_global,3:4])
        } else {
          hist_muestra_IC(x = muestras[[simulaciones_global]]$data1, y = muestras[[simulaciones_global]]$data2,
                          nombre_media = nombre_media_global, control_lanzamiento = TRUE, colores = ic[simulaciones_global,3:4])
        }
      })
    }
  })
}









