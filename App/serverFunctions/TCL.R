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