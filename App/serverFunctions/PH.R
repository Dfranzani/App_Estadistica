grafico_estadisticoPrueba = function(ic, mediaMuestra, varianza, mu0, tipoPH, nMuestra, est_prueba, valor_critico, confianza){
  
  max = max(abs(c(0 - c(est_prueba, valor_critico, 4))))
  limites = c(0 - max * 1.2, 0 + max * 1.2)
  
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