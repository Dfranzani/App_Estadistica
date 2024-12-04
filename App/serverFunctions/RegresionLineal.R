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
       main = "Regresión Lineal Simple", xlab = "Valores de X", ylab = "",
       ylim = c(min(c(y, y_confidence, y_prediction)), max(c(y, y_confidence, y_prediction))))
  segments(x0 = min(x), x1 = max(x), y0 = betas[1] + betas[2]*min(x), y1 = betas[1] + betas[2]*max(x), col = "red", lty = 1)
  lines(x = x[order(x)], y = y_confidence[,1], col = "blue", lty = 2)
  lines(x = x[order(x)], y = y_confidence[,2], col = "blue", lty = 2)
  lines(x = x[order(x)], y = y_prediction[,1], col = "darkgreen" ,lty = 2)
  lines(x = x[order(x)], y = y_prediction[,2], col = "darkgreen", lty = 2)
  mtext("Valores de Y", side = 2, line = 4, cex = 1)  
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
       main = "Homocedasticidad", xlab = "Valores ajustados", ylab = "Raíz del |residuos estandarizados|")
  qqnorm(rstandard(modelo), main = "Normalidad \n (Cuantil - Cuantil)", xlab = "Cuantiles teóricos", ylab = "Cuantiles observados")
  qqline(rstandard(modelo))
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
    par(mar = c(5.1, 4.1, 4.1, 2.1) + c(0, 1, 0, 0))
    grafico_RL(x = x_muestral, y = y_muestral, modelo = RL)
  })
  
  output$plot_supuestos = renderPlot({
    # par(mar = c(5.1, 4.1, 4.1, 2.1) + c(0,0,0,0), mgp = c(3,1,0) + c(0,0,3))
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
                    col.names = c("Estadístico F", "Error estándar residual", "R<sup>2</sup>", "R<sup>2</sup> ajustado")) |>
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