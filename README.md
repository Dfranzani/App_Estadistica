# Aplicación web de conceptos estadísticos

La [App web](https://dfranzani.github.io/App_Estadistica/) se desarrolló con el fin de poder visualizar y entender 
de una manera más sencilla conceptos estadísticos (pueden visitar los documentos web en donde se detallan dichos conceptos).
 Hasta el momento se han desarrollado los siguientes conceptos:

1. **Teorema central del límite** (TCL o TLC): en este apartado se puede visualizar cómo se comporta la distribución de la media de distintas
muestras de una población, y cómo se va aproximando a una distribución normal a medida que se aumenta el tamaño de la muestra y la
cantidad de simulaciones (muestras generadas). 

2. **Intervalos de confianza**: en esta sección se puede visualizar cómo se comportan los intervalos de confianza de la media de una muestra. 
Se pueden elegir distintos tipos de intervalos de confianza y distintos tamaños de muestra. Además, se puede visualizar cómo distribuyen
los valores muestrales de cada una de las simulaciones.

3. **Pruebas de hipótesis**: en esta sección se puede visualizar la los estadísticos de prueba, los valores críticos y el valor-p de
una prueba de hipótesis para la media de una población. Además, se puede observar el intervalo de confianza como método de rechazo.

4. **Regresión lineal**: en esta sección se puede visualizar cómo se comporta la recta de regresión lineal de una muestra de datos. Se
puede observar un resumen de los parámetros estimados del modelo y sus métricas de ajuste. Además, se incluye un análisis de los
supuestos del modelo. Se incluyó la posibilidad de violar los supuestos del modelo para observar cómo se comporta la recta de regresión.

Además, se ha habilitado una sección de [**discusiones**](https://github.com/Dfranzani/App_Estadistica/discussions) para
que los usuarios puedan aportar ideas, comentarios y sugerencias para mejorar la App.

En el futuro se espera seguir desarrollando más conceptos estadísticos y mejorar la interfaz de la App. Cabe mencionar, que la aplicación
no es una material autoinstruccional, sino que es una ayuda para la visualización de los conceptos ya mencionados.

## Desarrollo

La aplicación fue desarrollada con el paquete `shiny` de R, además de los paquetes `shinydashboard` para la interfaz, `shinyBS` para
las burbujas con anotaciones, `Cairo` para la renderización de los gráficos, `kableExtra` para la renderización de tablas y
`shinydashboardPlus` para elementos de la interfaz.

Para el despliegue en Github Pages se utilizó el paquete [`shinylive`](https://github.com/posit-dev/r-shinylive) vía [`webR`](https://docs.r-wasm.org/webr/latest/). 