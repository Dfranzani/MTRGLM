# MTRGLM
Paquete R para determinar métricas de desempeño de modelos lineales generalizados.

## Utilidades del paquete
El paquete posee una única función denominada ```mtr.glm (...)```, dicha función realiza los siguientes procedimientos:

1. Particionar la muestra en un conjutos de entrenamiento y prueba.
2. Balanceo de la muestra de entrenamiento.
3. Selección de variables para el modelo.
4. Contrucción del modelo lineal generalizado.
5. Curva de ROC.
6. Curva PR.
7. Gráfico KS.
8. Métricas (*Accuracy*, *Recall*, *Presicion*).

## Argumentos de la función

La función ```mtr.glm (...)``` recibe los siguientes argumentos:
1. ```y```: variable de estudio binaria.
2. ```x```: matriz de variables explicativas.
3. ```link```: link del modelo lineal generalizado, ```c("logit", "probit", "cloglog", ...)```. Por defecto es ```"logit"```.
4. ```n```: número entre 0 y 1 que indica el porcentaje de la muestra que corresponde al conjunto de entrenamiento, por defecto es 0.8.
5. ```balance```: indica si se balancea (metodología) la muestra de entrenamiento, ```c("over", "under")```. Por defecto es ```NULL``` (no se balancea).
6. ```ms```: método de selección de variables ```c("forward", "backward")```, por defecto es ```NULL``` (todas las variables).
7. ```semilla```: semilla para los procesos de aleatorios de balanceo, por defecto es 9999.
8. ```plots```: indica si las curvas ROC, PR y KS se muestran en el mismo gráfico, por defecto es ```FALSE```.
9. ```ref```: indica si la categoría de referencia de la variable respuesta se debe invertir. Por defecto es ```FALSE```.

### Notas de utilidad
1. No es necesario que la variable respuesta sea de caracter factor. La función parametriza la variable respuesta, además genera un atributo de la categoría de referencia usada.
2. Se recomienda mantener el atributo ```plots = FALSE``` debido al margen del gráfico y la ubicación de las leyendas.  
3. La función está testeada para los links: ```"logit", "probit", "cloglog"```. Para otro tipo pueden ocurrir errores.
4. Para instalar el paquete MTRGLM desde Github, de debe instalar primeramente el paquete **devtolls** (```install.packages("devtools")```). Posterior, se debe ejecutar el comando ```install_github("Dfranzani/MTRGLM")```.
5. Para consultar un ejemplo ejecute ```help(mtr.glm)```.
