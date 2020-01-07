#' Métricas de desempeño
#'
#' @param y: variable de estudio binaria.
#' @param x: matriz de variables explicativas.
#' @param link: link del modelo GLM, c("logit", "probit", "cloglog", ...), "logit" por defecto.
#' @param n: número entre 0 y 1 que indica el porcentaje de partición del conjunto de entrenamiento, por defecto es 0.8.
#' @param balance: indica si se balancea la muestra de entrenamiento c("over", "under"), por defecto es NULL (no se balancea).
#' @param ms: método de selección de variables c("forward", "backward"), por defecto es NULL (todas las variables).
#' @param semilla: semilla para los procesos de aleatorios de balanceo, por defecto es 9999.
#' @param plots: indica si los gráficos de desempeño se muestran en el mismo archivo, por defecto es FALSE.
#' @param ref: por defecto es FALSE, indica si la categoría de referencia de la variable respuesta se debe invertir.
#' @return ROC, PR, KS, metricas, conjunto test, conjunto train, modelo test, modelo train.
#' @export
#' @examples
#' n <- 500
#' x1 <- 2*runif(n) - 1
#' x2 <- 2*runif(n) - 1
#' x <- cbind(1, x1, x2)
#' beta <- c(-1, -3.0, 5.0)
#' probs <- exp(beta %*% t(x))/(1+exp(beta %*% t(x)))
#' y <- as.factor(rbinom(n,1,probs) + 1)
#' modelo = mtr.glm(y,x, link = "logit", balance = "under", plots = F, ms = "forward")
#' modelo$M.test



mtr.glm = function(y, x, link = "logit", n = 0.8, balance = NULL, ms = NULL, semilla = 9999, plots = F, ref = FALSE){

  niveles = as.data.frame(matrix(ncol = 2, nrow = 2))

  if(ref == FALSE){
    if(!is.factor(y)){
      y = as.factor(y)
      niveles[1, ] = levels(y)
      levels(y) = c(0, 1)
      niveles[2, ] = levels(y)
    } else {
      niveles[1, ] = levels(y)
      levels(y) = c(0, 1)
      niveles[2, ] = levels(y)
    }
  } else{
    if(!is.factor(y)){
      y = as.factor(y)
      niveles[1, ] = levels(y)
      levels(y) = c(1, 0)
      niveles[2, ] = levels(y)
    } else {
      niveles[1, ] = levels(y)
      levels(y) = c(1, 0)
      niveles[2, ] = levels(y)
    }
  }


  colnames(niveles) = c("", "")
  row.names(niveles) = c("Original", "Nuevo")

  datos = data.frame("y" = y, x)

  particion.y.balanceo = function(base){

    set.seed(semilla)
    ng = base[base$y == 0,]; sg = base[base$y == 1,]
    ind1 = seq(1,dim(ng)[1]); ind2 = seq(1,dim(sg)[1])
    nng = round(n*length(ind1),0); nsg = round(n*length(ind2),0)
    ng_train = sample(ind1,size = nng); sg_train = sample(ind2,size = nsg)
    train.ng = ng[ng_train,]; train.sg = sg[sg_train,]
    test.ng = ng[-ng_train,]; test.sg = sg[-sg_train,]
    train = rbind(train.ng, train.sg); test = rbind(test.ng, test.sg)

    aux1 = train.ng
    if(length(ind1) > length(ind2)){
      train.ng = train.sg ; train.sg = aux1
    }

    if(is.null(balance) == FALSE){
      if(balance == "over"){

        id.over = sample(seq(1, dim(train.ng)[1]), dim(train.sg)[1], replace = T)
        train = rbind(train.ng[id.over,], train.sg)
      }
      if(balance == "under"){
        id.under = sample(seq(1:dim(train.sg)[1]), dim(train.ng)[1])
        train = rbind(train.ng, train.sg[id.under,])
      }
    }

    conjunto = list(train, test)
    return(conjunto)
  }


  modelo.y.seleccion.variables = function(){

    conjuntos = particion.y.balanceo(datos)
    train = conjuntos[[1]]
    test = conjuntos[[2]]

    if(is.null(ms) == TRUE){
      modelo.train = glm(y ~ ., family = binomial(link = link), data = train)
      modelo.test = glm(y ~ ., family = binomial(link = link), data = test)
    } else{
      modelo.max = glm(y ~ ., family = binomial(link = link), data = train)
      modelo.nulo = glm(y ~ 1, data = train, family = binomial(link = link))
      formula.modelo = step(modelo.nulo, scope = list(lower = formula(modelo.nulo), upper = formula(modelo.max)), direction = ms, trace = 0)
      modelo.train = glm(formula(formula.modelo), family = binomial(link = link), data = train)
      modelo.test = glm(formula(formula.modelo), family = binomial(link = link), data = test)
    }
    modelos = list(modelo.train, modelo.test)

    pred.test = predict(modelo.test)
    pred.train = predict(modelo.train)

    if(plots == T){op = par(mfrow = c(3,1), bty = "n", xpd = TRUE)}
    else {op = par(mfrow = c(1,1), bty = "n", xpd = TRUE)}

    curva.roc = function(reales, probabilidades){
      levels(reales) = c(FALSE, TRUE)
      reales = as.logical(reales)
      reales = reales[order(probabilidades, decreasing = TRUE)]
      TPR = cumsum(reales)/sum(reales)
      FPR = cumsum(!reales)/sum(!reales)
      n = length(TPR)
      AUC = round(sum((FPR[2:n] - FPR[1:(n - 1)])*TPR[2:n])*100, 2)
      return(list(TPR, FPR, AUC))
    }

    prob.reales = predict(modelo.test, type = "response")
    r = curva.roc(test$y,prob.reales)
    AUC.test = r[[3]]
    plot(r[[1]] ~ r[[2]], type = "l", las = 1, lwd = 2,main = "Curva ROC", ylab = "Sensibilidad", xlab = "1 - Especificidad", ylim = c(0,1), xlim = c(0,1),
         col = "red")
    prob.reales = predict(modelo.train, type = "response")
    r = curva.roc(train$y,prob.reales)
    AUC.train = r[[3]]
    lines(r[[1]] ~ r[[2]],lwd = 2)
    lines(c(0,1), c(0,1), lty = 2, lwd = 1, col = "darkgreen")
    legend("bottomright", legend = c(paste("Test AUC = ", AUC.test), paste("Train AUC = ", AUC.train)),col = c("red", "black"), lty = 1, lwd = 2, bty = "n",
           cex = 0.8)


    curva.pr = function(reales, probabilidades){
      ind = as.matrix(seq(0.01,0.99,0.01))
      PPV = apply(ind, 1, FUN = function(x){
        pred = ifelse(probabilidades <= x, 0, 1)
        tabla = table(pred, reales)
        if(sum(pred == 0) == 0){
          tabla = rbind(c(0,0), tabla)
        }
        if(sum(pred == 1) == 0){
          tabla = rbind(tabla, c(0,0))
        }
        return(tabla[2,2]/sum(tabla[2,]))
      })
      TPR = apply(ind, 1, FUN = function(x){
        pred = ifelse(probabilidades <= x, 0, 1)
        tabla = table(pred, reales)
        if(sum(pred == 0) == 0){
          tabla = rbind(c(0,0), tabla)
        }
        if(sum(pred == 1) == 0){
          tabla = rbind(tabla, c(0,0))
        }
        return(tabla[2,2]/sum(tabla[,2]))
      })
      if(sum(is.nan(PPV)) != 0 | sum(is.nan(TPR)) != 0){
        nas  = which(is.nan(PPV) | is.na(TPR))
        PPV = PPV[-nas]
        TPR = TPR[-nas]
        n = length(TPR)
        AUC = round(sum((TPR[1:(n - 1)] - TPR[2:n])*PPV[2:n])*100, 2)
        warning("Existen NaN para valores extremos de TPR y/o PPV, AUC estimado.")
      } else {
        n = length(TPR)
        AUC = round(sum((TPR[1:(n - 1)] - TPR[2:n])*PPV[2:n])*100, 2)
      }

      return(list(PPV, TPR, AUC))
    }

    prob.reales = predict(modelo.test, type = "response")
    r = curva.pr(test$y, prob.reales)
    AUCpr.test = r[[3]]
    plot(r[[1]] ~ r[[2]], type = "l", las = 1, lwd = 2, main = "Curva PR ", ylab = "Precision", col = "red", xlab = "Recall", ylim = c(0,1), xlim = c(0,1))
    prob.reales = predict(modelo.train, type = "response")
    r = curva.pr(train$y, prob.reales)
    AUCpr.train = r[[3]]
    points(r[[1]] ~ r[[2]], type = "l", las = 1, lwd = 2)
    legend("bottomleft", legend = c(paste("Test AUC = ", AUCpr.test), paste("Train AUC = ", AUCpr.train)), col = c("red", "black"), lty = 1, lwd = 2,
           bty = "n", cex = 0.8)

    grafico.ks = function(reales, probabilidades){
      reales = reales[order(probabilidades, decreasing = TRUE)]
      acum.unos = cumsum(as.numeric(reales) - 1)
      acum.ceros = (seq(1,length.out = length(acum.unos)) - acum.unos)
      acum.unos = acum.unos/max(acum.unos)
      acum.ceros = acum.ceros/max(acum.ceros)
      ks = round(max(acum.unos - acum.ceros)*100,2)
      corte = which.max(acum.unos - acum.ceros)
      return(list(acum.unos, acum.ceros, ks, corte))
    }

    prob.reales = predict(modelo.test, type = "response")
    r = grafico.ks(test$y, prob.reales)
    ks.test = r[[3]]
    ind = seq(1, length.out = length(r[[2]]))
    plot(r[[2]] ~ ind, type = "l", main = "Gráfico KS", las = 1, col = "red", xaxt = "n", ylim = c(0, 1), xlab = "Tamaño de la muestra (%)",
         ylab = "Chance acumulada", lwd = 2)
    axis(1, at = seq(0, length(ind), length = 11), labels = seq(0,100, 10), las = 1, cex.axis = .9)
    lines(r[[1]] ~ ind, col = "red", lwd = 2)
    lines(c(ind[r[[4]]],ind[r[[4]]]), c( r[[2]][r[[4]]], r[[1]][r[[4]]]), col = "red", lty = 2, lwd = 1)

    prob.reales = predict(modelo.train, type = "response")
    r = grafico.ks(train$y, prob.reales)
    ks.train = r[[3]]
    ind = seq(1, length(ind), length.out = length(r[[2]]))
    lines(r[[2]] ~ ind, lwd = 2)
    lines(r[[1]] ~ ind, lwd = 2)
    lines(c(ind[r[[4]]],ind[r[[4]]]), c( r[[2]][r[[4]]], r[[1]][r[[4]]]), lty = 2, lwd = 1)
    legend("topleft", legend = c(paste("Test KS = ", ks.test), paste("Train KS = ", ks.train)), col = c("red", "black"), lty = 1, lwd = 2, bty = "n", cex = 0.8)
    par(op)

    metricas = function(x, y){
      pred = ifelse(y <= 0.5, 0, 1)
      matriz.confusion = table(pred, x)
      if(sum(pred == 0) == 0){
        matriz.confusion = rbind(c(0,0), matriz.confusion)
      }
      if(sum(pred == 1) == 0){
        matriz.confusion = rbind(matriz.confusion, c(0,0))
      }
      ACC = round(sum(diag(matriz.confusion))/sum(matriz.confusion)*100,4)
      Recall = round(matriz.confusion[2,2]/sum(matriz.confusion[,2])*100,4)
      Precision = round(matriz.confusion[2,2]/sum(matriz.confusion[2,])*100,4)
      M.test = data.frame(ACC, Recall, Precision)
      return(M.test)
    }

    prob.reales = predict(modelo.test, type = "response")
    M1 = metricas(test$y, prob.reales)
    prob.reales = predict(modelo.train, type = "response")
    M2 = metricas(train$y, prob.reales)

    return(list("Train" = train, "Test" = test, "Modelo.train" = modelo.train, "Modelo.test" = modelo.test, "AUC train" = AUC.train, "AUC test" = AUC.train,
                "PR test" = AUCpr.test, "PR train" = AUCpr.train, "ks train" = ks.train, "ks test" = ks.test, "M.test" = M1, "M.train" = M2, "Niveles" = niveles))
  }

  modelo.y.seleccion.variables()
}















