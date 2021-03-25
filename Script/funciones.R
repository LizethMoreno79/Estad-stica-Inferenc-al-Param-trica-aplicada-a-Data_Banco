MYEDA <- function (x, trim = 0.05,plot=FALSE) 
{
  if(!require(e1071)) install.packages("e1071", dependencies = T)
  if(!require(nortest)) install.packages("e1071", dependencies = T)
  Altblue <- "#A9E2FF"
  Adkblue <- "#0080FF"
  Ared <- "#C51111"
  varname <- deparse(substitute(x)) #saca el ombre del objeto y lo lleva a texto, va a usar al titulo de grafico
  N <- length(x)
  UM <- sum(is.na(x))
  n <- N - UM
  x <- x[!(is.na(x))]
  LQ1 <- (n + 1)/4 #posicion del cuartil 1 
  LQ3 <- (3 * (n + 1))/4
  Sort <- sort(x)
  V1 <- floor(LQ1)
  V2 <- floor(LQ3)
  V3 <- V1 + 1
  V4 <- V2 + 1
  Q1 <- round(Sort[V1] + (LQ1 - V1) * (Sort[V3] - Sort[V1]), 
              3)
  Q3 <- round(Sort[V2] + (LQ3 - V2) * (Sort[V4] - Sort[V2]), 
              3)
  IQR <- round(Q3 - Q1, 3)
  Min <- round(min(x), 3)
  Max <- round(max(x), 3)
  Stdev <- round(sd(x, na.rm = TRUE), 3)
  Mean <- round(mean(x, na.rm = TRUE), 3)
  Median <- round(median(x, na.rm = TRUE), 3)
  TrMean <- round(mean(x, trim = trim), 3)
  Var <- round(var(x, na.rm = TRUE), 3)
  SE <- round(Stdev/sqrt(n), 3)
  Range <- round(Max - Min, 3)
  
  ##graficos
  if(plot==TRUE){
    par(omi = c(0, 1, 0.5, 1))
    # par(mfrow = c(2, 2))
    par(mar = c(1, 0, 2, 0))
    par(pty = "s")
    print(varname)
    hist(x, probability = TRUE, col = Adkblue, xlab = "", ylab = "", 
         axes = FALSE, main = paste("Histograma de ", varname))
    box()
    iqd <- summary(x)[5] - summary(x)[2]
    plot(density(x, width = 2 * iqd, na.rm = TRUE), xlab = "", 
         ylab = "", axes = FALSE, type = "n", main = paste("Densidad de", 
                                                           varname))
    lines(density(x, width = 2 * iqd, na.rm = TRUE), col = Ared)
    box()
    l.out <- x[x < (Q1 - 1.5 * IQR)]
    r.out <- x[x > (Q3 + 1.5 * IQR)]
    outliers <- c(l.out, r.out)
    pos<- which(x %in% outliers )
    rest <- x[x > (Q1 - 1.5 * IQR) & x < (Q3 + 1.5 * IQR)]
    Minrest <- min(rest)
    Maxrest <- max(rest)
    plot(x, x, main = paste("Diagrama de caja", varname), xlab = "", 
         ylab = "", axes = FALSE, type = "n", xlim = c(min(x), 
                                                       max(x)), ylim = c(0, 1))
    box()
    polygon(c(Q1, Q1, Q3, Q3), c(0.3, 0.7, 0.7, 0.3), density = -1, 
            col = Altblue)
    points(outliers, c(rep(0.5, length(outliers))), col = Ared)
    lines(c(min(rest), Q1), c(0.5, 0.5), lty = 1)
    lines(c(Q3, max(rest)), c(0.5, 0.5), lty = 1)
    lines(c(min(rest), min(rest)), c(0.4, 0.6))
    lines(c(max(rest), max(rest)), c(0.4, 0.6))
    lines(c(Q1, Q1), c(0.3, 0.7))
    lines(c(Q3, Q3), c(0.3, 0.7))
    lines(c(Median, Median), c(0.3, 0.7))
    lines(c(Q1, Q3), c(0.3, 0.3))
    lines(c(Q1, Q3), c(0.7, 0.7))
    points(Mean, 0.5, pch = 16, col = "black")
    qqnorm(x, col = "black", main = paste("Gráfico de Cuantiles (Q-Q) de", varname), 
           xlab = "", ylab = "", axes = TRUE)
    qqline(x, col = Ared)
    box()
    mtext("ANALISIS EXPLORATORIO DE DATOS", side = 3, outer = TRUE, 
          cex = 1.5, col = Adkblue, line = 1)
    par(oma = c(0, 0, 0, 0))
    #par(mfrow = c(1, 1))
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    par(omi = c(0, 0, 0, 0))
    par(pty = "m")
    
  }
  #################
  SL <- lillie.test(x)
  K <- round(kurtosis(x), 3)
  S <- round(skewness(x), 3)
  SWpval <- round(SL$p.value, 3)
  TOT <- c(n, UM, Min, Q1, Mean, Median, TrMean, Q3, Max, Stdev, 
           Var, SE, IQR, Range, K, S, SWpval)
  names(TOT) <- c("Tamaño(n)", "Perdidos", "Mínimo", " 1er Cuartil", 
                  "Media", " Mediana", "Media Recortada", " 3er cuartil", "   Max.", 
                  "Desviación Estándar", "Varianza", "Error Estándar ", " Rango Intercuartílico", "Rango", 
                  "Kurtosis", "Skewness", "Lillie p-val")
  pos<-which(x%in% outliers) #los valores atipicos que tienes ese vector verifican donde estan devuelve un vector de verdaderos y falsos 
  pos
  res <- list(TOT=TOT, POS = pos, outliers = outliers)
  oldClass(res)<- c("RES","list")
  return(res)
}

Atipico <- function(x){
  N <- length(x)
  UM <- sum(is.na(x))
  n <- N - UM
  x <- x[!(is.na(x) > 0)]
  LQ1 <- (n + 1)/4
  LQ3 <- (3 * (n + 1))/4
  Sort <- sort(x)
  V1 <- floor(LQ1)
  V2 <- floor(LQ3)
  V3 <- V1 + 1
  V4 <- V2 + 1
  Q1 <- round(Sort[V1] + (LQ1 - V1) * (Sort[V3] - Sort[V1]),
              3)
  Q3 <- round(Sort[V2] + (LQ3 - V2) * (Sort[V4] - Sort[V2]),
              3)
  IQR <- round(Q3 - Q1, 3)
  l.out <- x[x < (Q1 - 1.5 * IQR)]
  r.out <- x[x > (Q3 + 1.5 * IQR)]
  outliers <- c(l.out, r.out)
  pos <- which(x %in% outliers)
  return(pos)
}
