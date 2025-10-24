# ==============================================
# TPI – Consignas 5 a 8 (R) – Piuzzi Andres
# ==============================================

# ========== 1) Librerías y cargar archivo ==========
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",  quietly = TRUE)) install.packages("dplyr")

library(readxl)
library(dplyr)

ruta  <- file.choose()
datos <- read_excel(ruta)

# ----- Nombres de columnas  -----
col_satis    <- "SATISFACCIÓN CON LA CARRERA"   # 1=Muy sat, 2=Sat, 3=Insat, 4=Muy insat
col_estatura <- "ESTATURA CM."                  # numérico
col_peso     <- "PESO KG."                      # numérico

# Limpieza básica
datos[[col_satis]]    <- as.integer(datos[[col_satis]])
datos[[col_estatura]] <- as.numeric(datos[[col_estatura]])
datos[[col_peso]]     <- as.numeric(datos[[col_peso]])


# =============================================================
# 5) Binomial – p desde TU población (tabla de satisfacción)
# =============================================================

map <- c("1"="Muy satisfecho","2"="Satisfecho","3"="Insatisfecho","4"="Muy insatisfecho")
satis_txt <- map[as.character(datos[[col_satis]])]
niveles   <- c("Muy satisfecho","Satisfecho","Insatisfecho","Muy insatisfecho")
tab_satis <- factor(satis_txt, levels = niveles) |> table() |> as.data.frame()
names(tab_satis) <- c("Categoria","fi")
Npop <- sum(tab_satis$fi)
tab_satis <- tab_satis |> mutate(fr = round(fi / Npop, 6))
print(tab_satis)

p_ms <- tab_satis$fr[tab_satis$Categoria=="Muy satisfecho"]
p_s  <- tab_satis$fr[tab_satis$Categoria=="Satisfecho"]
p_i  <- tab_satis$fr[tab_satis$Categoria=="Insatisfecho"]
p_mi <- tab_satis$fr[tab_satis$Categoria=="Muy insatisfecho"]

n <- 16

Pa <- 1 - pbinom(9, size=n, prob=p_ms)                    # > 9 muy satisfechos
Pb <- pbinom(8, size=n, prob=p_s) - pbinom(3, size=n, prob=p_s)   # 4..8 satisfechos
Pc <- pbinom(4, size=n, prob=p_i)                          # < 5 insatisfechos
Pd <- dbinom(10, size=n, prob=p_mi)                        # = 10 muy insatisfechos
cat("Se analiza la probabilidad de distintos niveles de satisfacción\n")
cat("en una muestra de 16 estudiantes, según los siguientes casos:\n\n")

cat(sprintf("a) Probabilidad de que más de 9 estén muy satisfechos:      P(X > 9)  = %.6f\n", Pa))
cat(sprintf("b) Probabilidad de que entre 4 y 8 estén satisfechos:        P(4 ≤ X ≤ 8) = %.6f\n", Pb))
cat(sprintf("c) Probabilidad de que menos de 5 estén insatisfechos:       P(X < 5)  = %.6f\n", Pc))
cat(sprintf("d) Probabilidad exacta de que 10 estén muy insatisfechos:    P(X = 10) = %.12f\n", Pd))

# ==================================
# 6) Poisson – consultas por tiempo
# ==================================

# λ representa la media de llegadas en cada intervalo
lambda_20 <- 10   # 15*(20/30)
lambda_30 <- 15   # promedio base
lambda_40 <- 20   # 15*(40/30)

# a) Al menos 6 consultas en 20 minutos
P6a <- 1 - ppois(5, lambda_20)

# b) A lo sumo 12 consultas en 40 minutos
P6b <- ppois(12, lambda_40)

# c) Entre 8 y 9 consultas en 30 minutos
P6c <- dpois(8, lambda_30) + dpois(9, lambda_30)

cat("\n--- Consigna 6 (Poisson) ---\n")
cat(sprintf("a) P(≥6 en 20 min)  = %.6f\n", P6a))
cat(sprintf("b) P(≤12 en 40 min) = %.6f\n", P6b))
cat(sprintf("c) P(8..9 en 30 min)= %.6f\n", P6c))

# ===============================================
# 7) Normal – Estatura (usa tu columna de cm)
# ===============================================
x <- datos[[col_estatura]]
x <- x[!is.na(x)]
mu    <- mean(x); sigma <- sd(x); n_est <- length(x)

P7a <- pnorm(179, mean=mu, sd=sigma, lower.tail=FALSE)  # >=179
P7b <- pnorm(172, mean=mu, sd=sigma) - pnorm(147, mean=mu, sd=sigma)
q975 <- qnorm(0.975, mean=mu, sd=sigma)

cat("\n--- Consigna 7 (Normal) ---\n")
cat(sprintf("Media (mu)=%.3f  Desvio (sigma)=%.3f  n=%d\n", mu, sigma, n_est))
cat(sprintf("a) P(X >= 179)               = %.6f\n", P7a))
cat(sprintf("b) P(147 <= X <= 172)        = %.6f\n", P7b))
cat(sprintf("c) Percentil 97.5%% (cm)      = %.3f\n", q975))

# Datos base
x <- datos[[col_estatura]]
x <- x[!is.na(x)]
mu <- mean(x); sigma <- sd(x)

# Secuencia de valores alrededor de la media
xseq <- seq(mu - 4*sigma, mu + 4*sigma, length=200)
yseq <- dnorm(xseq, mean=mu, sd=sigma)

# Gráfico de la curva normal
plot(xseq, yseq, type="l", lwd=2, col="blue",
     main="Distribución Normal de la Estatura",
     xlab="Estatura (cm)", ylab="Densidad")

# Línea vertical en la media
abline(v=mu, col="red", lwd=2)
text(mu, 0.0004, sprintf("Media = %.2f cm", mu), pos=4, col="red")

# =====================================================
# 8) Muestreo Aleatorio Simple – 6 muestras de n = 20
#     (usamos el PESO KG. como variable cuantitativa)
# =====================================================
set.seed(123)
k_m <- 6; n_m <- 20
peso <- datos[[col_peso]] |> as.numeric() |> (function(v) v[!is.na(v)])()

medias <- replicate(k_m, mean(sample(peso, size=n_m, replace=FALSE)))
mu_peso <- mean(peso)

cat("\n--- Consigna 8 (Muestreo – Peso) ---\n")
print(round(medias, 3))
cat(sprintf("Media poblacional de peso: %.3f kg\n", mu_peso))

res8 <- data.frame(
  muestra = 1:k_m,
  media_muestral = round(medias,3),
  dif_respecto_mu = round(medias - mu_peso, 3)
)
print(res8)

# (Opcional) gráfico
hist(medias, main="Medias muestrales de Peso (k=6, n=20)",
     xlab="Media muestral (kg)", breaks=5)
abline(v=mu_peso, lwd=2)
