# ========== 1) Librerías y cargar archivo ==========
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",  quietly = TRUE)) install.packages("dplyr")

library(readxl)
library(dplyr)

# Elegir archivo y leer UNA sola vez
ruta  <- file.choose()
datos <- read_excel(ruta)

# ========== 2) Definición de variables y tablas de frecuencias ==========

# 2.a) Horas semanales: tabla por intervalos (criterio de la profe: k=7, ancho=3)
hours_col <- "TIEMPO SEMANAL en HS. DEDIC. EST."
stopifnot(hours_col %in% names(datos))

x <- as.numeric(datos[[hours_col]])
x <- x[!is.na(x)]
n <- length(x)

# Info Sturges (solo informativo)
k_raw <- 1 + 3.322*log10(n)
k     <- round(k_raw)
h_raw <- (max(x) - min(x)) / k

# Cortes EXACTOS pedidos (k=7, ancho=3): [1,4) [4,7) ... [19,22)
edges <- c(1, 4, 7, 10, 13, 16, 19, 22)

# Intervalos cerrados a izquierda y abiertos a derecha
cats <- cut(x, breaks = edges, right = FALSE, include.lowest = TRUE)

tab  <- as.data.frame(table(cats))
names(tab) <- c("Intervalo","f_i")
tab$f_i  <- as.integer(tab$f_i)
N <- sum(tab$f_i)
tab$fr_i <- round(tab$f_i / N, 4)
tab$F_i  <- cumsum(tab$f_i)
tab$Fr_i <- round(tab$F_i / N, 4)

cat(sprintf("Sturges (info): n=%d | k_raw=%.4f -> k≈%d | h_raw=%.4f\n",
            n, k_raw, k, h_raw))
cat("Se aplica criterio de la profe: k=7, ancho=3h, intervalos [ , )\n")
print(tab, row.names = FALSE)

# 2.b) Satisfacción con la carrera (categórica ordinal)
satis_col <- "SATISFACCIÓN CON LA CARRERA"
stopifnot(satis_col %in% names(datos))

# Mapear 1..4 -> etiquetas
map <- c("1"="Muy satisfecho", "2"="Satisfecho", "3"="Insatisfecho", "4"="Muy insatisfecho")
satis_txt <- map[as.character(datos[[satis_col]])]

niveles_orden <- c("Muy insatisfecho","Insatisfecho","Satisfecho","Muy satisfecho")
satis_factor  <- factor(satis_txt, levels = niveles_orden, ordered = TRUE)

# Tabla de frecuencias respetando el orden del factor
tab_satis <- as.data.frame(table(satis_factor))
names(tab_satis) <- c("Categoria", "f_i")  

tab_satis <- tab_satis |>
  mutate(
    f_i  = as.integer(f_i),
    fr_i = round(f_i / sum(f_i), 4),
    F_i  = cumsum(f_i),
    Fr_i = round(F_i / sum(f_i), 4)
  )

cat("\nTabla de Satisfacción:\n")
print.data.frame(tab_satis, row.names = FALSE)

# 2.c) Cuarto intervalo de la tabla de horas (redacción pedida por la profe)
stopifnot(nrow(tab) >= 4)
cuarto <- tab[4, ]
n_tot  <- sum(tab$f_i)

pct_int   <- round(100 * cuarto$fr_i, 2)
pct_acum  <- round(100 * cuarto$Fr_i, 2)
pct_int_s  <- format(pct_int,  nsmall = 2, decimal.mark = ",")
pct_acum_s <- format(pct_acum, nsmall = 2, decimal.mark = ",")

msg_2c <- sprintf(
  "2-c) %d estudiantes dedican 10 horas o más pero menos de 13 horas (intervalo [10,13); el 13 NO se incluye). Esto representa el %s%% del total (n=%d).\nAdemás, %d de %d estudiantes (%s%%) dedican menos de 13 horas semanales (tampoco incluye el 13 por ser extremo abierto).",
  cuarto$f_i, pct_int_s, n_tot,
  cuarto$F_i, n_tot, pct_acum_s
)
cat("\n", msg_2c, "\n\n", sep="")

# 2.d) Interpretación de la categoría “Satisfecho” (corregido)
ts <- tab_satis
n_total <- sum(ts$f_i)

# Fila de "Satisfecho"
fila <- ts[as.character(ts$Categoria) == "Satisfecho", , drop = FALSE]
if (nrow(fila) == 0) stop("No hay categoría 'Satisfecho' en la tabla de satisfacción.")

# “Muy satisfecho”
ms <- ts[as.character(ts$Categoria) == "Muy satisfecho", , drop = FALSE]
ms_fi <- if (nrow(ms) == 1) ms$f_i else 0L
ms_fr <- if (nrow(ms) == 1) ms$fr_i else 0

# Valoración negativa (Insatisfecho + Muy insatisfecho)
neg_cats <- c("Insatisfecho","Muy insatisfecho")
neg_fi <- sum(ts$f_i[as.character(ts$Categoria) %in% neg_cats])
neg_fr <- if (n_total > 0) round(neg_fi / n_total, 4) else NA_real_

# Positivo = Satisfecho + Muy satisfecho
pos_fi <- fila$f_i + ms_fi
pos_fr <- round(fila$fr_i + ms_fr, 4)

# Razón positivo : negativo (evitar div/0)
pos_neg_ratio <- if (!is.na(neg_fr) && neg_fr > 0) round(pos_fr / neg_fr, 4) else Inf

# Moda (categoría con mayor frecuencia) y Mediana ordinal
mode_idx <- which.max(ts$f_i)
mode_cat <- as.character(ts$Categoria[mode_idx])
if (!"F_i" %in% names(ts)) ts$F_i <- cumsum(ts$f_i)
median_pos <- ceiling(n_total / 2)
med_idx <- which(ts$F_i >= median_pos)[1]
med_cat <- as.character(ts$Categoria[med_idx])

msg_2d <- sprintf(
  "2-d) “Satisfecho” reúne %d estudiantes (fr_i = %.4f → %.2f%%). Hasta esta categoría se acumulan F_i = %d (Fr_i = %.4f → %.2f%%).\nSumando “Muy satisfecho”, la valoración positiva es %d/%d = %.4f (%.2f%%). En contraste, la valoración negativa (Insatisfecho + Muy insatisfecho) es %d/%d = %.4f (%.2f%%), lo que muestra un balance favorable (positivo:negativo = %.4f:1).\nAdemás, la categoría modal es “%s” y la mediana ordinal cae en “%s”.",
  fila$f_i, fila$fr_i, 100*fila$fr_i,
  fila$F_i, fila$Fr_i, 100*fila$Fr_i,
  pos_fi, n_total, pos_fr, 100*pos_fr,
  neg_fi, n_total, neg_fr, 100*neg_fr,
  pos_neg_ratio, mode_cat, med_cat
)
cat(msg_2d, "\n\n")

# ========== 3) Medidas descriptivas ==========
cat("===== 3) Medidas descriptivas =====\n")

# Variable Horas (cuantitativa)
media_h    <- mean(x)
mediana_h  <- median(x)
# Modo como "clase modal" (el intervalo con mayor frecuencia)
moda_h_int <- as.character(tab$Intervalo[which.max(tab$f_i)])
rango_h    <- max(x) - min(x)
var_h      <- var(x)
sd_h       <- sd(x)
cv_h       <- sd_h / media_h
cuartiles_h<- quantile(x, probs = c(0.25, 0.5, 0.75), names = TRUE)

cat(sprintf("Horas de estudio -> Media: %.2f | Mediana: %.2f | Clase modal: %s\n", 
            media_h, mediana_h, moda_h_int))
cat(sprintf("Rango: %.2f | Varianza: %.2f | Desvío estándar: %.2f | CV: %.3f\n", 
            rango_h, var_h, sd_h, cv_h))
cat("Cuartiles (Q1, Q2, Q3):\n"); print(cuartiles_h)

# Variable Satisfacción (cualitativa ordinal): Moda, Mediana y Cuartiles ordinales
mode_idx <- which.max(tab_satis$f_i)
moda_satis <- as.character(tab_satis$Categoria[mode_idx])

n_total <- sum(tab_satis$f_i)
median_pos <- ceiling(n_total / 2)
med_idx <- which(tab_satis$F_i >= median_pos)[1]
mediana_satis <- as.character(tab_satis$Categoria[med_idx])

q1_pos <- ceiling(n_total * 0.25)
q3_pos <- ceiling(n_total * 0.75)
q1_cat <- as.character(tab_satis$Categoria[which(tab_satis$F_i >= q1_pos)[1]])
q3_cat <- as.character(tab_satis$Categoria[which(tab_satis$F_i >= q3_pos)[1]])

cat(sprintf("\nSatisfacción -> Moda: %s | Mediana: %s | Q1: %s | Q3: %s\n\n",
            moda_satis, mediana_satis, q1_cat, q3_cat))

# ========== 4) Gráficos ==========
# a) Histograma de horas (Frecuencia absoluta)
hist(x,
     breaks = edges,
     right  = FALSE,
     col    = "skyblue", border = "white",
     main   = "Histograma - Horas semanales dedicadas al estudio",
     xlab   = "Horas semanales", ylab = "Frecuencia absoluta")

# b) Diagrama circular de satisfacción (porcentual)
pie_labels <- paste0(tab_satis$Categoria, " (", round(tab_satis$fr_i*100, 2), "%)")
pie(tab_satis$f_i,
    labels = pie_labels,
    main = "Nivel de satisfacción con la carrera")

# c) Análisis (mensajes)
cat("===== 4) Análisis de gráficos =====\n")
cat("- Histograma: mayor concentración entre 7 y 13 horas; pocos casos en los extremos.\n")
cat("- Circular: predominan niveles positivos (Satisfecho + Muy satisfecho).\n")
cat("- En conjunto: carga de estudio moderada convive con una valoración positiva de la carrera.\n")

############################################################
# FIN
############################################################

