# ========== 1) Librerías y cargar archivo ==========
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",  quietly = TRUE)) install.packages("dplyr")

library(readxl)
library(dplyr)

# Elegir archivo y leer UNA sola vez
ruta <- file.choose()
datos <- read_excel(ruta)


# ========== 2.a) Horas semanales: tabla por intervalos ==========

hours_col <- "TIEMPO SEMANAL en HS. DEDIC. EST."

x <- as.numeric(datos[[hours_col]])
x <- x[!is.na(x)]
n <- length(x)

# Sturges
k_raw <- 1 + 3.322*log10(n)
k     <- round(k_raw)
h_raw <- (max(x) - min(x)) / k

# Elegí el múltiplo para "embellecer" (1 ó 0.5)
step_w <- 1      # <-- cambiá a 0.5 si querés más fino
w <- max(step_w, round(h_raw / step_w) * step_w)

# Bordes alineados (enteros) y cubriendo el rango
lo <- floor(min(x))
hi <- ceiling(max(x))
edges <- seq(lo, hi, by = w)
if (tail(edges, 1) < hi) edges <- c(edges, tail(edges, 1) + w)

# Clasificar y armar tabla
cats <- cut(x, breaks = edges, right = TRUE, include_lowest = TRUE)
tab  <- as.data.frame(table(cats))
names(tab) <- c("Intervalo","f_i")
tab$f_i  <- as.integer(tab$f_i)
N <- sum(tab$f_i)
tab$fr_i <- round(tab$f_i / N, 4)
tab$F_i  <- cumsum(tab$f_i)
tab$Fr_i <- round(tab$F_i / N, 4)

cat(sprintf("Sturges: n=%d | k_raw=%.4f -> k≈%d | h_raw=%.4f -> w=%.2f\n",
            n, k_raw, k, h_raw, w))
print(tab, row.names = FALSE)

# ========== 2.b) Satisfacción con la carrera ==========

satis_col <- "SATISFACCIÓN CON LA CARRERA"

# Mapear 1..4 -> etiquetas
map <- c("1"="Muy satisfecho", "2"="Satisfecho", "3"="Insatisfecho", "4"="Muy insatisfecho")
satis_txt <- map[as.character(datos[[satis_col]])]

niveles_orden <- c("Muy insatisfecho","Insatisfecho","Satisfecho","Muy satisfecho")
satis_factor  <- factor(satis_txt, levels = niveles_orden, ordered = TRUE)

# Tabla de frecuencias respetando el orden del factor
tab_satis <- as.data.frame(table(satis_factor))
names(tab_satis) <- c("Categoria", "f_i")  # evitar acentos/espacios en nombres

tab_satis <- tab_satis |>
  mutate(
    f_i  = as.integer(f_i),
    fr_i = round(f_i / sum(f_i), 4),
    F_i  = cumsum(f_i),
    Fr_i = round(F_i / sum(f_i), 4)
  )

print.data.frame(tab_satis, row.names = FALSE)

# ========== 2.c) Cuarto intervalo de la tabla de horas ==========

tab_h <- if (exists("tab_horas")) tab_horas else if (exists("tab")) tab else stop("No encuentro la tabla de horas (tab_horas/tab).")
stopifnot(nrow(tab_h) >= 4)

cuarto <- tab_h[4, ]
n_tot <- sum(tab_h$f_i)

cat(sprintf("Intervalo (4.º): %s\n", cuarto$Intervalo))
cat(sprintf("f_i  = %d estudiantes\n", cuarto$f_i))
cat(sprintf("fr_i = %.4f  (%.4f%%)\n", cuarto$fr_i, 100*cuarto$fr_i))
cat(sprintf("F_i  = %d estudiantes (acumulado)\n", cuarto$F_i))
cat(sprintf("Fr_i = %.4f  (%.4f%% acumulado)\n", cuarto$Fr_i, 100*cuarto$Fr_i))

msg_2c <- sprintf(
  "En el 4.º intervalo %s hay %d estudiantes (%.4f%% del total, n=%d). Hasta aquí se acumulan %d (%.4f%%), es decir, ~%.2f%% estudia ≤ el límite superior del 4.º intervalo.",
  cuarto$Intervalo, cuarto$f_i, 100*cuarto$fr_i, n_tot, cuarto$F_i, 100*cuarto$Fr_i, 100*cuarto$Fr_i
)
cat("\nInterpretación:\n", msg_2c, "\n", sep = "")

# ========== 2.d) Detalle para “Satisfecho” y positivo agregado ==========

ts <- as.data.frame(tab_satis)

fila <- ts[ts$Categoria == "Satisfecho", , drop = FALSE]
stopifnot(nrow(fila) == 1)

n_total <- sum(ts$f_i)

ms <- ts[ts$Categoria == "Muy satisfecho", , drop = FALSE]
ms_fi <- if (nrow(ms) == 1) ms$f_i else 0
ms_fr <- if (nrow(ms) == 1) ms$fr_i else 0

cat(sprintf("\nCategoría: %s\n", "Satisfecho"))
cat(sprintf("f_i  = %d estudiantes\n", fila$f_i))
cat(sprintf("fr_i = %.4f  (%.4f%%)\n", fila$fr_i, 100 * fila$fr_i))
cat(sprintf("F_i  = %d (acumulado)\n", fila$F_i))
cat(sprintf("Fr_i = %.4f  (%.4f%% acumulado)\n\n", fila$Fr_i, 100 * fila$Fr_i))

pos_fi <- fila$f_i + ms_fi
pos_fr <- fila$fr_i + ms_fr

msg_2d <- sprintf(
  "“Satisfecho” reúne %d estudiantes (%.4f%% del total, n=%d). Hasta esta categoría se acumulan %d (%.4f%%). Sumando “Muy satisfecho”, la valoración positiva es %d/%d = %.4f%%.",
  fila$f_i, 100 * fila$fr_i, n_total, fila$F_i, 100 * fila$Fr_i, pos_fi, n_total, 100 * pos_fr
)
cat("Interpretación:\n", msg_2d, "\n", sep = "")