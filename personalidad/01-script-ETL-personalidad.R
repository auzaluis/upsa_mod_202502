# Librerías
# install.packages("pacman")
library(pacman)
p_load(
  gsheet,
  tidyverse,
  scales
)

# Tema 01: Carga de datos ----

## Carga local
df <- read.csv(
  file = "personalidad/Personalidad y uso de apps.csv",
  check.names = FALSE
)

colnames(df)

## Carga vía API (online)

url <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

df <- read.csv(
  text = gsheet2text(url),
  check.names = FALSE
)

colnames(df)    # Ver los nombres de las columnas
class(df)       # Ver el tipo de variable/objeto
class(df$Sexo)  # Ver el tipo de dato
nrow(df)        # Cantidad de filas
ncol(df)        # Cantidad de columnas


# Tema 02: Transformación de datos ----

## Valores perdidos (NA) ----
# Los NAs pueden ser tratados de 2 maneras
# 1. Eliminar o Ignorar
# 2. Reemplazar / Imputar

df$`Escribe tu edad exacta`
df[,3]

# Convirtiendo la edad a tipo numeric
df$`Escribe tu edad exacta` <-
  as.numeric(gsub("[^0-9]", "", df$`Escribe tu edad exacta`))

df$`Escribe tu edad exacta`
is.na(df$`Escribe tu edad exacta`)
summary(is.na(df$`Escribe tu edad exacta`))


df$`Escribe tu edad exacta` |> 
  is.na() |> 
  summary()


## Imputación (reemplazo por la media)

edad_prom <-
  df$`Escribe tu edad exacta` |> 
  mean(na.rm = T) |> 
  round()

ifelse(
  test = is.na(df$`Escribe tu edad exacta`),
  yes = edad_prom,
  no = df$`Escribe tu edad exacta`
)


df2 <-
  df |> 
  mutate(edad2 = ifelse(test = is.na(df$`Escribe tu edad exacta`),
                        yes = edad_prom,
                        no = df$`Escribe tu edad exacta`)) |> 
  relocate(edad2, .after = `Escribe tu edad exacta`)


## Eliminar toda la fila
df2 <- na.omit(df2)


## Estandarización de variables ----

### Normalización
scale(df2$`Escribe tu edad exacta`)

df3 <- df2 |> 
  mutate(edad_z = scale(`Escribe tu edad exacta`)) |> 
  relocate(edad_z, .after = edad2)


### Rango (0-1)
rescale(df3$`Escribe tu edad exacta`)

df3 <- df3 |> 
  mutate(edad_r = rescale(`Escribe tu edad exacta`)) |> 
  relocate(edad_r, .after = edad_z)


## Agrupaciones ----

### Rangos numéricos ----
cut(
  df3$`Escribe tu edad exacta`,
  breaks = c(-Inf, 18, 21, Inf),
  labels = c("18 o menos", "19 a 21", "Más de 21")
)

df4 <- df3 |> 
  mutate(edad_gr = cut(`Escribe tu edad exacta`,
                       breaks = c(-Inf, 18, 21, Inf),
                       labels = c("18 o menos", "19 a 21", "Más de 21"))) |> 
  relocate(edad_gr, .after = edad_r)


### Categorías ----
unique(df4$`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [No discrimino y trato a todos por igual]`)
colnames(df4)
unique(df4[,9])
df4[,9] |> factor() |> summary()

# usando ifelse, convertir la df4[,9] en 1 (Top2Box) caso contrario 0
ifelse(
  test = df4[,9] == "Un poco verdadero" | df4[,9] == "Totalmente verdadero",
  yes = 1,
  no = 0
)

ifelse(
  test = df4[,9] %in% c("Un poco verdadero", "Totalmente verdadero"),
  yes = 1,
  no = 0
)

# Bucles (loop)
## Paso 1: Crear una lista (vector) que contenga los nombres
## de las columnas que quiero cambiar
frases <- df4 |> select(starts_with("Según")) |> colnames()
df5 <- df4

## Paso 2: Ejecutar el bucle
for (frase in frases) {
  df5[,frase] <- ifelse(
    test = df4[,frase] %in% c("Un poco verdadero", "Totalmente verdadero"),
    yes = 1,
    no = 0
  )
}


