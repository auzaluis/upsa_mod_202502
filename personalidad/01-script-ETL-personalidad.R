# Librerías
# install.packages("pacman")
library(pacman)
p_load(
  gsheet,
  tidyverse
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

# crear un df que se llame df3 que tenga una nueva columna que se llame
# edad_z que contenga el valor normalizado de 'escribe tu edad exacta'
# ubicarla al lado de edad2




