# Librerías
# install.packages("pacman")
library(pacman)
p_load(
  gsheet,
  tidyverse,
  scales,
  plotly
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
    test = df5[,frase] %in% c("Un poco verdadero", "Totalmente verdadero"),
    yes = 1,
    no = 0
  )
}


# Tema 03: Manipulación de datos ----

# Convertir el data frame en un tibble
df5 |> class()
df5 <- df5 |> as_tibble()
df5 |> class()

## Selección de columnas ----
df5 |> select(Sexo)
df5 |> select(Sexo, `Escribe tu edad exacta`)
df5 |> select(`Marca temporal`:Sexo)
df5 |> select(-`Marca temporal`)
df5 |> select(starts_with("edad"))
df5 |> select(contains("edad"))
df5 |> select(ends_with("00"))


## Selección de filas ----
df5 |> filter(Sexo == "Mujer")
df5 |> filter(Sexo != "Hombre")

df5 |>
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` >= 18, `Escribe tu edad exacta` <= 21)

df5 |>
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` >= 18 & `Escribe tu edad exacta` <= 21)

df5 |>
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(between(`Escribe tu edad exacta`, 18, 21))

df5 |> 
  select(Sexo, `Escribe tu edad exacta`, edad_gr) |> 
  filter(Sexo == "Hombre",
         edad_gr == "19 a 21")


## Nombres de las columnas
colnames(df5)
df6 <- df5

## apps

### Paso 1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "Youtube")

### Paso 2: Reemplazo
# colnames(df6)[34:37] <- apps
df6 <- df6 |> rename_with(~ apps, .cols = ends_with("00:00"))
colnames(df6)

# frases
colnames(df6) <- gsub(
  pattern = "Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: ",
  replacement = "",
  colnames(df6)
)


# Pivot ----
## Pivot Longer ----
df7 <- df6 |> 
  pivot_longer(
    cols = all_of(apps),
    names_to = "app",
    values_to = "time"
  )

df7 |> 
  select(
    `Marca temporal`,
    app,
    time
  )

## Pivot Wider
df8 <- df7 |> 
  pivot_wider(
    names_from = app,
    values_from = time
  )


# Tema 04: Valores atípicos

class(df7$time)

df7$time
strsplit("14:30:00", split = ":")

## Transformacion a número (horas)
df7$time <- sapply(
  strsplit(df7$time, split = ":"),
  function(x) {
    x <- as.numeric(x)
    x[1] + x[2]/60 + x[3]/60^2
  }
)

df7$time


## Detección gráfica

### Boxplot feo
boxplot(df7$time)

### Boxplot bonito
ggplotly(
  df7 |> 
    ggplot(aes(x = app, y = time, fill = app)) +
    geom_boxplot() +
    facet_wrap(~ Sexo) +
    theme_minimal() +
    theme(legend.position = "none")
)


## Identificación / imputación

df9 <- df7 |> 
  mutate(
    outlier = case_when(
      app == "Facebook"  & Sexo == "Hombre" & time > 10    ~ 1,
      app == "Instagram" & Sexo == "Hombre" & time > 10    ~ 1,
      app == "TikTok"    & Sexo == "Hombre" & time > 18.27 ~ 1,
      app == "Youtube"   & Sexo == "Hombre" & time > 11.57 ~ 1,
      app == "Facebook"  & Sexo == "Mujer"  & time > 10    ~ 1,
      app == "Instagram" & Sexo == "Mujer"  & time > 12    ~ 1,
      app == "TikTok"    & Sexo == "Mujer"  & time > 20    ~ 1,
      app == "Youtube"   & Sexo == "Mujer"  & time > 5.52  ~ 1,
      .default = 0
    )
  ) |> 
  mutate(
    time2 = ifelse(
      test = outlier == 1,
      yes = NA,
      no = time
    )
  )

ggplotly(
  df9 |> 
    ggplot(aes(x = app, y = time2, fill = app)) +
    geom_boxplot() +
    facet_wrap(~ Sexo) +
    theme_minimal() +
    theme(legend.position = "none")
)


# Tema 05: Aggregations ----

## Tablas de frecuencias
df9 |> 
  count(edad_gr,sort = T)

df9 |> 
  count(edad_gr) |> 
  arrange(desc(n)) |> 
  mutate(
    prop = n/sum(n),
    porcentaje = scales::percent(prop)
  )

df9 |> 
  group_by(Sexo) |> 
  count(edad_gr) |> 
  mutate(
    prop = n/sum(n),
    porcentaje = scales::percent(prop)
  )

df9 |> 
  group_by(Sexo) |> 
  count(edad_gr) |> 
  ungroup() |> 
  mutate(
    prop = n/sum(n),
    porcentaje = scales::percent(prop)
  )

df9 |> 
  group_by(app, Sexo) |> 
  summarise(
    n = n(),
    promedio = mean(time),
    desv = sd(time),
    min = min(time),
    max = max(time)
  )











