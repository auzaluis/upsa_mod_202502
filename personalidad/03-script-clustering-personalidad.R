library(FactoMineR)
library(NbClust)
library(tidyverse)

dimensiones <- c(
  "crecimiento",
  "status",
  "aventura",
  "humildad",
  "tradicion",
  "introversion"
)

df11 <-
  df7 |> 
  pivot_wider(
    names_from = app,
    values_from = time
  ) |> 
  select(
    `Marca temporal`,
    `Escribe tu edad exacta`,
    Sexo,
    all_of(apps)
  ) |> 
  left_join(
    df10 |> 
      select(
        `Marca temporal`,
        `Escribe tu edad exacta`,
        Sexo,
        all_of(dimensiones)
      ),
    by = join_by(
      `Marca temporal`,
      `Escribe tu edad exacta`,
      Sexo
    )
  )


# Clustering
clustering <- NbClust(
  data = df11 |> select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn",
  min.nc = 4,
  max.nc = 4
)

clustering

# Tamaño de cada segmento
table(clustering$Best.partition)
table(clustering$Best.partition) |> prop.table()


df12 <- df11 |> 
  mutate(segmento = clustering$Best.partition)


# Análisis de lo segmentos
## Rasgos de personalidad
df12 |> 
  group_by(segmento) |> 
  summarise(
    crecimiento  = mean(crecimiento),
    status       = mean(status),
    aventura     = mean(aventura),
    humildad     = mean(humildad),
    tradicion    = mean(tradicion),
    introversion = mean(introversion)
  )


df12 |> 
  group_by(segmento) |> 
  summarise(
    TikTok    = mean(TikTok),
    Facebook  = mean(Facebook),
    Instagram = mean(Instagram),
    Youtube   = mean(Youtube)
  )

## Mapa perceptual
df12 |> 
  group_by(segmento) |> 
  summarise(
    crecimiento  = rescale(crecimiento)  |> mean(),
    status       = rescale(status)       |> mean(),
    aventura     = rescale(aventura)     |> mean(),
    humildad     = rescale(humildad)     |> mean(),
    tradicion    = rescale(tradicion)    |> mean(),
    introversion = rescale(introversion) |> mean()
  ) |> 
  column_to_rownames("segmento") |> 
  CA()

df12 |> 
  group_by(segmento) |> 
  summarise(
    TikTok    = mean(TikTok),
    Facebook  = mean(Facebook),
    Instagram = mean(Instagram),
    Youtube   = mean(Youtube)
  ) |> 
  column_to_rownames("segmento") |> 
  CA()


