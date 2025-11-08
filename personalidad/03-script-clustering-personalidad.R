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



















