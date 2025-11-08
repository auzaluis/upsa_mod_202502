# Librerías
library(tidyverse)
library(plotly)
library(FactoMineR)
library(ggcorrplot)

frases2 <- df6[,9:32] |> colnames()

# Matriz de correlaciones
r <- cor(
  df6 |> select(all_of(frases2)),
  method = "spearman"
)

# Gráfica
ggplotly(
  ggcorrplot(
    corr = r,
    show.legend = F,
    tl.cex = 6,
    colors = c("#ffa69e", "#ffffff", "#5e6472")
  ) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank()
    )
)


# PCA: Principal Components Analysis

## Crecimiento
crecimiento <- frases2[c(4,15,19)]

pca_crecimiento <- 
  FactoMineR::PCA(
    df6 |> select(all_of(crecimiento)),
    ncp = 1
  )

pca_crecimiento$eig
pca_crecimiento$var$cor


# Status
status <- frases2[c(2,10,16)]

pca_status <- 
  FactoMineR::PCA(
    df6 |> select(all_of(status)),
    ncp = 1
  )

pca_status$eig
pca_status$var$cor
pca_status$ind$coord

## Comparando componente con vars originales
tibble(
  df6 |> select(all_of(status)),
  pca_status$ind$coord
)


# Aventura
aventura <- frases2[c(7,8)]

pca_aventura <- 
  FactoMineR::PCA(
    df6 |> select(all_of(aventura)),
    ncp = 1
  )

pca_aventura$eig
pca_aventura$var$cor
pca_aventura$ind$coord

## Comparando componente con vars originales
tibble(
  df6 |> select(all_of(aventura)),
  pca_aventura$ind$coord * -1
) |> print(n=20)


# Humilde
humildad <- frases2[c(1,6,18)]

pca_humildad <- 
  FactoMineR::PCA(
    df6 |> select(all_of(humildad)),
    ncp = 1
  )

pca_humildad$eig
pca_humildad$var$cor


# Tradición
tradicion <- frases2[c(9,12,14)]

pca_tradicion <- 
  FactoMineR::PCA(
    df6 |> select(all_of(tradicion)),
    ncp = 1
  )

pca_tradicion$eig
pca_tradicion$var$cor


# Introversión
introversion <- frases2[c(11,21,22)]

pca_introversion <- 
  FactoMineR::PCA(
    df6 |> select(all_of(introversion)),
    ncp = 1
  )

pca_introversion$eig
pca_introversion$var$cor


# Data frame con las dimensiones
df10 <- 
  df6 |> 
  mutate(
    crecimiento = pca_crecimiento$ind$coord*-1,
    status = pca_status$ind$coord,
    aventura = pca_aventura$ind$coord*-1,
    humildad = pca_humildad$ind$coord,
    tradicion = pca_tradicion$ind$coord*-1,
    introversion = pca_introversion$ind$coord
  )





















