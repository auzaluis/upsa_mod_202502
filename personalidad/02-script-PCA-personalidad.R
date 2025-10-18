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



