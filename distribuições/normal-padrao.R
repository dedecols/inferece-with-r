library(dplyr)
library(tibble)
library(ggplot2)
library(ggtext)
library(geomtextpath)
library(ggdensity)

#### 1. Normal Univariada ####

alpha <-  0.05
mean <- 0
sd <- 1

tb <- tibble::tibble(
  x = seq(mean-4*sd, mean+4*sd, length.out = 50)
)

# 1.1 Função de densidade da Normal Padrão
p <- tb %>% ggplot2::ggplot(ggplot2::aes(x = x)) +
  ggplot2::stat_function(
    fun = dnorm,
    args = list(mean,sd),
    geom = "area",
    xlim = c(mean-5*sd, mean+5*sd),
    fill = "lightblue",
    alpha = 0.8
  ) +
  ggplot2::stat_function(
    fun = dnorm,
    args = list(mean,sd),
    geom = "line",
    linewidth = 1
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = mean-3*sd, xend = mean-3*sd,
    y = 0, yend = dnorm(mean-3*sd),
    linetype = "dashed",
    linewidth = 0.6
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = mean-2*sd, xend = mean-2*sd,
    y = 0, yend = dnorm(mean-2*sd),
    linetype = "dashed",
    linewidth = 0.6
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = mean-1*sd, xend = mean-1*sd,
    y = 0, yend = dnorm(mean-1*sd),
    linetype = "dashed",
    linewidth = 0.6
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = mean, xend = mean,
    y = 0, yend = dnorm(mean),
    linetype = "dashed",
    linewidth = 0.6,
    color = "black"
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = mean+1*sd, xend = mean+1*sd,
    y = 0, yend = dnorm(mean+1*sd),
    linetype = "dashed",
    linewidth = 0.6
  ) + 
  ggplot2::annotate(
    geom = "segment",
    x = mean+2*sd, xend = mean+2*sd,
    y = 0, yend = dnorm(mean+2*sd),
    linetype = "dashed",
    linewidth = 0.6
  ) + 
  ggplot2::annotate(
    geom = "segment",
    x = mean+3*sd, xend = mean+3*sd,
    y = 0, yend = dnorm(mean+3*sd),
    linetype = "dashed",
    linewidth = 0.6
  ) + 
  ggplot2::annotate(
    geom = "text",
    x = mean-3*sd, y = -0.01,
    label = "-3σ",
    fontface = "bold",
    color = "black",
    alpha = 1
  ) +
  ggplot2::annotate(
    geom = "text",
    x = mean-2*sd, y = -0.01,
    label = "-2σ",
    fontface = "bold",
    color = "black",
    alpha = 1
  ) +
  ggplot2::annotate(
    geom = "text",
    x = mean-1*sd, y = -0.01,
    label = "-1σ",
    fontface = "bold",
    color = "black",
    alpha = 1
  ) +
  ggplot2::annotate(
    geom = "text",
    x = mean+1*sd, y = -0.01,
    label = "1σ",
    fontface = "bold",
    color = "black",
    alpha = 1
  ) +
  ggplot2::annotate(
    geom = "text",
    x = mean+2*sd, y = -0.01,
    label = "2σ",
    fontface = "bold",
    color = "black",
    alpha = 1
  ) +
  ggplot2::annotate(
    geom = "text",
    x = mean+3*sd, y = -0.01,
    label = "3σ",
    fontface = "bold",
    color = "black",
    alpha = 1
  ) +
  ggplot2::labs(
    title = "DISTRIBUIÇÃO NORMAL PADRÃO",
    subtitle = "Simetria em torno da média"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(
      face = "bold",
      size = 15,
      hjust = 0
    ),
    plot.subtitle = ggtext::element_markdown(
      face = "plain",
      size = 12,
      hjust = 0
    ),
    axis.title = element_blank(),
    axis.text = element_text(
      face = "plain",
      size = 8
    ),
    axis.ticks = element_blank()
  ) 
p

ggplot2::ggsave(filename = "normal_simetria.png",plot = p, limitsize = FALSE)
ggplot2::ggsave(filename = "normal_simetria.pdf",plot = p)

# 1.2 Função de distribuição acumulada da Normal Padrão
ggplot2::ggplot(data = tb, ggplot2::aes(x)) +
  ggplot2::stat_function(
    geom = "line",
    fun = pnorm,
    args = list(mean,sd),
    color = "red"
  ) +
  geom_textpath(
    aes(y = pnorm(x, mean = mean, sd = sd)),
    label = "Distribuição acumulada",
    fontface = "bold",
    size = 3, 
    color = "red",
    hjust = 1,                    
    vjust = 0.01,                  
    text_only = TRUE,
    upright = TRUE
  ) +
  ggplot2::theme_classic()


#### 2. Normal Padrão bivariada ####

# Dados de duas normais padrão 
tb <- tibble::tibble(
  x = rnorm(1000, mean = 0, sd = 1),
  y = rnorm(1000, mean = 0, sd = 1)
)


ggplot2::ggplot(data = tb, ggplot2::aes(x,y)) +
  ggplot2::geom_density2d()

ggplot2::ggplot(data = tb, ggplot2::aes(x,y)) +
  ggdensity::geom_hdr() +
  ggplot2::geom_point() +
  ggplot2::theme_classic() +
  ggplot2::labs(
    title = "VISUALIZAÇÃO DA NORMAL BIVARIADA PADRÃO",
    subtitle = "Regiões mais escuras indicam maior concentração de valores de X e Y",
    x = "Valores de X",
    y = "Valores de Y",
    caption = "Gráfico gerado no R"
  ) +
  ggplot2::theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(face = "plain")
  )












