library(dplyr)
library(ggplot2)
library(tibble)

# Gerar dados de uma t-Student
n <- 300000
df <- 6
eixo_x <- stats::rt(n = n, df = df)
eixo_y <- stats::dt(eixo_x, df = df)
tib <- tibble::tibble(eixo_x,eixo_y)

alpha <- 0.05
t_inferior <- qt(p = alpha/2, df = df)
t_superior <- qt(p = 1-alpha/2, df = df)

# Área de rejeição à esquerda
area_cauda_esquerda <- tibble::tibble(
  x = seq(-4, t_inferior, length.out = 100),
  y = dt(x, df = df)
)

# Área de rejeição à direita
area_cauda_direita <- tibble::tibble(
  x = seq(t_superior, 4, length.out = 100),
  y = dt(x, df = df)
)

ponto_esquerda <- area_cauda_esquerda %>% 
  dplyr::filter(x <= t_inferior) %>%  # Pega o ponto mais à esquerda
  dplyr::mutate(label = "Rejeita H0")


# Distribuição dos dados gerados (geom_density estima os valores da densidade)
ggplot2::ggplot(
  data = tib, 
  aes(x = eixo_x)
  ) +
  ggplot2::geom_density(fill = NA, linewidth = 1) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_vline(xintercept = -4) +
  ggplot2::geom_ribbon(
    data = area_cauda_esquerda,
    aes(x = x, ymin = 0, ymax = y),
    fill = 'red4', alpha = 0.3
  ) +
  ggplot2::geom_ribbon(
    data = area_cauda_direita,
    aes(x = x, ymin = y, ymax = 0),
    fill = 'red4', alpha = 0.3
  ) +
  # ggforce::geom_mark_circle(
  #   data = ponto_esquerda,
  #   stat = 'identity',
  #   position = 'identity',
  #   aes(x = x, x0 = t_inferior, 
  #       y = y, 
  #       label = label),
  #   expand = unit(-1, "mm"),  # Tamanho do círculo
  #   radius = unit(1, "mm"),
  #   label.buffer = unit(0.1, "cm"),  # Distância do texto
  #   con.colour = "red4",  # Cor da linha de conexão
  #   con.type = 'straight',
  #   label.fontsize = 12,
  #   label.hjust = 0
  # ) +
  ggplot2::labs(
    title = 'DISTRIBUIÇÃO DE PROBABILIDADE',
    subtitle = 't-Student com 6 graus de liberdade'
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-4,4),
    breaks = c(t_inferior,0,t_superior),
    labels = function(x) {
      ifelse(x == 0, '0', format(round(x,2)))
    }
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = 'bold'),
    plot.subtitle = ggplot2::element_text(),
    plot.background = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(face = 'plain', size = 11),
    axis.ticks = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = 'white', color = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = 'none'
  ) 

df <- 6

tb <- tibble::tibble(
  x = rt(1000,df)
)

ggplot2::ggplot(tb, ggplot2::aes(x)) +
  ggplot2::stat_function(
    geom = "line",
    fun = dt,
    args = list(df),
    linewidth = 1
  ) +
  ggplot2::labs(
    title = "DISTRIBUIÇÃO DE PROBABILIDADE",
    subtitle = "t-Student 6",
    x = "Valores da t",
    y = "Densidade",
    caption = "Gráfico gerado no R"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-6,6)
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(face = "plain", size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 11)
  )








