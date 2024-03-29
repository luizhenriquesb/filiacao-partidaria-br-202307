
# Mapa personalizado

library(tidyverse)
library(geobr)
library(extrafont)

##### Importando as bases de dados do TSE ----

# Base sobre filiação partidária mensal ----

filiados_mensais <- read_delim("dados/filiado_mensal.csv.zip", delim = ";", 
                 locale = locale(encoding = "latin1")) |> 
  janitor::clean_names()
  
# Base com o total de eleitores por Estado ----

eleitorado_total <- read_delim("dados/eleitorado_mensal.csv.zip") |> 
  janitor::clean_names()

# Juntando as bases ----

dados_eleitorado_total <- eleitorado_total |> 
  # Função para juntar as bases
  left_join(
    # Juntando a base "pop_total" na base "dados" a partir da coluna "uf"
    filiados_mensais, by = c("uf"))

# Importando base com as coordenadas dos Estados ----

shape <- geobr::read_state(code_state = "all", year = 2010)

# Criando objeto com o percentual de filiados por Estado ----

perc_filiados_uf <- dados_eleitorado_total |>
  # Agrupando por Estado e população total
  group_by(uf, quantidade_de_eleitor) |> 
  # Somando o total de pessoas filiadas
  summarise(total_filiados = sum(quantidade_de_filiados)) |> 
  # Criando coluna com o percentual de pessoas filiadas em relação à população
  # total do Estado
  mutate(perc_filiados = total_filiados/quantidade_de_eleitor)

# Adiconando as coordenadas de cada Estado ao dataset acima ----

shape_uf <- shape |> 
  # Cria uma coluna de nome "uf" com os valores da coluna "abbrev_state"
  mutate(uf = abbrev_state) |> 
  # Função para juntar as bases
  left_join(
    # Juntando a base "shape" na base "prop_filiados_uf" a partir da coluna "uf"
    perc_filiados_uf, by = c("uf")) |>
  # Selecionando somente as colunas de interesse para fazer o mapa
  select(uf, perc_filiados, geom)

# Função que carrega diferentes tipos de fontes

extrafont::loadfonts("win")

# Fazendo o mapa ----

# Referência: <https://r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html>

shape_uf |> 
  ggplot() +
  geom_sf(aes(fill = perc_filiados)) +
  scale_fill_viridis_c(labels = scales::percent,
                       name = "",
                       guide = guide_legend(keyheight = unit(5, units = "mm"),
                                            keywidth = unit(8, units = "mm"),
                                            label.position = "left",
                                            title.position = "top",
                                            reverse = TRUE,
                                            byrow = FALSE)) +
  theme_void() +
  labs(
    title = "Mapa do percentual de filiados por Estado",
    subtitle = "Em relação ao total de eleitores de cada Estado (atualizado em 07/2023)",
    caption = "Fonte: Portal de Dados Abertos | TSE",
    fill = "Percentual (%)"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", colour = NA),
    panel.background = element_rect(fill = "#f5f5f2", colour = NA),
    legend.background = element_rect(fill = "#f5f5f2", colour = NA),
    plot.title = element_text(size= 14, 
                              color = "#4e4d47",
                              face = "bold",
                              family = "Calibri",
                              hjust = 0),
    plot.subtitle = element_text(size= 13,
                                 color = "#4e4d47",
                                 family = "Calibri",
                                 hjust = 0),
    plot.caption = element_text(size=11, 
                                color = "#4e4d47",
                                hjust = 0,
                                vjust = 5),
    legend.position = c(.90, .2),
    legend.direction = "vertical",
    legend.title = element_text(size= 12,
                                color = "#4e4d47",
                                family = "Calibri"))

# Salvando o mapa em uma imagem

ggsave(filename = "mapa.jpg",
       plot = mapa,
       dpi = 900)

