
mapa <- shape_uf |> 
  ggplot() +
  geom_sf(aes(fill = prop_filiados)) +
  scale_fill_viridis_c(name = "Proporção (%)",
                       guide = guide_legend(keyheight = unit(5, units = "mm"),
                                            keywidth = unit(8, units = "mm"),
                                            label.position = "top",
                                            title.position = "top",
                                            reverse = FALSE)) +
  theme_void() +
  labs(
    title = "Mapa da proporção de filiados por Estado",
    subtitle = "Em relação ao total de eleitores de cada Estado (atualizado em 07/2023)",
    caption = "Fonte: Portal de Dados Abertos | TSE",
    fill = "Proporção (%)"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", colour = NA),
    panel.background = element_rect(fill = "#f5f5f2", colour = NA),
    legend.background = element_rect(fill = "#f5f5f2", colour = NA),
    plot.title = element_text(size= 16, 
                              color = "#4e4d47",
                              face = "bold",
                              family = "Calibri"),
    plot.subtitle = element_text(size= 14,
                                 color = "#4e4d47",
                                 family = "Calibri"),
    plot.caption = element_text(size=12, 
                                color = "#4e4d47",
                                hjust = .05,
                                vjust = 5),
    legend.position = c(.90, .1),
    legend.direction = "horizontal"
  )
  
  
ggsave(filename = "mapa.jpg",
       plot = mapa,
       dpi = 900)
