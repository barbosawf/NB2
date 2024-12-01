# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)
# Remover o # se for necessário instalar o pacote ggh4x
# devtools::install_github("teunbrand/ggh4x")
library(ggh4x)
library(ggtext)



# Useful Functions --------------------------------------------------------


create_dir_as_doc <-
  readRDS("Useful_Functions/create_dir_as_doc.rds")



create_full_dir <-
  readRDS("Useful_Functions/create_full_dir.rds")



# Data --------------------------------------------------------------------


list.files("Data")


files_dadosI <- list.files("Data", pattern = "^dadosI[0-9]+\\.xlsx$")


# Para dados sem interação, supondo que na pasta Data os nomes os arquivos xlsx
# sejam dados50.xlsx, dados100.xlsx..., ou seja, sem o I depois da palavra dados.

# files_dados <- list.files("Data", pattern = "^dados[0-9]+\\.xlsx$")


# Dados com interação
str_c("Data/", files_dadosI) |>
  set_names(c("50I", "100I", "500I")) |>
  map_dfr(read_xlsx, .id = "Dados") |>
  mutate_at(vars(Freq), ~ case_match(., 0 ~ NA, .default = .)) |>
  mutate_at(vars(Metodo), ~ str_replace_all(., "_", " ")) |>
  mutate_at(vars(Ordem), ~ str_replace_all(., "1", "1ª")) |>
  mutate_at(vars(Ordem), ~ str_replace_all(., "2", "2ª")) |>
  mutate_at(vars(Ordem), ~ str_replace_all(., "3", "3ª")) |>
  mutate_if(is_character, as_factor) |>
  mutate_at(vars(Ordem), as_factor) |>
  mutate_at(
    vars(Metodo),
    ~ fct_relevel(
      .,
      "Stepwise",
      "MARS",
      "Random Forest",
      "Bagging",
      "Garson",
      "Olden"
    )
  ) ->
  dadosI


dadosI


# Dados sem interação
# str_c("Data/", files_dados) |>
#   set_names(c("50I", "100I", "500I")) |>
#   map_dfr(read_xlsx, .id = "Dados") |>
#   mutate_at(vars(Freq), ~ case_match(., 0 ~ NA, .default = .)) |>
#   mutate_at(vars(Metodo), ~ str_replace_all(., "_", " ")) |>
#   mutate_at(vars(Ordem), ~ str_replace_all(., "1", "1ª")) |>
#   mutate_at(vars(Ordem), ~ str_replace_all(., "2", "2ª")) |>
#   mutate_at(vars(Ordem), ~ str_replace_all(., "3", "3ª")) |>
#   mutate_if(is_character, as_factor) |>
#   mutate_at(vars(Ordem), as_factor) |>
#   mutate_at(
#     vars(Metodo),
#     ~ fct_relevel(
#       .,
#       "Stepwise",
#       "MARS",
#       "Random Forest",
#       "Bagging",
#       "Garson",
#       "Olden"
#     )
#   ) ->
#   dados
#
#
# dados


# Graphics with interaction -----------------------------------------------


dadosI |>
  ggplot(aes(
    x = interaction(Metodo, Var, sep = "|"),
    y = Freq,
    fill = Ordem,
    label = Freq
  )) +
  geom_col(position = "stack",
           just = 0.5,
           width = 0.7) +
  labs(x = "Método", y = "Frequência") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_bw() +
  ggsci::scale_fill_jco() +
  scale_x_discrete(guide = guide_axis_nested(delim = "|"), name = NULL) +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(
      size = 10,
      color = "black",
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    legend.title = element_text(
      size = 12,
      color = "black",
      face = "bold"
    ),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.text = element_text(size = 10),
    strip.text = element_textbox(
      size = 12,
      face = "bold",
      halign = 0.5,
      margin = margin(1, 0, 1, 0, "mm")
    ),
    panel.grid = element_blank()
  ) +
  facet_wrap(vars(Dados), nrow = 3, scales = "free_x") +
  facetted_pos_scales(x = list(
    Dados %in% c("50I", "100I") ~ scale_x_discrete(labels = NULL, breaks = NULL)
  )) ->
  graphic_int_typeI


graphic_int_typeI


dadosI |>
  ggplot(aes(
    x = Metodo,
    y = Freq,
    fill = Ordem,
    label = Freq
  )) +
  geom_col(position = "stack",
           just = 0.5,
           width = 0.7) +
  labs(x = "Método", y = "Frequência") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_bw() +
  ggsci::scale_fill_jco() +
  scale_x_discrete(name = NULL) +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(
      size = 10,
      color = "black",
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    legend.title = element_text(
      size = 12,
      color = "black",
      face = "bold"
    ),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.text = element_text(size = 10),
    strip.text = element_textbox(
      size = 12,
      face = "bold",
      halign = 0.5,
      margin = margin(1, 0, 1, 0, "mm")
    ),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_nested_wrap(vars(Dados, Var), nrow = 3, scales = "free_x") +
  facetted_pos_scales(x = list(
    Dados %in% c("50I", "100I") ~ scale_x_discrete(labels = NULL, breaks = NULL)
  )) ->
  graphic_int_typeII


graphic_int_typeII


dadosI$Dados |>
  unique() ->
  names_Dados_dadosI


names(names_Dados_dadosI) <- names_Dados_dadosI

x <- names_Dados_dadosI[1]


dadosI |>
  filter_at(vars(Dados), all_vars(. == x)) |>
  ggplot(aes(
    x = interaction(Metodo, Var),
    y = Freq,
    fill = Ordem,
    label = Freq
  )) +
  geom_col(position = "stack",
           just = 0.5,
           width = 0.7) +
  labs(x = "Método", y = NULL) +
  facet_grid(
    . ~ Var,
    switch = "x",
    scales = "free_x",
    space = "free_x"
  ) +
  scale_x_discrete(labels = ~ str_replace(.x, "[.]+(..)", ""), name = NULL) +
  coord_cartesian(ylim = c(0, 10), clip = "off") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(
      size = 10,
      color = "black",
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    legend.title = element_text(
      size = 14,
      color = "black",
      face = "bold"
    ),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.text = element_text(size = 12),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.background.x = element_blank(),
    strip.placement = "outside",
    panel.spacing.x = unit(15, "pt"),
    strip.text.x.bottom = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  ) +
  ggsci::scale_fill_jco() +
  scale_y_continuous(expand = c(0, 0))



# Graphics without interaction -------------------------------------------


# dados |>
#   ggplot(aes(
#     x = interaction(Metodo, Var, sep = "|"),
#     y = Freq,
#     fill = Ordem,
#     label = Freq
#   )) +
#   geom_col(position = "stack",
#            just = 0.5,
#            width = 0.7) +
#   labs(x = "Método", y = "Frequência") +
#   geom_text(size = 3, position = position_stack(vjust = 0.5)) +
#   theme_bw() +
#   ggsci::scale_fill_jco() +
#   scale_x_discrete(guide = guide_axis_nested(delim = "|"), name = NULL) +
#   theme(
#     axis.text.y = element_text(size = 10, color = "black"),
#     axis.text.x = element_text(
#       size = 10,
#       color = "black",
#       angle = 90,
#       vjust = 0.5,
#       hjust = 1
#     ),
#     axis.title.y = element_text(size = 14, face = "bold"),
#     axis.title.x = element_blank(),
#     legend.title = element_text(
#       size = 12,
#       color = "black",
#       face = "bold"
#     ),
#     axis.ticks.length = unit(0.2, "cm"),
#     legend.position = "top",
#     legend.text = element_text(size = 10),
#     strip.text = element_textbox(
#       size = 12,
#       face = "bold",
#       halign = 0.5,
#       margin = margin(1, 0, 1, 0, "mm")
#     ),
#     panel.grid = element_blank()
#   ) +
#   facet_wrap(vars(Dados), nrow = 3, scales = "free_x") +
#   facetted_pos_scales(x = list(
#     Dados %in% c("50I", "100I") ~ scale_x_discrete(labels = NULL, breaks = NULL)
#   )) ->
#   graphic_typeI
#
#
# graphic_typeI
#
#
# dados |>
#   ggplot(aes(
#     x = Metodo,
#     y = Freq,
#     fill = Ordem,
#     label = Freq
#   )) +
#   geom_col(position = "stack",
#            just = 0.5,
#            width = 0.7) +
#   labs(x = "Método", y = "Frequência") +
#   geom_text(size = 3, position = position_stack(vjust = 0.5)) +
#   theme_bw() +
#   ggsci::scale_fill_jco() +
#   scale_x_discrete(name = NULL) +
#   theme(
#     axis.text.y = element_text(size = 10, color = "black"),
#     axis.text.x = element_text(
#       size = 10,
#       color = "black",
#       angle = 90,
#       vjust = 0.5,
#       hjust = 1
#     ),
#     axis.title.y = element_text(size = 14, face = "bold"),
#     axis.title.x = element_blank(),
#     legend.title = element_text(
#       size = 12,
#       color = "black",
#       face = "bold"
#     ),
#     axis.ticks.length = unit(0.2, "cm"),
#     legend.position = "top",
#     legend.text = element_text(size = 10),
#     strip.text = element_textbox(
#       size = 12,
#       face = "bold",
#       halign = 0.5,
#       margin = margin(1, 0, 1, 0, "mm")
#     ),
#     panel.grid = element_blank()
#   ) +
#   facet_nested_wrap(vars(Dados, Var), nrow = 3, scales = "free_x") +
#   facetted_pos_scales(x = list(
#     Dados %in% c("50I", "100I") ~ scale_x_discrete(labels = NULL, breaks = NULL)
#   )) ->
#   graphic_typeII
#
#
# graphic_typeII



# Directories -------------------------------------------------------------


create_dir_as_doc()


list(jpg = "Graphics/JPG",
     svg = "Graphics/SVG",
     pdf = "Graphics/PDF") ->
  dirs


dirs |>
  walk(create_full_dir)



# Saving graphics ---------------------------------------------------------


# Para dados com interação
list(graphic_int_typeI = graphic_int_typeI,
     graphic_int_typeII = graphic_int_typeII) |>
  iwalk(\(x, idx) {

    dirs |>
      iwalk(\(y, idy) {

        ggsave(filename = paste0(idx, ".", idy),
               plot = x,
               path = y,
               units = "mm",
               height = 250,
               width = 210
              )

      })



  })



# Para dados sem interação
# list(graphic_typeI = graphic_typeI,
#      graphic_typeII = graphic_typeII) |>
#   iwalk(\(x, idx) {
#
#     dirs |>
#       iwalk(\(y, idy) {
#
#         ggsave(filename = paste0(idx, ".", idy),
#                plot = x,
#                path = y,
#                units = "mm",
#                height = 250,
#                width = 210
#         )
#
#       })
#
#
#
#   })



# Useful Links ------------------------------------------------------------

# Graphic with interaction
# https://stackoverflow.com/questions/75306882/adding-gaps-at-specific-intervals-on-the-x-axis-in-a-ggplot-bar-graph


# To add statistics
# https://github.com/kassambara/ggpubr/issues/65
