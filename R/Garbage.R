library("ggplot2")
library("grid")
library("dplyr")

df <- data.frame(
  day = c(rep(11, 6), rep(12, 6), rep(23, 9), rep(31, 9), rep(47, 9)),
  hz = c(
    rep(1, 3),
    rep(2, 3),
    rep(1, 3),
    rep(2, 3),
    rep(1, 3),
    rep(2, 3),
    rep(4, 3) ,
    rep(1, 3),
    rep(2, 3),
    rep(4, 3),
    rep(1, 3),
    rep(2, 3),
    rep(4, 3)
  ),
  ft = c(
    38,
    42,
    45,
    28,
    30,
    45,
    38,
    41,
    45,
    25,
    30,
    45,
    45,
    43,
    50,
    30,
    28,
    48,
    30,
    33,
    40,
    45,
    41,
    47,
    38,
    35,
    43,
    30,
    32,
    50,
    46,
    40,
    50,
    38,
    30,
    40,
    30,
    28,
    40
  ),
  sp = c(rep(c(
    "Ctrl", "Absrc", "Ab42"
  ), 13))
)

se <- function(a)  sd(a) / sqrt(length(a))



ggplot(df, aes(
  x = interaction(hz, day),
  y = ft,
  group = desc(sp),
  fill = sp
)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymax = ft + se(ft), ymin = ft - se(ft)),
                position = position_dodge(0.9),
                width = 0.5) +
  scale_x_discrete(labels = ~ paste(substr(.x, 1, 1), "hz")) +
  facet_grid(
    . ~ day,
    switch = "x",
    scales = "free_x",
    space = "free_x"
  #  labeller = labeller(day = ~ paste("day", .x))
  ) +
  coord_cartesian(ylim = c(0, 60), clip = "off") +
  theme_classic() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    axis.title.x = element_blank(),
    strip.background.x = element_blank(),
    strip.placement = "outside",
    #panel.spacing.x = unit(0, "pt")
  ) +
  scale_fill_grey(breaks = c("Ctrl", "Absrc", "Ab42")) +
  scale_y_continuous(expand = c(0, 0))





dadosI |>
  mutate(eixo_x = factor(interaction(Metodo, Var, sep = "|"))) |>
  mutate(
    posicao_x = case_match(
      eixo_x,
      "Stepwise|X1" ~ 1,
      "MARS|X1" ~ 2,
      "Random Forest|X1" ~ 3,
      "Bagging|X1" ~ 4,
      "Garson|X1" ~ 5,
      "Olden|X1" ~ 6,
      "Stepwise|X2" ~ 8,
      "MARS|X2" ~ 9,
      "Random Forest|X2" ~ 10,
      "Bagging|X2" ~ 11,
      "Garson|X2" ~ 12,
      "Olden|X2" ~ 13,
      "Stepwise|X3" ~ 15,
      "MARS|X3" ~ 16,
      "Random Forest|X3" ~ 17,
      "Bagging|X3" ~ 18,
      "Garson|X3" ~ 19,
      "Olden|X3"  ~  20

    )
  ) |>
  ggplot(aes(
    x = posicao_x,
    # Usar a variável numérica para espaçamento
    y = Freq,
    fill = Ordem,
    label = Freq
  )) +
  geom_bar(position = "stack",
           stat = "identity",
           width = 0.8) +  # Ajuste da largura
  labs(x = "Método", y = "Frequência") +
  scale_x_discrete(guide = guide_axis_nested(delim = "|"), name = NULL) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_bw() +
  ggsci::scale_fill_jco() +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(
      size = 10,
      color = "black",
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.y = element_blank(),
    legend.title = element_text(
      size = 12,
      color = "black",
      face = "bold"
    ),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.text = element_text(size = 10.5),
    strip.text = element_textbox(
      size = 10,
      face = "bold",
      halign = 0.5
    ),
    panel.grid = element_blank()
  ) +
  facet_wrap(vars(Dados), nrow = 3, scales = "free_x") +
  facetted_pos_scales(x = list(Dados %in% c("50I", "100I") ~
                                 scale_x_discrete(labels = NULL)))
