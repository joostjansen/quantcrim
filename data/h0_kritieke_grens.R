# Steekproevenverdeling onder H0 met de kritieke grenzen (-2,2 en 2,2 glas)
# bij een significantieniveau van 5% (tweezijdig). Het gebied voorbij
# deze grenzen is rood gekleurd: daar wordt H0 verworpen.
# Het gevonden verschil (2,7 glas) staat als doorgetrokken lijn in de figuur.

library(ggplot2)

komma <- \(b) format(b, decimal.mark = ",", trim = TRUE)

plot_h0_overschrijding <- function(n_mannen = 20, n_vrouwen = 20,
                                   s_mannen = 3.5, s_vrouwen = 3.5,
                                   gevonden = 2.7, grens = 2.2,
                                   bereik_se = 4) {
  # Standaardfout van het verschil tussen twee gemiddelden
  se <- sqrt(s_mannen^2 / n_mannen + s_vrouwen^2 / n_vrouwen)
  top <- dnorm(0, sd = se)

  x <- seq(-bereik_se * se, bereik_se * se, length.out = 1000)
  data <- data.frame(verschil = x, dichtheid = dnorm(x, mean = 0, sd = se))

  # Twee staarten voorbij de kritieke grenzen
  staart_links  <- subset(data, verschil <= -grens)
  staart_rechts <- subset(data, verschil >= grens)

  ggplot(data, aes(x = verschil, y = dichtheid)) +
    geom_area(data = staart_links,  fill = "red", alpha = 0.6) +
    geom_area(data = staart_rechts, fill = "red", alpha = 0.6) +
    geom_line(linewidth = 0.9) +
    # Stippellijnen bij de kritieke grenzen, met de waarden erboven
    annotate(
      "segment", x = c(-grens, grens), xend = c(-grens, grens),
      y = 0, yend = top * 0.25, linetype = "dotted", linewidth = 0.7
    ) +
    annotate(
      "text", x = c(-grens, grens), y = top * 0.30,
      label = komma(c(-grens, grens)), size = 4
    ) +
    # Verticale lijn bij het gevonden verschil, met het getal erboven
    annotate(
      "segment", x = gevonden, xend = gevonden,
      y = 0, yend = top * 0.4, linewidth = 0.7
    ) +
    annotate(
      "text", x = gevonden, y = top * 0.45,
      label = komma(gevonden), size = 4
    ) +
    annotate(
      "segment", x = 0, xend = 0, y = 0, yend = top,
      linetype = "dashed", linewidth = 0.5
    ) +
    annotate(
      "text", x = 0, y = top * 1.06,
      label = "H[0]: mu[mannen] - mu[vrouwen] == 0",
      parse = TRUE, size = 4
    ) +
    scale_x_continuous(
      breaks = seq(-floor(bereik_se * se), floor(bereik_se * se), by = 1),
      labels = komma
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      x = "Verschil in gemiddeld aantal glazen alcohol (mannen - vrouwen)",
      y = "Kansdichtheid"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# 20 mannen en 20 vrouwen, s = 3,5, gevonden verschil = 11,4 - 8,7 = 2,7
# Kritieke grens: t(0,975; df = 38) * SE = 2,024 * 1,107 = 2,24 (afgerond 2,2)
figuur <- plot_h0_overschrijding(n_mannen = 20, n_vrouwen = 20,
                                 gevonden = 2.7, grens = 2.2)

dir.create("afbeeldingen", showWarnings = FALSE)
ggsave("afbeeldingen/h0_kritieke_grens.png", figuur,
       width = 7, height = 4, dpi = 300)
