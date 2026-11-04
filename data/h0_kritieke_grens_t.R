# t-verdeling onder H0 met de kritieke t-waarden (-2,02 en 2,02) bij een
# significantieniveau van 5% (tweezijdig). Het gebied voorbij deze grenzen
# is rood gekleurd: daar wordt H0 verworpen.
# De gevonden t-waarde (2,44) staat als doorgetrokken lijn in de figuur.

library(ggplot2)

komma <- \(b) format(b, decimal.mark = ",", trim = TRUE)

plot_h0_t <- function(n_mannen = 20, n_vrouwen = 20,
                      s_mannen = 3.5, s_vrouwen = 3.5,
                      verschil = 2.7, alpha = 0.05, bereik_t = 4) {
  # Standaardfout, vrijheidsgraden, gevonden t en kritieke t
  se       <- sqrt(s_mannen^2 / n_mannen + s_vrouwen^2 / n_vrouwen)
  df       <- n_mannen + n_vrouwen - 2
  gevonden <- round(verschil / se, 2)
  grens    <- round(qt(1 - alpha / 2, df = df), 2)
  top      <- dt(0, df = df)

  t <- seq(-bereik_t, bereik_t, length.out = 1000)
  data <- data.frame(t = t, dichtheid = dt(t, df = df))

  # Twee staarten voorbij de kritieke t-waarden
  staart_links  <- subset(data, t <= -grens)
  staart_rechts <- subset(data, t >= grens)

  ggplot(data, aes(x = t, y = dichtheid)) +
    geom_area(data = staart_links,  fill = "red", alpha = 0.6) +
    geom_area(data = staart_rechts, fill = "red", alpha = 0.6) +
    geom_line(linewidth = 0.9) +
    # Stippellijnen bij de kritieke t-waarden, met de waarden erboven
    annotate(
      "segment", x = c(-grens, grens), xend = c(-grens, grens),
      y = 0, yend = top * 0.25, linetype = "dotted", linewidth = 0.7
    ) +
    annotate(
      "text", x = c(-grens, grens), y = top * 0.30,
      label = komma(c(-grens, grens)), size = 4
    ) +
    # Verticale lijn bij de gevonden t-waarde, met het getal erboven
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
    scale_x_continuous(breaks = -bereik_t:bereik_t, labels = komma) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      x = paste0("t-waarde (df = ", df, ")"),
      y = "Kansdichtheid"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# 20 mannen en 20 vrouwen, s = 3,5, gevonden verschil = 11,4 - 8,7 = 2,7
# Gevonden t = 2,7 / 1,107 = 2,44; kritieke t(0,975; df = 38) = 2,02
figuur <- plot_h0_t(n_mannen = 20, n_vrouwen = 20, verschil = 2.7)

dir.create("afbeeldingen", showWarnings = FALSE)
ggsave("afbeeldingen/h0_kritieke_grens_t.png", figuur,
       width = 7, height = 4, dpi = 300)
