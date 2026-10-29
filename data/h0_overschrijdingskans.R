# Steekproevenverdeling onder H0 met het gebied vanaf het gevonden
# verschil (2,7 glas) rood gekleurd: de (eenzijdige) overschrijdingskans.

library(ggplot2)

plot_h0_overschrijding <- function(n_mannen = 20, n_vrouwen = 20,
                                   s_mannen = 3.5, s_vrouwen = 3.5,
                                   gevonden = 2.7, bereik_se = 4) {
  # Standaardfout van het verschil tussen twee gemiddelden
  se <- sqrt(s_mannen^2 / n_mannen + s_vrouwen^2 / n_vrouwen)

  x <- seq(-bereik_se * se, bereik_se * se, length.out = 1000)
  data <- data.frame(verschil = x, dichtheid = dnorm(x, mean = 0, sd = se))
  staart <- subset(data, verschil >= gevonden)

  ggplot(data, aes(x = verschil, y = dichtheid)) +
    geom_line(linewidth = 0.9) +
    # Verticale lijn bij het gevonden verschil, met het getal erboven
    annotate(
      "segment", x = gevonden, xend = gevonden,
      y = 0, yend = dnorm(0, sd = se) * 0.4, linewidth = 0.7
    ) +
    annotate(
      "text", x = gevonden, y = dnorm(0, sd = se) * 0.45,
      label = format(gevonden, decimal.mark = ","), size = 4
    ) +
    annotate(
      "segment", x = 0, xend = 0, y = 0, yend = dnorm(0, sd = se),
      linetype = "dashed", linewidth = 0.5
    ) +
    annotate(
      "text", x = 0, y = dnorm(0, sd = se) * 1.06,
      label = "H[0]: mu[mannen] - mu[vrouwen] == 0",
      parse = TRUE, size = 4
    ) +
    scale_x_continuous(
      breaks = seq(-floor(bereik_se * se), floor(bereik_se * se), by = 1),
      labels = \(b) format(b, decimal.mark = ",", trim = TRUE)
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
figuur <- plot_h0_overschrijding(n_mannen = 20, n_vrouwen = 20, gevonden = 2.7)

dir.create("afbeeldingen", showWarnings = FALSE)
ggsave("afbeeldingen/h0_overschrijdingskans.png", figuur,
       width = 7, height = 4, dpi = 300)
