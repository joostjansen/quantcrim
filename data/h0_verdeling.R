# Steekproevenverdeling onder H0: verschil in gemiddeld aantal glazen
# alcohol (mannen - vrouwen). Onder H0 is het verschil in de populatie 0,
# dus de verdeling van alle mogelijke steekproefverschillen ligt rond 0.

library(ggplot2)

plot_h0_verdeling <- function(n_mannen = 20, n_vrouwen = 20,
                              s_mannen = 3.5, s_vrouwen = 3.5,
                              bereik_se = 4) {
  # Standaardfout van het verschil tussen twee gemiddelden
  se <- sqrt(s_mannen^2 / n_mannen + s_vrouwen^2 / n_vrouwen)

  x <- seq(-bereik_se * se, bereik_se * se, length.out = 1000)
  data <- data.frame(verschil = x, dichtheid = dnorm(x, mean = 0, sd = se))

  ggplot(data, aes(x = verschil, y = dichtheid)) +
    geom_line(linewidth = 0.9) +
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

# 30 respondenten: 15 mannen en 15 vrouwen, sd = 3,5 glas per groep
figuur <- plot_h0_verdeling(n_mannen = 20, n_vrouwen = 20)

dir.create("afbeeldingen", showWarnings = FALSE)
ggsave("afbeeldingen/h0_verdeling.png", figuur, width = 7, height = 4, dpi = 300)
