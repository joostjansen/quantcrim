# Normale verdeling van een proportie met gearceerd 95%-interval
# Gemiddelde = 0,685, standaardfout = 0,0837
# Draai dit script vanuit de projectmap, zodat het relatieve opslagpad klopt.

library(ggplot2)

# Parameters ---------------------------------------------------------------
p_hat <- 0.685
se    <- 0.085
z     <- qnorm(0.975)              # 1,96

ci_onder <- p_hat - z * se
ci_boven <- p_hat + z * se

# Getallen met decimale komma
komma <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")

# Data voor de curve -------------------------------------------------------
curve_df <- data.frame(x = seq(p_hat - 4 * se, p_hat + 4 * se, length.out = 1000)) |>
  transform(y = dnorm(x, mean = p_hat, sd = se))

arcering_df <- subset(curve_df, x >= ci_onder & x <= ci_boven)

# Grafiek ------------------------------------------------------------------
grafiek <- ggplot(curve_df, aes(x = x, y = y)) +
  geom_area(data = arcering_df, fill = "steelblue", alpha = 0.35) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = p_hat, linetype = "dashed") +
  geom_vline(xintercept = c(ci_onder, ci_boven), linetype = "dotted", colour = "steelblue4") +
  annotate("text", x = p_hat, y = max(curve_df$y) * 1.05,
           label = paste("p =", komma(p_hat)), hjust = -0.1) +
  annotate("text", x = c(ci_onder, ci_boven), y = max(curve_df$y) * 0.15,
           label = komma(c(ci_onder, ci_boven)),
           hjust = c(1.1, -0.1), colour = "steelblue4") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = \(x) komma(x, 1)) +
  labs(
    title    = "Steekproevenverdeling van de proportie",
    subtitle = sprintf("Proportie (populatie) = %s, standaardfout = %s; gearceerd = 95%%-interval [%s; %s]",
                       komma(p_hat), komma(se), komma(ci_onder), komma(ci_boven)),
    x = "Proportie",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Opslaan ------------------------------------------------------------------
dir.create("afbeeldingen", showWarnings = FALSE)
ggsave("afbeeldingen/normal_distribution.png", grafiek,
       width = 8, height = 5, dpi = 300, bg = "white")
