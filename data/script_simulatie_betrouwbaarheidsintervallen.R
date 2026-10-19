# Simulatie: 100 keer een 95%-betrouwbaarheidsinterval voor een proportie
# Populatieproportie = 0,685; steekproefgrootte n = 30
# Draai dit script vanuit de projectmap, zodat het relatieve opslagpad klopt.

library(ggplot2)

# Parameters ---------------------------------------------------------------
p_pop   <- 0.685                   # populatieproportie
n       <- 30                      # steekproefgrootte
n_sim   <- 100                     # aantal steekproeven
z       <- qnorm(0.975)            # 1,96
doel_mis <- 5                      # gewenst aantal intervallen dat p_pop mist (5%)

# Getallen met decimale komma
komma <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")

# Simulatie ----------------------------------------------------------------
# Per steekproef: aantal 'successen' uit n personen, steekproefproportie,
# standaardfout en 95%-interval (p ± 1,96 * SE)
simuleer <- function(seed) {
  set.seed(seed)
  data.frame(steekproef = 1:n_sim,
             successen  = rbinom(n_sim, size = n, prob = p_pop)) |>
    transform(p_hat = successen / n) |>
    transform(se    = sqrt(p_hat * (1 - p_hat) / n)) |>
    transform(ondergrens = p_hat - z * se,
              bovengrens = p_hat + z * se) |>
    transform(bevat_pop = ondergrens <= p_pop & bovengrens >= p_pop)
}

# Zoek de eerste seed waarbij precies 'doel_mis' intervallen p_pop niet bevatten
seed <- 1
repeat {
  sim_df <- simuleer(seed)
  if (sum(!sim_df$bevat_pop) == doel_mis) break
  seed <- seed + 1
}
cat(sprintf("Gebruikte seed: %d\n", seed))

sim_df$status <- factor(ifelse(sim_df$bevat_pop,
                               "Bevat populatieproportie",
                               "Bevat populatieproportie niet"),
                        levels = c("Bevat populatieproportie",
                                   "Bevat populatieproportie niet"))

n_mis <- sum(!sim_df$bevat_pop)
cat(sprintf("%d van de %d intervallen bevatten de populatieproportie (%d%%).\n",
            n_sim - n_mis, n_sim, round(100 * (n_sim - n_mis) / n_sim)))

# Grafiek ------------------------------------------------------------------
grafiek <- ggplot(sim_df, aes(y = steekproef, colour = status)) +
  geom_vline(xintercept = p_pop, linetype = "dashed", colour = "black") +
  geom_segment(aes(x = ondergrens, xend = bovengrens, yend = steekproef),
               linewidth = 0.6) +
  geom_point(aes(x = p_hat), size = 1.2) +
  scale_colour_manual(values = c("Bevat populatieproportie"      = "grey60",
                                 "Bevat populatieproportie niet" = "firebrick"),
                      name = NULL) +
  scale_x_continuous(breaks = seq(0, 1.2, by = 0.1),
                     labels = \(x) komma(x, 1)) +
  scale_y_reverse(breaks = c(1, seq(10, n_sim, by = 10)), expand = c(0.01, 0.01)) +
  labs(
    title    = sprintf("%d betrouwbaarheidsintervallen van 95%% (n = %d per steekproef)", n_sim, n),
    subtitle = sprintf("Stippellijn = populatieproportie (%s); %d van de %d intervallen bevatten deze niet",
                       komma(p_pop), n_mis, n_sim),
    x = "Proportie",
    y = "Steekproef"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Opslaan ------------------------------------------------------------------
dir.create("afbeeldingen", showWarnings = FALSE)
ggsave("afbeeldingen/simulatie_betrouwbaarheidsintervallen.png", grafiek,
       width = 8, height = 10, dpi = 300, bg = "white")
