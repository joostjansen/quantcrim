# Voorbeeldfiguur: correlatie tussen alcohol- en sigarettengebruik op een uitgaansdag
# Vijf fictieve respondenten; r ≈ 0,35

library(ggplot2)

# Data -------------------------------------------------------------------
drugs <- data.frame(
  respondent                  = 1:5,
  glazen_alcohol_uitgaansdag  = c(2, 4, 5, 7, 12),
  sigaretten_uitgaansdag      = c(2, 4, 8, 6, 5)
)

gem_x <- mean(drugs$glazen_alcohol_uitgaansdag)   # 6
gem_y <- mean(drugs$sigaretten_uitgaansdag)       # 5
r     <- cor(drugs$glazen_alcohol_uitgaansdag, drugs$sigaretten_uitgaansdag)

# Stappen van de berekening (handig voor in de tekst)
drugs |>
  transform(
    afw_x    = glazen_alcohol_uitgaansdag - gem_x,
    afw_y    = sigaretten_uitgaansdag - gem_y
  ) |>
  transform(product = afw_x * afw_y) |>
  print()
cat("r =", round(r, 2), "\n")

# Uitgelicht datapunt ----------------------------------------------------
punt <- drugs[drugs$respondent == 1, ]
px <- punt$glazen_alcohol_uitgaansdag
py <- punt$sigaretten_uitgaansdag

# Figuur -----------------------------------------------------------------
fig <- ggplot(drugs, aes(glazen_alcohol_uitgaansdag, sigaretten_uitgaansdag)) +
  # gemiddelden
  geom_vline(xintercept = gem_x, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = gem_y, linetype = "dashed", colour = "grey40") +
  annotate("text", x = gem_x, y = 9.6, label = paste0("bar(x) == ", gem_x),
           parse = TRUE, hjust = -0.15, colour = "grey30") +
  annotate("text", x = 13.4, y = gem_y, label = paste0("bar(y) == ", gem_y),
           parse = TRUE, vjust = -0.6, hjust = 1, colour = "grey30") +
  # afstand tot gemiddelde x (horizontaal) en tot gemiddelde y (verticaal)
  annotate("segment", x = px, xend = gem_x, y = py, yend = py,
           colour = "#D55E00", linewidth = 0.9) +
  annotate("segment", x = px, xend = px, y = py, yend = gem_y,
           colour = "#0072B2", linewidth = 0.9) +
  annotate("text", x = (px + gem_x) / 2, y = py, vjust = 1.6, colour = "#D55E00",
           label = paste0("x - bar(x) == ", px - gem_x), parse = TRUE) +
  annotate("text", x = px, y = (py + gem_y) / 2, hjust = -0.15, colour = "#0072B2",
           label = paste0("y - bar(y) == ", py - gem_y), parse = TRUE) +
  # datapunten
  geom_point(size = 3.5) +
  geom_text(aes(label = respondent), nudge_x = -0.3, nudge_y = 0.45, size = 3.5) +
  scale_x_continuous(limits = c(0, 13.5), breaks = seq(0, 12, 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
  labs(
    x = "Aantal glazen alcohol",
    y = "Aantal sigaretten",
    title = paste0("Correlatie tussen glazen alcohol en sigaretten (r = ",
                   format(round(r, 2), decimal.mark = ","), ")")
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# standaarddeviaties
sd(drugs$glazen_alcohol_uitgaansdag)
sd(drugs$sigaretten_uitgaansdag)

# Opslaan ----------------------------------------------------------------
dir.create("afbeeldingen", showWarnings = FALSE)
ggsave("afbeeldingen/correlatie_alcohol_sigaretten.png", fig,
       width = 7, height = 5, dpi = 300, bg = "white")
