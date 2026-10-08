# genereren voorbeeld data voor H1

voorbeeld_data <- data.frame(
  geslacht = factor(
    c("vrouw", "man", "vrouw", "man", "man", "vrouw", "man"),
    levels = c("man", "vrouw", "anders")
  ),
  leeftijd = c(30, 21, 35, 16, 26, 18, 24),
  opleidingsniveau = factor(
    c("midden", "laag", "midden", "hoog", NA, "laag", "hoog"),
    levels = c("laag", "midden", "hoog")
  ),
  alcohol_ooit = factor(
    c("ja", "ja", "ja", "ja", "ja", "ja", "ja"),
    levels = c("nee", "ja")
  ),
  alcohol_laatste_jaar = factor(
    c("ja", "ja", "nee", "ja", "ja", "ja", "ja"),
    levels = c("nee", "ja")
  )
)

# opslaan voorbeeld_data
saveRDS(voorbeeld_data, "data/voorbeeld_data.rds")