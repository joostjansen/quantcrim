###############################################################################
# Simulatie fictieve dataset: drugsgebruik onder jongeren (15-25 jaar)
#
# Doel: oefendataset voor een introductiecursus statistiek & methoden
#       (criminologie). Alle data zijn VERZONNEN, maar de structuur en de
#       samenhangen zijn gebaseerd op Nederlands survey-onderzoek:
#       - Nationale Drug Monitor (Trimbos-instituut/WODC)
#       - Leefstijlmonitor / Aanvullende module middelengebruik (CBS/RIVM)
#       - Peilstationsonderzoek Scholieren
#
# Ingebouwde (literatuurgebaseerde) samenhangen:
#   1. Mannen gebruiken vaker dan vrouwen (sterker voor XTC dan voor cannabis)
#   2. Prevalentie loopt op met leeftijd; piek rond 20-24 jaar
#   3. Opleiding: XTC-gebruik is juist HOGER onder hoger opgeleiden/studenten,
#      cannabisgebruik iets hoger onder lager opgeleiden -> mooi tegenvoorbeeld
#      voor studenten die "alle risicofactoren wijzen dezelfde kant op" denken
#   4. Cannabis- en XTC-gebruik hangen sterk samen (gedeelde aanleg/leefstijl)
#   5. Logische trechter: laatste maand ⊂ laatste jaar ⊂ ooit
#   6. Cannabis is veel prevalenter dan XTC; XTC is vaker incidenteel gebruik
#
# De parameters zijn zo gekalibreerd dat de prevalenties bij benadering
# uitkomen op (bij n = 350 fluctueert dit een paar procentpunten):
#   cannabis ooit ~34%, laatste jaar ~22%, laatste maand ~13%
#   XTC      ooit ~13%, laatste jaar ~ 8%, laatste maand ~ 3%
###############################################################################

## 0. Voorbereiding -----------------------------------------------------------

library(MASS)   # voor mvrnorm(): gecorreleerde latente variabelen

set.seed(2026)  # reproduceerbaar; pas aan voor een andere trekking

n <- 350        # aantal respondenten


## 1. Hulpfunctie -------------------------------------------------------------
# Trek een 0/1-variabele op basis van een logit (log-odds).
# 'filter' zorgt voor de trechterlogica: alleen wie ooit gebruikte, kan
# in het laatste jaar gebruikt hebben, enzovoort.

trek_binair <- function(logit, filter = NULL) {
  kans <- plogis(logit)
  uitkomst <- rbinom(length(logit), size = 1, prob = kans)
  if (!is.null(filter)) uitkomst <- uitkomst * filter
  uitkomst
}


## 2. Achtergrondkenmerken ----------------------------------------------------

# Geslacht (ruwweg gebalanceerd, met een kleine categorie 'anders')
geslacht <- sample(
  c("man", "vrouw", "anders"),
  size = n, replace = TRUE, prob = c(0.48, 0.49, 0.03)
) |> factor(levels = c("man", "vrouw", "anders"))

# Leeftijd 15-25, licht oververtegenwoordigd in de studentenleeftijd
leeftijd <- sample(
  15:25, size = n, replace = TRUE,
  prob = c(0.06, 0.07, 0.08, 0.10, 0.11, 0.11, 0.10, 0.09, 0.09, 0.09, 0.10)
)

# Opleidingsniveau: afhankelijk van leeftijd (een 15-jarige zit niet op het hbo)
trek_opleiding <- function(lft) {
  if (lft <= 16) {
    sample(c("vmbo/mbo", "havo/vwo", "hbo/wo"), 1, prob = c(0.55, 0.45, 0.00))
  } else if (lft <= 18) {
    sample(c("vmbo/mbo", "havo/vwo", "hbo/wo"), 1, prob = c(0.45, 0.40, 0.15))
  } else if (lft <= 21) {
    sample(c("vmbo/mbo", "havo/vwo", "hbo/wo"), 1, prob = c(0.40, 0.18, 0.42))
  } else {
    sample(c("vmbo/mbo", "havo/vwo", "hbo/wo"), 1, prob = c(0.42, 0.08, 0.50))
  }
}

opleidingsniveau <- vapply(leeftijd, trek_opleiding, character(1)) |>
  factor(levels = c("vmbo/mbo", "havo/vwo", "hbo/wo"))


## 3. Latente gebruiksgeneigdheid ---------------------------------------------
# Twee gecorreleerde latente variabelen ('aanleg' voor cannabis en voor XTC).
# Deze zorgen ervoor dat de twee middelen samenhangen, ook na controle voor
# geslacht, leeftijd en opleiding: precies wat je in de literatuur ziet.

rho <- 0.60  # correlatie tussen de aanleg voor cannabis en die voor XTC

aanleg <- mvrnorm(
  n  = n,
  mu = c(0, 0),
  Sigma = matrix(c(1, rho,
                   rho, 1), nrow = 2)
)

aanleg_cannabis <- aanleg[, 1]
aanleg_xtc      <- aanleg[, 2]


## 4. Effectparameters (log-odds) ---------------------------------------------
# Dummycodering: referentie = vrouw, opleiding = vmbo/mbo.
# Leeftijd is gecentreerd op 20 jaar.

lft_c <- leeftijd - 20

man    <- as.numeric(geslacht == "man")
anders <- as.numeric(geslacht == "anders")
havo   <- as.numeric(opleidingsniveau == "havo/vwo")
hbo    <- as.numeric(opleidingsniveau == "hbo/wo")

# --- Cannabis: ooit gebruikt -------------------------------------------------
logit_can_ooit <- -0.70 +           # intercept
  0.45 * man +                     # mannen vaker
  0.10 * anders +
  0.22 * lft_c +                   # oploop met leeftijd
  -0.03 * lft_c^2 +                 # lichte afvlakking aan de bovenkant
  -0.20 * havo +                    # cannabis iets lager bij havo/vwo
  -0.45 * hbo +                     # en bij hbo/wo
  1.00 * aanleg_cannabis

# --- XTC: ooit gebruikt ------------------------------------------------------
logit_xtc_ooit <- -3.00 +
  0.60 * man +                     # sterker genderverschil dan bij cannabis
  0.10 * anders +
  0.30 * lft_c +                   # XTC start later dan cannabis
  -0.04 * lft_c^2 +
  0.25 * havo +
  0.55 * hbo +                     # XTC juist HOGER bij hoger opgeleiden
  1.00 * aanleg_xtc +
  0.60 * aanleg_cannabis           # extra samenhang met cannabisgeneigdheid

cannabis_ooit <- trek_binair(logit_can_ooit)
xtc_ooit      <- trek_binair(logit_xtc_ooit)

# --- Laatste jaar (alleen mogelijk als 'ooit' = 1) ---------------------------
logit_can_jaar <- 0.35 +
  0.25 * man +
  -0.08 * lft_c +                   # ouderen zijn vaker gestopt
  0.70 * aanleg_cannabis

logit_xtc_jaar <- -0.20 +
  0.25 * man +
  -0.10 * lft_c +
  0.60 * aanleg_xtc +
  0.30 * cannabis_ooit             # co-gebruik

cannabis_laatste_jaar <- trek_binair(logit_can_jaar, filter = cannabis_ooit)
xtc_laatste_jaar      <- trek_binair(logit_xtc_jaar, filter = xtc_ooit)

# --- Laatste maand (alleen mogelijk als 'laatste jaar' = 1) ------------------
# Cannabis kent relatief veel frequent gebruik, XTC vooral incidenteel gebruik.
logit_can_maand <- -0.25 +
  0.35 * man +
  -0.30 * hbo +
  0.65 * aanleg_cannabis

logit_xtc_maand <- -1.30 +          # veel lagere doorstroom: XTC is uitgaansdrug
  0.30 * man +
  0.50 * aanleg_xtc

cannabis_laatste_maand <- trek_binair(logit_can_maand, filter = cannabis_laatste_jaar)
xtc_laatste_maand      <- trek_binair(logit_xtc_maand, filter = xtc_laatste_jaar)


## 5. Dataset samenstellen ----------------------------------------------------

drugs <- data.frame(
  respondentnr           = 1:n,
  geslacht               = geslacht,
  leeftijd               = leeftijd,
  opleidingsniveau       = opleidingsniveau,
  xtc_ooit               = xtc_ooit,
  xtc_laatste_jaar       = xtc_laatste_jaar,
  xtc_laatste_maand      = xtc_laatste_maand,
  cannabis_ooit          = cannabis_ooit,
  cannabis_laatste_jaar  = cannabis_laatste_jaar,
  cannabis_laatste_maand = cannabis_laatste_maand
)

# Optioneel: een beetje item-nonrespons, zodat studenten met NA's leren omgaan.
# Zet op FALSE als je een volledig complete dataset wilt.
voeg_missings_toe <- TRUE

if (voeg_missings_toe) {
  drugs$opleidingsniveau[sample(n, 8)]  <- NA
  drugs$xtc_ooit[sample(n, 5)]          <- NA
  drugs$cannabis_ooit[sample(n, 5)]     <- NA
}


## 6. Controle: kloppen de cijfers met de literatuur? -------------------------

prevalenties <- sapply(
  drugs[, c("cannabis_ooit", "cannabis_laatste_jaar", "cannabis_laatste_maand",
            "xtc_ooit", "xtc_laatste_jaar", "xtc_laatste_maand")],
  function(x) round(mean(x, na.rm = TRUE) * 100, 1)
)

cat("\n--- Prevalenties in % ---\n")
print(prevalenties)

cat("\n--- Controle trechterlogica (0 = geen fouten) ---\n")
cat("cannabis maand > jaar:", sum(drugs$cannabis_laatste_maand > drugs$cannabis_laatste_jaar, na.rm = TRUE), "\n")
cat("cannabis jaar  > ooit:", sum(drugs$cannabis_laatste_jaar  > drugs$cannabis_ooit,         na.rm = TRUE), "\n")
cat("xtc maand      > jaar:", sum(drugs$xtc_laatste_maand      > drugs$xtc_laatste_jaar,      na.rm = TRUE), "\n")
cat("xtc jaar       > ooit:", sum(drugs$xtc_laatste_jaar       > drugs$xtc_ooit,              na.rm = TRUE), "\n")

cat("\n--- Cannabis ooit naar geslacht (%) ---\n")
print(round(prop.table(table(drugs$geslacht, drugs$cannabis_ooit), 1) * 100, 1))

cat("\n--- XTC ooit naar opleidingsniveau (%) ---\n")
print(round(prop.table(table(drugs$opleidingsniveau, drugs$xtc_ooit), 1) * 100, 1))

cat("\n--- Samenhang cannabis en XTC (ooit) ---\n")
print(table(cannabis = drugs$cannabis_ooit, xtc = drugs$xtc_ooit))
print(chisq.test(table(drugs$cannabis_ooit, drugs$xtc_ooit)))


## 7. Opslaan -----------------------------------------------------------------

write.csv2(drugs, "data/drugsgebruik_jongeren.csv", row.names = FALSE)  # ; als scheidingsteken
saveRDS(drugs, "data/drugsgebruik_jongeren.rds")

# Voor studenten die met SPSS werken:
# haven::write_sav(drugs, "drugsgebruik_jongeren.sav")
