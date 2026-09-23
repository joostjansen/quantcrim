###############################################################################
# Simulatie fictieve dataset: middelengebruik onder uitgaanders (16-35 jaar)
#
# Doel: oefendataset voor een introductiecursus statistiek & methoden
#       (criminologie). Alle respondenten en waarnemingen zijn VERZONNEN.
#
# BRON VAN DE PARAMETERS
# -----------------------
# Prevalenties, man/vrouw-verschillen, leeftijdsverschillen, verschillen naar
# opleidingsniveau en gebruiksfrequentie zijn overgenomen uit:
#   Trimbos-instituut (2024). Het Grote Uitgaansonderzoek 2023 (AF2122).
#   - Tabel 4.1  : ooit- en laatste-jaar-gebruik per middel
#   - Tabel C.1  : laatste-jaar-gebruik naar geslacht, leeftijd en opleiding
#   - Figuur 5.1 : frequentie van gebruik onder laatste-jaar-gebruikers
#   - Par. 3.1   : demografie van de (gewogen) steekproef
#
# BELANGRIJKE KANTTEKENING VOOR DE COLLEGEZAAL
# -----------------------------------------------------------------------------
# Het rapport gaat over UITGAANDERS (bezoekers van feesten/festivals/clubs),
# een sterke risicogroep. De prevalenties liggen daardoor veel hoger dan in
# de algemene jongerenpopulatie (bijv. 46,9% laatste-jaar-cannabisgebruik,
# tegen ~15-20% in schoolonderzoeken onder de algemene bevolking). Deze
# dataset simuleert dus een "uitgaanders-achtige" steekproef, geen
# representatieve steekproef van alle Nederlandse jongeren. Vertel dit
# uw studenten erbij.
#
# De leeftijd in deze dataset is bewust beperkt tot 16 t/m 35 jaar, exact het
# bereik dat het rapport zelf bestrijkt (par. 3.1). Een eerdere versie
# extrapoleerde de leeftijdscurve tot 15-45 jaar, maar dat vereiste aannames
# buiten het bereik van de brondata. Door de leeftijd bij 16-35 jaar te
# houden, zijn alle leeftijdseffecten in deze dataset direct terug te voeren
# op rapportcijfers (Tabel C.1), zonder extrapolatie.
#
# INGEBOUWDE SAMENHANGEN
# -----------------------------------------------------------------------------
#   1. Mannen gebruiken vaker dan vrouwen, met uitzondering van vapen (waar
#      vrouwen net iets vaker gebruiken dan mannen) - conform het rapport.
#   2. Middelen clusteren: alcohol/tabak/vapen/cannabis vormen een cluster
#      van meer "alledaagse" middelen; XTC/cocaine/3-MMC een cluster van
#      klassieke uitgaansdrugs. Binnen een cluster is de samenhang sterker
#      dan tussen clusters (vgl. het 'common liability'-model van
#      polydruggebruik, Vanyukov et al., 2003).
#   3. Leeftijd: cannabis/vapen pieken relatief vroeg, XTC/cocaine/3-MMC
#      pieken juist rond de 25-30 jaar (precies zoals in het rapport).
#   4. Opleiding: tegenovergesteld patroon voor XTC (hoger bij hbo/wo) versus
#      cannabis/cocaine/3-MMC (hoger bij lager opgeleiden) - conform rapport.
#   5. Logische trechter: frequentie laatste jaar is alleen ingevuld bij
#      laatste-jaar-gebruik; laatste-jaar-gebruik alleen bij ooit-gebruik.
###############################################################################


## 0. Voorbereiding -----------------------------------------------------------

set.seed(2026)
n <- 350


## 1. Hulpfuncties -------------------------------------------------------------

# Percentage (0-100) naar log-odds
naar_logit <- function(pct) log(pct / (100 - pct))

# Effect van leeftijd op de log-odds, geschat via een natuurlijke spline door
# controlepunten (leeftijd, percentage). Zo volgt de curve de rapportcijfers
# exact op de vier leeftijdscategorieen uit Tabel C.1 (16-35 jaar) en
# interpoleert hij soepel tussen de categorieen in.
maak_leeftijdseffect <- function(leeftijd, ctrl_leeftijd, ctrl_pct) {
  f <- splinefun(ctrl_leeftijd, naar_logit(ctrl_pct), method = "natural")
  f(leeftijd)
}

# Vindt het intercept waarvoor het gemiddelde voorspelde percentage in de
# steekproef gelijk is aan het doelpercentage uit het rapport. Zo houden we
# de relatieve (log-odds-)effecten van geslacht/leeftijd/opleiding aan uit
# het rapport, maar kalibreren we het niveau op de gerapporteerde prevalentie.
kalibreer_intercept <- function(doel_pct, effecten) {
  doel <- doel_pct / 100
  doelfunctie <- function(intercept) mean(plogis(intercept + effecten)) - doel
  uniroot(doelfunctie, interval = c(-15, 15))$root
}

# Trekt een 0/1-variabele op basis van een kans, met optioneel een filter
# (bijv. "laatste jaar" kan alleen 1 zijn als "ooit" al 1 is).
trek_binair <- function(kans, filter = NULL) {
  uitkomst <- rbinom(length(kans), size = 1, prob = kans)
  if (!is.null(filter)) uitkomst <- uitkomst * filter
  uitkomst
}

# Wijst frequentiecategorieen toe aan gebruikers, op basis van hun rang op
# een latente continue score (z). Door de rangorde te gebruiken (in plaats
# van vaste afkapwaarden) komt de gerealiseerde verdeling nagenoeg exact
# overeen met de doelpercentages uit Figuur 5.1, terwijl mensen met een
# hogere onderliggende gebruiksgeneigdheid systematisch vaker in de zwaardere
# categorieen vallen.
trek_frequentie <- function(z, percentages, labels) {
  rangpercentiel <- (rank(z, ties.method = "random") - 0.5) / length(z)
  grens <- cumsum(percentages) / 100
  index <- findInterval(rangpercentiel, grens) + 1
  index <- pmin(index, length(labels))
  factor(labels[index], levels = labels, ordered = TRUE)
}


## 2. Achtergrondkenmerken ----------------------------------------------------

# Geslacht: 50,4% man / 48,6% vrouw / 0,9% anders (par. 3.1)
geslacht <- sample(
  c("man", "vrouw", "anders"),
  size = n, replace = TRUE, prob = c(0.504, 0.486, 0.010)
) |> factor(levels = c("man", "vrouw", "anders"))

# Leeftijd 16-35, exact het bereik en de verdeling uit het rapport (par. 3.1):
# 16-19 jaar: 20%, 20-24 jaar: 47%, 25-29 jaar: 23%, 30-35 jaar: 10%.
leeftijd_gewicht <- c(
  "16" = 5, "17" = 5, "18" = 5, "19" = 5,
  "20" = 9, "21" = 9, "22" = 9, "23" = 9, "24" = 9,
  "25" = 5, "26" = 5, "27" = 5, "28" = 5, "29" = 5,
  "30" = 2, "31" = 2, "32" = 2, "33" = 2, "34" = 2, "35" = 2
)
leeftijd <- sample(
  as.integer(names(leeftijd_gewicht)), size = n, replace = TRUE,
  prob = prop.table(leeftijd_gewicht)
)

# Opleidingsniveau: 2,5% laag / 28,7% midden / 68,8% hoog (par. 3.1)
# Let op: dit weerspiegelt de (hoogopgeleide) uitgaanderspopulatie, niet de
# algemene bevolking.
opleidingsniveau <- sample(
  c("laag", "midden", "hoog"),
  size = n, replace = TRUE, prob = c(0.025, 0.287, 0.688)
) |> factor(levels = c("laag", "midden", "hoog"))

man    <- as.numeric(geslacht == "man")
midden <- as.numeric(opleidingsniveau == "midden")
hoog   <- as.numeric(opleidingsniveau == "hoog")


## 3. Latente gebruiksgeneigdheid (voor de samenhang tussen middelen) --------
# Bifactor-structuur: een algemene "aanleg" voor middelengebruik, met daarop
# twee gecorreleerde clusters (alledaagse middelen vs. klassieke uitgaans-
# drugs), en per middel weer een eigen unieke component.

algemene_aanleg  <- rnorm(n)
cluster_legaal   <- 0.6 * algemene_aanleg + sqrt(1 - 0.6^2) * rnorm(n)
cluster_party    <- 0.6 * algemene_aanleg + sqrt(1 - 0.6^2) * rnorm(n)

maak_aanleg <- function(cluster, lading = 0.7) {
  lading * cluster + sqrt(1 - lading^2) * rnorm(n)
}

aanleg <- list(
  alcohol  = maak_aanleg(cluster_legaal),
  tabak    = maak_aanleg(cluster_legaal),
  vapen    = maak_aanleg(cluster_legaal),
  cannabis = maak_aanleg(cluster_legaal),
  xtc      = maak_aanleg(cluster_party),
  cocaine  = maak_aanleg(cluster_party),
  mmc      = maak_aanleg(cluster_party)
)


## 4. Parameters per middel, overgenomen uit het rapport ---------------------
# lft_ctrl / lft_pct: controlepunten voor de leeftijdsspline - de vier
# leeftijdscategorieen uit Tabel C.1 (16-19, 20-24, 25-29, 30-35 jaar),
# met als x-waarde het midden van elke categorie (17,5 / 22 / 27 / 32,5).
# freq: percentages in de volgorde van 'frequentie_labels' (Figuur 5.1).

frequentie_labels <- c(
  "een keer", "minder dan maandelijks", "eens per maand",
  "een paar keer per maand", "eens per week", "een paar keer per week",
  "(bijna) elke dag"
)

parameters <- list(
  alcohol = list(
    ooit = 99.4, jaar = 98.2, man = 98.3, vrouw = 98.2,
    opl = c(laag = 93.3, midden = 97.7, hoog = 98.7),
    lft_ctrl = c(17.5, 22, 27, 32.5),
    lft_pct  = c(97.6, 98.5, 98.6, 97.8),
    freq = c(0.2, 3.7, 3.5, 15.6, 25.3, 47.1, 4.7)
  ),
  tabak = list(
    ooit = 73.2, jaar = 58.9, man = 61.4, vrouw = 56.3,
    opl = c(laag = 78.6, midden = 67.3, hoog = 54.6),
    lft_ctrl = c(17.5, 22, 27, 32.5),
    lft_pct  = c(58.8, 60.5, 58.8, 51.9),
    freq = c(3.8, 19.2, 7.2, 10.1, 9.0, 13.3, 37.4)
  ),
  vapen = list(
    ooit = 59.9, jaar = 50.2, man = 47.9, vrouw = 52.5,
    opl = c(laag = 59.8, midden = 54.9, hoog = 47.8),
    lft_ctrl = c(17.5, 22, 27, 32.5),
    lft_pct  = c(61.6, 53.1, 43.3, 30.3),
    freq = c(10.5, 33.8, 11.5, 13.4, 8.3, 10.4, 12.0)
  ),
  cannabis = list(
    ooit = 76.4, jaar = 46.9, man = 54.2, vrouw = 39.5,
    opl = c(laag = 53.1, midden = 49.1, hoog = 45.7),
    lft_ctrl = c(17.5, 22, 27, 32.5),
    lft_pct  = c(49.2, 49.6, 43.6, 37.9),
    freq = c(12.0, 42.5, 10.7, 9.5, 6.5, 8.3, 10.5)
  ),
  xtc = list(
    ooit = 64.3, jaar = 53.8, man = 61.0, vrouw = 46.4,
    opl = c(laag = 53.2, midden = 48.6, hoog = 55.9),
    lft_ctrl = c(17.5, 22, 27, 32.5),
    lft_pct  = c(33.4, 55.7, 64.9, 59.9),
    freq = c(15.3, 73.3, 7.9, 3.1, 0.3, 0.1, 0.0)
  ),
  cocaine = list(
    ooit = 43.6, jaar = 33.5, man = 40.5, vrouw = 26.2,
    opl = c(laag = 42.7, midden = 33.6, hoog = 33.0),
    lft_ctrl = c(17.5, 22, 27, 32.5),
    lft_pct  = c(14.9, 32.0, 47.8, 44.2),
    freq = c(18.1, 54.6, 12.1, 11.3, 2.2, 1.3, 0.4)
  ),
  mmc = list(
    ooit = 41.2, jaar = 33.7, man = 39.6, vrouw = 27.5,
    opl = c(laag = 40.2, midden = 33.5, hoog = 33.5),
    lft_ctrl = c(17.5, 22, 27, 32.5),
    lft_pct  = c(22.3, 38.9, 35.5, 27.9),
    freq = c(21.4, 47.9, 13.3, 12.1, 3.5, 1.4, 0.2)
  )
)


## 5. Simulatie per middel -----------------------------------------------------

simuleer_middel <- function(p, aanleg_middel) {
  
  or_man    <- naar_logit(p$man) - naar_logit(p$vrouw)
  or_midden <- naar_logit(p$opl["midden"]) - naar_logit(p$opl["laag"])
  or_hoog   <- naar_logit(p$opl["hoog"])   - naar_logit(p$opl["laag"])
  
  lft_effect   <- maak_leeftijdseffect(leeftijd, p$lft_ctrl, p$lft_pct)
  lft_effect_c <- lft_effect - mean(lft_effect)   # centreren rond 0
  
  effecten <- or_man * man + or_midden * midden + or_hoog * hoog +
    lft_effect_c + 1.0 * aanleg_middel
  
  # --- ooit gebruikt --------------------------------------------------------
  b_ooit <- kalibreer_intercept(p$ooit, effecten)
  ooit   <- trek_binair(plogis(b_ooit + effecten))
  
  # --- laatste jaar gebruikt, gegeven ooit-gebruik --------------------------
  doel_conditioneel <- p$jaar / p$ooit * 100
  b_jaar <- kalibreer_intercept(doel_conditioneel, effecten[ooit == 1])
  laatste_jaar <- trek_binair(plogis(b_jaar + effecten), filter = ooit)
  
  # --- frequentie laatste jaar, gegeven laatste-jaar-gebruik ----------------
  frequentie <- rep(NA_character_, length(ooit)) |>
    factor(levels = frequentie_labels, ordered = TRUE)
  gebruikers <- which(laatste_jaar == 1)
  z <- aanleg_middel[gebruikers] + rnorm(length(gebruikers), sd = 0.5)
  frequentie[gebruikers] <- trek_frequentie(z, p$freq, frequentie_labels)
  
  data.frame(ooit = ooit, laatste_jaar = laatste_jaar, frequentie = frequentie)
}

# Trekt een antwoord op een stelling (5-punts eens/oneens-schaal + "weet niet").
# 'kans_weet_niet' en 'percentages' (5 stuks, exclusief weet-niet, hoeven niet
# op te tellen tot 100 - ze worden intern herschaald) komen uit het rapport.
# Wie geen "weet niet" antwoordt, krijgt een rangordegebaseerd antwoord op
# basis van z (zie trek_frequentie hierboven): hogere z = meer "mee eens".
stelling_labels <- c(
  "helemaal mee oneens", "mee oneens", "niet mee eens of oneens",
  "mee eens", "helemaal mee eens", "weet niet"
)

trek_stelling <- function(z, kans_weet_niet, percentages) {
  n_resp <- length(z)
  weet_niet <- rbinom(n_resp, 1, kans_weet_niet / 100) == 1
  uitkomst <- factor(rep(NA_character_, n_resp), levels = stelling_labels)
  uitkomst[weet_niet] <- "weet niet"
  
  overig <- which(!weet_niet)
  antwoord_overig <- trek_frequentie(
    z[overig], percentages, stelling_labels[1:5]
  )
  uitkomst[overig] <- as.character(antwoord_overig)
  uitkomst
}

# Trekt een aantal (sigaretten/glazen) als telvariabele met een lange
# rechterstaart (negatief-binomiaal), gegeven een gemiddelde 'mu' per
# respondent. 'dispersie' regelt hoe sterk de verdeling scheef is (kleiner
# getal = schever, met meer respondenten met een laag aantal en een kleine
# groep met een hoog aantal - typisch voor gebruikshoeveelheden).
trek_aantal <- function(mu, dispersie) {
  rnbinom(length(mu), mu = mu, size = dispersie) + 1  # +1: minimaal 1 op een dag dat men gebruikt
}

resultaten <- Map(simuleer_middel, parameters, aanleg)


## 6. Gebruikshoeveelheden op een uitgaansdag ---------------------------------
# Bron: Tabel 5.1 (gemiddelden per middel) en de tekst daaromheen (verschil
# man/vrouw voor alcohol). Omdat het rapport voor beide middelen een sterk
# rechtsscheve verdeling laat zien (gemiddelde >> modus), koppelen we het
# gemiddelde aantal aan de eigen gebruiksfrequentie: wie vaker gebruikt,
# gebruikt op een uitgaansdag doorgaans ook meer.

# --- Sigaretten op een uitgaansdag (alleen bij laatste-jaar-tabaksgebruik) --
# Gewogen over de gerealiseerde tabak-frequentieverdeling komt dit uit op een
# gemiddelde van ca. 10,1 sigaretten - vrijwel gelijk aan het rapportcijfer
# van 10,2 (Tabel 5.1, laatste-jaar-gebruikers op een uitgaansdag).
mu_sigaretten <- c(
  "een keer" = 2, "minder dan maandelijks" = 3, "eens per maand" = 4,
  "een paar keer per maand" = 5, "eens per week" = 7,
  "een paar keer per week" = 10, "(bijna) elke dag" = 18
)

sigaretten_uitgaansdag <- rep(NA_integer_, n)
rokers <- which(resultaten$tabak$laatste_jaar == 1)
mu_rokers <- mu_sigaretten[as.character(resultaten$tabak$frequentie[rokers])]
sigaretten_uitgaansdag[rokers] <- trek_aantal(mu_rokers, dispersie = 1.5)

# --- Glazen alcohol op een uitgaansdag (alleen bij laatste-jaar-alcoholgebruik) --
# Rapport: gemiddeld 5,1 glazen voor het uitgaan ("indrinken") + 6,1 glazen
# tijdens het uitgaan = ca. 11,2 glazen in totaal op een uitgaansdag, met
# mannen die meer drinken dan vrouwen (13,1 resp. 9,2 glazen, samengeteld).
mu_glazen_basis <- c(
  "een keer" = 5, "minder dan maandelijks" = 7, "eens per maand" = 8,
  "een paar keer per maand" = 9, "eens per week" = 10,
  "een paar keer per week" = 11, "(bijna) elke dag" = 13
)

drinkers <- which(resultaten$alcohol$laatste_jaar == 1)
mu_drinkers <- mu_glazen_basis[as.character(resultaten$alcohol$frequentie[drinkers])]
# mannen drinken op een uitgaansdag ca. 40% meer glazen dan vrouwen (rapport)
mu_drinkers <- mu_drinkers * ifelse(man[drinkers] == 1, 1.1, 0.78)

glazen_uitgaansdag <- rep(NA_integer_, n)
glazen_uitgaansdag[drinkers] <- trek_aantal(mu_drinkers, dispersie = 4)


## 7. Stellingen over acceptatie en verkrijgbaarheid --------------------------
# Bron: Figuur 8.2 (acceptatie onder vrienden) en Figuur 8.4 (verkrijgbaarheid).
# De antwoorden hangen samen met het eigen gebruik en met de onderliggende
# gebruiksgeneigdheid: wie zelf gebruikt (of in een gebruikerskring zit),
# vindt het middel logischerwijs vaker geaccepteerd en drugs makkelijker
# verkrijgbaar (vgl. sociale-normen-/normaliseringsliteratuur, bijv.
# Parker, Aldridge & Measham's normalisatiethese).

z_acceptatie_alcohol  <- aanleg$alcohol + 0.8 * resultaten$alcohol$laatste_jaar + rnorm(n, sd = 0.4)
z_acceptatie_xtc      <- aanleg$xtc     + 0.8 * resultaten$xtc$laatste_jaar     + rnorm(n, sd = 0.4)
z_acceptatie_cocaine  <- aanleg$cocaine + 0.8 * resultaten$cocaine$laatste_jaar + rnorm(n, sd = 0.4)
z_verkrijgbaarheid    <- algemene_aanleg + 0.6 * cluster_party + rnorm(n, sd = 0.4)

acceptatie_alcohol_vrienden <- trek_stelling(z_acceptatie_alcohol, kans_weet_niet = 0.3,
                                             percentages = c(1.9, 0.3, 0.5, 3.0, 94.1))
acceptatie_xtc_vrienden     <- trek_stelling(z_acceptatie_xtc, kans_weet_niet = 1.0,
                                             percentages = c(8.7, 6.4, 7.0, 27.5, 49.5))
acceptatie_cocaine_vrienden <- trek_stelling(z_acceptatie_cocaine, kans_weet_niet = 1.4,
                                             percentages = c(21.4, 18.3, 14.3, 24.5, 20.2))
makkelijk_aan_drugs_komen   <- trek_stelling(z_verkrijgbaarheid, kans_weet_niet = 5.8,
                                             percentages = c(3.0, 4.3, 5.2, 27.1, 54.5))


## 8. Algemene gezondheid -----------------------------------------------------
# Bron: rapporttekst "Algemene gezondheid" (par. 6.1, doorlopende tekst, geen
# apart tabel-/figuurnummer). Het rapport vraagt naar ervaren gezondheid op
# een 5-punts schaal "zeer slecht / slecht / niet goed·niet slecht / goed /
# zeer goed": zeer slecht 0,2% / slecht 4,1% / niet goed/niet slecht 20,8% /
# goed 58,9% / zeer goed 16,0%.
#
# Deze dataset gebruikt in plaats daarvan de standaard CBS/RIVM-schaal voor
# ervaren gezondheid: Uitstekend / Zeer goed / Goed / Matig / Slecht. Omdat
# het rapport GEEN percentages geeft op deze schaal, is de volgende
# (docenten-)omrekening gemaakt, die de totaalverhouding "goed of beter" vs.
# "matig/slecht" uit het rapport exact aanhoudt:
#   - Matig            = rapport "niet goed/niet slecht"        = 20,8%
#   - Slecht            = rapport "slecht" + "zeer slecht"        =  4,3%
#   - Goed              = rapport "goed" (1-op-1)                 = 58,9%
#   - Zeer goed + Uitstekend = rapport "zeer goed" (16,0%), opgesplitst
#     in een aanname-ratio van 65/35: Zeer goed = 10,4%, Uitstekend = 5,6%
#     (er is geen "Uitstekend"-categorie in het rapport om op te kalibreren;
#     de aanname is dat de ruimere top van de rapportschaal, "zeer goed",
#     zich op deze fijnere schaal verdeelt over de twee hoogste categorieen).
# Totaal: 4,3 + 20,8 + 58,9 + 10,4 + 5,6 = 100%. Het aandeel "goed of beter"
# (Goed+Zeer goed+Uitstekend = 74,9%) komt exact overeen met het rapport
# (goed 58,9% + zeer goed 16,0% = 74,9%).
#
# Ter info (niet gebruikt als kalibratiedoel, want andere populatie): in de
# algemene bevolking beoordeelt 87,7% (16-20 jr), 85,7% (20-30 jr) en 83,0%
# (30-40 jr) de eigen gezondheid als (zeer) goed (CBS, 2022) - uitgaanders
# scoren met 74,9% dus duidelijk lager dan leeftijdsgenoten in de algemene
# bevolking. Een mooi gespreksonderwerp voor de collegezaal: hoe kan een
# groep die er zelf tevreden uitziet toch systematisch lager scoren dan de
# algemene bevolking?
#
# Het rapport geeft GEEN uitsplitsing van ervaren gezondheid naar geslacht,
# leeftijd of middelengebruik. Om de variabele toch een realistische
# samenhang met de rest van de dataset te geven, is - als aanname van de
# docent, dus NIET uit het rapport - een negatief verband gelegd met de
# algemene gebruiksgeneigdheid en met zwaar (frequent) gebruik van tabak en
# cannabis, in lijn met de bredere gezondheidsliteratuur over de samenhang
# tussen middelengebruik en ervaren gezondheid. Dit is een aanname die
# expliciet te bespreken is met studenten: net als bij de leeftijdskeuze
# hierboven is transparantie over wat wel en niet rechtstreeks uit het
# rapport komt, essentieel bij het werken met een gesimuleerde dataset.

gezondheid_labels <- c("Slecht", "Matig", "Goed", "Zeer goed", "Uitstekend")
gezondheid_pct     <- c(4.3, 20.8, 58.9, 10.4, 5.6)

# "Zwaar" gebruik = minstens een paar keer per week (NA/geen gebruik telt als 0)
is_zwaar_gebruiker <- function(frequentie) {
  ifelse(!is.na(frequentie) & frequentie %in% c("een paar keer per week", "(bijna) elke dag"), 1, 0)
}
zwaar_tabak    <- is_zwaar_gebruiker(resultaten$tabak$frequentie)
zwaar_cannabis <- is_zwaar_gebruiker(resultaten$cannabis$frequentie)

# Hogere z = betere ervaren gezondheid
z_gezondheid <- -0.6 * algemene_aanleg - 0.4 * zwaar_tabak - 0.3 * zwaar_cannabis +
  rnorm(n, sd = 0.8)

ervaren_gezondheid <- trek_frequentie(z_gezondheid, gezondheid_pct, gezondheid_labels)


## 9. Wijze waarop drugs verkregen worden ------------------------------------
# Bron: Figuur 5.8 (par. 5.4, n = 4033 - laatste-jaar-gebruikers van drugs in
# het algemeen): koopt van een dealer 40,8% / koopt van vrienden of
# kennissen 27,2% / krijgt van vrienden of kennissen 25,6% / ontvangen via
# de post 1,5% / dropping 0% / anders 4,9%.
#
# Populatie: alleen laatste-jaar-gebruikers van minstens een van de vier
# "drugs" in deze dataset (cannabis, XTC, cocaine, 3-MMC) - net als in het
# rapport gaat het hier om een algemene vraag over drugs, niet per middel.
# Voor niet-gebruikers is deze variabele NA.
#
# Het rapport geeft geen uitsplitsing van deze wijze van verkrijgen naar
# geslacht, leeftijd, opleiding of gebruiksintensiteit. Als aanname van de
# docent (NIET uit het rapport, zie ook de kanttekeningen bij leeftijd en
# ervaren gezondheid hierboven) is de keuze tussen dealer en vrienden
# gekoppeld aan de gebruiksintensiteit over de vier drugs samen: frequentere/
# zwaardere gebruikers hebben vaker een vaste dealer, incidentele gebruikers
# krijgen of kopen vaker via vrienden (vgl. routine-activity- en
# marktbetrokkenheidstheorie uit de criminologie). De kleine restcategorieen
# (post, dropping, anders) zijn - bij gebrek aan een theoretische link -
# onafhankelijk van de gebruiksintensiteit getrokken.

verkrijgen_labels <- c(
  "koopt van een dealer", "koopt van vrienden/kennissen",
  "krijgt van vrienden/kennissen", "ontvangen via de post",
  "dropping", "anders"
)

# Gebruikt minstens een van de vier drugs in het laatste jaar
drug_gebruiker <- pmax(
  resultaten$cannabis$laatste_jaar, resultaten$xtc$laatste_jaar,
  resultaten$cocaine$laatste_jaar, resultaten$mmc$laatste_jaar
)

# Combinatiescore van gebruiksintensiteit over de vier drugs (0 voor
# niet-gebruikers van dat middel, hoger voor frequenter/meervoudig gebruik)
intensiteit_drugs <- rowSums(sapply(
  list(resultaten$cannabis$frequentie, resultaten$xtc$frequentie,
       resultaten$cocaine$frequentie, resultaten$mmc$frequentie),
  function(f) ifelse(is.na(f), 0, as.numeric(f))
))

manier_verkrijgen_drugs <- factor(rep(NA_character_, n), levels = verkrijgen_labels)
gebruikers_drugs <- which(drug_gebruiker == 1)

# Stap 1: kleine restcategorie, los van gebruiksintensiteit (1,5+0+4,9=6,4%)
is_restcategorie <- rbinom(length(gebruikers_drugs), 1, 0.064) == 1
rest_gebruikers  <- gebruikers_drugs[is_restcategorie]
manier_verkrijgen_drugs[rest_gebruikers] <- sample(
  c("ontvangen via de post", "dropping", "anders"),
  size = length(rest_gebruikers), replace = TRUE,
  prob = c(1.5, 0, 4.9)
)

# Stap 2: overigen verdelen over dealer/vrienden, gekoppeld aan intensiteit.
# De drie percentages (25,6 / 27,2 / 40,8) worden herschaald zodat ze precies
# optellen tot 100% binnen deze subgroep (93,6% van alle drugsgebruikers).
hoofd_gebruikers  <- gebruikers_drugs[!is_restcategorie]
hoofd_percentages <- c(25.6, 27.2, 40.8)
hoofd_percentages <- hoofd_percentages / sum(hoofd_percentages) * 100

z_verkrijgen <- 0.7 * scale(intensiteit_drugs[hoofd_gebruikers])[, 1] +
  0.5 * cluster_party[hoofd_gebruikers] + rnorm(length(hoofd_gebruikers), sd = 0.6)

manier_verkrijgen_drugs[hoofd_gebruikers] <- as.character(trek_frequentie(
  z_verkrijgen, hoofd_percentages,
  c("krijgt van vrienden/kennissen", "koopt van vrienden/kennissen", "koopt van een dealer")
))


## 10. Gemoedstoestand op dinsdag na XTC-gebruik in het weekend -------------
# LET OP: dit is een volledig FICTIEVE variabele, NIET afkomstig uit het
# rapport. Ze is toegevoegd op uitdrukkelijk verzoek van de docent, als
# oefenmateriaal voor bijvoorbeeld correlatie- of regressie-opdrachten met
# een continue uitkomstmaat. Score van 0 (negatief) tot 100 (positief).
#
# Populatie: alleen laatste-jaar-XTC-gebruikers (resultaten$xtc$laatste_jaar
# == 1) - voor de rest van de steekproef is de variabele NA.
#
# Opgegeven specificatie:
#   - gemiddelde ca. 60, standaardafwijking ca. 15
#   - matig NEGATIEVE samenhang met de gebruiksfrequentie van XTC - conform de
#     literatuur over de "midweek dip"/serotoninedepletie na MDMA-gebruik:
#     frequentere gebruikers ervaren doorgaans een sterkere stemmingsdaling
#     enkele dagen na gebruik (vgl. Parrott, 2001; Curran & Travill, 1997).
#   - hogere leeftijd samenhangend met een relatief betere gemoedstoestand
#   - ongeveer 40% van de respondenten rondt het antwoord af op een vijftal
#     (0, 5, 10, ..., 100), de rest geeft een "los" getal.
# De leeftijdssamenhang blijft een aanname van de docent (net als bij de
# eerdere fictieve/deels-fictieve variabelen in dit script), niet uit het
# rapport afgeleid; de richting van de samenhang met gebruiksfrequentie is nu
# wel in lijn met de MDMA-literatuur.

xtc_gebruikers <- which(resultaten$xtc$laatste_jaar == 1)
freq_xtc_num   <- as.numeric(resultaten$xtc$frequentie[xtc_gebruikers])  # 1 = "een keer" ... 7 = "(bijna) elke dag"

# Gestandaardiseerde voorspellers binnen de groep XTC-gebruikers
freq_z     <- scale(freq_xtc_num)[, 1]
leeftijd_z <- scale(leeftijd[xtc_gebruikers])[, 1]

# Gewichten zo gekozen dat de samenhang met frequentie "matig negatief" is
# (r ~ -0,3) en met leeftijd kleiner-positief ("relatief beter", r ~ 0,15);
# de rest is ruis, zodanig geschaald dat de totale z-score ongeveer sd = 1
# heeft.
gewicht_freq     <- -0.30
gewicht_leeftijd <- 0.15
gewicht_ruis     <- sqrt(max(0, 1 - gewicht_freq^2 - gewicht_leeftijd^2))

z_gemoed <- gewicht_freq * freq_z + gewicht_leeftijd * leeftijd_z +
  gewicht_ruis * rnorm(length(xtc_gebruikers))

# Terugschalen naar gemiddelde 60, sd 15, en begrenzen tot [0, 100]
ruwe_score <- 60 + 15 * z_gemoed
ruwe_score <- pmin(pmax(ruwe_score, 0), 100)

# Afrondgedrag: ca. 40% rondt af op een vijftal, de rest geeft een willekeurig
# geheel getal (zoals bij een 0-100-schaal in een vragenlijst vaak gebeurt).
rondt_op_vijftal <- rbinom(length(xtc_gebruikers), 1, 0.25) == 1
score_afgerond <- ifelse(rondt_op_vijftal,
                         round(ruwe_score / 5) * 5,
                         round(ruwe_score))

gemoed_dinsdag_na_xtc <- rep(NA_integer_, n)
gemoed_dinsdag_na_xtc[xtc_gebruikers] <- as.integer(score_afgerond)


## 11. Dataset samenstellen ---------------------------------------------------

drugs <- data.frame(
  respondentnr     = 1:n,
  geslacht         = geslacht,
  leeftijd         = leeftijd,
  opleidingsniveau = opleidingsniveau
)

for (middel in names(resultaten)) {
  drugs[[paste0(middel, "_ooit")]]          <- resultaten[[middel]]$ooit
  drugs[[paste0(middel, "_laatste_jaar")]]  <- resultaten[[middel]]$laatste_jaar
  drugs[[paste0(middel, "_frequentie")]]    <- resultaten[[middel]]$frequentie
}

drugs$sigaretten_uitgaansdag       <- sigaretten_uitgaansdag
drugs$glazen_alcohol_uitgaansdag   <- glazen_uitgaansdag
drugs$acceptatie_alcohol_vrienden  <- acceptatie_alcohol_vrienden
drugs$acceptatie_xtc_vrienden      <- acceptatie_xtc_vrienden
drugs$acceptatie_cocaine_vrienden  <- acceptatie_cocaine_vrienden
drugs$makkelijk_aan_drugs_komen    <- makkelijk_aan_drugs_komen
drugs$ervaren_gezondheid           <- ervaren_gezondheid
drugs$manier_verkrijgen_drugs      <- manier_verkrijgen_drugs
drugs$gemoed_dinsdag_na_xtc        <- gemoed_dinsdag_na_xtc

# Optioneel: een beetje item-nonrespons, zodat studenten met NA's leren omgaan.
voeg_missings_toe <- TRUE
if (voeg_missings_toe) {
  drugs$opleidingsniveau[sample(n, 6)] <- NA
  drugs$xtc_ooit[sample(n, 4)]         <- NA
  drugs$cocaine_ooit[sample(n, 4)]     <- NA
}


## 12. Controle: kloppen de cijfers met het rapport? -------------------------

cat("\n--- Gerealiseerde vs. gerapporteerde prevalenties (laatste jaar, %) ---\n")
for (middel in names(parameters)) {
  gerealiseerd <- mean(drugs[[paste0(middel, "_laatste_jaar")]], na.rm = TRUE) * 100
  cat(sprintf("%-10s gerealiseerd: %5.1f  |  rapport: %5.1f\n",
              middel, gerealiseerd, parameters[[middel]]$jaar))
}

cat("\n--- Controle trechterlogica (moet overal 0 zijn) ---\n")
for (middel in names(parameters)) {
  fout <- sum(drugs[[paste0(middel, "_laatste_jaar")]] > drugs[[paste0(middel, "_ooit")]], na.rm = TRUE)
  cat(sprintf("%-10s fouten: %d\n", middel, fout))
}

cat("\n--- Cannabis laatste jaar naar geslacht (%) (rapport: man 54,2 / vrouw 39,5) ---\n")
print(round(prop.table(table(drugs$geslacht, drugs$cannabis_laatste_jaar), 1) * 100, 1))

cat("\n--- Samenhang tussen middelen (laatste jaar, phi-coefficient) ---\n")
laatste_jaar_vars <- drugs[, grep("_laatste_jaar$", names(drugs))]
print(round(cor(laatste_jaar_vars, use = "pairwise.complete.obs"), 2))

cat("\n--- Sigaretten op een uitgaansdag: gerealiseerd gemiddelde ---\n")
cat(sprintf("gemiddelde: %.1f  |  rapport: 10,2\n", mean(drugs$sigaretten_uitgaansdag, na.rm = TRUE)))

cat("\n--- Glazen alcohol op een uitgaansdag: gerealiseerd gemiddelde naar geslacht ---\n")
print(round(tapply(drugs$glazen_alcohol_uitgaansdag, drugs$geslacht, mean, na.rm = TRUE), 1))
cat("(rapport, indrinken + uitgaan samen: man ca. 13,1 / vrouw ca. 9,2)\n")

cat("\n--- Acceptatie alcohol/XTC/cocaine onder vrienden (%) ---\n")
print(round(prop.table(table(drugs$acceptatie_alcohol_vrienden)) * 100, 1))
print(round(prop.table(table(drugs$acceptatie_xtc_vrienden)) * 100, 1))
print(round(prop.table(table(drugs$acceptatie_cocaine_vrienden)) * 100, 1))

cat("\n--- Samenhang: eigen cocainegebruik en acceptatie cocaine onder vrienden ---\n")
print(round(prop.table(table(drugs$cocaine_laatste_jaar, drugs$acceptatie_cocaine_vrienden), 1) * 100, 1))

cat("\n--- Ervaren gezondheid (%) (doel, omgerekend uit rapport: Slecht 4,3 / Matig 20,8 / Goed 58,9 / Zeer goed 10,4 / Uitstekend 5,6) ---\n")
print(round(prop.table(table(drugs$ervaren_gezondheid)) * 100, 1))
cat(sprintf("Aandeel 'Goed of beter' gerealiseerd: %.1f%%  |  rapport (goed+zeer goed): 74,9%%\n",
            mean(drugs$ervaren_gezondheid %in% c("Goed", "Zeer goed", "Uitstekend")) * 100))

cat("\n--- Ervaren gezondheid naar zware tabak/cannabisgebruikers (docentenaanname, niet uit rapport) ---\n")
print(round(prop.table(table(zwaar_tabak, drugs$ervaren_gezondheid), 1) * 100, 1))

cat("\n--- Manier waarop drugs verkregen worden (%) (rapport: dealer 40,8 / koopt vrienden 27,2 / krijgt vrienden 25,6 / post 1,5 / dropping 0 / anders 4,9) ---\n")
print(round(prop.table(table(drugs$manier_verkrijgen_drugs)) * 100, 1))

cat("\n--- Samenhang: gebruiksintensiteit (0 = geen van de vier drugs) en manier van verkrijgen (docentenaanname) ---\n")
print(round(tapply(intensiteit_drugs, drugs$manier_verkrijgen_drugs, mean), 1))

cat("\n--- Gemoedstoestand dinsdag na XTC-gebruik (FICTIEF, niet uit rapport) ---\n")
cat(sprintf("gemiddelde: %.1f (doel ca. 60)  |  sd: %.1f (doel ca. 15)\n",
            mean(drugs$gemoed_dinsdag_na_xtc, na.rm = TRUE),
            sd(drugs$gemoed_dinsdag_na_xtc, na.rm = TRUE)))
cat(sprintf("correlatie met XTC-gebruiksfrequentie: %.2f (doel: matig negatief, ca. -0,3 - conform de literatuur)\n",
            cor(as.numeric(drugs$xtc_frequentie), drugs$gemoed_dinsdag_na_xtc, use = "pairwise.complete.obs")))
cat(sprintf("correlatie met leeftijd: %.2f (doel: klein-matig positief, ca. 0,15)\n",
            cor(drugs$leeftijd, drugs$gemoed_dinsdag_na_xtc, use = "pairwise.complete.obs")))
cat(sprintf("aandeel score deelbaar door 5: %.1f%% (doel ca. 40%% + toevalstreffers vanuit de rest)\n",
            mean(drugs$gemoed_dinsdag_na_xtc[!is.na(drugs$gemoed_dinsdag_na_xtc)] %% 5 == 0) * 100))


## 13. Rijen husselen, met een niet-drinker in de eerste vijf rijen ----------
# Puur voor onderwijsdoeleinden: bij 98% laatste-jaar-alcoholgebruik bestaat
# anders een reele kans dat de eerste rijen (head(drugs)) allemaal precies
# hetzelfde alcohol_laatste_jaar-patroon laten zien, wat bij het inspecteren
# van de dataset in college een vertekend beeld geeft. respondentnr blijft
# het oorspronkelijke, unieke ID - alleen de rijvolgorde verandert.

niet_drinkers <- which(drugs$alcohol_laatste_jaar == 0)
if (length(niet_drinkers) == 0) {
  warning("Geen enkele respondent met alcohol_laatste_jaar == 0 - herschikking overgeslagen.")
} else {
  gekozen_niet_drinker <- sample(niet_drinkers, 1)
  overige_rijen        <- sample(setdiff(seq_len(nrow(drugs)), gekozen_niet_drinker))
  positie               <- sample(1:5, 1)  # willekeurige plek tussen rij 1 en 5
  nieuwe_volgorde <- append(overige_rijen, gekozen_niet_drinker, after = positie - 1)
  
  drugs <- drugs[nieuwe_volgorde, ]
  rownames(drugs) <- NULL
}

head(drugs[, c("respondentnr", "geslacht", "leeftijd", "alcohol_laatste_jaar")])


## 14. Opslaan ----------------------------------------------------------------
# Wordt weggeschreven naar een 'data/' submap van de werkmap; deze wordt
# aangemaakt als hij nog niet bestaat.

dir.create("data", showWarnings = FALSE)
write.csv2(drugs, "data/middelengebruik_uitgaanders.csv", row.names = FALSE)
saveRDS(drugs, "data/middelengebruik_uitgaanders.rds")

# Voor studenten die met SPSS werken:
# haven::write_sav(drugs, "data/middelengebruik_uitgaanders.sav")