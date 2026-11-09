#!/usr/bin/env Rscript
# =============================================================================
#  cli_statistique.R — passerelle en ligne de commande vers moteur_statistique.R
#  Utilisée par statistiques.py (application de bureau) et telegram_bot_statistiques.py.
#
#  Usage : Rscript cli_statistique.R <donnees.csv> <dossier_sortie>
#    - <donnees.csv>   : CSV normalisé (séparateur virgule, point décimal, UTF-8, en-tête)
#    - <dossier_sortie>: doit contenir params.json (la commande et ses paramètres) ;
#                        le script y écrit resultat.json et les figures PNG.
#
#  Commandes (champ "commande" de params.json) :
#    descriptif, normalite, t, anova, capabilite, kappa
#    et, dans commandes_avancees.R : ftest, khi2, carte, rr1, rr_croisee, rr_emboitee
#  Dépendance R : jsonlite  (sudo apt install r-cran-jsonlite)
#  Auteur : Jean-François BRUNET – JFBConseils – Octobre 2026
# =============================================================================

options(warn = 1, stringsAsFactors = FALSE)

# -- emplacement du script, pour retrouver moteur_statistique.R ----------------
args_all <- commandArgs(FALSE)
f <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
dossier_script <- if (length(f)) dirname(normalizePath(f[1])) else getwd()
args <- commandArgs(TRUE)
if (length(args) < 2) {
  cat("Usage : Rscript cli_statistique.R <donnees.csv> <dossier_sortie>\n", file = stderr())
  quit(status = 2)
}
entree <- args[1]
sortie <- args[2]
dir.create(sortie, showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("Package R manquant : jsonlite (sudo apt install r-cran-jsonlite)\n", file = stderr())
  quit(status = 3)
}
source(file.path(dossier_script, "moteur_statistique.R"), local = TRUE, encoding = "UTF-8")

ecrire <- function(res) {
  if (is.list(res$tables)) res$tables <- lapply(res$tables, function(t) { rownames(t) <- NULL; t })
  jsonlite::write_json(res, file.path(sortie, "resultat.json"), auto_unbox = TRUE,
                       na = "null", digits = NA, dataframe = "columns", pretty = FALSE)
}

# -- utilitaires -----------------------------------------------------------------
num <- function(v) {
  if (is.null(v) || length(v) == 0 || is.na(v)) return(NA_real_)
  suppressWarnings(as.numeric(v))
}
vrai <- function(v) isTRUE(v) || identical(v, "true") || identical(v, 1L) || identical(v, 1)

nom_colonne <- function(d, nom, role = "colonne") {
  if (is.null(nom) || !nzchar(nom) || !nom %in% names(d))
    stop(sprintf("%s introuvable : « %s ».", role, if (is.null(nom)) "" else nom), call. = FALSE)
  nom
}
col_num <- function(d, nom) {
  nom_colonne(d, nom)
  v <- d[[nom]]
  if (!is.numeric(v)) stop(sprintf("La colonne « %s » n'est pas numérique.", nom), call. = FALSE)
  v
}
noms_numeriques <- function(d, voulus = NULL) {
  tous <- names(d)[vapply(d, is.numeric, logical(1))]
  if (!is.null(voulus) && length(voulus)) {
    inconnus <- setdiff(voulus, tous)
    if (length(inconnus)) stop("Colonne(s) non numérique(s) ou introuvable(s) : ", paste(inconnus, collapse = ", "), call. = FALSE)
    tous <- voulus
  }
  if (!length(tous)) stop("Aucune colonne numérique.", call. = FALSE)
  tous
}
fmt <- function(v, n = 6) {
  ifelse(is.na(v), "non calculable",
         ifelse(is.infinite(v), ifelse(v > 0, "+∞", "−∞"),
                sub("\\.", ",", format(signif(v, n), scientific = FALSE, trim = TRUE))))
}
fmt_p <- function(p) {
  if (is.na(p)) "non calculable" else if (p < 1e-4) "< 0,0001" else sub("\\.", ",", formatC(round(p, 4), format = "f", digits = 4))
}
ALPHA <- 0.05  # risque alpha des tests (fixé par params.json : alpha_test)
alpha_txt <- function() alpha_dec(ALPHA)
conf_txt <- function() pct_txt(1 - ALPHA)
regle <- function() sprintf("Règle de décision (risque α = %s) : p-value ≥ α → H0 non rejetée ; p-value < α → H0 rejetée au profit de H1.", alpha_txt())
# Phrase de décision : « ok » quand H0 n'est pas rejetée, « ko » quand elle l'est.
decision <- function(pv, ok, ko) {
  if (length(pv) != 1 || is.na(pv)) return("Décision : non calculable.")
  rej <- pv < ALPHA
  sprintf("Décision : p-value %s %s α (%s) → H0 %s : %s", fmt_p(pv), if (rej) "<" else "≥", alpha_txt(),
          if (rej) "rejetée" else "non rejetée", if (rej) ko else ok)
}
ECH <- 1   # échelle de rendu des figures (1 = taille de base) ; fixée par params.json
PREFIXE <- ""        # préfixe des fichiers PNG (analyse par groupe : g1_, g2_, ...)
ETIQ_GROUPE <- ""    # étiquette du groupe en cours (« Centre = Est »), inscrite en haut de chaque figure
lire_png <- function(nom, largeur = 900, hauteur = 600) {
  nom <- paste0(PREFIXE, nom)
  chemin <- file.path(sortie, nom)
  bandeau <- if (nzchar(ETIQ_GROUPE)) 38 else 0           # bandeau supplémentaire pour l'étiquette du groupe
  l <- round(largeur * ECH); h <- round((hauteur + bandeau) * ECH); r <- 110 * ECH
  if (isTRUE(capabilities("cairo"))) png(chemin, width = l, height = h, res = r, type = "cairo")
  else png(chemin, width = l, height = h, res = r)
  if (nzchar(ETIQ_GROUPE)) par(oma = c(0, 0, 2.4, 0))
  nom
}
# Les commandes ferment leurs figures par dev.off() : on y inscrit l'étiquette du groupe avant la fermeture.
dev.off <- function(which = grDevices::dev.cur()) {
  if (nzchar(ETIQ_GROUPE) && which > 1)
    try(graphics::mtext(ETIQ_GROUPE, side = 3, outer = TRUE, line = 0.3, adj = 0, font = 2, cex = 1.05, col = "#b03030"), silent = TRUE)
  grDevices::dev.off(which)
}
# Étiquettes d'une colonne d'ordre : dates ISO (AAAA-MM-JJ [hh:mm[:ss]]) écrites jj/mm/aaaa [hh:mm].
etiq_ordre <- function(v) {
  v <- as.character(v)
  iso <- grepl("^\\d{4}-\\d{2}-\\d{2}", v)
  if (length(v) && all(iso | is.na(v))) {
    a <- substr(v, 1, 10); h <- ifelse(nchar(v) >= 16, substr(v, 12, 16), "")
    out <- paste0(substr(a, 9, 10), "/", substr(a, 6, 7), "/", substr(a, 1, 4), ifelse(nzchar(h), paste0(" ", h), ""))
    out[is.na(v)] <- NA_character_
    return(out)
  }
  v
}
sans_na <- function(v) v[is.finite(v)]

lignes_stat <- function(d, cols) {
  do.call(rbind, lapply(cols, function(nm) {
    x <- sans_na(d[[nm]])
    data.frame(Serie = nm, n = length(x), Moyenne = if (length(x)) mean(x) else NA_real_,
               Ecart_type = if (length(x) >= 2) stats::sd(x) else NA_real_, stringsAsFactors = FALSE)
  }))
}

# -- commandes -------------------------------------------------------------------
cmd_descriptif <- function(d, p) {
  cols <- noms_numeriques(d, unlist(p$colonnes))
  tab <- resume_multi_series(d[, cols, drop = FALSE], alpha = ALPHA)
  k <- length(cols)
  series <- lapply(cols, function(nm) sans_na(d[[nm]]))
  names(series) <- cols

  fig1 <- lire_png("boites.png", largeur = max(700, 150 * k + 300), hauteur = 560)
  par(mar = c(5, 4.5, 3, 1))
  boxplot(series, main = "Boîtes à moustaches par série", ylab = "Mesure", col = "#cfe0f5", las = if (k > 6) 2 else 1)
  invisible(dev.off())

  nc <- min(k, 3); nl <- ceiling(k / nc)
  fig2 <- lire_png("histogrammes.png", largeur = 330 * nc + 60, hauteur = 300 * nl + 60)
  par(mfrow = c(nl, nc), mar = c(4, 4, 3, 1))
  for (nm in cols) {
    x <- series[[nm]]
    hist(x, main = nm, xlab = "Mesure", ylab = "Effectif", col = "#cfe0f5", border = "white")
  }
  invisible(dev.off())

  tab_aff <- tab[, setdiff(names(tab), "Interpretation_normalite"), drop = FALSE]
  pn <- tab$P_Shapiro_Wilk
  rej <- tab$Serie[!is.na(pn) & pn < ALPHA]
  nrej <- tab$Serie[!is.na(pn) & pn >= ALPHA]
  list(titre = "Statistiques descriptives",
       conclusion = paste0(k, " série(s) décrite(s).",
                           if (length(rej)) paste0(" Normalité rejetée (p-value < α = ", alpha_txt(), ") : ", paste(rej, collapse = ", "), ".") else "",
                           if (length(nrej)) paste0(" Normalité retenue (p-value ≥ α) : ", paste(nrej, collapse = ", "), ".") else ""),
       lignes = c(sprintf("%d série(s) numérique(s) analysée(s).", k),
                  "Test de normalité de Shapiro-Wilk — H0 : la série suit une loi normale ; H1 : elle ne suit pas une loi normale.",
                  regle(),
                  paste0(tab$Serie, " — Shapiro-Wilk : ", tab$Interpretation_normalite)),
       tables = list(Descriptif = tab_aff), figures = c(fig1, fig2),
       avertissements = list())
}

# -- tests de normalité (implémentés ici : aucun package supplémentaire n'est requis) ------------------------
TESTS_NORMALITE <- c(sw = "Shapiro-Wilk", ad = "Anderson-Darling", lf = "Kolmogorov-Smirnov (Lilliefors)",
                     cvm = "Cramér-von Mises", jb = "Jarque-Bera", rj = "Ryan-Joiner")

tn_sw <- function(x) {
  n <- length(x)
  if (n < 3 || n > 5000 || stats::sd(x) == 0) return(c(NA_real_, NA_real_))
  sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
  if (is.null(sw)) c(NA_real_, NA_real_) else c(unname(sw$statistic), sw$p.value)
}
tn_probas <- function(x) {
  p <- stats::pnorm((sort(x) - mean(x)) / stats::sd(x))
  pmin(pmax(p, 1e-12), 1 - 1e-12)
}
tn_ad <- function(x) {                       # Anderson-Darling, p-value : approximation de Stephens (1986)
  n <- length(x)
  if (n < 8 || stats::sd(x) == 0) return(c(NA_real_, NA_real_))
  p <- tn_probas(x); i <- seq_len(n)
  A <- -n - mean((2 * i - 1) * (log(p) + log(1 - rev(p))))
  AA <- (1 + 0.75 / n + 2.25 / n^2) * A
  pv <- if (AA < 0.2) 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
        else if (AA < 0.34) 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
        else if (AA < 0.6) exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
        else if (AA < 10) exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
        else 3.7e-24
  c(A, min(1, max(0, pv)))
}
tn_cvm <- function(x) {                      # Cramér-von Mises, p-value : approximation de Stephens (1986)
  n <- length(x)
  if (n < 8 || stats::sd(x) == 0) return(c(NA_real_, NA_real_))
  p <- tn_probas(x); i <- seq_len(n)
  W <- 1 / (12 * n) + sum((p - (2 * i - 1) / (2 * n))^2)
  WW <- (1 + 0.5 / n) * W
  pv <- if (WW < 0.0275) 1 - exp(-13.953 + 775.5 * WW - 12542.61 * WW^2)
        else if (WW < 0.051) 1 - exp(-5.903 + 179.546 * WW - 1515.29 * WW^2)
        else if (WW < 0.092) exp(0.886 - 31.62 * WW + 10.897 * WW^2)
        else if (WW < 1.1) exp(1.111 - 34.242 * WW + 12.832 * WW^2)
        else 7.37e-10
  c(W, min(1, max(0, pv)))
}
tn_lf <- function(x) {                       # Kolmogorov-Smirnov avec correction de Lilliefors (paramètres estimés)
  n <- length(x)
  if (n < 5 || stats::sd(x) == 0) return(c(NA_real_, NA_real_))
  p <- tn_probas(x); i <- seq_len(n)
  D <- max(max(i / n - p), max(p - (i - 1) / n))
  Kd <- D; nd <- n
  if (n > 100) { Kd <- D * (n / 100)^0.49; nd <- 100 }
  pv <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598 / sqrt(nd) + 1.67997 / nd)
  if (pv > 0.1) {
    kk <- (sqrt(n) - 0.01 + 0.85 / sqrt(n)) * D
    pv <- if (kk <= 0.302) 1
          else if (kk <= 0.5) 2.76773 - 19.828315 * kk + 80.709644 * kk^2 - 138.55152 * kk^3 + 81.218052 * kk^4
          else if (kk <= 0.9) -4.901232 + 40.662806 * kk - 97.490286 * kk^2 + 94.029866 * kk^3 - 32.355711 * kk^4
          else if (kk <= 1.31) 6.198765 - 19.558097 * kk + 23.186922 * kk^2 - 12.234627 * kk^3 + 2.423045 * kk^4
          else 0
  }
  c(D, min(1, max(0, pv)))
}
tn_jb <- function(x) {                       # Jarque-Bera (loi asymptotique du khi² à 2 ddl : prudence si n est petit)
  n <- length(x)
  if (n < 8 || stats::sd(x) == 0) return(c(NA_real_, NA_real_))
  m <- x - mean(x); s2 <- mean(m^2)
  S <- mean(m^3) / s2^1.5; K <- mean(m^4) / s2^2
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
  c(JB, stats::pchisq(JB, 2, lower.tail = FALSE))
}
tn_rj <- function(x) {                       # Ryan-Joiner : r = corrélation entre les données ordonnées et les scores normaux (Blom) ;
  n <- length(x)                             # p-value par simulation (la loi de r sous H0 ne dépend que de n)
  if (n < 5 || stats::sd(x) == 0) return(c(NA_real_, NA_real_))
  sc <- stats::qnorm((seq_len(n) - 0.375) / (n + 0.25))
  r <- stats::cor(sort(x), sc)
  nsim <- as.integer(min(10000, max(2000, 3e6 / n)))
  graine <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(graine)) assign(".Random.seed", graine, envir = globalenv()), add = TRUE)
  set.seed(20240601 + n)
  rs <- replicate(nsim, stats::cor(sort(stats::rnorm(n)), sc))
  c(r, (1 + sum(rs <= r)) / (nsim + 1))
}
tn_calcul <- function(id, x) switch(id, rj = tn_rj(x), sw = tn_sw(x), ad = tn_ad(x), lf = tn_lf(x), cvm = tn_cvm(x), jb = tn_jb(x))

cmd_normalite <- function(d, p) {
  voulus <- unlist(p$colonnes)
  if (!length(voulus)) voulus <- p$colonne
  if (is.null(voulus) || !length(voulus)) stop("Choisir au moins une série.", call. = FALSE)
  ids <- intersect(unlist(p$tests), names(TESTS_NORMALITE))
  if (!length(ids)) ids <- "sw"
  cols <- noms_numeriques(d, voulus)
  lignes <- character(0); figs <- character(0); ecartees <- character(0)
  tab <- data.frame(Serie = character(0), Test = character(0), Statistique = numeric(0), p_value = numeric(0),
                    Decision = character(0), stringsAsFactors = FALSE)
  par_serie <- list()
  for (nom in cols) {
    x <- sans_na(col_num(d, nom))
    n <- length(x)
    if (n < 3) { ecartees <- c(ecartees, nom); next }
    rejets <- character(0); gardes <- character(0); non_calc <- character(0)
    for (id in ids) {
      r <- tn_calcul(id, x)
      tab[nrow(tab) + 1, ] <- list(nom, TESTS_NORMALITE[[id]], r[1], r[2],
                                   if (is.na(r[2])) "non calculable" else if (r[2] < ALPHA) "H0 rejetée : normalité rejetée" else "H0 non rejetée : normalité retenue")
      if (is.na(r[2])) non_calc <- c(non_calc, TESTS_NORMALITE[[id]])
      else if (r[2] < ALPHA) rejets <- c(rejets, TESTS_NORMALITE[[id]])
      else gardes <- c(gardes, TESTS_NORMALITE[[id]])
      lignes <- c(lignes, sprintf("%s — %s (n = %d) : %s = %s ; p-value %s", nom, TESTS_NORMALITE[[id]], n,
                                  if (id == "sw") "W" else if (id == "ad") "A²" else if (id == "lf") "D" else if (id == "cvm") "W²" else if (id == "rj") "RJ" else "JB",
                                  fmt(r[1]), fmt_p(r[2])))
      lignes[length(lignes)] <- paste0(lignes[length(lignes)], if (is.na(r[2])) "" else if (r[2] < ALPHA) " < α → normalité rejetée" else " ≥ α → normalité retenue")
    }
    par_serie[[nom]] <- list(rejets = rejets, gardes = gardes, non_calc = non_calc, n = n)
    fig <- lire_png(sprintf("normalite_%d.png", length(par_serie)), largeur = 1000, hauteur = 480)
    par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
    h <- hist(x, plot = FALSE); sx <- stats::sd(x)
    yh <- max(h$density, if (sx > 0) dnorm(mean(x), mean(x), sx), na.rm = TRUE)       # la courbe normale doit tenir dans le cadre
    hist(x, freq = FALSE, main = paste("Distribution :", nom), xlab = "Mesure", ylab = "Densité", col = "#cfe0f5", border = "white",
         breaks = h$breaks, ylim = c(0, yh * 1.1))
    if (sx > 0) curve(dnorm(x, mean(x), sx), add = TRUE, lwd = 2, col = "#b03030")
    mx <- mean(x); sx2 <- stats::sd(x); zr <- range(stats::qnorm(stats::ppoints(n))); zz <- seq(zr[1] - 0.2, zr[2] + 0.2, length.out = 100)
    ec <- stats::qnorm(1 - ALPHA / 2) * sx2 * sqrt((1 + zz^2 / 2) / n)          # limites de confiance ponctuelles de la droite (comme Minitab)
    qqnorm(x, main = paste("Droite de Henry :", nom), xlab = "Quantiles théoriques", ylab = "Quantiles observés", pch = 19, col = "#2a5d9f",
           ylim = range(x, mx + sx2 * zz - ec, mx + sx2 * zz + ec))
    lines(zz, mx + sx2 * zz, col = "#b03030", lwd = 2)
    lines(zz, mx + sx2 * zz - ec, col = "#b03030", lty = 2); lines(zz, mx + sx2 * zz + ec, col = "#b03030", lty = 2)
    legend("topleft", bty = "n", cex = 0.8, lty = c(1, 2), col = "#b03030", legend = c("Droite ajustée", sprintf("Limites de confiance à %s", conf_txt())))
    invisible(dev.off())
    figs <- c(figs, fig)
  }
  if (!length(par_serie)) stop("Il faut au moins 3 valeurs valides par série.", call. = FALSE)
  verdict <- function(nom) {
    r <- par_serie[[nom]]
    if (length(r$rejets) + length(r$gardes) == 0) return("test non calculable (effectif insuffisant, données constantes ou n > 5000 pour Shapiro-Wilk).")
    if (!length(r$rejets)) return(if (length(ids) == 1) sprintf("p-value ≥ α (%s) : H0 non rejetée, la normalité est retenue.", alpha_txt()) else sprintf("p-value ≥ α (%s) pour tous les tests : H0 non rejetée, la normalité est retenue.", alpha_txt()))
    if (!length(r$gardes)) return(if (length(ids) == 1) sprintf("p-value < α (%s) : H0 rejetée, la normalité est rejetée.", alpha_txt()) else sprintf("p-value < α (%s) pour tous les tests : H0 rejetée, la normalité est rejetée.", alpha_txt()))
    paste0("avis partagés — rejetée par : ", paste(r$rejets, collapse = ", "), " ; non rejetée par : ", paste(r$gardes, collapse = ", "), ".")
  }
  noms <- names(par_serie)
  une <- length(noms) == 1
  lignes <- c("H0 : la série suit une loi normale ; H1 : elle ne suit pas une loi normale (quel que soit le test).", regle(),
              "Pour valider la normalité, il faut que la p-value soit supérieure ou égale à α.", "", lignes)
  concl <- if (une) paste0("« ", noms, " » : ", verdict(noms))
           else paste0(length(noms), " série(s) testée(s). ", paste0(noms, " : ", vapply(noms, verdict, ""), collapse = " "))
  avert <- list()
  if (any(vapply(par_serie, function(r) length(r$rejets) > 0, logical(1))))
    avert <- c(avert, list("Un rejet peut venir d'un petit écart sans importance pratique, surtout avec un grand effectif : examiner aussi les graphiques."))
  if (length(ids) > 1) avert <- c(avert, list("Les tests n'ont pas tous la même sensibilité (Shapiro-Wilk et Anderson-Darling sont réputés les plus puissants ; Kolmogorov-Smirnov avec correction de Lilliefors l'est moins ; Ryan-Joiner, proche de Shapiro-Francia, est le test proposé par Minitab ; Jarque-Bera repose sur une loi asymptotique, à éviter avec peu de données) : des avis partagés sont possibles."))
  if (length(ecartees)) avert <- c(avert, list(paste0("Série(s) ignorée(s) faute d'au moins 3 valeurs valides : ", paste(ecartees, collapse = ", "), ".")))
  list(titre = if (une && length(ids) == 1) paste0("Test de normalité (", TESTS_NORMALITE[[ids]], ") — ", noms)
               else if (une) paste("Tests de normalité —", noms) else "Tests de normalité",
       lignes = lignes, conclusion = concl,
       tables = list(Normalite = tab), figures = figs, avertissements = avert)
}

cohen_d <- function(x, y = NULL, mu = 0, paired = FALSE) {
  if (is.null(y)) return((mean(x) - mu) / stats::sd(x))
  if (paired) { dd <- x - y; return(mean(dd) / stats::sd(dd)) }
  n1 <- length(x); n2 <- length(y)
  sp <- sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) / (n1 + n2 - 2))
  (mean(x) - mean(y)) / sp
}

cmd_t <- function(d, p) {
  xn <- nom_colonne(d, p$x, "Série X")
  x <- col_num(d, xn)
  yn <- p$y
  y <- NULL
  if (!is.null(yn) && nzchar(yn)) y <- col_num(d, nom_colonne(d, yn, "Série Y"))
  paired <- vrai(p$apparie)
  alt <- p$alternative %||% "two.sided"
  mu <- num(p$mu); if (is.na(mu)) mu <- 0
  res <- test_t(x, y, mu = mu, paired = paired && !is.null(y), alternative = alt, var_equal = vrai(p$variances_egales), conf_level = 1 - ALPHA)

  if (is.null(y)) {
    xc <- sans_na(x)
    d_c <- cohen_d(xc, mu = mu)
    cols <- xn
    hyp <- switch(alt, two.sided = sprintf("H1 : la moyenne de « %s » diffère de %s.", xn, fmt(mu)),
                  greater = sprintf("H1 : la moyenne de « %s » est supérieure à %s.", xn, fmt(mu)),
                  less = sprintf("H1 : la moyenne de « %s » est inférieure à %s.", xn, fmt(mu)))
    h0 <- switch(alt, two.sided = sprintf("H0 : la moyenne de « %s » est égale à %s.", xn, fmt(mu)),
                 greater = sprintf("H0 : la moyenne de « %s » est inférieure ou égale à %s.", xn, fmt(mu)),
                 less = sprintf("H0 : la moyenne de « %s » est supérieure ou égale à %s.", xn, fmt(mu)))
    tab <- lignes_stat(d, xn)
  } else {
    if (paired) { ok <- is.finite(x) & is.finite(y); xc <- x[ok]; yc <- y[ok]
    } else { xc <- sans_na(x); yc <- sans_na(y) }
    d_c <- cohen_d(xc, yc, paired = paired)
    cols <- c(xn, yn)
    hyp <- switch(alt, two.sided = sprintf("H1 : les moyennes de « %s » et « %s » diffèrent.", xn, yn),
                  greater = sprintf("H1 : la moyenne de « %s » est supérieure à celle de « %s ».", xn, yn),
                  less = sprintf("H1 : la moyenne de « %s » est inférieure à celle de « %s ».", xn, yn))
    h0 <- switch(alt, two.sided = sprintf("H0 : les moyennes de « %s » et « %s » sont égales.", xn, yn),
                 greater = sprintf("H0 : la moyenne de « %s » est inférieure ou égale à celle de « %s ».", xn, yn),
                 less = sprintf("H0 : la moyenne de « %s » est supérieure ou égale à celle de « %s ».", xn, yn))
    tab <- lignes_stat(d, cols)
    if (paired) tab$n <- length(xc)
  }
  fig <- lire_png("test_t.png", largeur = 700, hauteur = 520)
  par(mar = c(5, 4.5, 3, 1))
  sets <- lapply(cols, function(nm) sans_na(d[[nm]])); names(sets) <- cols
  boxplot(sets, main = "Séries comparées", ylab = "Mesure", col = "#cfe0f5")
  if (is.null(y)) abline(h = mu, lty = 2, col = "#b03030")
  invisible(dev.off())

  ic <- res$intervalle_confiance
  avert <- list("Vérifier l'indépendance des observations et la forme des distributions (voir le test de normalité). Une différence significative n'est pas forcément importante en pratique : considérer aussi la taille d'effet.")
  if (!is.null(y) && !paired && !vrai(p$variances_egales)) avert <- c(avert, list("Test de Welch : ne suppose pas l'égalité des variances."))
  dec_t <- decision(res$p_value, "aucune différence n'est mise en évidence.", "la différence est statistiquement significative.")
  list(titre = paste("Test t —", res$type),
       lignes = c(h0, hyp, regle(), sprintf("Statistique t : %s", fmt(res$statistic)), sprintf("Degrés de liberté : %s", fmt(res$ddl)),
                  sprintf("p-value : %s", fmt_p(res$p_value)),
                  sprintf("Intervalle de confiance à %s : [%s ; %s]", conf_txt(), fmt(ic[1]), fmt(ic[2])),
                  sprintf("Taille d'effet (d de Cohen) : %s", fmt(d_c, 4)), dec_t),
       conclusion = sprintf("%s (d de Cohen %s)", sub("^Décision : ", "", sub("\\.$", "", dec_t)), fmt(d_c, 3)),
       tables = list(Series = tab), figures = c(fig), avertissements = avert)
}

cmd_anova <- function(d, p) {
  if (identical(p$mode, "long")) {
    vn <- nom_colonne(d, p$valeur, "Colonne mesure"); gn <- nom_colonne(d, p$groupe, "Colonne groupe")
    v <- col_num(d, vn)
    g <- d[[gn]]
    dd <- data.frame(valeur = v, groupe = g)
  } else {
    cols <- noms_numeriques(d, unlist(p$colonnes))
    if (length(cols) < 2) stop("Sélectionnez au moins deux colonnes numériques.", call. = FALSE)
    s <- stack(d[, cols, drop = FALSE])
    dd <- data.frame(valeur = s$values, groupe = factor(as.character(s$ind), levels = cols))
  }
  dd <- dd[is.finite(dd$valeur) & !is.na(dd$groupe), , drop = FALSE]
  dd$groupe <- as.factor(dd$groupe)
  dd$groupe <- droplevels(dd$groupe)
  if (nlevels(dd$groupe) < 2) stop("L'ANOVA exige au moins deux groupes.", call. = FALSE)
  eff <- table(dd$groupe)
  if (all(eff <= 1)) stop("Chaque groupe ne contient qu'une observation : l'ANOVA n'est pas calculable.", call. = FALSE)

  fit <- stats::aov(valeur ~ groupe, data = dd)
  tab <- summary(fit)[[1]]
  tab_df <- data.frame(Source = ifelse(trimws(rownames(tab)) == "Residuals", "Résidus", "Groupe"), ddl = tab[["Df"]], Somme_carres = tab[["Sum Sq"]],
                       Carre_moyen = tab[["Mean Sq"]], F = tab[["F value"]], p_value = tab[["Pr(>F)"]],
                       stringsAsFactors = FALSE)
  pv <- tab[["Pr(>F)"]][1]
  moy <- data.frame(Groupe = levels(dd$groupe),
                    n = as.integer(tapply(dd$valeur, dd$groupe, length)),
                    Moyenne = as.numeric(tapply(dd$valeur, dd$groupe, mean)),
                    Ecart_type = as.numeric(tapply(dd$valeur, dd$groupe, stats::sd)), stringsAsFactors = FALSE)

  res_sw <- NA_real_
  r <- stats::residuals(fit)
  if (length(r) >= 3 && length(r) <= 5000 && stats::sd(r) > 0) res_sw <- tryCatch(stats::shapiro.test(r)$p.value, error = function(e) NA_real_)
  fl <- tryCatch(stats::fligner.test(valeur ~ groupe, data = dd)$p.value, error = function(e) NA_real_)
  lev <- if (AVANCEES) levene_bf(dd$valeur, dd$groupe) else c(stat = NA_real_, ddl1 = NA_real_, ddl2 = NA_real_, p = NA_real_)
  wl <- tryCatch(stats::oneway.test(valeur ~ groupe, data = dd, var.equal = FALSE), error = function(e) NULL)
  kw <- tryCatch(stats::kruskal.test(valeur ~ groupe, data = dd), error = function(e) NULL)
  ss <- tab[["Sum Sq"]]; eta2 <- ss[1] / sum(ss)
  comp <- data.frame(Test = c("ANOVA de Welch (variances inégales)", "Kruskal-Wallis (non paramétrique)",
                              "Levene — homogénéité des variances", "Fligner-Killeen — homogénéité des variances",
                              "Shapiro-Wilk — normalité des résidus"),
                     Statistique = c(if (is.null(wl)) NA_real_ else unname(wl$statistic), if (is.null(kw)) NA_real_ else unname(kw$statistic),
                                     unname(lev["stat"]), NA_real_, NA_real_),
                     ddl = c(if (is.null(wl)) NA_character_ else sprintf("%s ; %s", fmt(unname(wl$parameter[1]), 4), fmt(unname(wl$parameter[2]), 4)),
                             if (is.null(kw)) NA_character_ else as.character(unname(kw$parameter)),
                             if (is.na(lev["ddl1"])) NA_character_ else sprintf("%d ; %d", as.integer(lev["ddl1"]), as.integer(lev["ddl2"])),
                             NA_character_, NA_character_),
                     p_value = c(if (is.null(wl)) NA_real_ else wl$p.value, if (is.null(kw)) NA_real_ else kw$p.value,
                                 unname(lev["p"]), fl, res_sw), stringsAsFactors = FALSE)
  d_ok <- function(pv, ok, ko) if (is.na(pv)) "non calculable" else if (pv < ALPHA) ko else ok
  comp$Decision <- c(d_ok(comp$p_value[1], "moyennes non différentes", "moyennes différentes"),
                     d_ok(comp$p_value[2], "distributions non différentes", "distributions différentes"),
                     d_ok(comp$p_value[3], "variances homogènes (H0 retenue)", "variances différentes (H0 rejetée)"),
                     d_ok(comp$p_value[4], "variances homogènes (H0 retenue)", "variances différentes (H0 rejetée)"),
                     d_ok(comp$p_value[5], "normalité retenue (H0 non rejetée)", "normalité rejetée (H0 rejetée)"))
  tables <- list(ANOVA = tab_df, Groupes = moy, `Tests complémentaires` = comp)
  lignes <- c("H0 : toutes les moyennes de groupe sont égales ; H1 : au moins une moyenne diffère.",
              sprintf("Nombre de groupes : %d — observations : %d", nlevels(dd$groupe), nrow(dd)),
              sprintf("F = %s — p-value : %s", fmt(tab[["F value"]][1], 5), fmt_p(pv)),
              regle(),
              decision(pv, "aucune différence de moyenne n'est mise en évidence.", "au moins une moyenne de groupe diffère statistiquement."),
              sprintf("Taille d'effet : η² = %s (part de la variance expliquée par le facteur)", fmt(eta2, 3)),
              sprintf("Contrôle des résidus — Shapiro-Wilk : p = %s ; homogénéité des variances — Levene : p = %s, Fligner-Killeen : p = %s",
                      fmt_p(res_sw), fmt_p(unname(lev["p"])), fmt_p(fl)),
              sprintf("Pour ces trois contrôles, H0 est l'hypothèse requise (normalité des résidus ; variances égales) : elle est retenue si p-value ≥ α = %s.", alpha_txt()))
  avert <- list("Vérifier l'indépendance, la normalité approximative des résidus et l'homogénéité des variances.")
  var_inegales <- !is.na(lev["p"]) && lev["p"] < ALPHA
  if (var_inegales && !is.null(wl)) {
    lignes <- c(lignes, sprintf("Les variances diffèrent (Levene) : se référer à l'ANOVA de Welch — F = %s, p-value %s.", fmt(unname(wl$statistic), 5), fmt_p(wl$p.value)))
    pw <- tryCatch(stats::pairwise.t.test(dd$valeur, dd$groupe, pool.sd = FALSE, p.adjust.method = "holm"), error = function(e) NULL)
    if (!is.null(pw) && !is.na(wl$p.value) && wl$p.value < ALPHA) {
      m <- pw$p.value
      pairs <- which(!is.na(m), arr.ind = TRUE)
      tables$`Comparaisons deux à deux (Welch, Holm)` <- data.frame(Comparaison = sprintf("%s - %s", colnames(m)[pairs[, 2]], rownames(m)[pairs[, 1]]),
                                                                    p_ajustee = m[pairs], stringsAsFactors = FALSE)
      avert <- c(avert, list("Variances inégales : comparaisons deux à deux par tests de Welch avec ajustement de Holm (le test de Tukey suppose des variances égales)."))
    }
  }
  if (!is.na(pv) && pv < ALPHA && nlevels(dd$groupe) >= 2) {
    tk <- tryCatch(stats::TukeyHSD(fit, conf.level = 1 - ALPHA)$groupe, error = function(e) NULL)
    if (!is.null(tk)) {
      tables$Tukey <- data.frame(Comparaison = rownames(tk), Ecart = tk[, "diff"], Borne_inf = tk[, "lwr"],
                                 Borne_sup = tk[, "upr"], p_ajustee = tk[, "p adj"], stringsAsFactors = FALSE)
      avert <- c(avert, list("Comparaisons deux à deux : méthode de Tukey (HSD), p-values ajustées pour les comparaisons multiples."))
    }
  }
  nl <- nlevels(dd$groupe)
  fig <- lire_png("anova.png", largeur = max(700, 110 * nl + 300), hauteur = 560)
  par(mar = c(5, 4.5, 3, 1))
  boxplot(valeur ~ groupe, data = dd, main = "Mesures par groupe", xlab = "Groupe", ylab = "Mesure", col = "#cfe0f5", las = if (nl > 6) 2 else 1)
  points(seq_len(nl), moy$Moyenne, pch = 18, cex = 1.6, col = "#b03030")
  invisible(dev.off())
  fig2 <- lire_png("anova_intervalles.png", largeur = max(700, 110 * nl + 300), hauteur = 520)
  par(mar = c(5, 4.5, 3, 1))
  se <- moy$Ecart_type / sqrt(moy$n); tq <- stats::qt(1 - ALPHA / 2, pmax(moy$n - 1, 1))
  bas <- moy$Moyenne - tq * se; haut <- moy$Moyenne + tq * se
  ok <- is.finite(bas) & is.finite(haut)
  yl <- range(c(bas[ok], haut[ok], moy$Moyenne), na.rm = TRUE)
  plot(seq_len(nl), moy$Moyenne, ylim = yl, xlim = c(0.5, nl + 0.5), xaxt = "n", pch = 19, cex = 1.3, col = "#2a5d9f",
       xlab = "Groupe", ylab = "Moyenne", main = paste0("Moyennes et intervalles de confiance à ", conf_txt()))
  axis(1, at = seq_len(nl), labels = moy$Groupe, las = if (nl > 6) 2 else 1)
  arrows(seq_len(nl)[ok], bas[ok], seq_len(nl)[ok], haut[ok], angle = 90, code = 3, length = 0.06, lwd = 2, col = "#2a5d9f")
  abline(h = mean(dd$valeur), lty = 2, col = "#888888")
  invisible(dev.off())
  list(titre = "ANOVA à un facteur", lignes = lignes,
       conclusion = sprintf("F = %s. %s", fmt(tab[["F value"]][1], 4),
                            sub("^Décision : ", "", decision(pv, "aucune différence de moyenne n'est mise en évidence.", "au moins une moyenne de groupe diffère statistiquement."))),
       tables = tables, figures = c(fig, fig2), avertissements = avert)
}

cmd_capabilite <- function(d, p) {
  nom <- nom_colonne(d, p$colonne, "Série de mesures")
  x <- sans_na(col_num(d, nom))
  lsl <- num(p$lsl); usl <- num(p$usl); cible <- num(p$cible); sw <- num(p$sigma_intra)
  res <- suppressWarnings(indices_capabilite(x, LSL = lsl, USL = usl, cible = cible, sigma_within = sw))
  longue <- data.frame(Indicateur = names(res), Valeur = as.numeric(unlist(res[1, ], use.names = FALSE)), stringsAsFactors = FALSE)
  longue <- longue[!is.na(longue$Valeur), , drop = FALSE]
  avert <- list()
  if (is.na(sw) || sw <= 0) avert <- c(avert, list("Écart-type intra-sous-groupe non fourni : Cm, Cmk, Cp et Cpk ne sont pas calculés (l'écart-type global n'est pas substitué). Pp et Ppk utilisent l'écart-type global."))
  if (is.na(lsl) || is.na(usl)) avert <- c(avert, list("Une seule limite de spécification : les indices bilatéraux (Cp, Pp, Cm) ne sont pas calculables."))
  avert <- c(avert, list("Les PPM sont estimés avec une loi normale ajustée : indicatifs, ils supposent un procédé stable et des données approximativement normales."))

  fig <- lire_png("capabilite.png", largeur = 900, hauteur = 560)
  par(mar = c(5, 4.5, 3, 1))
  s <- stats::sd(x)
  h <- hist(x, plot = FALSE)
  xl <- range(c(x, lsl, usl, cible, if (s > 0) mean(x) + c(-3.5, 3.5) * s), na.rm = TRUE)     # la courbe normale doit tenir dans le cadre
  yh <- max(h$density, if (s > 0) dnorm(mean(x), mean(x), s), na.rm = TRUE)
  hist(x, freq = FALSE, main = paste("Distribution :", nom), xlab = "Mesure", ylab = "Densité", col = "#cfe0f5", border = "white",
       xlim = xl, ylim = c(0, yh * 1.3), breaks = h$breaks)
  if (s > 0) curve(dnorm(x, mean(x), s), from = xl[1], to = xl[2], n = 400, add = TRUE, lwd = 2, col = "#2a5d9f")
  if (!is.na(lsl)) abline(v = lsl, lty = 2, lwd = 2, col = "#b03030")
  if (!is.na(usl)) abline(v = usl, lty = 2, lwd = 2, col = "#b03030")
  if (!is.na(cible)) abline(v = cible, lty = 3, lwd = 2, col = "#2e8b57")
  abline(v = mean(x), lty = 1, col = "#444444")
  leg <- c("Moyenne", if (!is.na(lsl) || !is.na(usl)) "Limites de spécification", if (!is.na(cible)) "Cible")
  col <- c("#444444", if (!is.na(lsl) || !is.na(usl)) "#b03030", if (!is.na(cible)) "#2e8b57")
  lty <- c(1, if (!is.na(lsl) || !is.na(usl)) 2, if (!is.na(cible)) 3)
  legend("topright", legend = leg, col = col, lty = lty, lwd = 2, bty = "n", cex = 0.85)
  invisible(dev.off())

  g <- function(nm) res[[nm]][1]
  l <- c(sprintf("Effectif : %d — moyenne : %s — écart-type global : %s", length(x), fmt(g("Moyenne")), fmt(g("Sigma_global"))),
         sprintf("Pp = %s — Ppk = %s", fmt(g("Pp"), 4), fmt(g("Ppk"), 4)))
  if (!is.na(sw) && sw > 0) l <- c(l, sprintf("Cp = %s — Cpk = %s — Cm = %s — Cmk = %s", fmt(g("Cp"), 4), fmt(g("Cpk"), 4), fmt(g("Cm"), 4), fmt(g("Cmk"), 4)))
  l <- c(l, sprintf("PPM estimés sous LSL : %s — au-dessus de USL : %s", fmt(g("PPM_estimes_sous_LSL"), 5), fmt(g("PPM_estimes_sur_USL"), 5)))
  concl_c <- paste(c(sprintf("Pp = %s ; Ppk = %s", fmt(g("Pp"), 3), fmt(g("Ppk"), 3)),
                     if (!is.na(sw) && sw > 0) sprintf("Cp = %s ; Cpk = %s", fmt(g("Cp"), 3), fmt(g("Cpk"), 3))), collapse = " — ")
  list(titre = paste("Capabilité —", nom), lignes = l, conclusion = concl_c,
       tables = list(Indicateurs = longue), figures = c(fig), avertissements = avert)
}

ch_cat <- function(v) {
  v <- trimws(as.character(v))
  v[is.na(v) | v == ""] <- NA_character_
  v
}

# Kappa de chaque évaluateur contre une colonne de référence (norme), avec verdict, et évaluateurs entre eux.
cmd_kappa_reference <- function(d, p) {
  rn <- nom_colonne(d, p$reference, "Référence (norme)")
  evs <- setdiff(unique(as.character(unlist(p$evaluateurs))), rn)
  if (!length(evs)) stop("Sélectionnez au moins un évaluateur, différent de la colonne de référence.", call. = FALSE)
  for (e in evs) nom_colonne(d, e, "Évaluateur")
  r0 <- ch_cat(d[[rn]])
  X <- lapply(evs, function(e) ch_cat(d[[e]]))
  names(X) <- evs

  tab <- do.call(rbind, lapply(evs, function(e) {
    x <- X[[e]]
    ok <- !is.na(x) & !is.na(r0)
    n <- sum(ok)
    if (n == 0) stop(sprintf("Aucune paire valide entre « %s » et la référence.", e), call. = FALSE)
    k <- kappa_agreement(r0, x)
    acc <- sum(x[ok] == r0[ok])
    ic <- stats::binom.test(acc, n)$conf.int
    data.frame(Evaluateur = e, Paires = n, Accords = acc, Accord_pct = round(100 * acc / n, 1),
               IC_inf = round(100 * ic[1], 1), IC_sup = round(100 * ic[2], 1),
               Kappa = k$kappa_cohen, Verdict = verdict_kappa(k$kappa_cohen), stringsAsFactors = FALSE)
  }))

  E <- do.call(cbind, X)
  compl <- !is.na(r0) & rowSums(is.na(E)) == 0
  nc <- sum(compl)
  tous <- if (nc > 0) sum(vapply(which(compl), function(i) all(E[i, ] == r0[i]), logical(1))) else 0L
  ligne_tous <- if (nc > 0) {
    icc <- stats::binom.test(tous, nc)$conf.int
    sprintf("Échantillons évalués par tous : %d — tous d'accord avec la référence : %d (%s %%, IC à 95 %% : %s à %s %%)",
            nc, tous, fmt(round(100 * tous / nc, 1)), fmt(round(100 * icc[1], 1)), fmt(round(100 * icc[2], 1)))
  } else "Aucun échantillon n'est évalué par tous les évaluateurs."
  grp <- function(lib) { z <- tab$Evaluateur[tab$Verdict == lib]; if (length(z)) paste(z, collapse = ", ") else "aucun" }
  lignes <- c(sprintf("Référence (norme) : « %s » — %d évaluateur(s) : %s", rn, length(evs), paste(evs, collapse = ", ")),
              ligne_tous, "",
              sprintf("Satisfaisants (Kappa > 0,75) : %s", grp("Satisfaisant")),
              sprintf("À améliorer (Kappa de 0,40 à 0,75) : %s", grp("À améliorer")),
              sprintf("Insuffisants (Kappa < 0,40) : %s", grp("Insuffisant")),
              "", KAPPA_REFERENCES)

  tables <- list(Evaluateurs_vs_reference = tab)
  if (length(evs) >= 2) {
    cb <- utils::combn(evs, 2)
    pair <- do.call(rbind, lapply(seq_len(ncol(cb)), function(i) {
      x <- X[[cb[1, i]]]; y <- X[[cb[2, i]]]
      ok <- !is.na(x) & !is.na(y)
      if (!any(ok)) return(NULL)
      k <- kappa_agreement(x, y)
      data.frame(Evaluateur_1 = cb[1, i], Evaluateur_2 = cb[2, i], Paires = sum(ok),
                 Accord_pct = round(100 * mean(x[ok] == y[ok]), 1), Kappa = k$kappa_cohen, stringsAsFactors = FALSE)
    }))
    if (!is.null(pair)) tables$Evaluateurs_entre_eux <- pair
  }
  if (length(evs) <= 4) {
    for (e in evs) {
      m <- as.data.frame.matrix(kappa_agreement(r0, X[[e]])$matrice)
      m <- cbind(Reference = rownames(m), m, stringsAsFactors = FALSE)
      rownames(m) <- NULL
      tables[[paste0("Matrice de confusion — ", e)]] <- m
    }
  }

  fig <- lire_png("kappa_evaluateurs.png", largeur = max(700, 120 * length(evs) + 300), hauteur = 520)
  par(mar = c(6, 4.5, 3, 1))
  kv <- tab$Kappa; kv[is.na(kv)] <- 0
  couleurs <- ifelse(kv > .75, "#2e8b57", ifelse(kv >= .40, "#e0a030", "#b03030"))
  bp <- barplot(kv, names.arg = tab$Evaluateur, ylim = c(min(0, min(kv)), 1.08), col = couleurs, ylab = "Kappa contre la référence",
                main = "Accord de chaque évaluateur avec la référence (norme)", las = if (length(evs) > 6) 2 else 1)
  abline(h = c(.40, .75), lty = 2, col = "#444444")
  text(bp, kv, labels = ifelse(is.na(tab$Kappa), "n.c.", fmt(kv, 3)), pos = ifelse(kv >= 0, 3, 1), cex = 0.9)
  mtext("Pointillés : repères AIAG (Kappa = 0,40 et 0,75)", side = 1, line = 4.5, cex = 0.85)
  invisible(dev.off())

  avert <- list("Kappa non pondéré. Pour des catégories ordinales, une version pondérée peut être préférable.",
                "Le résultat dépend de la qualité de la référence : elle doit être établie indépendamment des évaluateurs (expert, étalon).",
                "La valeur du Kappa dépend de la prévalence des catégories : examiner aussi l'accord observé et les matrices de confusion.")
  if (min(tab$Paires) < 30) avert <- c(avert, list("Moins de 30 échantillons : les intervalles de confiance de l'accord sont larges, interpréter avec prudence."))
  list(titre = sprintf("Évaluateurs et référence (norme) — Kappa de Cohen — « %s »", rn),
       lignes = lignes,
       conclusion = sprintf("Satisfaisants : %s ; à améliorer : %s ; insuffisants : %s.",
                            grp("Satisfaisant"), grp("À améliorer"), grp("Insuffisant")),
       tables = tables, figures = c(fig), avertissements = avert)
}

cmd_kappa <- function(d, p) {
  if (!is.null(p$reference) && nzchar(p$reference)) return(cmd_kappa_reference(d, p))
  an <- nom_colonne(d, p$a, "Première évaluation"); bn <- nom_colonne(d, p$b, "Deuxième évaluation")
  r <- kappa_agreement(d[[an]], d[[bn]])
  m <- as.data.frame.matrix(r$matrice)
  m <- cbind(Reference = rownames(m), m, stringsAsFactors = FALSE)
  rownames(m) <- NULL
  list(titre = sprintf("Kappa de Cohen — « %s » / « %s »", an, bn),
       lignes = c(sprintf("Paires valides : %d", r$effectif),
                  sprintf("Accord observé : %s %%", fmt(round(r$accord_observe * 100, 2), 5)),
                  sprintf("Accord attendu par hasard : %s %%", fmt(round(r$accord_attendu * 100, 2), 5)),
                  sprintf("Kappa : %s", fmt(r$kappa_cohen, 4)), r$interpretation, "", r$reference),
       conclusion = sprintf("Kappa = %s — %s", fmt(r$kappa_cohen, 4), r$interpretation),
       tables = list(Matrice_de_confusion = m), figures = character(0),
       avertissements = list(r$note, "La valeur du Kappa dépend de la prévalence des catégories : examiner aussi la matrice et l'accord observé."))
}

# -- commandes avancées (fichier séparé) ------------------------------------------
fichier_avance <- file.path(dossier_script, "commandes_avancees.R")
AVANCEES <- file.exists(fichier_avance)
if (AVANCEES) source(fichier_avance, local = TRUE, encoding = "UTF-8")
avancee <- function(nom, d, p) {
  if (!AVANCEES) stop("Le fichier commandes_avancees.R est introuvable dans le dossier du programme.", call. = FALSE)
  get(nom, mode = "function")(d, p)
}

# -- exécution -------------------------------------------------------------------
lancer_commande <- function(d, p) {
  switch(p$commande,
         descriptif = cmd_descriptif(d, p), normalite = cmd_normalite(d, p), t = cmd_t(d, p),
         anova = cmd_anova(d, p), capabilite = cmd_capabilite(d, p), kappa = cmd_kappa(d, p),
         ftest = avancee("cmd_ftest", d, p), khi2 = avancee("cmd_khi2", d, p),
         carte = avancee("cmd_carte", d, p), rr1 = avancee("cmd_rr1", d, p),
         rr_croisee = avancee("cmd_rr_croisee", d, p), rr_emboitee = avancee("cmd_rr_emboitee", d, p), anova2 = avancee("cmd_anova2", d, p), generer = avancee("cmd_generer", d, p),
         z = avancee("cmd_z", d, p), poisson = avancee("cmd_poisson", d, p), estimation = avancee("cmd_estimation", d, p),
         identification = avancee("cmd_identification", d, p), correlation = avancee("cmd_correlation", d, p),
         regression = avancee("cmd_regression", d, p), carte_attributs = avancee("cmd_carte_attributs", d, p), graphique = avancee("cmd_graphique", d, p), serie = avancee("cmd_serie", d, p),
         stop("Commande inconnue : ", p$commande, call. = FALSE))
}

# Analyse groupe par groupe : la colonne technique « __groupe__ » (écrite par l'application) donne le groupe de chaque ligne ;
# la commande est lancée une fois par groupe retenu, et les résultats sont réunis (texte, tableaux, figures).
lancer_par_groupes <- function(d, p) {
  g <- as.character(d[["__groupe__"]])
  vals <- as.character(unlist(p$groupes))
  nom_g <- if (is.null(p$groupe_nom) || !nzchar(p$groupe_nom)) "Groupe" else p$groupe_nom
  d0 <- d[, setdiff(names(d), "__groupe__"), drop = FALSE]
  res <- list(); erreurs <- character(0); etiqs <- character(0)
  for (i in seq_along(vals)) {
    sel <- !is.na(g) & g == vals[i]
    PREFIXE <<- sprintf("g%d_", i); ETIQ_GROUPE <<- sprintf("%s = %s", nom_g, vals[i])
    r <- tryCatch(lancer_commande(d0[sel, , drop = FALSE], p), error = function(e) {
      erreurs <<- c(erreurs, sprintf("Groupe « %s » : %s", vals[i], conditionMessage(e))); NULL })
    while (grDevices::dev.cur() > 1) grDevices::dev.off()
    if (!is.null(r)) { res[[length(res) + 1]] <- r; etiqs <- c(etiqs, vals[i]) }
  }
  PREFIXE <<- ""; ETIQ_GROUPE <<- ""
  if (!length(res)) stop(paste(erreurs, collapse = " "), call. = FALSE)
  tables <- list(); libres <- character(0)
  lignes <- character(0); concl <- character(0); figs <- character(0); avert <- list()
  for (i in seq_along(res)) {
    r <- res[[i]]; lab <- etiqs[i]
    lignes <- c(lignes, sprintf("══════ %s = %s ══════", nom_g, lab), r$lignes, "")
    if (length(r$conclusion) && nzchar(r$conclusion)) concl <- c(concl, sprintf("[%s] %s", lab, r$conclusion))
    for (nm in names(r$tables)) {
      nn <- paste0(lab, " — ", nm)
      tables[[nn]] <- r$tables[[nm]]
      if (nm %in% unlist(r$tables_libres)) libres <- c(libres, nn)
    }
    figs <- c(figs, unlist(r$figures))
    avert <- c(avert, unlist(r$avertissements))
  }
  avert <- c(as.list(unique(unlist(avert))), as.list(erreurs))
  list(titre = paste0(res[[1]]$titre, " — par groupe (", nom_g, ")"), lignes = lignes,
       conclusion = paste(concl, collapse = " "), tables = tables, tables_libres = libres,
       figures = figs, avertissements = avert, groupes = etiqs)
}

principal <- function() {
  p <- jsonlite::fromJSON(file.path(sortie, "params.json"), simplifyVector = TRUE)
  e <- num(p$echelle)
  ECH <<- if (is.na(e)) 1 else max(1, min(2.5, e))
  a <- num(p$alpha_test)
  ALPHA <<- if (is.na(a) || a <= 0 || a >= 0.5) 0.05 else a
  d <- utils::read.csv(entree, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                       na.strings = c("", "NA", "NaN"), encoding = "UTF-8", fileEncoding = "UTF-8")
  if (!nrow(d)) stop("Aucune ligne de données.", call. = FALSE)
  res <- if ("__groupe__" %in% names(d) && length(unlist(p$groupes))) lancer_par_groupes(d, p)
         else lancer_commande(d[, setdiff(names(d), "__groupe__"), drop = FALSE], p)
  res$ok <- TRUE
  res$commande <- p$commande
  ecrire(res)
}

resultat <- tryCatch(principal(), error = function(e) {
  ecrire(list(ok = FALSE, erreur = conditionMessage(e)))
})
quit(status = 0)
