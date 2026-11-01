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
  if (is.na(p)) "non calculable" else if (p < 1e-4) "< 0,0001" else sub("\\.", ",", format(round(p, 4), nsmall = 4))
}
ECH <- 1   # échelle de rendu des figures (1 = taille de base) ; fixée par params.json
lire_png <- function(nom, largeur = 900, hauteur = 600) {
  chemin <- file.path(sortie, nom)
  l <- round(largeur * ECH); h <- round(hauteur * ECH); r <- 110 * ECH
  if (isTRUE(capabilities("cairo"))) png(chemin, width = l, height = h, res = r, type = "cairo")
  else png(chemin, width = l, height = h, res = r)
  nom
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
  tab <- resume_multi_series(d[, cols, drop = FALSE])
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
  notes <- unique(tab$Interpretation_normalite)
  list(titre = "Statistiques descriptives",
       lignes = c(sprintf("%d série(s) numérique(s) analysée(s).", k),
                  paste0(tab$Serie, " — Shapiro-Wilk : ", tab$Interpretation_normalite)),
       tables = list(Descriptif = tab_aff), figures = c(fig1, fig2),
       avertissements = list())
}

cmd_normalite <- function(d, p) {
  nom <- nom_colonne(d, p$colonne, "Colonne")
  x <- sans_na(col_num(d, nom))
  n <- length(x)
  if (n < 3) stop("Il faut au moins 3 valeurs valides.", call. = FALSE)
  W <- NA_real_; pv <- NA_real_
  if (n <= 5000 && stats::sd(x) > 0) {
    sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
    if (!is.null(sw)) { W <- unname(sw$statistic); pv <- sw$p.value }
  }
  concl <- if (is.na(pv)) "Test non calculable (n > 5000, données constantes ou trop peu de valeurs distinctes)."
           else if (pv < 0.05) "La normalité est rejetée au seuil de 5 %."
           else "La normalité n'est pas rejetée au seuil de 5 %."
  fig <- lire_png("normalite.png", largeur = 1000, hauteur = 480)
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  hist(x, freq = FALSE, main = paste("Distribution :", nom), xlab = "Mesure", ylab = "Densité", col = "#cfe0f5", border = "white")
  if (stats::sd(x) > 0) curve(dnorm(x, mean(x), stats::sd(x)), add = TRUE, lwd = 2, col = "#b03030")
  qqnorm(x, main = "Droite de Henry (QQ-plot normal)", xlab = "Quantiles théoriques", ylab = "Quantiles observés", pch = 19, col = "#2a5d9f")
  qqline(x, col = "#b03030", lwd = 2)
  invisible(dev.off())
  list(titre = paste("Test de normalité de Shapiro-Wilk —", nom),
       lignes = c(sprintf("Effectif : %d", n), sprintf("Statistique W : %s", fmt(W)),
                  sprintf("p-value : %s", fmt_p(pv)), concl),
       tables = list(), figures = c(fig),
       avertissements = if (!is.na(pv) && pv < 0.05)
         list("Un rejet peut venir d'un petit écart sans importance pratique, surtout avec un grand effectif : examiner aussi le graphique.")
       else list())
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
  res <- test_t(x, y, mu = mu, paired = paired && !is.null(y), alternative = alt, var_equal = vrai(p$variances_egales))

  if (is.null(y)) {
    xc <- sans_na(x)
    d_c <- cohen_d(xc, mu = mu)
    cols <- xn
    hyp <- switch(alt, two.sided = sprintf("H1 : la moyenne de « %s » diffère de %s.", xn, fmt(mu)),
                  greater = sprintf("H1 : la moyenne de « %s » est supérieure à %s.", xn, fmt(mu)),
                  less = sprintf("H1 : la moyenne de « %s » est inférieure à %s.", xn, fmt(mu)))
    tab <- lignes_stat(d, xn)
  } else {
    if (paired) { ok <- is.finite(x) & is.finite(y); xc <- x[ok]; yc <- y[ok]
    } else { xc <- sans_na(x); yc <- sans_na(y) }
    d_c <- cohen_d(xc, yc, paired = paired)
    cols <- c(xn, yn)
    hyp <- switch(alt, two.sided = sprintf("H1 : les moyennes de « %s » et « %s » diffèrent.", xn, yn),
                  greater = sprintf("H1 : la moyenne de « %s » est supérieure à celle de « %s ».", xn, yn),
                  less = sprintf("H1 : la moyenne de « %s » est inférieure à celle de « %s ».", xn, yn))
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
  list(titre = paste("Test t —", res$type),
       lignes = c(hyp, sprintf("Statistique t : %s", fmt(res$statistic)), sprintf("Degrés de liberté : %s", fmt(res$ddl)),
                  sprintf("p-value : %s", fmt_p(res$p_value)),
                  sprintf("Intervalle de confiance à 95 %% : [%s ; %s]", fmt(ic[1]), fmt(ic[2])),
                  sprintf("Taille d'effet (d de Cohen) : %s", fmt(d_c, 4)), res$conclusion_5pct),
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

  tables <- list(ANOVA = tab_df, Groupes = moy)
  lignes <- c(sprintf("Nombre de groupes : %d — observations : %d", nlevels(dd$groupe), nrow(dd)),
              sprintf("F = %s — p-value : %s", fmt(tab[["F value"]][1]), fmt_p(pv)),
              if (is.na(pv)) "Résultat non calculable."
              else if (pv < 0.05) "Au moins une moyenne de groupe diffère statistiquement (seuil 5 %)."
              else "Aucune différence de moyenne n'est mise en évidence au seuil de 5 %.",
              sprintf("Contrôle des résidus — Shapiro-Wilk : p = %s ; homogénéité des variances (Fligner-Killeen) : p = %s",
                      fmt_p(res_sw), fmt_p(fl)))
  avert <- list("Vérifier l'indépendance, la normalité approximative des résidus et l'homogénéité des variances.")
  if (!is.na(pv) && pv < 0.05 && nlevels(dd$groupe) >= 2) {
    tk <- tryCatch(stats::TukeyHSD(fit)$groupe, error = function(e) NULL)
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
  list(titre = "ANOVA à un facteur", lignes = lignes, tables = tables, figures = c(fig), avertissements = avert)
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
  hist(x, freq = FALSE, main = paste("Distribution :", nom), xlab = "Mesure", ylab = "Densité", col = "#cfe0f5", border = "white",
       xlim = range(c(x, lsl, usl, cible), na.rm = TRUE))
  s <- stats::sd(x)
  if (s > 0) curve(dnorm(x, mean(x), s), add = TRUE, lwd = 2, col = "#2a5d9f")
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
  list(titre = paste("Capabilité —", nom), lignes = l, tables = list(Indicateurs = longue), figures = c(fig), avertissements = avert)
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
       lignes = lignes, tables = tables, figures = c(fig), avertissements = avert)
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
       tables = list(Matrice_de_confusion = m), figures = character(0),
       avertissements = list(r$note, "La valeur du Kappa dépend de la prévalence des catégories : examiner aussi la matrice et l'accord observé."))
}

# -- exécution -------------------------------------------------------------------
principal <- function() {
  p <- jsonlite::fromJSON(file.path(sortie, "params.json"), simplifyVector = TRUE)
  e <- num(p$echelle)
  ECH <<- if (is.na(e)) 1 else max(1, min(2.5, e))
  d <- utils::read.csv(entree, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                       na.strings = c("", "NA", "NaN"), encoding = "UTF-8", fileEncoding = "UTF-8")
  if (!nrow(d)) stop("Aucune ligne de données.", call. = FALSE)
  res <- switch(p$commande,
                descriptif = cmd_descriptif(d, p), normalite = cmd_normalite(d, p), t = cmd_t(d, p),
                anova = cmd_anova(d, p), capabilite = cmd_capabilite(d, p), kappa = cmd_kappa(d, p),
                stop("Commande inconnue : ", p$commande, call. = FALSE))
  res$ok <- TRUE
  res$commande <- p$commande
  ecrire(res)
}

resultat <- tryCatch(principal(), error = function(e) {
  ecrire(list(ok = FALSE, erreur = conditionMessage(e)))
})
quit(status = 0)
