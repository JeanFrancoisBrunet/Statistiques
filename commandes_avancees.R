# =============================================================================
#  commandes_avancees.R — commandes supplémentaires de cli_statistique.R
#    ftest        : test F de comparaison de deux variances
#    khi2         : khi² d'indépendance, d'homogénéité et de conformité
#    carte        : cartes de contrôle (X-barre/R, X-barre/S, I-MR)
#    rr1          : étude R&R de type 1 (biais, répétabilité, Cg/Cgk)
#    rr_croisee   : étude R&R croisée (méthode ANOVA, conventions Minitab)
#  Ce fichier est chargé par cli_statistique.R (il utilise ses fonctions utilitaires).
#  Auteur : Jean-François BRUNET – JFBConseils – Novembre 2026
# =============================================================================

PAL <- function(n) grDevices::hcl.colors(max(n, 1), "Dark 3")

# Test de Levene (variante de Brown-Forsythe : écarts à la médiane), robuste à la non-normalité.
levene_bf <- function(v, g) {
  g <- factor(g)
  z <- abs(v - stats::ave(v, g, FUN = stats::median))
  a <- tryCatch(stats::anova(stats::lm(z ~ g)), error = function(e) NULL)
  if (is.null(a)) return(c(stat = NA_real_, ddl1 = NA_real_, ddl2 = NA_real_, p = NA_real_))
  c(stat = a[["F value"]][1], ddl1 = a[["Df"]][1], ddl2 = a[["Df"]][2], p = a[["Pr(>F)"]][1])
}

# =============================================================================
#  Test F : comparaison de deux variances
# =============================================================================
cmd_ftest <- function(d, p) {
  xn <- nom_colonne(d, p$x, "Série X"); yn <- nom_colonne(d, p$y, "Série Y")
  if (identical(xn, yn)) stop("Choisir deux séries différentes.", call. = FALSE)
  x <- sans_na(col_num(d, xn)); y <- sans_na(col_num(d, yn))
  if (length(x) < 2 || length(y) < 2) stop("Chaque série doit contenir au moins 2 valeurs.", call. = FALSE)
  if (stats::sd(x) == 0 || stats::sd(y) == 0) stop("Une série est constante : le rapport des variances n'est pas calculable.", call. = FALSE)
  alt <- p$alternative %||% "two.sided"
  vt <- stats::var.test(x, y, alternative = alt, conf.level = 1 - ALPHA)
  F <- unname(vt$statistic); ddl1 <- unname(vt$parameter[1]); ddl2 <- unname(vt$parameter[2])
  pv <- vt$p.value; ic <- unname(vt$conf.int)
  lev <- levene_bf(c(x, y), rep(c("X", "Y"), c(length(x), length(y))))
  sw <- function(v) if (length(v) >= 3 && length(v) <= 5000) tryCatch(stats::shapiro.test(v)$p.value, error = function(e) NA_real_) else NA_real_
  pxs <- sw(x); pys <- sw(y)

  hyp <- switch(alt,
                two.sided = sprintf("H1 : les variances de « %s » et de « %s » diffèrent.", xn, yn),
                greater = sprintf("H1 : la variance de « %s » est supérieure à celle de « %s ».", xn, yn),
                less = sprintf("H1 : la variance de « %s » est inférieure à celle de « %s ».", xn, yn))
  signif <- !is.na(pv) && pv < ALPHA
  h0f <- switch(alt, two.sided = sprintf("H0 : les variances de « %s » et de « %s » sont égales.", xn, yn),
                greater = sprintf("H0 : la variance de « %s » est inférieure ou égale à celle de « %s ».", xn, yn),
                less = sprintf("H0 : la variance de « %s » est supérieure ou égale à celle de « %s ».", xn, yn))
  concl_f <- decision(pv, "aucune différence de variance n'est mise en évidence.", "les variances diffèrent statistiquement.")
  tab <- data.frame(Serie = c(xn, yn), n = c(length(x), length(y)), Moyenne = c(mean(x), mean(y)),
                    Variance = c(stats::var(x), stats::var(y)), Ecart_type = c(stats::sd(x), stats::sd(y)),
                    stringsAsFactors = FALSE)
  lignes <- c(h0f, hyp, regle(),
              sprintf("Rapport des variances (X / Y) : %s", fmt(stats::var(x) / stats::var(y), 5)),
              sprintf("Statistique F : %s — degrés de liberté : %d et %d", fmt(F, 5), as.integer(ddl1), as.integer(ddl2)),
              sprintf("p-value : %s", fmt_p(pv)),
              sprintf("Intervalle de confiance à %s du rapport des variances : [%s ; %s]", conf_txt(), fmt(ic[1], 5), fmt(ic[2], 5)),
              concl_f,
              "",
              sprintf("Test de Levene (Brown-Forsythe), moins sensible à la non-normalité : p-value %s (H0 : variances égales ; %s)", fmt_p(lev[["p"]]),
                      if (is.na(lev[["p"]])) "non calculable" else if (lev[["p"]] < ALPHA) "p-value < α → H0 rejetée" else "p-value ≥ α → H0 non rejetée"))
  if (alt == "two.sided") {
    lignes <- c(lignes, "",
                if (signif) "Pour comparer les moyennes de ces deux séries, utiliser le test t de Welch (variances inégales)."
                else "Pour comparer les moyennes, le test t de Student (variances égales) est utilisable ; le test de Welch reste valable dans les deux cas.")
  }
  avert <- list()
  if (any(c(pxs, pys) < ALPHA, na.rm = TRUE))
    avert <- c(avert, list("Au moins une série s'écarte de la normalité : le test F y est très sensible. Se référer plutôt au test de Levene ci-dessus."))
  avert <- c(avert, list("Le test F suppose deux échantillons indépendants issus de distributions normales."))

  fig <- lire_png("test_f.png", largeur = 760, hauteur = 520)
  par(mar = c(5, 4.5, 3, 1))
  sets <- list(x, y); names(sets) <- c(xn, yn)
  boxplot(sets, main = "Dispersion des deux séries", ylab = "Mesure", col = "#cfe0f5", outline = TRUE)
  stripchart(sets, vertical = TRUE, method = "jitter", jitter = 0.12, pch = 19, col = "#2a5d9f66", add = TRUE)
  invisible(dev.off())
  list(titre = "Test F de comparaison de deux variances",
       conclusion = sprintf("F = %s. %s", fmt(F, 4), sub("^Décision : ", "", concl_f)),
       lignes = lignes, tables = list(Series = tab), figures = c(fig), avertissements = avert)
}

# =============================================================================
#  Khi² : indépendance, homogénéité (tableau de contingence) et conformité
# =============================================================================
force_integer_counts <- function(m) {
  if (any(m < 0) || any(abs(m - round(m)) > 1e-8)) stop("Les effectifs doivent être des entiers positifs ou nuls.", call. = FALSE)
  round(m)
}

# Tableau de contingence à partir de deux colonnes de données brutes ou d'un tableau d'effectifs.
tableau_contingence <- function(d, p) {
  if (identical(p$format, "table")) {
    cols <- noms_numeriques(d, unlist(p$colonnes))
    if (length(cols) < 2) stop("Sélectionner au moins deux colonnes d'effectifs.", call. = FALSE)
    m <- as.matrix(d[, cols, drop = FALSE])
    lib <- if (!is.null(p$libelles) && nzchar(p$libelles)) ch_cat(d[[nom_colonne(d, p$libelles, "Colonne des libellés")]]) else NULL
    vides <- apply(m, 1, function(r) all(is.na(r))) & (if (is.null(lib)) TRUE else is.na(lib))
    m <- m[!vides, , drop = FALSE]
    if (!is.null(lib)) lib <- lib[!vides]
    if (anyNA(m)) stop("Le tableau d'effectifs contient des cases vides : les compléter par 0.", call. = FALSE)
    m <- force_integer_counts(m)
    if (is.null(lib) || anyNA(lib)) lib <- paste("Ligne", seq_len(nrow(m)))
    rownames(m) <- make.unique(lib)
    tab <- as.table(m)
    names(dimnames(tab)) <- c("Lignes", "Colonnes")
    return(list(tab = tab, nom_l = "Lignes", nom_c = "Colonnes", exclues = 0L))
  }
  an <- nom_colonne(d, p$a, "Variable A"); bn <- nom_colonne(d, p$b, "Variable B")
  if (identical(an, bn)) stop("Choisir deux variables différentes.", call. = FALSE)
  a <- ch_cat(d[[an]]); b <- ch_cat(d[[bn]])
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 2) stop("Pas assez de lignes complètes.", call. = FALSE)
  tab <- table(a[ok], b[ok])
  names(dimnames(tab)) <- c(an, bn)
  list(tab = tab, nom_l = an, nom_c = bn, exclues = sum(!ok))
}

force_ordre_numerique <- function(niveaux) {
  n <- suppressWarnings(as.numeric(sub(",", ".", niveaux)))
  if (!anyNA(n)) niveaux[order(n)] else sort(niveaux)
}

cmd_khi2_contingence <- function(d, p, homogeneite) {
  ct <- tableau_contingence(d, p)
  tab <- ct$tab
  avert <- list()
  vide_l <- rowSums(tab) == 0; vide_c <- colSums(tab) == 0
  if (any(vide_l) || any(vide_c)) {
    avert <- c(avert, list("Lignes ou colonnes sans aucun effectif écartées du calcul."))
    tab <- tab[!vide_l, !vide_c, drop = FALSE]
  }
  if (nrow(tab) < 2 || ncol(tab) < 2) stop("Il faut au moins deux lignes et deux colonnes d'effectifs non nuls.", call. = FALSE)
  nr <- nrow(tab); nc <- ncol(tab); n <- sum(tab)
  ch <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  E <- ch$expected
  X2 <- unname(ch$statistic); ddl <- unname(ch$parameter); pv <- ch$p.value
  dfs <- min(nr - 1, nc - 1)
  V <- sqrt(X2 / (n * dfs))
  seuils <- c(0.1, 0.3, 0.5) / sqrt(dfs)                  # repères de Cohen ajustés au nombre de ddl
  force_v <- if (V < seuils[1]) "négligeable" else if (V < seuils[2]) "faible" else if (V < seuils[3]) "modérée" else "forte"
  part_petits <- mean(E < 5)
  valide <- part_petits <= 0.2 && all(E >= 1)
  signif <- !is.na(pv) && pv < ALPHA

  exact <- NULL; yates <- NULL
  if (nr == 2 && nc == 2) {
    yates <- suppressWarnings(stats::chisq.test(tab, correct = TRUE))
    exact <- tryCatch(stats::fisher.test(tab)$p.value, error = function(e) NULL)
  } else if (!valide) {
    exact <- tryCatch(stats::fisher.test(tab)$p.value, error = function(e) {
      set.seed(1); tryCatch(stats::fisher.test(tab, simulate.p.value = TRUE, B = 20000)$p.value, error = function(e2) NULL)
    })
  }
  if (!valide)
    avert <- c(avert, list(sprintf("%s des effectifs théoriques sont inférieurs à 5 (ou l'un est inférieur à 1) : l'approximation du khi² est douteuse. Regrouper des modalités ou se référer au test exact de Fisher.",
                                   sprintf("%d %%", round(100 * part_petits)))))
  if (ct$exclues > 0) avert <- c(avert, list(sprintf("%d ligne(s) de données incomplète(s) écartée(s).", ct$exclues)))

  titre <- if (homogeneite) "Test du khi² d'homogénéité" else "Test du khi² d'indépendance"
  hyp <- if (homogeneite) {
    c("H0 : la répartition des modalités est la même dans toutes les populations (lignes).",
      "H1 : la répartition diffère d'au moins une population à l'autre.")
  } else {
    c(sprintf("H0 : les variables « %s » et « %s » sont indépendantes.", ct$nom_l, ct$nom_c),
      "H1 : les variables sont liées.")
  }
  concl <- if (homogeneite) {
    decision(pv, "aucune différence de répartition entre les populations n'est mise en évidence.", "les populations n'ont pas la même répartition.")
  } else {
    decision(pv, "aucune dépendance n'est mise en évidence entre les deux variables.", "une dépendance entre les deux variables est mise en évidence.")
  }
  lignes <- c(hyp, regle(),
              sprintf("Effectif total : %d — tableau de %d ligne(s) × %d colonne(s)", n, nr, nc),
              sprintf("Khi² = %s — degrés de liberté : %d — p-value : %s", fmt(X2, 5), as.integer(ddl), fmt_p(pv)),
              sprintf("V de Cramér : %s (association %s)", fmt(V, 3), force_v),
              concl)
  if (!is.null(yates)) lignes <- c(lignes, sprintf("Avec correction de continuité de Yates : khi² = %s, p-value %s", fmt(unname(yates$statistic), 5), fmt_p(yates$p.value)))
  if (!is.null(exact)) lignes <- c(lignes, sprintf("Test exact de Fisher : p-value %s", fmt_p(exact)))

  mk <- function(m, total = FALSE) {
    df <- data.frame(Modalite = rownames(m), as.data.frame.matrix(m, optional = TRUE), check.names = FALSE, stringsAsFactors = FALSE)
    names(df)[1] <- ct$nom_l
    if (total) df$Total <- rowSums(m)
    df
  }
  obs <- mk(unclass(tab), total = TRUE)
  tot <- c(setNames(list("Total"), ct$nom_l), as.list(colSums(tab)), list(Total = n))
  obs <- rbind(obs, as.data.frame(tot, check.names = FALSE, stringsAsFactors = FALSE))
  res_std <- mk(round(unclass(ch$stdres), 3))
  prof <- mk(round(100 * prop.table(unclass(tab), 1), 2))
  tables <- list(`Effectifs observés` = obs, `Effectifs théoriques` = mk(round(unclass(E), 3)),
                 `Résidus standardisés` = res_std, `Profils des lignes (%)` = prof)

  fig1 <- lire_png("khi2_profils.png", largeur = max(760, 90 * nr + 360), hauteur = 560)
  par(mar = c(5, 4.5, 3, 8), xpd = TRUE)
  pr <- t(prop.table(unclass(tab), 1)) * 100
  cl <- PAL(nc)
  barplot(pr, col = cl, main = "Répartition par ligne (%)", ylab = "%", ylim = c(0, 100), las = if (nr > 6) 2 else 1, border = "white")
  legend("topright", inset = c(-0.2, 0), legend = colnames(tab), fill = cl, bty = "n", cex = 0.85, title = ct$nom_c)
  invisible(dev.off())
  figs <- c(fig1)
  if (nr * nc <= 100) {
    fig2 <- lire_png("khi2_mosaique.png", largeur = 760, hauteur = 560)
    ok_m <- tryCatch({ suppressWarnings(graphics::mosaicplot(unclass(tab), shade = TRUE, main = "Résidus (rouge : excès, bleu : déficit)", las = 1)); TRUE },
                     error = function(e) FALSE)
    invisible(dev.off())
    if (ok_m) figs <- c(figs, fig2)
  }
  list(titre = titre, lignes = lignes,
       conclusion = sprintf("Khi² = %s, ddl %d. %s", fmt(X2, 4), as.integer(ddl), sub("^Décision : ", "", concl)),
       tables = tables, tables_libres = names(tables), figures = figs, avertissements = avert)
}

# Proportions attendues saisies sous forme de texte : « 0,25 0,25 0,5 » ou « 1 1 2 » (normalisées).
lire_proportions <- function(txt, k, niveaux) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(rep(1 / k, k))
  v <- suppressWarnings(as.numeric(strsplit(gsub(",", ".", trimws(txt)), "[ ;:]+")[[1]]))
  if (anyNA(v) || any(v < 0) || sum(v) <= 0) stop("Proportions attendues invalides : saisir des nombres positifs séparés par des espaces.", call. = FALSE)
  if (length(v) != k)
    stop(sprintf("%d proportion(s) saisie(s) pour %d modalité(s) (ordre : %s).", length(v), k, paste(niveaux, collapse = " ; ")), call. = FALSE)
  v / sum(v)
}

cmd_khi2_conformite <- function(d, p) {
  loi <- p$loi %||% "repartition"
  if (identical(loi, "normale")) return(cmd_khi2_normalite(d, p))
  # -- conformité à une répartition attendue
  if (identical(p$format, "table")) {
    cols <- noms_numeriques(d, unlist(p$colonnes))
    if (length(cols) != 1) stop("Sélectionner une seule colonne d'effectifs observés.", call. = FALSE)
    obs <- d[[cols]]
    lib <- if (!is.null(p$libelles) && nzchar(p$libelles)) ch_cat(d[[nom_colonne(d, p$libelles, "Colonne des libellés")]]) else NULL
    ok <- is.finite(obs)
    obs <- obs[ok]; lib <- if (is.null(lib)) paste("Modalité", seq_along(obs)) else lib[ok]
    lib[is.na(lib)] <- "(sans nom)"
    obs <- force_integer_counts(matrix(obs, ncol = 1))[, 1]
    niveaux <- make.unique(lib)
    names(obs) <- niveaux
  } else {
    vn <- nom_colonne(d, p$variable, "Variable")
    v <- ch_cat(d[[vn]]); v <- v[!is.na(v)]
    if (length(v) < 2) stop("Pas assez de valeurs.", call. = FALSE)
    niveaux <- force_ordre_numerique(unique(v))
    obs <- as.vector(table(factor(v, levels = niveaux))); names(obs) <- niveaux
  }
  k <- length(obs)
  if (k < 2) stop("Il faut au moins deux modalités.", call. = FALSE)
  n <- sum(obs)
  if (n < 1) stop("Aucun effectif.", call. = FALSE)
  prop <- lire_proportions(p$proportions, k, niveaux)
  E <- n * prop
  mparam <- num(p$nb_parametres); if (is.na(mparam)) mparam <- 0
  ddl <- k - 1 - mparam
  if (ddl < 1) stop("Degrés de liberté insuffisants (trop de paramètres estimés pour ce nombre de modalités).", call. = FALSE)
  X2 <- sum((obs - E)^2 / E); pv <- stats::pchisq(X2, ddl, lower.tail = FALSE)
  signif <- pv < ALPHA
  concl <- decision(pv, "la répartition observée est compatible avec la répartition attendue (conformité retenue).", "la répartition observée n'est pas conforme à la répartition attendue.")
  avert <- list()
  if (mean(E < 5) > 0.2 || any(E < 1))
    avert <- c(avert, list("Plus de 20 % des effectifs théoriques sont inférieurs à 5 (ou l'un est inférieur à 1) : regrouper des modalités."))
  egal <- !nzchar(trimws(p$proportions %||% ""))
  tab <- data.frame(Modalite = niveaux, Observe = as.numeric(obs), Proportion_attendue_pct = round(100 * prop, 3),
                    Theorique = round(E, 3), Residu_standardise = round((obs - E) / sqrt(E), 3),
                    Contribution = round((obs - E)^2 / E, 4), stringsAsFactors = FALSE)
  fig <- lire_png("khi2_conformite.png", largeur = max(760, 70 * k + 300), hauteur = 520)
  par(mar = c(5, 4.5, 3, 1))
  m <- rbind(Observé = as.numeric(obs), Théorique = E)
  colnames(m) <- niveaux
  barplot(m, beside = TRUE, col = c("#2a5d9f", "#e0a030"), main = "Effectifs observés et théoriques", ylab = "Effectif",
          legend.text = TRUE, args.legend = list(x = "topright", bty = "n"), las = if (k > 6) 2 else 1, border = "white")
  invisible(dev.off())
  list(titre = "Test du khi² de conformité",
       lignes = c("H0 : la répartition observée est conforme à la répartition attendue.",
                  "H1 : elle s'en écarte.", regle(),
                  sprintf("Effectif total : %d — modalités : %d — répartition attendue : %s", n, k, if (egal) "égale pour toutes les modalités" else "proportions saisies"),
                  sprintf("Khi² = %s — degrés de liberté : %d — p-value : %s", fmt(X2, 5), as.integer(ddl), fmt_p(pv)), concl),
       conclusion = sprintf("Khi² = %s, ddl %d. %s", fmt(X2, 4), as.integer(ddl), sub("^Décision : ", "", concl)),
       tables = list(Conformité = tab), figures = c(fig), avertissements = avert)
}

# Conformité à une loi normale : classes équiprobables sous la loi ajustée (effectif théorique = n/k >= 5).
cmd_khi2_normalite <- function(d, p) {
  vn <- nom_colonne(d, p$variable, "Variable")
  x <- sans_na(col_num(d, vn)); n <- length(x)
  if (n < 20) stop("Au moins 20 valeurs sont nécessaires pour un test du khi² de normalité.", call. = FALSE)
  mu <- num(p$moyenne); sdv <- num(p$ecart_type)
  est_mu <- is.na(mu); est_sd <- is.na(sdv)
  if (est_mu) mu <- mean(x)
  if (est_sd) sdv <- stats::sd(x)
  if (!is.finite(sdv) || sdv <= 0) stop("L'écart-type doit être strictement positif.", call. = FALSE)
  npar <- as.integer(est_mu) + as.integer(est_sd)
  kmax <- floor(n / 5)
  k <- num(p$classes)
  avert <- list()
  if (is.na(k)) k <- min(max(floor(2 * n^0.4), 4), kmax)
  else {
    k <- as.integer(k)
    if (k > kmax) { avert <- c(avert, list(sprintf("Nombre de classes ramené à %d pour garder au moins 5 valeurs attendues par classe.", kmax))); k <- kmax }
  }
  if (k < npar + 2) stop("Trop peu de classes pour ce test : augmenter l'effectif ou le nombre de classes.", call. = FALSE)
  bornes <- stats::qnorm(seq(0, 1, length.out = k + 1), mu, sdv)
  cl <- cut(x, breaks = bornes, include.lowest = TRUE)
  obs <- as.vector(table(cl)); E <- rep(n / k, k)
  X2 <- sum((obs - E)^2 / E); ddl <- k - 1 - npar
  pv <- stats::pchisq(X2, ddl, lower.tail = FALSE)
  signif <- pv < ALPHA
  concl <- decision(pv, "la normalité est retenue.", "la normalité est rejetée.")
  sw <- if (n <= 5000 && stats::sd(x) > 0) tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_) else NA_real_
  bf <- function(v) ifelse(is.infinite(v), ifelse(v < 0, "−∞", "+∞"), fmt(v, 5))
  tab <- data.frame(Classe = sprintf("%s ; %s", bf(bornes[-(k + 1)]), bf(bornes[-1])), Observe = obs, Theorique = round(E, 3),
                    Contribution = round((obs - E)^2 / E, 4), stringsAsFactors = FALSE)
  fig <- lire_png("khi2_normalite.png", largeur = 1000, hauteur = 480)
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  h <- hist(x, plot = FALSE)
  hist(x, freq = FALSE, main = paste("Distribution :", vn), xlab = "Mesure", ylab = "Densité", col = "#cfe0f5", border = "white",
       breaks = h$breaks, ylim = c(0, max(h$density, stats::dnorm(mu, mu, sdv)) * 1.1))
  curve(stats::dnorm(x, mu, sdv), add = TRUE, lwd = 2, col = "#b03030")
  m <- rbind(Observé = obs, Théorique = E)
  barplot(m, beside = TRUE, col = c("#2a5d9f", "#e0a030"), main = sprintf("Classes équiprobables (%d)", k), xlab = "Classe", ylab = "Effectif",
          legend.text = TRUE, args.legend = list(x = "topright", bty = "n", cex = 0.8), border = "white", names.arg = seq_len(k))
  invisible(dev.off())
  lignes <- c("H0 : les données suivent une loi normale.", "H1 : elles s'en écartent.", regle(),
              sprintf("Effectif : %d — classes équiprobables : %d (effectif théorique par classe : %s)", n, k, fmt(n / k, 4)),
              sprintf("Moyenne : %s (%s) — écart-type : %s (%s)", fmt(mu, 5), if (est_mu) "estimée" else "donnée", fmt(sdv, 5), if (est_sd) "estimé" else "donné"),
              sprintf("Khi² = %s — degrés de liberté : %d (classes − 1 − %d paramètre(s) estimé(s)) — p-value : %s", fmt(X2, 5), as.integer(ddl), npar, fmt_p(pv)),
              concl)
  if (!is.na(sw)) lignes <- c(lignes, "", sprintf("Pour comparaison, test de Shapiro-Wilk (en général plus puissant pour la normalité) : p-value %s", fmt_p(sw)))
  list(titre = paste("Test du khi² de conformité à la loi normale —", vn), lignes = lignes,
       conclusion = sprintf("« %s » — khi² = %s, ddl %d. %s", vn, fmt(X2, 4), as.integer(ddl), sub("^Décision : ", "", concl)),
       tables = list(Classes = tab), figures = c(fig), avertissements = avert)
}

cmd_khi2 <- function(d, p) {
  switch(p$test %||% "independance",
         independance = cmd_khi2_contingence(d, p, homogeneite = FALSE),
         homogeneite = cmd_khi2_contingence(d, p, homogeneite = TRUE),
         conformite = cmd_khi2_conformite(d, p),
         stop("Type de test du khi² inconnu.", call. = FALSE))
}

# =============================================================================
#  Cartes de contrôle (SPC) : X-barre/R, X-barre/S et valeurs individuelles/étendues mobiles (I-MR)
# =============================================================================
.cst <- new.env()

# Constantes d'estimation, calculées exactement (aucune table recopiée) :
#   d2(n) = espérance de l'étendue de n valeurs N(0,1) ; d3(n) = son écart-type ; c4(n) = espérance de s pour n valeurs.
cst_d2 <- function(n) {
  cle <- paste0("d2_", n)
  if (is.null(.cst[[cle]]))
    .cst[[cle]] <- stats::integrate(function(x) 1 - stats::pnorm(x)^n - (1 - stats::pnorm(x))^n, -Inf, Inf, rel.tol = 1e-10)$value
  .cst[[cle]]
}
cst_d3 <- function(n) {
  cle <- paste0("d3_", n)
  if (is.null(.cst[[cle]])) {
    fr <- function(r) vapply(r, function(ri)
      n * stats::integrate(function(x) stats::dnorm(x) * (stats::pnorm(x + ri) - stats::pnorm(x))^(n - 1), -Inf, Inf, rel.tol = 1e-9)$value, numeric(1))
    er2 <- stats::integrate(function(r) 2 * r * (1 - fr(r)), 0, 16, rel.tol = 1e-8, subdivisions = 200L)$value
    .cst[[cle]] <- sqrt(max(er2 - cst_d2(n)^2, 0))
  }
  .cst[[cle]]
}
cst_c4 <- function(n) sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))

DESC_TESTS <- c("1 point à plus de 3 écarts-types de la ligne centrale",
                "9 points consécutifs du même côté de la ligne centrale",
                "6 points consécutifs en augmentation ou en diminution",
                "14 points consécutifs alternant vers le haut et vers le bas",
                "2 points sur 3 à plus de 2 écarts-types, du même côté",
                "4 points sur 5 à plus de 1 écart-type, du même côté",
                "15 points consécutifs à moins de 1 écart-type de la ligne centrale",
                "8 points consécutifs à plus de 1 écart-type, de part et d'autre")

# Tests de causes spéciales (conventions de Minitab / règles de Nelson). Retourne la liste des points signalés par test.
tests_causes_speciales <- function(y, cl, sig, tests) {
  n <- length(y); z <- (y - cl) / sig
  out <- list()
  fen <- function(k, f) { r <- integer(0); if (n >= k) for (i in k:n) if (isTRUE(f(i - k + seq_len(k)))) r <- c(r, i); r }
  if (1 %in% tests) out[["1"]] <- which(abs(z) > 3)
  if (2 %in% tests) out[["2"]] <- fen(9, function(w) all(z[w] > 0) || all(z[w] < 0))
  if (3 %in% tests) out[["3"]] <- fen(6, function(w) all(diff(y[w]) > 0) || all(diff(y[w]) < 0))
  if (4 %in% tests) out[["4"]] <- fen(14, function(w) { dd <- diff(y[w]); all(dd[-1] * dd[-length(dd)] < 0) })
  if (5 %in% tests) out[["5"]] <- fen(3, function(w) (sum(z[w] > 2) >= 2 && z[w[3]] > 2) || (sum(z[w] < -2) >= 2 && z[w[3]] < -2))
  if (6 %in% tests) out[["6"]] <- fen(5, function(w) (sum(z[w] > 1) >= 4 && z[w[5]] > 1) || (sum(z[w] < -1) >= 4 && z[w[5]] < -1))
  if (7 %in% tests) out[["7"]] <- fen(15, function(w) all(abs(z[w]) < 1))
  if (8 %in% tests) out[["8"]] <- fen(8, function(w) all(abs(z[w]) > 1))
  out[vapply(out, length, integer(1)) > 0]
}

# Un panneau de carte de contrôle.
trace_carte <- function(titre, etiq, y, cl, lcl, ucl, sig, signaux, ylab, zones = FALSE, xlab = "Sous-groupe") {
  k <- length(y); x <- seq_len(k)
  hauts <- c(y, ucl, if (all(is.finite(lcl))) lcl)
  yl <- range(hauts[is.finite(hauts)], na.rm = TRUE); marge <- diff(yl) * 0.08; yl <- yl + c(-marge, marge)
  longues0 <- max(nchar(etiq), 0, na.rm = TRUE) > 8
  if (longues0) par(mar = c(9, par("mar")[2:4]))
  plot(x, y, type = "n", xaxt = "n", xlab = if (longues0) "" else xlab, ylab = ylab, main = titre, ylim = yl, xlim = c(0.5, k + 0.5))
  longues <- max(nchar(etiq), 0, na.rm = TRUE) > 8                      # étiquettes de dates : plus de place, moins de repères
  idx <- if (k <= (if (longues) 20 else 30)) x else unique(round(seq(1, k, length.out = min(k, if (longues) 14 else 15))))
  axis(1, at = idx, labels = etiq[idx], las = if (k > 15 || longues) 2 else 1, cex.axis = 0.8)
  if (longues) mtext(xlab, side = 1, line = par("mar")[1] - 1.3)
  if (zones) for (m in c(-2, -1, 1, 2)) lines(x, cl + m * sig, type = "s", lty = 3, col = "#aaaaaa")
  lines(x, ucl, type = "s", col = "#b03030", lwd = 2)
  lines(x, lcl, type = "s", col = "#b03030", lwd = 2)
  lines(x, cl, type = "s", col = "#2e8b57", lwd = 2)
  lines(x, y, col = "#2a5d9f", lwd = 1.2)
  pts <- sort(unique(unlist(signaux)))
  points(x, y, pch = 19, cex = if (k > 60) 0.6 else 0.9, col = "#2a5d9f")
  if (length(pts)) {
    points(pts, y[pts], pch = 19, col = "#d62728", cex = if (k > 60) 0.9 else 1.3)
    tl <- vapply(pts, function(i) paste(names(signaux)[vapply(signaux, function(v) i %in% v, logical(1))], collapse = ","), "")
    text(pts, y[pts], labels = tl, pos = 3, cex = 0.7, col = "#d62728", xpd = TRUE)
  }
  usable <- is.finite(c(ucl[k], cl[k], lcl[k]))
  mtext(sprintf("LSC = %s", fmt(ucl[k], 5)), side = 4, at = ucl[k], las = 1, cex = 0.7, line = 0.2, col = "#b03030")
  mtext(sprintf("LC = %s", fmt(cl[k], 5)), side = 4, at = cl[k], las = 1, cex = 0.7, line = 0.2, col = "#2e8b57")
  if (is.finite(lcl[k])) mtext(sprintf("LIC = %s", fmt(lcl[k], 5)), side = 4, at = lcl[k], las = 1, cex = 0.7, line = 0.2, col = "#b03030")
}

cmd_carte <- function(d, p) {
  type <- p$type %||% "auto"
  tests <- suppressWarnings(as.integer(unlist(p$tests))); tests <- tests[!is.na(tests) & tests >= 1 & tests <= 8]
  if (!length(p$tests)) tests <- 1:4
  # ---- ordre des données (colonne facultative : date ou numéro d'ordre) : tri, puis étiquettes des points
  lab_ordre <- NULL; nom_ordre <- NULL
  if (!is.null(p$ordre) && nzchar(p$ordre)) {
    nom_ordre <- nom_colonne(d, p$ordre, "Colonne d'ordre")
    d <- d[order(d[[nom_ordre]], na.last = TRUE), , drop = FALSE]
    lab_ordre <- etiq_ordre(d[[nom_ordre]])
  }
  # ---- constitution des sous-groupes
  if (identical(p$format, "long")) {
    vn <- nom_colonne(d, p$valeur, "Colonne des mesures")
    v <- col_num(d, vn)
    if (identical(type, "imr")) {
      ok <- is.finite(v); groupes <- as.list(v[ok])
      etiq <- if (!is.null(lab_ordre)) lab_ordre[ok] else as.character(seq_along(groupes))
    } else {
      gn <- nom_colonne(d, p$groupe, "Colonne des sous-groupes")
      g <- ch_cat(d[[gn]]); ok <- is.finite(v) & !is.na(g)
      if (!any(ok)) stop("Aucune mesure exploitable.", call. = FALSE)
      niv <- unique(g[ok]); groupes <- lapply(niv, function(l) v[ok & g == l]); etiq <- niv
    }
  } else {
    cols <- noms_numeriques(d, unlist(p$colonnes))
    if (identical(type, "imr") || length(cols) == 1) {
      if (length(cols) > 1 && identical(type, "imr")) stop("Pour les valeurs individuelles, choisir une seule colonne.", call. = FALSE)
      ok <- is.finite(d[[cols[1]]]); x <- d[[cols[1]]][ok]; groupes <- as.list(x)
      etiq <- if (!is.null(lab_ordre)) lab_ordre[ok] else as.character(seq_along(x))
    } else {
      M <- as.matrix(d[, cols, drop = FALSE])
      groupes <- lapply(seq_len(nrow(M)), function(i) M[i, is.finite(M[i, ])])
      etiq <- if (!is.null(lab_ordre)) lab_ordre else as.character(seq_along(groupes))
    }
  }
  nn <- vapply(groupes, length, integer(1))
  garde <- nn > 0
  groupes <- groupes[garde]; etiq <- etiq[garde]; nn <- nn[garde]
  if (!length(groupes)) stop("Aucune donnée exploitable.", call. = FALSE)
  avert <- list()

  # ---- type de carte
  if (identical(type, "auto")) type <- if (all(nn == 1)) "imr" else if (stats::median(nn) >= 8) "xbar_s" else "xbar_r"
  auto <- identical(p$type %||% "auto", "auto")
  if (type == "imr" && !all(nn == 1)) {
    groupes <- as.list(unlist(groupes)); nn <- rep(1L, length(groupes)); etiq <- as.character(seq_along(groupes))
    avert <- c(avert, list("Valeurs individuelles : toutes les mesures sont prises dans l'ordre des données, une par point."))
  }
  if (type != "imr") {
    petit <- nn < 2
    if (all(petit)) stop("Les sous-groupes ne contiennent qu'une valeur : utiliser la carte des valeurs individuelles (I-MR).", call. = FALSE)
    if (any(petit)) {
      avert <- c(avert, list(sprintf("Sous-groupe(s) d'une seule valeur écarté(s) : %s.", paste(etiq[petit], collapse = ", "))))
      groupes <- groupes[!petit]; etiq <- etiq[!petit]; nn <- nn[!petit]
    }
  }
  k <- length(groupes)
  if (k < 2) stop("Il faut au moins 2 sous-groupes (ou 2 mesures) pour construire une carte de contrôle.", call. = FALSE)
  if (k < 20 && type != "imr") avert <- c(avert, list(sprintf("Seulement %d sous-groupes : on recommande au moins 20 à 25 sous-groupes pour établir des limites de contrôle fiables.", k)))
  if (k < 20 && type == "imr") avert <- c(avert, list(sprintf("Seulement %d valeurs : on recommande au moins 20 à 25 valeurs pour établir des limites de contrôle fiables.", k)))
  variable <- length(unique(nn)) > 1

  # ---- statistiques et limites
  if (type == "imr") {
    x <- unlist(groupes)
    mr <- c(NA_real_, abs(diff(x)))
    mrbar <- mean(mr, na.rm = TRUE); sig <- mrbar / cst_d2(2)
    cl1 <- rep(mean(x), k); s1 <- rep(sig, k)
    ucl1 <- cl1 + 3 * sig; lcl1 <- cl1 - 3 * sig
    d3_2 <- cst_d3(2)
    cl2 <- rep(mrbar, k); s2 <- rep(d3_2 * sig, k)
    ucl2 <- cl2 + 3 * s2; lcl2 <- pmax(cl2 - 3 * s2, 0)
    y1 <- x; y2 <- mr
    nom1 <- "Valeurs individuelles"; nom2 <- "Étendues mobiles (2 valeurs)"
    titre <- "Carte de contrôle des valeurs individuelles et des étendues mobiles (I-MR)"
    meth <- sprintf("σ̂ = moyenne des étendues mobiles / d2(2) = %s / %s", fmt(mrbar, 5), fmt(cst_d2(2), 5))
    zero_mr <- is.na(mr)
  } else {
    xbar <- vapply(groupes, mean, numeric(1))
    xdb <- sum(nn * xbar) / sum(nn)
    if (type == "xbar_r") {
      est <- vapply(groupes, function(g) diff(range(g)), numeric(1))
      sigi <- est / vapply(nn, cst_d2, numeric(1))
      sig <- mean(sigi)
      cl2 <- sig * vapply(nn, cst_d2, numeric(1)); s2 <- sig * vapply(nn, cst_d3, numeric(1))
      nom2 <- "Étendues (R)"
      titre <- "Cartes de contrôle X-barre / R"
      meth <- sprintf("σ̂ = moyenne des R_i / d2(n_i) = %s", fmt(sig, 5))
    } else {
      est <- vapply(groupes, stats::sd, numeric(1))
      c4 <- vapply(nn, cst_c4, numeric(1))
      sigi <- est / c4
      sig <- mean(sigi)
      cl2 <- sig * c4; s2 <- sig * sqrt(1 - c4^2)
      nom2 <- "Écarts-types (S)"
      titre <- "Cartes de contrôle X-barre / S"
      meth <- sprintf("σ̂ = moyenne des s_i / c4(n_i) = %s", fmt(sig, 5))
    }
    s1 <- sig / sqrt(nn); cl1 <- rep(xdb, k)
    ucl1 <- cl1 + 3 * s1; lcl1 <- cl1 - 3 * s1
    ucl2 <- cl2 + 3 * s2; lcl2 <- pmax(cl2 - 3 * s2, 0)
    y1 <- xbar; y2 <- est
    nom1 <- "Moyennes (X-barre)"
    zero_mr <- rep(FALSE, k)
  }
  if (!is.finite(sig) || sig <= 0) stop("Les mesures sont constantes (dispersion nulle) : impossible de calculer des limites de contrôle.", call. = FALSE)

  # ---- tests de causes spéciales
  tests_var <- intersect(tests, 1:4)
  s1_sig <- tests_causes_speciales(y1, cl1, s1, tests)
  ok2 <- !zero_mr
  s2_sig <- list()
  if (length(tests_var)) {
    r2 <- tests_causes_speciales(y2[ok2], cl2[ok2], s2[ok2], tests_var)
    s2_sig <- lapply(r2, function(ix) which(ok2)[ix])
  }
  lis <- function(sg) if (length(sg)) paste0("Test ", names(sg), " : ", vapply(sg, function(v) paste(etiq[v], collapse = ", "), "")) else character(0)
  signaux <- rbind(
    if (length(s1_sig)) data.frame(Carte = nom1, Test = as.integer(names(s1_sig)), Description = DESC_TESTS[as.integer(names(s1_sig))],
                                   Points = vapply(s1_sig, function(v) paste(etiq[v], collapse = ", "), ""), stringsAsFactors = FALSE),
    if (length(s2_sig)) data.frame(Carte = nom2, Test = as.integer(names(s2_sig)), Description = DESC_TESTS[as.integer(names(s2_sig))],
                                   Points = vapply(s2_sig, function(v) paste(etiq[v], collapse = ", "), ""), stringsAsFactors = FALSE))
  nsig <- if (is.null(signaux)) 0L else nrow(signaux)
  marque <- function(sg, kk) { r <- character(kk); for (t in names(sg)) r[sg[[t]]] <- ifelse(nzchar(r[sg[[t]]]), paste0(r[sg[[t]]], ",", t), t); r }
  tab1 <- data.frame(Sous_groupe = etiq, n = nn, Valeur = y1, LIC = lcl1, LC = cl1, LSC = ucl1, Tests = marque(s1_sig, k), stringsAsFactors = FALSE)
  tab2 <- data.frame(Sous_groupe = etiq, n = nn, Valeur = y2, LIC = lcl2, LC = cl2, LSC = ucl2, Tests = marque(s2_sig, k), stringsAsFactors = FALSE)
  if (type == "imr") tab2 <- tab2[ok2, , drop = FALSE]
  if (type == "imr") tab1$n <- NULL
  if (type == "imr") tab2$n <- NULL

  # ---- figure
  zones <- any(tests %in% 5:8)
  fig <- lire_png("carte_controle.png", largeur = max(900, min(2000, 22 * k + 500)), hauteur = 760 + if (!is.null(lab_ordre) && !identical(p$format, "long")) 200 else 0)
  layout(matrix(1:2, ncol = 1)); par(mar = c(4.5, 4.5, 2.5, 7))
  xl <- if (!is.null(lab_ordre) && !identical(p$format, "long")) nom_ordre else if (type == "imr") "Observation" else "Sous-groupe"
  trace_carte(nom1, etiq, y1, cl1, lcl1, ucl1, s1, s1_sig, if (type == "imr") "Valeur individuelle" else "Moyenne", zones, xl)
  par(mar = c(4.5, 4.5, 2.5, 7))
  if (type == "imr") {
    trace_carte(nom2, etiq[ok2], y2[ok2], cl2[ok2], lcl2[ok2], ucl2[ok2], s2[ok2], s2_sig, "Étendue mobile", FALSE, xl)
  } else trace_carte(nom2, etiq, y2, cl2, lcl2, ucl2, s2, s2_sig, if (type == "xbar_r") "Étendue" else "Écart-type", FALSE, xl)
  invisible(dev.off())

  taille <- if (type == "imr") "valeurs individuelles" else if (variable) sprintf("%d à %d valeurs par sous-groupe (limites variables)", min(nn), max(nn)) else sprintf("sous-groupes de %d valeurs", nn[1])
  concl <- if (!length(tests)) "Aucun test de causes spéciales demandé : seules les limites de contrôle sont calculées."
           else if (nsig == 0) sprintf("Aucun signal de cause spéciale (tests %s) : le procédé paraît sous contrôle statistique.", paste(tests, collapse = ", "))
           else sprintf("%d signal(aux) de cause spéciale détecté(s) (tests %s) : le procédé n'est pas stable.", nsig, paste(unique(signaux$Test), collapse = ", "))
  lignes <- c(sprintf("Type de carte : %s%s — %d %s (%s)", titre, if (auto) " (choisi automatiquement)" else "", k, if (type == "imr") "points" else "sous-groupes", taille),
              meth,
              if (variable) sprintf("Ligne centrale (moyenne) : %s — limites de contrôle variables selon la taille des sous-groupes", fmt(cl1[1], 6)),
              if (!variable) sprintf("%s : LIC = %s ; LC = %s ; LSC = %s", nom1, fmt(lcl1[1], 6), fmt(cl1[1], 6), fmt(ucl1[1], 6)),
              if (!variable) sprintf("%s : LIC = %s ; LC = %s ; LSC = %s", nom2, fmt(lcl2[1], 6), fmt(cl2[1], 6), fmt(ucl2[1], 6)),
              sprintf("%s : σ̂ = %s (à reporter comme « σ intra » dans l'onglet Capabilité pour Cp/Cpk)",
                      if (type == "imr") "Écart-type à court terme estimé" else "Écart-type intra-sous-groupe estimé", fmt(sig, 6)),
              "", concl,
              if (nsig > 0) c("", if (length(s1_sig)) paste0(nom1, " — ", lis(s1_sig)), if (length(s2_sig)) paste0(nom2, " — ", lis(s2_sig))))
  tabs <- list(Signaux = signaux, Carte_1 = tab1, Carte_2 = tab2)
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]
  names(tabs)[names(tabs) == "Carte_1"] <- paste("Carte :", nom1)
  names(tabs)[names(tabs) == "Carte_2"] <- paste("Carte :", nom2)
  list(titre = titre, lignes = lignes[!vapply(lignes, is.null, logical(1))], conclusion = concl,
       tables = tabs, figures = c(fig), avertissements = avert)
}

# =============================================================================
#  R&R : étude de type 1 (un seul étalon, biais et répétabilité)
#  Formules et valeurs par défaut de Minitab : K = 20 % de la tolérance, L = 6 écarts-types.
# =============================================================================
cmd_rr1 <- function(d, p) {
  cn <- nom_colonne(d, p$colonne, "Mesures")
  x <- sans_na(col_num(d, cn)); n <- length(x)
  ref <- num(p$reference); tol <- num(p$tolerance)
  K <- num(p$k); if (is.na(K)) K <- 20
  L <- num(p$l); if (is.na(L)) L <- 6
  if (is.na(ref)) stop("Saisir la valeur de référence de l'étalon.", call. = FALSE)
  if (is.na(tol) || tol <= 0) stop("Saisir la tolérance (écart entre les limites de spécification), strictement positive.", call. = FALSE)
  if (K <= 0 || L <= 0) stop("K et L doivent être strictement positifs.", call. = FALSE)
  if (n < 5) stop("Il faut au moins 5 mesures répétées de la même pièce (on recommande 25 à 50).", call. = FALSE)
  m <- mean(x); s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) stop("Toutes les mesures sont identiques : la répétabilité n'est pas calculable.", call. = FALSE)
  biais <- m - ref
  tt <- stats::t.test(x, mu = ref)
  Cg <- (K / 100 * tol) / (L * s)
  Cgk <- (K / 200 * tol - abs(biais)) / (L / 2 * s)
  pvr <- 100 * (L * s) / tol
  pvrb <- 100 * (L * s + 2 * abs(biais)) / tol
  ok_g <- Cg >= 1.33; ok_gk <- Cgk >= 1.33
  concl <- if (ok_g && ok_gk) "Le moyen de mesure est capable (Cg et Cgk ≥ 1,33)."
           else if (ok_g) "La répétabilité est suffisante (Cg ≥ 1,33) mais le biais pénalise le Cgk : régler ou étalonner le moyen de mesure."
           else "La répétabilité est insuffisante (Cg < 1,33) : la dispersion des mesures est trop grande par rapport à la tolérance."
  tab <- data.frame(Indicateur = c("Nombre de mesures", "Moyenne", "Écart-type", "Valeur de référence", "Biais (moyenne − référence)",
                                   "Tolérance", "Cg", "Cgk", "% Var (répétabilité)", "% Var (répétabilité et biais)"),
                    Valeur = c(n, m, s, ref, biais, tol, Cg, Cgk, pvr, pvrb), stringsAsFactors = FALSE)
  avert <- list(sprintf("Conventions de Minitab : K = %s %% de la tolérance et variation d'étude sur L = %s écarts-types ; seuil de capabilité : 1,33.", fmt(K, 4), fmt(L, 4)))
  if (n < 25) avert <- c(avert, list(sprintf("Seulement %d mesures : on recommande au moins 25 mesures (Minitab : 50) pour une estimation fiable.", n)))
  fig <- lire_png("rr_type1.png", largeur = 960, hauteur = 520)
  par(mar = c(4.5, 4.5, 3, 7))
  dem <- K / 200 * tol
  plot(seq_len(n), x, type = "b", pch = 19, col = "#2a5d9f", xlab = "Observation", ylab = "Mesure",
       main = "Mesures répétées de l'étalon", ylim = range(c(x, ref - dem, ref + dem)))
  abline(h = ref, col = "#444444", lwd = 2)
  abline(h = c(ref - dem, ref + dem), col = "#b03030", lty = 2, lwd = 2)
  abline(h = m, col = "#2e8b57", lty = 3, lwd = 2)
  mtext("Référence", side = 4, at = ref, las = 1, cex = 0.75, line = 0.2)
  mtext(sprintf("Réf. ± %s %% tol./2", fmt(K, 3)), side = 4, at = ref + dem, las = 1, cex = 0.7, line = 0.2, col = "#b03030")
  mtext("Moyenne", side = 4, at = m, las = 1, cex = 0.75, line = 0.2, col = "#2e8b57")
  invisible(dev.off())
  list(titre = "R&R de type 1 (étude du moyen de mesure sur un étalon)",
       lignes = c(sprintf("Mesures : %d — moyenne : %s — écart-type (répétabilité) : %s", n, fmt(m, 6), fmt(s, 5)),
                  sprintf("Référence : %s — biais : %s — test t du biais (H0 : biais nul) : p-value %s%s", fmt(ref, 6), fmt(biais, 4), fmt_p(tt$p.value),
                          if (tt$p.value < ALPHA) sprintf(" < α (%s) : H0 rejetée, biais significatif", alpha_txt()) else sprintf(" ≥ α (%s) : H0 non rejetée, biais non significatif", alpha_txt())),
                  sprintf("Cg = %s — Cgk = %s", fmt(Cg, 4), fmt(Cgk, 4)),
                  sprintf("%% Var (répétabilité) = %s %% — %% Var (répétabilité et biais) = %s %%", fmt(pvr, 4), fmt(pvrb, 4)),
                  concl),
       conclusion = sprintf("Cg = %s, Cgk = %s, biais %s (p-value %s). %s", fmt(Cg, 3), fmt(Cgk, 3), fmt(biais, 3), fmt_p(tt$p.value), concl),
       tables = list(Indicateurs = tab), figures = c(fig), avertissements = avert)
}

# =============================================================================
#  R&R croisée (pièces × opérateurs × répétitions) — méthode ANOVA, conventions de Minitab
# =============================================================================
cmd_rr_croisee <- function(d, p) {
  pn <- nom_colonne(d, p$piece, "Pièces"); on <- nom_colonne(d, p$operateur, "Opérateurs"); mn <- nom_colonne(d, p$mesure, "Mesures")
  if (length(unique(c(pn, on, mn))) < 3) stop("Les colonnes des pièces, des opérateurs et des mesures doivent être différentes.", call. = FALSE)
  tol <- num(p$tolerance)
  L <- num(p$l); if (is.na(L)) L <- 6
  alpha <- num(p$alpha); if (is.na(alpha)) alpha <- 0.05
  if (L <= 0) stop("Le multiplicateur L doit être strictement positif.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1) stop("Le seuil de l'interaction doit être compris entre 0 et 1.", call. = FALSE)
  if (!is.na(tol) && tol <= 0) stop("La tolérance doit être strictement positive.", call. = FALSE)
  y <- col_num(d, mn)
  pc <- ch_cat(d[[pn]]); oc <- ch_cat(d[[on]])
  ok <- is.finite(y) & !is.na(pc) & !is.na(oc)
  avert <- list()
  if (sum(!ok) > 0) avert <- c(avert, list(sprintf("%d ligne(s) incomplète(s) écartée(s).", sum(!ok))))
  dd <- data.frame(P = factor(pc[ok], levels = force_ordre_numerique(unique(pc[ok]))), O = factor(oc[ok], levels = unique(oc[ok])), y = y[ok])
  a <- nlevels(dd$P); b <- nlevels(dd$O)
  if (a < 2 || b < 2) stop("Il faut au moins 2 pièces et 2 opérateurs.", call. = FALSE)
  cnt <- table(dd$P, dd$O)
  if (any(cnt == 0) || length(unique(as.vector(cnt))) != 1)
    stop(sprintf("Plan non équilibré : chaque opérateur doit mesurer chaque pièce le même nombre de fois (%d pièces, %d opérateurs ; effectifs observés de %d à %d par case).",
                 a, b, min(cnt), max(cnt)), call. = FALSE)
  n <- unname(cnt[1, 1])
  if (n < 2) stop("Il faut au moins 2 mesures par pièce et par opérateur (répétitions).", call. = FALSE)
  if (a < 5) avert <- c(avert, list(sprintf("Seulement %d pièces : on recommande au moins 10 pièces représentatives de l'étendue du procédé.", a)))
  if (b < 3) avert <- c(avert, list(sprintf("Seulement %d opérateurs : on en recommande au moins 3.", b)))

  fit <- stats::aov(y ~ P * O, data = dd)
  sm <- summary(fit)[[1]]
  rn <- trimws(rownames(sm))
  SS <- setNames(sm[["Sum Sq"]], rn); DF <- setNames(sm[["Df"]], rn)
  SSp <- SS[["P"]]; SSo <- SS[["O"]]; SSpo <- SS[["P:O"]]; SSe <- SS[["Residuals"]]
  dfp <- DF[["P"]]; dfo <- DF[["O"]]; dfpo <- DF[["P:O"]]; dfe <- DF[["Residuals"]]
  MSp <- SSp / dfp; MSo <- SSo / dfo; MSpo <- SSpo / dfpo; MSe <- SSe / dfe
  div <- function(u, v) if (v == 0) Inf else u / v
  Fp <- div(MSp, MSpo); Fo <- div(MSo, MSpo); Fpo <- div(MSpo, MSe)
  pp <- stats::pf(Fp, dfp, dfpo, lower.tail = FALSE); po <- stats::pf(Fo, dfo, dfpo, lower.tail = FALSE)
  ppo <- stats::pf(Fpo, dfpo, dfe, lower.tail = FALSE)
  sstot <- SSp + SSo + SSpo + SSe; dftot <- dfp + dfo + dfpo + dfe
  nm <- c("Pièce", "Opérateur", "Pièce × Opérateur", "Répétabilité (erreur)", "Total")
  tab1 <- data.frame(Source = nm, ddl = c(dfp, dfo, dfpo, dfe, dftot), Somme_carres = c(SSp, SSo, SSpo, SSe, sstot),
                     Carre_moyen = c(MSp, MSo, MSpo, MSe, NA), F = c(Fp, Fo, Fpo, NA, NA), p_value = c(pp, po, ppo, NA, NA), stringsAsFactors = FALSE)
  inter <- is.finite(ppo) && ppo < alpha      # l'interaction est conservée si elle est significative
  tables <- list(`ANOVA à deux facteurs avec interaction` = tab1)
  if (inter) {
    s2e <- MSe; brut <- c((MSpo - MSe) / n, (MSo - MSpo) / (a * n), (MSp - MSpo) / (b * n))
    s2po <- max(0, brut[1]); s2o <- max(0, brut[2]); s2p <- max(0, brut[3])
    modele <- "avec interaction Pièce × Opérateur"
  } else {
    dfpool <- dfpo + dfe; MSpool <- (SSpo + SSe) / dfpool
    Fp2 <- div(MSp, MSpool); Fo2 <- div(MSo, MSpool)
    tab2 <- data.frame(Source = c("Pièce", "Opérateur", "Répétabilité (erreur + interaction)", "Total"),
                       ddl = c(dfp, dfo, dfpool, dftot), Somme_carres = c(SSp, SSo, SSpo + SSe, sstot),
                       Carre_moyen = c(MSp, MSo, MSpool, NA), F = c(Fp2, Fo2, NA, NA),
                       p_value = c(stats::pf(Fp2, dfp, dfpool, lower.tail = FALSE), stats::pf(Fo2, dfo, dfpool, lower.tail = FALSE), NA, NA), stringsAsFactors = FALSE)
    tables$`ANOVA sans interaction` <- tab2
    s2e <- MSpool; s2po <- 0; brut <- c((MSo - MSpool) / (a * n), (MSp - MSpool) / (b * n))
    s2o <- max(0, brut[1]); s2p <- max(0, brut[2])
    modele <- "sans interaction (non significative)"
  }
  s2rep <- s2e; s2rpd <- s2o + s2po; s2grr <- s2rep + s2rpd; s2tot <- s2grr + s2p
  if (s2tot <= 0) stop("Variation totale nulle : les mesures sont identiques.", call. = FALSE)
  src <- c("R&R totale", "  Répétabilité", "  Reproductibilité", if (inter) c("    Opérateur", "    Pièce × Opérateur") else "    Opérateur",
           "Pièce à pièce", "Variation totale")
  vc <- c(s2grr, s2rep, s2rpd, if (inter) c(s2o, s2po) else s2o, s2p, s2tot)
  sd <- sqrt(vc)
  pct_c <- 100 * vc / s2tot
  pct_sv <- 100 * sd / sd[length(sd)]
  tab_vc <- data.frame(Source = src, Composante_variance = vc, Contribution_pct = pct_c, stringsAsFactors = FALSE)
  tab_sv <- data.frame(Source = src, Ecart_type = sd, Variation_etude = L * sd, Pct_variation_etude = pct_sv, stringsAsFactors = FALSE)
  if (!is.na(tol)) tab_sv$Pct_tolerance <- 100 * L * sd / tol
  tables$`Composantes de la variance` <- tab_vc
  tables$`Variation d'étude` <- tab_sv
  ndc <- floor(1.41 * sqrt(s2p) / sqrt(s2grr))
  if (!is.finite(ndc)) ndc <- NA_integer_
  sv_grr <- pct_sv[1]; c_grr <- pct_c[1]; tol_grr <- if (!is.na(tol)) 100 * L * sd[1] / tol else NA_real_
  jugement <- function(v) if (is.na(v)) NA_character_ else if (v < 10) "acceptable" else if (v <= 30) "acceptable sous conditions" else "inacceptable"
  concl <- sprintf("R&R totale : %s %% de la variation d'étude (%s), %s %% de la variance%s ; %s catégorie(s) distincte(s)%s.",
                   fmt(sv_grr, 4), jugement(sv_grr), fmt(c_grr, 4),
                   if (!is.na(tol)) sprintf(", %s %% de la tolérance (%s)", fmt(tol_grr, 4), jugement(tol_grr)) else "",
                   if (is.na(ndc)) "?" else as.character(ndc), if (!is.na(ndc) && ndc < 5) " (il en faut au moins 5)" else "")
  avert <- c(avert, list(sprintf("Conventions de Minitab : méthode ANOVA, composantes aléatoires, variation d'étude sur L = %s écarts-types, interaction retirée si sa p-value dépasse %s (la norme AIAG retient 0,25).", fmt(L, 3), fmt(alpha, 3)),
                         "Repères : moins de 10 % acceptable, de 10 à 30 % acceptable sous conditions, plus de 30 % inacceptable ; au moins 5 catégories distinctes."))
  if (any(brut < 0)) avert <- c(avert, list("Une composante de variance estimée négative a été ramenée à 0 (convention de Minitab)."))

  # ---- figures
  pcol <- PAL(b)
  lp <- levels(dd$P); lo <- levels(dd$O)
  fig1 <- lire_png("rr_composantes.png", largeur = 1100, hauteur = 860)
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))
  idx <- c(1, 2, 3, length(src) - 1)
  met <- rbind(`% Contribution` = pct_c[idx], `% Variation d'étude` = pct_sv[idx])
  if (!is.na(tol)) met <- rbind(met, `% Tolérance` = 100 * L * sd[idx] / tol)
  bp <- barplot(met, beside = TRUE, col = c("#2a5d9f", "#e0a030", "#2e8b57")[seq_len(nrow(met))], names.arg = c("R&R totale", "Répétabilité", "Reproductib.", "Pièce à pièce"),
                main = "Composantes de la variation", ylab = "%", ylim = c(0, max(110, max(met, na.rm = TRUE) * 1.05)), border = "white", cex.names = 0.85)
  legend("topleft", legend = rownames(met), fill = c("#2a5d9f", "#e0a030", "#2e8b57")[seq_len(nrow(met))], bty = "n", cex = 0.8)
  # mesures par pièce
  xp <- as.integer(dd$P)
  plot(xp, dd$y, xaxt = "n", pch = 19, col = pcol[as.integer(dd$O)], xlab = "Pièce", ylab = "Mesure", main = "Mesures par pièce", cex = 0.9)
  axis(1, at = seq_len(a), labels = lp, las = if (a > 12) 2 else 1)
  lines(seq_len(a), as.numeric(tapply(dd$y, dd$P, mean)), col = "#444444", lwd = 2)
  legend("topleft", legend = lo, col = pcol, pch = 19, bty = "n", cex = 0.8, title = "Opérateur")
  boxplot(y ~ O, data = dd, col = "#cfe0f5", xlab = "Opérateur", ylab = "Mesure", main = "Mesures par opérateur")
  mp <- tapply(dd$y, list(dd$P, dd$O), mean)
  matplot(seq_len(a), mp, type = "b", pch = 19, lty = 1, col = pcol, xaxt = "n", xlab = "Pièce", ylab = "Moyenne", main = "Interaction Pièce × Opérateur")
  axis(1, at = seq_len(a), labels = lp, las = if (a > 12) 2 else 1)
  legend("topleft", legend = lo, col = pcol, lty = 1, pch = 19, bty = "n", cex = 0.8, title = "Opérateur")
  invisible(dev.off())

  # cartes par opérateur : étendues et moyennes de chaque pièce, limites communes (R-barre global)
  rg <- tapply(dd$y, list(dd$P, dd$O), function(v) diff(range(v)))
  mg <- tapply(dd$y, list(dd$P, dd$O), mean)
  rbar <- mean(rg); xbb <- mean(mg)
  D4 <- 1 + 3 * cst_d3(n) / cst_d2(n); D3 <- max(0, 1 - 3 * cst_d3(n) / cst_d2(n)); A2 <- 3 / (cst_d2(n) * sqrt(n))
  fig2 <- lire_png("rr_cartes_operateur.png", largeur = 1000, hauteur = 760)
  layout(matrix(1:2, ncol = 1)); par(mar = c(4.5, 4.5, 2.5, 7))
  pos <- seq_len(a * b)
  carte_op <- function(v, cl, lcl, ucl, titre, yl) {
    plot(pos, as.vector(v), type = "n", xaxt = "n", xlab = "", ylab = yl, main = titre,
         ylim = range(c(v, lcl, ucl), na.rm = TRUE) + c(-1, 1) * 0.05 * diff(range(c(v, lcl, ucl), na.rm = TRUE)))
    for (j in seq_len(b)) {
      ix <- (j - 1) * a + seq_len(a)
      lines(ix, v[, j], col = "#2a5d9f"); points(ix, v[, j], pch = 19, col = "#2a5d9f", cex = 0.8)
      if (j > 1) abline(v = (j - 1) * a + 0.5, lty = 3, col = "#888888")
      mtext(lo[j], side = 3, at = (j - 1) * a + (a + 1) / 2, line = -1.2, cex = 0.8)
    }
    abline(h = cl, col = "#2e8b57", lwd = 2); abline(h = c(lcl, ucl), col = "#b03030", lwd = 2)
    mtext(sprintf("LSC = %s", fmt(ucl, 4)), side = 4, at = ucl, las = 1, cex = 0.7, line = 0.2, col = "#b03030")
    mtext(sprintf("LC = %s", fmt(cl, 4)), side = 4, at = cl, las = 1, cex = 0.7, line = 0.2, col = "#2e8b57")
    if (lcl > -Inf && lcl != 0) mtext(sprintf("LIC = %s", fmt(lcl, 4)), side = 4, at = lcl, las = 1, cex = 0.7, line = 0.2, col = "#b03030")
  }
  carte_op(rg, rbar, D3 * rbar, D4 * rbar, "Carte des étendues par opérateur", "Étendue")
  par(mar = c(4.5, 4.5, 2.5, 7))
  carte_op(mg, xbb, xbb - A2 * rbar, xbb + A2 * rbar, "Carte des moyennes par opérateur", "Moyenne")
  axis(1, at = pos, labels = rep(lp, b), las = if (a > 8) 2 else 1, cex.axis = 0.7); mtext("Pièce (par opérateur)", side = 1, line = 3, cex = 0.85)
  invisible(dev.off())

  lignes <- c(sprintf("Plan : %d pièces × %d opérateurs × %d répétitions (%d mesures) — modèle %s", a, b, n, nrow(dd), modele),
              sprintf("Interaction Pièce × Opérateur : p-value %s (seuil de conservation : %s)", fmt_p(ppo), fmt(alpha, 3)),
              sprintf("Effet pièce : p-value %s — effet opérateur : p-value %s", fmt_p(if (inter) pp else tables$`ANOVA sans interaction`$p_value[1]),
                      fmt_p(if (inter) po else tables$`ANOVA sans interaction`$p_value[2])),
              "",
              sprintf("R&R totale : %s %% de la variance, %s %% de la variation d'étude%s", fmt(c_grr, 4), fmt(sv_grr, 4),
                      if (!is.na(tol)) sprintf(", %s %% de la tolérance", fmt(tol_grr, 4)) else ""),
              sprintf("Répétabilité : %s %% de la variation d'étude — reproductibilité : %s %%", fmt(pct_sv[2], 4), fmt(pct_sv[3], 4)),
              sprintf("Nombre de catégories distinctes : %s", if (is.na(ndc)) "non calculable" else as.character(ndc)),
              "", concl)
  list(titre = "R&R croisée (méthode ANOVA)", lignes = lignes, conclusion = concl,
       tables = tables, figures = c(fig1, fig2), avertissements = avert)
}
