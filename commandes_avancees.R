# =============================================================================
#  commandes_avancees.R — commandes supplémentaires de cli_statistique.R
#    ftest        : test F de comparaison de deux variances
#    khi2         : khi² d'indépendance, d'homogénéité et de conformité
#    carte        : cartes de contrôle (X-barre/R, X-barre/S, I-MR)
#    rr1          : étude R&R de type 1 (biais, répétabilité, Cg/Cgk)
#    rr_croisee   : étude R&R croisée (méthode ANOVA, conventions Minitab)
#    rr_emboitee  : étude R&R emboîtée (pièces emboîtées dans les opérateurs)
#    anova2       : ANOVA à deux facteurs (avec ou sans interaction)
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
  # deux modalités dont l'une est une négation / un défaut (pas, non, absent, défaut…) : rouge pour la négation, vert pour l'autre
  if (nc == 2) {
    neg <- grepl("(^|[^[:alpha:]])(pas|non|sans|aucun|absen|d[ée]faut|d[ée]fectu|refus|[ée]chec|rejet|mauvais|ko|no|not|fail|bad|reject|defect)", tolower(colnames(tab)), perl = TRUE)
    if (sum(neg) == 1) cl <- ifelse(neg, "#c0392b", "#2e8b57")
  }
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
trace_carte <- function(titre, etiq, y, cl, lcl, ucl, sig, signaux, ylab, zones = FALSE, xlab = "Sous-groupe", ph = NULL) {
  k <- length(y); x <- seq_len(k)
  segs <- if (is.null(ph)) list(x) else lapply(unique(ph), function(l) which(ph == l))
  hauts <- c(y, ucl, if (all(is.finite(lcl))) lcl)
  yl <- range(hauts[is.finite(hauts)], na.rm = TRUE); marge <- diff(yl) * 0.08; yl <- yl + c(-marge, marge)
  longues0 <- max(nchar(etiq), 0, na.rm = TRUE) > 8
  if (longues0) par(mar = c(9, par("mar")[2:4]))
  plot(x, y, type = "n", xaxt = "n", xlab = if (longues0) "" else xlab, ylab = ylab, main = titre, ylim = yl, xlim = c(0.5, k + 0.5))
  longues <- max(nchar(etiq), 0, na.rm = TRUE) > 8                      # étiquettes de dates : plus de place, moins de repères
  idx <- if (k <= (if (longues) 20 else 30)) x else unique(round(seq(1, k, length.out = min(k, if (longues) 14 else 15))))
  axis(1, at = idx, labels = etiq[idx], las = if (k > 15 || longues) 2 else 1, cex.axis = 0.8)
  if (longues) mtext(xlab, side = 1, line = par("mar")[1] - 1.3)
  for (sg in segs) {
    if (zones) for (m in c(-2, -1, 1, 2)) lines(sg, (cl + m * sig)[sg], type = "s", lty = 3, col = "#aaaaaa")
    lines(sg, ucl[sg], type = "s", col = "#b03030", lwd = 2)
    lines(sg, lcl[sg], type = "s", col = "#b03030", lwd = 2)
    lines(sg, cl[sg], type = "s", col = "#2e8b57", lwd = 2)
  }
  if (length(segs) > 1) {
    for (j in seq_along(segs)) {
      if (j > 1) abline(v = min(segs[[j]]) - 0.5, lty = 2, col = "#555555")
      mtext(unique(ph)[j], side = 3, at = mean(range(segs[[j]])), line = -1.1, cex = 0.85, font = 2, col = "#555555")
    }
  }
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

# Statistiques et limites de contrôle d'un jeu de sous-groupes (une phase).
limites_carte <- function(type, groupes, nn) {
  k <- length(groupes)
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
  list(sig = sig, meth = meth, titre = titre, nom1 = nom1, nom2 = nom2, y1 = y1, y2 = y2, cl1 = cl1, s1 = s1, ucl1 = ucl1, lcl1 = lcl1,
       cl2 = cl2, s2 = s2, ucl2 = ucl2, lcl2 = lcl2, zero_mr = zero_mr)
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
  # ---- phases : colonne facultative (une valeur par ligne) ou numéro du premier point de la phase « Après »
  ph_rows <- NULL; ph_pts <- NULL; nom_phase <- NULL
  if (!is.null(p$phase) && nzchar(p$phase)) { nom_phase <- nom_colonne(d, p$phase, "Colonne de phases"); ph_rows <- as.character(d[[nom_phase]]) }
  rupt <- num(p$rupture)
  # ---- constitution des sous-groupes
  if (identical(p$format, "long")) {
    vn <- nom_colonne(d, p$valeur, "Colonne des mesures")
    v <- col_num(d, vn)
    if (identical(type, "imr")) {
      ok <- is.finite(v); groupes <- as.list(v[ok])
      etiq <- if (!is.null(lab_ordre)) lab_ordre[ok] else as.character(seq_along(groupes))
      if (!is.null(ph_rows)) ph_pts <- ph_rows[ok]
    } else {
      gn <- nom_colonne(d, p$groupe, "Colonne des sous-groupes")
      g <- ch_cat(d[[gn]]); ok <- is.finite(v) & !is.na(g)
      if (!any(ok)) stop("Aucune mesure exploitable.", call. = FALSE)
      niv <- unique(g[ok]); groupes <- lapply(niv, function(l) v[ok & g == l]); etiq <- niv
      if (!is.null(ph_rows)) ph_pts <- vapply(niv, function(l) ph_rows[which(ok & g == l)[1]], "")
    }
  } else {
    cols <- noms_numeriques(d, unlist(p$colonnes))
    if (identical(type, "imr") || length(cols) == 1) {
      if (length(cols) > 1 && identical(type, "imr")) stop("Pour les valeurs individuelles, choisir une seule colonne.", call. = FALSE)
      ok <- is.finite(d[[cols[1]]]); x <- d[[cols[1]]][ok]; groupes <- as.list(x)
      etiq <- if (!is.null(lab_ordre)) lab_ordre[ok] else as.character(seq_along(x))
      if (!is.null(ph_rows)) ph_pts <- ph_rows[ok]
    } else {
      M <- as.matrix(d[, cols, drop = FALSE])
      groupes <- lapply(seq_len(nrow(M)), function(i) M[i, is.finite(M[i, ])])
      etiq <- if (!is.null(lab_ordre)) lab_ordre else as.character(seq_along(groupes))
      if (!is.null(ph_rows)) ph_pts <- ph_rows
    }
  }
  nn <- vapply(groupes, length, integer(1))
  garde <- nn > 0
  groupes <- groupes[garde]; etiq <- etiq[garde]; nn <- nn[garde]
  if (!is.null(ph_pts)) ph_pts <- ph_pts[garde]
  if (!length(groupes)) stop("Aucune donnée exploitable.", call. = FALSE)
  avert <- list()

  # ---- type de carte
  if (identical(type, "auto")) type <- if (all(nn == 1)) "imr" else if (stats::median(nn) >= 8) "xbar_s" else "xbar_r"
  auto <- identical(p$type %||% "auto", "auto")
  if (type == "imr" && !all(nn == 1)) {
    groupes <- as.list(unlist(groupes)); nn <- rep(1L, length(groupes)); etiq <- as.character(seq_along(groupes)); ph_pts <- NULL
    avert <- c(avert, list("Valeurs individuelles : toutes les mesures sont prises dans l'ordre des données, une par point."))
  }
  if (type != "imr") {
    petit <- nn < 2
    if (all(petit)) stop("Les sous-groupes ne contiennent qu'une valeur : utiliser la carte des valeurs individuelles (I-MR).", call. = FALSE)
    if (any(petit)) {
      avert <- c(avert, list(sprintf("Sous-groupe(s) d'une seule valeur écarté(s) : %s.", paste(etiq[petit], collapse = ", "))))
      groupes <- groupes[!petit]; etiq <- etiq[!petit]; nn <- nn[!petit]
      if (!is.null(ph_pts)) ph_pts <- ph_pts[!petit]
    }
  }
  k <- length(groupes)
  if (k < 2) stop("Il faut au moins 2 sous-groupes (ou 2 mesures) pour construire une carte de contrôle.", call. = FALSE)
  if (k < 20 && type != "imr") avert <- c(avert, list(sprintf("Seulement %d sous-groupes : on recommande au moins 20 à 25 sous-groupes pour établir des limites de contrôle fiables.", k)))
  if (k < 20 && type == "imr") avert <- c(avert, list(sprintf("Seulement %d valeurs : on recommande au moins 20 à 25 valeurs pour établir des limites de contrôle fiables.", k)))
  variable <- length(unique(nn)) > 1

  # ---- phases (avant / après) : colonne de phases, ou rupture à partir d'un numéro de point
  ph <- ph_pts
  if (is.null(ph) && !is.na(rupt)) ph <- ifelse(seq_len(k) < rupt, "Avant", "Après")
  if (!is.null(ph)) { ph <- as.character(ph); ph[is.na(ph)] <- "(vide)" }
  niv_ph <- if (is.null(ph)) NULL else unique(ph)
  idx_ph <- if (is.null(ph)) list(seq_len(k)) else lapply(niv_ph, function(l) which(ph == l))
  if (!is.null(ph) && length(niv_ph) < 2) { ph <- NULL; niv_ph <- NULL; idx_ph <- list(seq_len(k)) }
  # ---- statistiques et limites, phase par phase
  L0 <- NULL; sigs <- numeric(0); mets <- character(0)
  vec <- function() rep(NA_real_, k)
  y1 <- y2 <- cl1 <- s1 <- ucl1 <- lcl1 <- cl2 <- s2 <- ucl2 <- lcl2 <- vec(); zero_mr <- rep(FALSE, k)
  for (j in seq_along(idx_ph)) {
    ix <- idx_ph[[j]]
    if (length(ix) < 2) stop(sprintf("La phase « %s » ne contient qu'un point : il en faut au moins 2 par phase.", niv_ph[j]), call. = FALSE)
    Lj <- limites_carte(type, groupes[ix], nn[ix])
    if (!is.finite(Lj$sig) || Lj$sig <= 0) stop(sprintf("%s : les mesures sont constantes (dispersion nulle), impossible de calculer des limites de contrôle.", if (is.null(ph)) "Carte" else sprintf("Phase « %s »", niv_ph[j])), call. = FALSE)
    if (is.null(L0)) L0 <- Lj
    y1[ix] <- Lj$y1; y2[ix] <- Lj$y2; cl1[ix] <- Lj$cl1; s1[ix] <- Lj$s1; ucl1[ix] <- Lj$ucl1; lcl1[ix] <- Lj$lcl1
    cl2[ix] <- Lj$cl2; s2[ix] <- Lj$s2; ucl2[ix] <- Lj$ucl2; lcl2[ix] <- Lj$lcl2; zero_mr[ix] <- Lj$zero_mr
    sigs <- c(sigs, Lj$sig); mets <- c(mets, Lj$meth)
  }
  sig <- sigs[1]; meth <- mets[1]; nom1 <- L0$nom1; nom2 <- L0$nom2; titre <- L0$titre
  if (!is.null(ph) && k < 40 * length(idx_ph)) avert <- c(avert, list("Plusieurs phases : chaque phase a ses propres limites ; une phase courte (moins de 20 points) donne des limites peu fiables."))

  # ---- tests de causes spéciales (appliqués séparément dans chaque phase)
  tests_var <- intersect(tests, 1:4)
  fusion <- function(a, b) { for (nm in names(b)) a[[nm]] <- c(a[[nm]], b[[nm]]); a }
  s1_sig <- list(); s2_sig <- list()
  for (ix in idx_ph) {
    r1 <- tests_causes_speciales(y1[ix], cl1[ix], s1[ix], tests)
    s1_sig <- fusion(s1_sig, lapply(r1, function(v) ix[v]))
    if (length(tests_var)) {
      ok2j <- ix[!zero_mr[ix]]
      r2 <- tests_causes_speciales(y2[ok2j], cl2[ok2j], s2[ok2j], tests_var)
      s2_sig <- fusion(s2_sig, lapply(r2, function(v) ok2j[v]))
    }
  }
  s1_sig <- s1_sig[order(as.integer(names(s1_sig)))]; s2_sig <- s2_sig[order(as.integer(names(s2_sig)))]
  ok2 <- !zero_mr
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
  if (!is.null(ph)) { tab1 <- cbind(Phase = ph, tab1); tab2 <- cbind(Phase = ph, tab2) }
  if (type == "imr") tab2 <- tab2[ok2, , drop = FALSE]
  if (type == "imr") tab1$n <- NULL
  if (type == "imr") tab2$n <- NULL

  # ---- figure
  zones <- any(tests %in% 5:8)
  fig <- lire_png("carte_controle.png", largeur = max(900, min(2000, 22 * k + 500)), hauteur = 760 + if (!is.null(lab_ordre) && !identical(p$format, "long")) 200 else 0)
  layout(matrix(1:2, ncol = 1)); par(mar = c(4.5, 4.5, 2.5, 7))
  xl <- if (!is.null(lab_ordre) && !identical(p$format, "long")) nom_ordre else if (type == "imr") "Observation" else "Sous-groupe"
  trace_carte(nom1, etiq, y1, cl1, lcl1, ucl1, s1, s1_sig, if (type == "imr") "Valeur individuelle" else "Moyenne", zones, xl, ph)
  par(mar = c(4.5, 4.5, 2.5, 7))
  if (type == "imr") {
    trace_carte(nom2, etiq[ok2], y2[ok2], cl2[ok2], lcl2[ok2], ucl2[ok2], s2[ok2], lapply(s2_sig, function(v) match(v, which(ok2))), "Étendue mobile", FALSE, xl, if (is.null(ph)) NULL else ph[ok2])
  } else trace_carte(nom2, etiq, y2, cl2, lcl2, ucl2, s2, s2_sig, if (type == "xbar_r") "Étendue" else "Écart-type", FALSE, xl, ph)
  invisible(dev.off())

  taille <- if (type == "imr") "valeurs individuelles" else if (variable) sprintf("%d à %d valeurs par sous-groupe (limites variables)", min(nn), max(nn)) else sprintf("sous-groupes de %d valeurs", nn[1])
  concl <- if (!length(tests)) "Aucun test de causes spéciales demandé : seules les limites de contrôle sont calculées."
           else if (nsig == 0) sprintf("Aucun signal de cause spéciale (tests %s) : le procédé paraît sous contrôle statistique.", paste(tests, collapse = ", "))
           else sprintf("%d signal(aux) de cause spéciale détecté(s) (tests %s) : le procédé n'est pas stable.", nsig, paste(unique(signaux$Test), collapse = ", "))
  lignes <- c(sprintf("Type de carte : %s%s — %d %s (%s)", titre, if (auto) " (choisi automatiquement)" else "", k, if (type == "imr") "points" else "sous-groupes", taille),
              meth,
              if (is.null(ph) && variable) sprintf("Ligne centrale (moyenne) : %s — limites de contrôle variables selon la taille des sous-groupes", fmt(cl1[1], 6)),
              if (is.null(ph) && !variable) sprintf("%s : LIC = %s ; LC = %s ; LSC = %s", nom1, fmt(lcl1[1], 6), fmt(cl1[1], 6), fmt(ucl1[1], 6)),
              if (is.null(ph) && !variable) sprintf("%s : LIC = %s ; LC = %s ; LSC = %s", nom2, fmt(lcl2[1], 6), fmt(cl2[1], 6), fmt(ucl2[1], 6)),
              if (!is.null(ph)) unlist(lapply(seq_along(idx_ph), function(j) { i1 <- idx_ph[[j]][1]
                c(sprintf("Phase « %s » (%d %s) : %s — LIC = %s ; LC = %s ; LSC = %s %s", niv_ph[j], length(idx_ph[[j]]), if (type == "imr") "points" else "sous-groupes", nom1, fmt(lcl1[i1], 6), fmt(cl1[i1], 6), fmt(ucl1[i1], 6), if (type != "imr" && length(unique(nn[idx_ph[[j]]])) > 1) "(limites variables : valeurs du premier sous-groupe)" else ""),
                  sprintf("                      %s — LIC = %s ; LC = %s ; LSC = %s ; σ̂ = %s", nom2, fmt(lcl2[i1], 6), fmt(cl2[i1], 6), fmt(ucl2[i1], 6), fmt(sigs[j], 6))) })),
              if (is.null(ph)) sprintf("%s : σ̂ = %s (à reporter comme « σ intra » dans l'onglet Capabilité pour Cp/Cpk)",
                      if (type == "imr") "Écart-type à court terme estimé" else "Écart-type intra-sous-groupe estimé", fmt(sig, 6)),
              "", concl,
              if (nsig > 0) c("", if (length(s1_sig)) paste0(nom1, " — ", lis(s1_sig)), if (length(s2_sig)) paste0(nom2, " — ", lis(s2_sig))))
  tabs <- list(Signaux = signaux, Carte_1 = tab1, Carte_2 = tab2)
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]
  names(tabs)[names(tabs) == "Carte_1"] <- paste("Carte :", nom1)
  names(tabs)[names(tabs) == "Carte_2"] <- paste("Carte :", nom2)
  if (!is.null(ph)) titre <- paste0(titre, sprintf(" — %d phases (%s)", length(idx_ph), paste(niv_ph, collapse = " / ")))
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

# =============================================================================
#  ANOVA à deux facteurs (avec ou sans interaction) — sommes des carrés ajustées (type III, comme Minitab)
# =============================================================================
cmd_anova2 <- function(d, p) {
  vn <- nom_colonne(d, p$valeur, "Colonne des mesures"); an <- nom_colonne(d, p$facteur_a, "Facteur A"); bn <- nom_colonne(d, p$facteur_b, "Facteur B")
  if (length(unique(c(vn, an, bn))) < 3) stop("La réponse et les deux facteurs doivent être trois colonnes différentes.", call. = FALSE)
  inter <- !identical(p$interaction, FALSE) && !identical(p$interaction, "false")
  y <- col_num(d, vn); ga <- ch_cat(d[[an]]); gb <- ch_cat(d[[bn]])
  ok <- is.finite(y) & !is.na(ga) & !is.na(gb)
  avert <- list()
  if (sum(!ok) > 0) avert <- c(avert, list(sprintf("%d ligne(s) incomplète(s) écartée(s).", sum(!ok))))
  A <- factor(ga[ok], levels = force_ordre_numerique(unique(ga[ok]))); B <- factor(gb[ok], levels = force_ordre_numerique(unique(gb[ok]))); y <- y[ok]
  if (nlevels(A) < 2 || nlevels(B) < 2) stop("Chaque facteur doit avoir au moins 2 niveaux.", call. = FALSE)
  cell <- table(A, B)
  if (inter && any(cell == 0)) { inter <- FALSE; avert <- c(avert, list("Certaines combinaisons des deux facteurs n'ont aucune mesure : l'interaction n'est pas estimable et a été retirée du modèle.")) }
  equilibre <- length(unique(as.vector(cell))) == 1
  dd <- data.frame(y = y, A = A, B = B)
  contrasts(dd$A) <- stats::contr.sum(nlevels(A)); contrasts(dd$B) <- stats::contr.sum(nlevels(B))
  form <- if (inter) y ~ A * B else y ~ A + B
  X <- stats::model.matrix(form, dd); asg <- attr(X, "assign"); etiq <- attr(stats::terms(form), "term.labels")
  rss <- function(M) { f <- stats::lm.fit(M, dd$y); sum(f$residuals^2) }
  fitc <- stats::lm.fit(X, dd$y); n <- length(y); rang <- fitc$rank
  dfe <- n - rang; SSe <- sum(fitc$residuals^2)
  if (dfe < 1) stop("Pas assez de mesures : il faut plusieurs mesures par combinaison des facteurs (ou retirer l'interaction) pour estimer l'erreur.", call. = FALSE)
  MSe <- SSe / dfe
  SS <- DF <- numeric(length(etiq))
  for (i in seq_along(etiq)) { Xr <- X[, asg != i, drop = FALSE]; SS[i] <- rss(Xr) - SSe; DF[i] <- sum(asg == i) }
  MS <- SS / DF; Fv <- MS / MSe; pv <- stats::pf(Fv, DF, dfe, lower.tail = FALSE)
  sst <- sum((y - mean(y))^2)
  nom_t <- c(an, bn, sprintf("%s × %s", an, bn))[seq_along(etiq)]
  tab <- data.frame(Source = c(nom_t, "Erreur", "Total"), ddl = c(DF, dfe, n - 1), Somme_carres = c(SS, SSe, sst), Carre_moyen = c(MS, MSe, NA),
                    F = c(Fv, NA, NA), p_value = c(pv, NA, NA), stringsAsFactors = FALSE)
  eta <- SS / (SS + SSe)
  eff <- data.frame(Source = nom_t, Eta2_partiel = eta, stringsAsFactors = FALSE)
  moy_a <- data.frame(Niveau = levels(A), n = as.integer(table(A)), Moyenne = as.numeric(tapply(y, A, mean)), Ecart_type = as.numeric(tapply(y, A, stats::sd)), stringsAsFactors = FALSE)
  moy_b <- data.frame(Niveau = levels(B), n = as.integer(table(B)), Moyenne = as.numeric(tapply(y, B, mean)), Ecart_type = as.numeric(tapply(y, B, stats::sd)), stringsAsFactors = FALSE)
  mc <- tapply(y, list(A, B), mean)
  tab_c <- data.frame(Niveau = levels(A), stringsAsFactors = FALSE); for (j in seq_len(nlevels(B))) tab_c[[levels(B)[j]]] <- as.numeric(mc[, j])
  names(tab_c)[1] <- an
  tables <- list(`ANOVA à deux facteurs` = tab, `Taille d'effet` = eff)
  tables[[sprintf("Moyennes — %s", an)]] <- moy_a; tables[[sprintf("Moyennes — %s", bn)]] <- moy_b
  if (nlevels(B) * nlevels(A) <= 60) tables$`Moyennes par combinaison` <- tab_c
  # contrôle des résidus
  rs <- fitc$residuals; ajus <- dd$y - rs
  sw <- if (length(rs) >= 3 && length(rs) <= 5000 && stats::sd(rs) > 0) tryCatch(stats::shapiro.test(rs)$p.value, error = function(e) NA_real_) else NA_real_
  cellf <- interaction(A, B, drop = TRUE)
  fl <- if (nlevels(cellf) >= 2 && all(table(cellf) >= 2)) tryCatch(stats::fligner.test(y, cellf)$p.value, error = function(e) NA_real_) else NA_real_
  lignes <- c(sprintf("Réponse : %s — facteurs : %s (%d niveaux) et %s (%d niveaux) — %d observations%s.", vn, an, nlevels(A), bn, nlevels(B), n, if (equilibre) sprintf(", plan équilibré (%d par combinaison)", cell[1, 1]) else ", plan déséquilibré"),
              sprintf("Sommes des carrés ajustées (type III)%s.", if (inter) " ; modèle avec interaction" else " ; modèle sans interaction"), regle(), "")
  for (i in seq_along(etiq)) {
    h0 <- if (etiq[i] == "A:B") sprintf("H0 : pas d'interaction entre %s et %s", an, bn) else sprintf("H0 : les moyennes ne diffèrent pas selon %s", nom_t[i])
    lignes <- c(lignes, sprintf("%s — F = %s ; p-value %s → %s", h0, fmt(Fv[i], 4), fmt_p(pv[i]),
                                if (pv[i] < ALPHA) "H0 rejetée" else "H0 non rejetée"))
  }
  lignes <- c(lignes, "", sprintf("Contrôle des résidus — Shapiro-Wilk : p = %s ; homogénéité des variances entre combinaisons (Fligner-Killeen) : p = %s.", fmt_p(sw), fmt_p(fl)),
              sprintf("Pour ces deux contrôles, H0 est l'hypothèse requise : elle est retenue si p-value ≥ α = %s.", alpha_txt()))
  if (inter && length(pv) == 3 && pv[3] < ALPHA) {
    lignes <- c(lignes, "L'interaction est significative : l'effet d'un facteur dépend du niveau de l'autre ; interpréter les effets principaux avec prudence (voir le graphique d'interaction).")
  }
  sig <- nom_t[pv < ALPHA]
  concl <- if (length(sig)) sprintf("Effet(s) significatif(s) au risque α = %s : %s.", alpha_txt(), paste(sig, collapse = " ; ")) else sprintf("Aucun effet significatif au risque α = %s.", alpha_txt())
  # comparaisons de Tukey pour les facteurs significatifs (plan équilibré uniquement)
  if (equilibre) {
    tk <- tryCatch(stats::TukeyHSD(stats::aov(form, data = dd), which = c("A", "B")[c(pv[1] < ALPHA, pv[2] < ALPHA)]), error = function(e) NULL)
    if (!is.null(tk)) for (k in names(tk)) { m <- tk[[k]]; tables[[sprintf("Tukey — %s", if (k == "A") an else bn)]] <- data.frame(Comparaison = rownames(m), Difference = m[, "diff"], Borne_inf = m[, "lwr"], Borne_sup = m[, "upr"], p_ajustee = m[, "p adj"], stringsAsFactors = FALSE) }
  } else avert <- c(avert, list("Plan déséquilibré : les comparaisons de Tukey ne sont pas fournies."))
  avert <- c(avert, list("Vérifier l'indépendance des observations, la normalité des résidus et l'homogénéité des variances. Il faut au moins 2 mesures par combinaison pour tester l'interaction."))
  if (!is.na(sw) && sw < ALPHA) avert <- c(avert, list("La normalité des résidus est rejetée : interpréter les p-values avec prudence."))
  # figures
  fig1 <- lire_png("anova2_effets.png", largeur = 1200, hauteur = 520)
  par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))
  yl <- range(y); ec <- diff(yl) * 0.06; yl <- yl + c(-ec, ec)
  pl <- function(g, nom) { m <- tapply(y, g, mean); plot(seq_along(m), m, type = "b", pch = 19, col = "#2a5d9f", xaxt = "n", xlab = nom, ylab = sprintf("Moyenne de %s", vn), main = sprintf("Effet principal — %s", nom), ylim = yl); axis(1, at = seq_along(m), labels = names(m)); abline(h = mean(y), lty = 3, col = "#888888") }
  pl(A, an); pl(B, bn)
  matplot(seq_len(nlevels(A)), mc, type = "b", pch = 19, lty = 1, col = PAL(nlevels(B)), xaxt = "n", xlab = an, ylab = sprintf("Moyenne de %s", vn), main = "Interaction", ylim = yl)
  axis(1, at = seq_len(nlevels(A)), labels = levels(A)); legend("topleft", legend = levels(B), col = PAL(nlevels(B)), lty = 1, pch = 19, bty = "n", cex = 0.8, title = bn)
  invisible(dev.off())
  fig2 <- lire_png("anova2_residus.png", largeur = 1000, hauteur = 480)
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  qq <- stats::qqnorm(rs, plot.it = FALSE); plot(qq$x, qq$y, pch = 19, cex = 0.8, col = "#2a5d9f", xlab = "Quantiles normaux", ylab = "Résidus", main = "Droite de Henry des résidus")
  if (length(rs) >= 3) { qqs <- stats::quantile(rs, c(0.25, 0.75)); sl <- diff(qqs) / diff(stats::qnorm(c(0.25, 0.75))); abline(qqs[1] - sl * stats::qnorm(0.25), sl, col = "#b03030", lwd = 2) }
  plot(ajus, rs, pch = 19, cex = 0.8, col = "#2a5d9f", xlab = "Valeurs ajustées", ylab = "Résidus", main = "Résidus selon les valeurs ajustées"); abline(h = 0, col = "#b03030", lwd = 2)
  invisible(dev.off())
  list(titre = "ANOVA à deux facteurs", lignes = c(lignes, "", concl), conclusion = concl, tables = tables, figures = c(fig1, fig2), avertissements = avert)
}

# =============================================================================
#  ANOVA à plusieurs facteurs (2 à 6), croisés : effets principaux et interactions jusqu'à l'ordre choisi.
#  Sommes des carrés ajustées (type III, comme Minitab) ; comparaisons de Tukey des effets principaux significatifs (plan équilibré).
# =============================================================================
cmd_anovan <- function(d, p) {
  vn <- nom_colonne(d, p$valeur, "Colonne des mesures")
  fn <- unique(as.character(unlist(p$facteurs)))
  if (length(fn) < 2) stop("Choisir au moins deux facteurs.", call. = FALSE)
  if (length(fn) > 6) stop("Six facteurs au plus.", call. = FALSE)
  for (f in fn) nom_colonne(d, f, "Facteur")
  if (vn %in% fn) stop("La réponse ne peut pas être aussi un facteur.", call. = FALSE)
  k <- length(fn)
  ordre <- tolower(as.character(if (is.null(p$interactions)) "2" else p$interactions))
  ordre <- if (ordre %in% c("aucune", "0", "1")) 1L else if (ordre %in% c("toutes", "all", "max")) k else min(2L, k)
  y <- col_num(d, vn); G <- lapply(fn, function(f) ch_cat(d[[f]]))
  ok <- is.finite(y); for (g in G) ok <- ok & !is.na(g)
  avert <- list()
  if (sum(!ok) > 0) avert <- c(avert, list(sprintf("%d ligne(s) incomplète(s) écartée(s).", sum(!ok))))
  y <- y[ok]; dd <- data.frame(y = y)
  noms_f <- paste0("F", seq_len(k)); names(noms_f) <- fn
  for (i in seq_len(k)) { g <- G[[i]][ok]; dd[[noms_f[i]]] <- factor(g, levels = force_ordre_numerique(unique(g))) }
  nl <- vapply(noms_f, function(f) nlevels(dd[[f]]), 1L)
  if (any(nl < 2)) stop("Chaque facteur doit avoir au moins 2 niveaux.", call. = FALSE)
  for (f in noms_f) contrasts(dd[[f]]) <- stats::contr.sum(nlevels(dd[[f]]))
  n <- length(y)
  construire <- function(o) {
    form <- stats::as.formula(if (o <= 1L) paste("y ~", paste(noms_f, collapse = " + ")) else paste("y ~ (", paste(noms_f, collapse = " + "), ")^", o))
    X <- stats::model.matrix(form, dd); list(form = form, X = X, fit = stats::lm.fit(X, dd$y))
  }
  m <- construire(ordre)
  while (m$fit$rank < ncol(m$X) && ordre > 1L) {
    ordre <- ordre - 1L; m <- construire(ordre)
    avert <- c(avert, list(sprintf("Certaines combinaisons de niveaux n'ont aucune mesure : les interactions d'ordre supérieur ne sont pas estimables ; le modèle est limité aux interactions d'ordre %d.", ordre)))
  }
  if (m$fit$rank < ncol(m$X)) stop("Le plan est trop incomplet pour estimer les effets principaux.", call. = FALSE)
  form <- m$form; X <- m$X; fitc <- m$fit
  asg <- attr(X, "assign"); etiq <- attr(stats::terms(form), "term.labels")
  dfe <- n - fitc$rank; SSe <- sum(fitc$residuals^2)
  if (dfe < 1) stop("Pas assez de mesures pour estimer l'erreur : il faut plusieurs mesures par combinaison, ou retirer des interactions (« aucune » ou ordre 2).", call. = FALSE)
  MSe <- SSe / dfe
  SS <- DF <- numeric(length(etiq))
  for (i in seq_along(etiq)) { Xr <- X[, asg != i, drop = FALSE]; SS[i] <- sum(stats::lm.fit(Xr, dd$y)$residuals^2) - SSe; DF[i] <- sum(asg == i) }
  MS <- SS / DF; Fv <- MS / MSe; pv <- stats::pf(Fv, DF, dfe, lower.tail = FALSE)
  sst <- sum((y - mean(y))^2)
  vers_nom <- function(e) paste(fn[match(strsplit(e, ":", fixed = TRUE)[[1]], noms_f)], collapse = " × ")
  nom_t <- vapply(etiq, vers_nom, ""); names(nom_t) <- NULL
  tab <- data.frame(Source = c(nom_t, "Erreur", "Total"), ddl = c(DF, dfe, n - 1), Somme_carres = c(SS, SSe, sst), Carre_moyen = c(MS, MSe, NA),
                    F = c(Fv, NA, NA), p_value = c(pv, NA, NA), stringsAsFactors = FALSE)
  eff <- data.frame(Source = nom_t, Eta2_partiel = SS / (SS + SSe), stringsAsFactors = FALSE)
  tables <- list(`ANOVA à plusieurs facteurs` = tab, `Taille d'effet` = eff)
  for (i in seq_len(k)) {
    g <- dd[[noms_f[i]]]
    tables[[sprintf("Moyennes — %s", fn[i])]] <- data.frame(Niveau = levels(g), n = as.integer(table(g)), Moyenne = as.numeric(tapply(y, g, mean)),
                                                            Ecart_type = as.numeric(tapply(y, g, stats::sd)), stringsAsFactors = FALSE)
  }
  cellf <- interaction(dd[noms_f], drop = TRUE)
  cnt <- table(cellf); equilibre <- length(unique(as.vector(cnt))) == 1 && nlevels(cellf) == prod(nl)
  rs <- fitc$residuals; ajus <- dd$y - rs
  sw <- if (length(rs) >= 3 && length(rs) <= 5000 && stats::sd(rs) > 0) tryCatch(stats::shapiro.test(rs)$p.value, error = function(e) NA_real_) else NA_real_
  fl <- if (nlevels(cellf) >= 2 && all(cnt >= 2)) tryCatch(stats::fligner.test(y, cellf)$p.value, error = function(e) NA_real_) else NA_real_
  lignes <- c(sprintf("Réponse : %s — %d facteurs : %s — %d observations%s.", vn, k, paste(sprintf("%s (%d niveaux)", fn, nl), collapse = ", "), n,
                      if (equilibre) sprintf(", plan équilibré (%d par combinaison)", cnt[[1]]) else ", plan déséquilibré ou incomplet"),
              sprintf("Sommes des carrés ajustées (type III) ; %s.", if (ordre == 1L) "modèle sans interaction" else if (ordre >= k) "modèle avec toutes les interactions" else sprintf("interactions jusqu'à l'ordre %d", ordre)),
              regle(), "")
  for (i in seq_along(etiq)) {
    ordre_t <- length(strsplit(etiq[i], ":", fixed = TRUE)[[1]])
    h0 <- if (ordre_t > 1) sprintf("H0 : pas d'interaction %s", nom_t[i]) else sprintf("H0 : les moyennes ne diffèrent pas selon %s", nom_t[i])
    lignes <- c(lignes, sprintf("%s — F = %s ; p-value %s → %s", h0, fmt(Fv[i], 4), fmt_p(pv[i]), if (pv[i] < ALPHA) "H0 rejetée" else "H0 non rejetée"))
  }
  lignes <- c(lignes, "", sprintf("Contrôle des résidus — Shapiro-Wilk : p = %s ; homogénéité des variances entre combinaisons (Fligner-Killeen) : p = %s.", fmt_p(sw), fmt_p(fl)),
              sprintf("Pour ces deux contrôles, H0 est l'hypothèse requise : elle est retenue si p-value ≥ α = %s.", alpha_txt()))
  inter_sig <- nom_t[grepl("×", nom_t, fixed = TRUE) & pv < ALPHA]
  if (length(inter_sig)) lignes <- c(lignes, sprintf("Interaction(s) significative(s) : %s ; l'effet d'un facteur dépend alors du niveau des autres, interpréter les effets principaux avec prudence (voir les graphiques d'interaction).", paste(inter_sig, collapse = " ; ")))
  sig <- nom_t[pv < ALPHA]
  concl <- if (length(sig)) sprintf("Effet(s) significatif(s) au risque α = %s : %s.", alpha_txt(), paste(sig, collapse=" ; ")) else sprintf("Aucun effet significatif au risque α = %s.", alpha_txt())
  prin <- which(!grepl(":", etiq, fixed = TRUE) & pv < ALPHA)
  if (equilibre && length(prin)) {
    tk <- tryCatch(stats::TukeyHSD(stats::aov(form, data = dd), which = etiq[prin]), error = function(e) NULL)
    if (!is.null(tk)) for (kk in names(tk)) { mm <- tk[[kk]]; tables[[sprintf("Tukey — %s", fn[match(kk, noms_f)])]] <- data.frame(Comparaison = rownames(mm), Difference = mm[, "diff"], Borne_inf = mm[, "lwr"], Borne_sup = mm[, "upr"], p_ajustee = mm[, "p adj"], stringsAsFactors = FALSE) }
  } else if (!equilibre) avert <- c(avert, list("Plan déséquilibré ou incomplet : les comparaisons de Tukey ne sont pas fournies."))
  avert <- c(avert, list("Vérifier l'indépendance des observations, la normalité des résidus et l'homogénéité des variances. Plus il y a de facteurs et d'interactions, plus il faut de mesures : il faut au moins 2 mesures par combinaison pour tester l'interaction d'ordre le plus élevé."))
  if (!is.na(sw) && sw < ALPHA) avert <- c(avert, list("La normalité des résidus est rejetée : interpréter les p-values avec prudence."))
  # figures : effets principaux
  nc <- min(k, 3); nr <- ceiling(k / nc)
  fig1 <- lire_png("anovan_effets.png", largeur = 380 * nc, hauteur = 380 * nr)
  par(mfrow = c(nr, nc), mar = c(4.5, 4.5, 3, 1))
  yl <- range(y); ec <- diff(yl) * 0.06; yl <- yl + c(-ec, ec)
  for (i in seq_len(k)) { g <- dd[[noms_f[i]]]; mm <- tapply(y, g, mean)
    plot(seq_along(mm), mm, type = "b", pch = 19, col = "#2a5d9f", xaxt = "n", xlab = fn[i], ylab = sprintf("Moyenne de %s", vn), main = sprintf("Effet principal — %s", fn[i]), ylim = yl)
    axis(1, at = seq_along(mm), labels = names(mm)); abline(h = mean(y), lty = 3, col = "#888888") }
  invisible(dev.off())
  figs <- c(fig1)
  # interactions d'ordre 2 (au plus 6 paires)
  if (ordre >= 2L) {
    paires <- utils::combn(k, 2, simplify = FALSE); paires <- paires[seq_len(min(length(paires), 6))]
    npc <- min(length(paires), 3); npr <- ceiling(length(paires) / npc)
    fig2 <- lire_png("anovan_interactions.png", largeur = 380 * npc, hauteur = 380 * npr)
    par(mfrow = c(npr, npc), mar = c(4.5, 4.5, 3, 1))
    for (pr in paires) { a <- dd[[noms_f[pr[1]]]]; b <- dd[[noms_f[pr[2]]]]; mc <- tapply(y, list(a, b), mean)
      matplot(seq_len(nlevels(a)), mc, type = "b", pch = 19, lty = 1, col = PAL(nlevels(b)), xaxt = "n", xlab = fn[pr[1]], ylab = sprintf("Moyenne de %s", vn), main = sprintf("%s × %s", fn[pr[1]], fn[pr[2]]), ylim = yl)
      axis(1, at = seq_len(nlevels(a)), labels = levels(a)); legend("topleft", legend = levels(b), col = PAL(nlevels(b)), lty = 1, pch = 19, bty = "n", cex = 0.75, title = fn[pr[2]]) }
    invisible(dev.off()); figs <- c(figs, fig2)
  }
  fig3 <- lire_png("anovan_residus.png", largeur = 1000, hauteur = 480)
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  qq <- stats::qqnorm(rs, plot.it = FALSE); plot(qq$x, qq$y, pch = 19, cex = 0.8, col = "#2a5d9f", xlab = "Quantiles normaux", ylab = "Résidus", main = "Droite de Henry des résidus")
  if (length(rs) >= 3) { qqs <- stats::quantile(rs, c(0.25, 0.75)); sl <- diff(qqs) / diff(stats::qnorm(c(0.25, 0.75))); abline(qqs[1] - sl * stats::qnorm(0.25), sl, col = "#b03030", lwd = 2) }
  plot(ajus, rs, pch = 19, cex = 0.8, col = "#2a5d9f", xlab = "Valeurs ajustées", ylab = "Résidus", main = "Résidus selon les valeurs ajustées"); abline(h = 0, col = "#b03030", lwd = 2)
  invisible(dev.off())
  list(titre = "ANOVA à plusieurs facteurs", lignes = c(lignes, "", concl), conclusion = concl, tables = tables, figures = c(figs, fig3), avertissements = avert)
}

# =============================================================================
#  R&R emboîtée (pièces emboîtées dans les opérateurs : essais destructifs) — méthode ANOVA, conventions de Minitab
#  Chaque pièce n'est mesurée que par un seul opérateur ; plan équilibré (même nombre de pièces par opérateur,
#  même nombre de répétitions par pièce).
# =============================================================================
cmd_rr_emboitee <- function(d, p) {
  pn <- nom_colonne(d, p$piece, "Pièces"); on <- nom_colonne(d, p$operateur, "Opérateurs"); mn <- nom_colonne(d, p$mesure, "Mesures")
  if (length(unique(c(pn, on, mn))) < 3) stop("Les colonnes des pièces, des opérateurs et des mesures doivent être différentes.", call. = FALSE)
  tol <- num(p$tolerance)
  L <- num(p$l); if (is.na(L)) L <- 6
  if (L <= 0) stop("Le multiplicateur L doit être strictement positif.", call. = FALSE)
  if (!is.na(tol) && tol <= 0) stop("La tolérance doit être strictement positive.", call. = FALSE)
  y <- col_num(d, mn); pc <- ch_cat(d[[pn]]); oc <- ch_cat(d[[on]])
  ok <- is.finite(y) & !is.na(pc) & !is.na(oc)
  avert <- list()
  if (sum(!ok) > 0) avert <- c(avert, list(sprintf("%d ligne(s) incomplète(s) écartée(s).", sum(!ok))))
  O <- factor(oc[ok], levels = unique(oc[ok]))
  cle <- paste(oc[ok], pc[ok], sep = "\r")                               # une pièce = un couple (opérateur, numéro) : l'emboîtement
  ordre_cle <- unique(cle[order(as.integer(O), match(pc[ok], force_ordre_numerique(unique(pc[ok]))))])
  PO <- factor(cle, levels = ordre_cle)
  dd <- data.frame(O = O, PO = PO, P = pc[ok], y = y[ok])
  b <- nlevels(dd$O)
  if (b < 2) stop("Il faut au moins 2 opérateurs.", call. = FALSE)
  pieces_par_op <- tapply(as.character(dd$PO), dd$O, function(v) length(unique(v)))
  if (length(unique(pieces_par_op)) != 1) stop(sprintf("Plan non équilibré : chaque opérateur doit mesurer le même nombre de pièces (de %d à %d pièces selon l'opérateur).", min(pieces_par_op), max(pieces_par_op)), call. = FALSE)
  a <- unname(pieces_par_op[1])
  cnt <- table(dd$PO)
  if (length(unique(as.vector(cnt))) != 1) stop(sprintf("Plan non équilibré : chaque pièce doit être mesurée le même nombre de fois (de %d à %d mesures).", min(cnt), max(cnt)), call. = FALSE)
  n <- unname(cnt[1])
  if (a < 2) stop("Il faut au moins 2 pièces par opérateur.", call. = FALSE)
  if (n < 2) stop("Il faut au moins 2 mesures par pièce (répétitions).", call. = FALSE)
  commun <- Reduce(intersect, split(as.character(dd$P), dd$O))
  if (length(commun) == length(unique(dd$P)) && length(commun) > 0)
    avert <- c(avert, list("Les mêmes numéros de pièce apparaissent chez tous les opérateurs : ils sont traités comme des pièces différentes (plan emboîté). Si chaque opérateur mesure les mêmes pièces, utiliser la R&R croisée."))
  if (a * b < 10) avert <- c(avert, list(sprintf("Seulement %d pièces au total : on recommande au moins 10 pièces représentatives de l'étendue du procédé.", a * b)))
  if (b < 3) avert <- c(avert, list(sprintf("Seulement %d opérateurs : on en recommande au moins 3.", b)))

  fit <- stats::aov(y ~ O + PO, data = dd)
  sm <- summary(fit)[[1]]; rn <- trimws(rownames(sm))
  SS <- setNames(sm[["Sum Sq"]], rn); DF <- setNames(sm[["Df"]], rn)
  SSo <- SS[["O"]]; SSp <- SS[["PO"]]; SSe <- SS[["Residuals"]]
  dfo <- DF[["O"]]; dfp <- DF[["PO"]]; dfe <- DF[["Residuals"]]
  MSo <- SSo / dfo; MSp <- SSp / dfp; MSe <- SSe / dfe
  div <- function(u, v) if (v == 0) Inf else u / v
  Fo <- div(MSo, MSp); Fp <- div(MSp, MSe)
  po <- stats::pf(Fo, dfo, dfp, lower.tail = FALSE); pp <- stats::pf(Fp, dfp, dfe, lower.tail = FALSE)
  sstot <- SSo + SSp + SSe; dftot <- dfo + dfp + dfe
  tab1 <- data.frame(Source = c("Opérateur", "Pièce (opérateur)", "Répétabilité (erreur)", "Total"), ddl = c(dfo, dfp, dfe, dftot),
                     Somme_carres = c(SSo, SSp, SSe, sstot), Carre_moyen = c(MSo, MSp, MSe, NA), F = c(Fo, Fp, NA, NA), p_value = c(po, pp, NA, NA), stringsAsFactors = FALSE)
  brut <- c((MSo - MSp) / (a * n), (MSp - MSe) / n)
  s2o <- max(0, brut[1]); s2p <- max(0, brut[2]); s2rep <- MSe
  s2rpd <- s2o; s2grr <- s2rep + s2rpd; s2tot <- s2grr + s2p
  if (s2tot <= 0) stop("Variation totale nulle : les mesures sont identiques.", call. = FALSE)
  src <- c("R&R totale", "  Répétabilité", "  Reproductibilité", "Pièce à pièce", "Variation totale")
  vc <- c(s2grr, s2rep, s2rpd, s2p, s2tot); sd <- sqrt(vc)
  pct_c <- 100 * vc / s2tot; pct_sv <- 100 * sd / sd[length(sd)]
  tab_vc <- data.frame(Source = src, Composante_variance = vc, Contribution_pct = pct_c, stringsAsFactors = FALSE)
  tab_sv <- data.frame(Source = src, Ecart_type = sd, Variation_etude = L * sd, Pct_variation_etude = pct_sv, stringsAsFactors = FALSE)
  if (!is.na(tol)) tab_sv$Pct_tolerance <- 100 * L * sd / tol
  tables <- list(`ANOVA emboîtée` = tab1, `Composantes de la variance` = tab_vc, `Variation d'étude` = tab_sv)
  ndc <- floor(1.41 * sqrt(s2p) / sqrt(s2grr)); if (!is.finite(ndc)) ndc <- NA_integer_
  sv_grr <- pct_sv[1]; c_grr <- pct_c[1]; tol_grr <- if (!is.na(tol)) 100 * L * sd[1] / tol else NA_real_
  jugement <- function(v) if (is.na(v)) NA_character_ else if (v < 10) "acceptable" else if (v <= 30) "acceptable sous conditions" else "inacceptable"
  concl <- sprintf("R&R totale : %s %% de la variation d'étude (%s), %s %% de la variance%s ; %s catégorie(s) distincte(s)%s.",
                   fmt(sv_grr, 4), jugement(sv_grr), fmt(c_grr, 4),
                   if (!is.na(tol)) sprintf(", %s %% de la tolérance (%s)", fmt(tol_grr, 4), jugement(tol_grr)) else "",
                   if (is.na(ndc)) "?" else as.character(ndc), if (!is.na(ndc) && ndc < 5) " (il en faut au moins 5)" else "")
  avert <- c(avert, list(sprintf("Conventions de Minitab : R&R emboîtée, méthode ANOVA, composantes aléatoires, variation d'étude sur L = %s écarts-types. Elle convient quand une pièce ne peut être mesurée que par un seul opérateur (essai destructif, par exemple) ; la reproductibilité mesure alors l'écart entre opérateurs, mêlé à l'écart entre les lots de pièces.", fmt(L, 3)),
                         "Repères : moins de 10 % acceptable, de 10 à 30 % acceptable sous conditions, plus de 30 % inacceptable ; au moins 5 catégories distinctes."))
  if (any(brut < 0)) avert <- c(avert, list("Une composante de variance estimée négative a été ramenée à 0 (convention de Minitab)."))

  pcol <- PAL(b); lo <- levels(dd$O); np <- nlevels(dd$PO)
  xp <- as.integer(dd$PO); op <- as.integer(dd$O)
  lab_p <- vapply(levels(dd$PO), function(s) sub("^.*\r", "", s), "")
  fig1 <- lire_png("rr_emboitee_composantes.png", largeur = 1100, hauteur = 860)
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))
  idx <- c(1, 2, 3, 4)
  met <- rbind(`% Contribution` = pct_c[idx], `% Variation d'étude` = pct_sv[idx])
  if (!is.na(tol)) met <- rbind(met, `% Tolérance` = 100 * L * sd[idx] / tol)
  barplot(met, beside = TRUE, col = c("#2a5d9f", "#e0a030", "#2e8b57")[seq_len(nrow(met))], names.arg = c("R&R totale", "Répétabilité", "Reproductib.", "Pièce à pièce"),
          main = "Composantes de la variation", ylab = "%", ylim = c(0, max(110, max(met, na.rm = TRUE) * 1.05)), border = "white", cex.names = 0.85)
  legend("topleft", legend = rownames(met), fill = c("#2a5d9f", "#e0a030", "#2e8b57")[seq_len(nrow(met))], bty = "n", cex = 0.8)
  plot(xp, dd$y, xaxt = "n", pch = 19, col = pcol[op], xlab = "Pièce (emboîtée dans l'opérateur)", ylab = "Mesure", main = "Mesures par pièce", cex = 0.9)
  axis(1, at = seq_len(np), labels = lab_p, las = if (np > 12) 2 else 1, cex.axis = 0.8)
  mpo <- as.numeric(tapply(dd$y, dd$PO, mean)); points(seq_len(np), mpo, pch = 3, col = "#444444", cex = 1.2)
  for (j in seq_len(b - 1)) abline(v = j * a + 0.5, lty = 3, col = "#888888")
  legend("topleft", legend = lo, col = pcol, pch = 19, bty = "n", cex = 0.8, title = "Opérateur")
  boxplot(y ~ O, data = dd, col = "#cfe0f5", xlab = "Opérateur", ylab = "Mesure", main = "Mesures par opérateur")
  rg <- as.numeric(tapply(dd$y, dd$PO, function(v) diff(range(v))))
  plot(seq_len(np), rg, type = "b", pch = 19, col = "#2a5d9f", xaxt = "n", xlab = "Pièce (emboîtée dans l'opérateur)", ylab = "Étendue des répétitions", main = "Répétabilité par pièce")
  axis(1, at = seq_len(np), labels = lab_p, las = if (np > 12) 2 else 1, cex.axis = 0.8)
  for (j in seq_len(b - 1)) abline(v = j * a + 0.5, lty = 3, col = "#888888")
  abline(h = mean(rg), col = "#2e8b57", lwd = 2)
  invisible(dev.off())

  lignes <- c(sprintf("Plan : %d opérateurs × %d pièces par opérateur (soit %d pièces) × %d répétitions (%d mesures) — pièces emboîtées dans les opérateurs", b, a, a * b, n, nrow(dd)),
              sprintf("Effet opérateur : p-value %s — effet pièce (dans l'opérateur) : p-value %s", fmt_p(po), fmt_p(pp)),
              "",
              sprintf("R&R totale : %s %% de la variance, %s %% de la variation d'étude%s", fmt(c_grr, 4), fmt(sv_grr, 4),
                      if (!is.na(tol)) sprintf(", %s %% de la tolérance", fmt(tol_grr, 4)) else ""),
              sprintf("Répétabilité : %s %% de la variation d'étude — reproductibilité : %s %%", fmt(pct_sv[2], 4), fmt(pct_sv[3], 4)),
              sprintf("Nombre de catégories distinctes : %s", if (is.na(ndc)) "non calculable" else as.character(ndc)),
              "", concl)
  list(titre = "R&R emboîtée (méthode ANOVA)", lignes = lignes, conclusion = concl, tables = tables, figures = c(fig1), avertissements = avert)
}

# =============================================================================
#  Génération de données aléatoires (lois diverses) — valeurs renvoyées dans « valeurs »
# =============================================================================
cmd_generer <- function(d, p) {
  n <- suppressWarnings(as.integer(num(p$n)))
  if (is.na(n) || n < 1 || n > 100000) stop("Le nombre de valeurs doit être compris entre 1 et 100000.", call. = FALSE)
  g <- num(p$graine)
  if (!is.na(g)) set.seed(as.integer(g))
  q <- p$parametres
  par <- function(k, nom, minimum = -Inf, strict = FALSE) {
    v <- num(q[[k]])
    if (is.na(v)) stop(sprintf("Saisir %s.", nom), call. = FALSE)
    if (if (strict) v <= minimum else v < minimum) stop(sprintf("%s doit être %s %s.", nom, if (strict) "strictement supérieure à" else "supérieure ou égale à", format(minimum)), call. = FALSE)
    v
  }
  entier <- function(k, nom, minimum) {
    v <- par(k, nom, minimum)
    if (v != round(v)) stop(sprintf("%s doit être un nombre entier.", nom), call. = FALSE)
    as.integer(v)
  }
  proba <- function(k, nom) { v <- par(k, nom); if (v <= 0 || v > 1) stop(sprintf("%s doit être comprise entre 0 (exclu) et 1.", nom), call. = FALSE); v }
  loi <- p$loi
  discrete <- loi %in% c("poisson", "binomiale", "geometrique", "hypergeometrique")
  x <- switch(loi,
    normale = stats::rnorm(n, par("moyenne", "La moyenne"), par("ecart_type", "L'écart-type", 0, TRUE)),
    uniforme =, aleatoire = { a <- par("min", "Le minimum"); b <- par("max", "Le maximum"); if (a >= b) stop("Le minimum doit être inférieur au maximum.", call. = FALSE); stats::runif(n, a, b) },
    poisson = stats::rpois(n, par("lambda", "La moyenne λ", 0, TRUE)),
    student = stats::rt(n, par("df", "Le nombre de degrés de liberté", 0, TRUE)),
    fisher = stats::rf(n, par("df1", "Le ddl du numérateur", 0, TRUE), par("df2", "Le ddl du dénominateur", 0, TRUE)),
    khi2 = stats::rchisq(n, par("df", "Le nombre de degrés de liberté", 0, TRUE)),
    binomiale = stats::rbinom(n, entier("essais", "Le nombre d'essais", 1), proba("p", "La probabilité de succès")),
    geometrique = stats::rgeom(n, proba("p", "La probabilité de succès")) + 1,      # nombre d'essais jusqu'au premier succès (au moins 1)
    hypergeometrique = {
      N <- entier("N", "La taille de la population", 1); K <- entier("K", "Le nombre de succès dans la population", 0); m <- entier("m", "La taille du tirage", 1)
      if (K > N) stop("Le nombre de succès ne peut pas dépasser la taille de la population.", call. = FALSE)
      if (m > N) stop("Le tirage ne peut pas dépasser la taille de la population.", call. = FALSE)
      stats::rhyper(n, K, N - K, m)
    },
    exponentielle = stats::rexp(n, 1 / par("moyenne", "La moyenne", 0, TRUE)),
    gamma = stats::rgamma(n, shape = par("forme", "La forme", 0, TRUE), scale = par("echelle", "L'échelle", 0, TRUE)),
    logistique = stats::rlogis(n, par("position", "La position"), par("echelle", "L'échelle", 0, TRUE)),
    weibull = stats::rweibull(n, shape = par("forme", "La forme", 0, TRUE), scale = par("echelle", "L'échelle", 0, TRUE)),
    lognormale = stats::rlnorm(n, par("mu", "La moyenne du logarithme"), par("sigma", "L'écart-type du logarithme", 0, TRUE)),
    beta = stats::rbeta(n, par("a", "Le paramètre a", 0, TRUE), par("b", "Le paramètre b", 0, TRUE)),
    stop("Loi inconnue : ", loi, call. = FALSE))
  dec <- suppressWarnings(as.integer(num(p$decimales))); if (is.na(dec)) dec <- 3L
  if (!discrete) x <- round(x, max(0L, min(10L, dec)))
  list(titre = paste("Valeurs aléatoires —", loi), lignes = c(sprintf("%d valeur(s) générée(s) ; moyenne %s, écart-type %s.", n, fmt(mean(x)), fmt(if (n > 1) stats::sd(x) else NA_real_))),
       valeurs = as.numeric(x), tables = list(), figures = character(0), avertissements = list())
}

# =============================================================================
#  Lot 2 — Test Z, tests de Poisson, estimation, identification de loi
# =============================================================================
hyp_textes <- function(alt, sujet, ref, f = FALSE) {
  e <- if (f) "e" else ""
  switch(alt,
         greater = list(h0 = sprintf("H0 : %s est inférieur%s ou égal%s à %s.", sujet, e, e, ref), h1 = sprintf("H1 : %s est supérieur%s à %s.", sujet, e, ref)),
         less = list(h0 = sprintf("H0 : %s est supérieur%s ou égal%s à %s.", sujet, e, e, ref), h1 = sprintf("H1 : %s est inférieur%s à %s.", sujet, e, ref)),
         list(h0 = sprintf("H0 : %s est égal%s à %s.", sujet, e, ref), h1 = sprintf("H1 : %s diffère de %s.", sujet, ref)))
}
val_pos <- function(v, nom, strict = TRUE) {
  if (is.na(v) || (if (strict) v <= 0 else v < 0)) stop(sprintf("%s doit être %s.", nom, if (strict) "strictement positif" else "positif ou nul"), call. = FALSE)
  v
}
alt_lire <- function(a) if (isTRUE(a %in% c("greater", "less"))) a else "two.sided"

# Courbe de la loi de la statistique sous H0 : zone de rejet, statistique observée.
dessin_stat <- function(titre, stat, alt, ddl = NA) {
  est_z <- is.na(ddl)
  qf <- function(q) if (est_z) stats::qnorm(q) else stats::qt(q, ddl)
  df <- function(x) if (est_z) stats::dnorm(x) else stats::dt(x, ddl)
  lim <- max(4, min(abs(stat), 12) * 1.2)
  x <- seq(-lim, lim, length.out = 500); y <- df(x)
  par(mar = c(4.5, 4.5, 3, 1))
  plot(x, y, type = "l", lwd = 2, xlab = "Statistique", ylab = "Densité", main = titre, ylim = c(0, max(y) * 1.15))
  zone <- function(sel) { if (any(sel)) polygon(c(x[sel][1], x[sel], tail(x[sel], 1)), c(0, y[sel], 0), col = "#e8b4b4", border = NA) }
  if (alt == "two.sided") { zone(x <= qf(ALPHA / 2)); zone(x >= qf(1 - ALPHA / 2)) }
  else if (alt == "greater") zone(x >= qf(1 - ALPHA)) else zone(x <= qf(ALPHA))
  lines(x, y, lwd = 2)
  abline(v = stat, col = "#b03030", lwd = 2)
  legend("topright", bty = "n", legend = c(sprintf("zone de rejet (α = %s)", alpha_txt()), sprintf("statistique observée = %s", fmt(stat, 4))),
         fill = c("#e8b4b4", NA), border = NA, lty = c(NA, 1), col = c(NA, "#b03030"), lwd = c(NA, 2), merge = TRUE, cex = 0.85)
}

cmd_z <- function(d, p) {
  alt <- alt_lire(p$alternative)
  deux <- identical(as.character(p$mode), "2")
  mu0 <- num(p$mu); if (is.na(mu0)) mu0 <- 0
  xn <- nom_colonne(d, p$x, "Série X"); x <- sans_na(col_num(d, xn)); n1 <- length(x)
  s1 <- val_pos(num(p$sigma), "L'écart-type connu (σ)")
  if (n1 < 1) stop("La série X ne contient aucune valeur.", call. = FALSE)
  if (!deux) {
    m <- mean(x); se <- s1 / sqrt(n1); est <- m - mu0
    sujet <- sprintf("la moyenne de « %s »", xn); ref <- fmt(mu0)
    tab <- data.frame(Serie = xn, n = n1, Moyenne = m, Sigma_connu = s1, Erreur_standard = se, stringsAsFactors = FALSE)
    centre <- m
  } else {
    yn <- nom_colonne(d, p$y, "Série Y"); y <- sans_na(col_num(d, yn)); n2 <- length(y)
    s2 <- val_pos(num(p$sigma2), "L'écart-type connu de la série Y (σ₂)")
    if (n2 < 1) stop("La série Y ne contient aucune valeur.", call. = FALSE)
    se <- sqrt(s1^2 / n1 + s2^2 / n2); centre <- mean(x) - mean(y); est <- centre - mu0
    sujet <- sprintf("la différence des moyennes (« %s » − « %s »)", xn, yn); ref <- fmt(mu0)
    tab <- data.frame(Serie = c(xn, yn), n = c(n1, n2), Moyenne = c(mean(x), mean(y)), Sigma_connu = c(s1, s2),
                      Erreur_standard = c(s1 / sqrt(n1), s2 / sqrt(n2)), stringsAsFactors = FALSE)
  }
  z <- est / se
  pv <- switch(alt, two.sided = 2 * stats::pnorm(-abs(z)), greater = stats::pnorm(z, lower.tail = FALSE), less = stats::pnorm(z))
  zc <- if (alt == "two.sided") stats::qnorm(1 - ALPHA / 2) else stats::qnorm(1 - ALPHA)
  ic <- switch(alt, two.sided = c(centre - zc * se, centre + zc * se), greater = c(centre - zc * se, Inf), less = c(-Inf, centre + zc * se))
  h <- hyp_textes(alt, sujet, ref, f = TRUE)
  fig <- lire_png("test_z.png", largeur = 760, hauteur = 520)
  dessin_stat(if (deux) "Test Z à 2 échantillons — loi N(0 ; 1) sous H0" else "Test Z à 1 échantillon — loi N(0 ; 1) sous H0", z, alt)
  invisible(dev.off())
  dec <- decision(pv, "aucune différence n'est mise en évidence.", "la différence est statistiquement significative.")
  lib_ic <- switch(alt, two.sided = sprintf("Intervalle de confiance à %s", conf_txt()), greater = sprintf("Borne inférieure à %s", conf_txt()), less = sprintf("Borne supérieure à %s", conf_txt()))
  val_ic <- switch(alt, two.sided = sprintf("[%s ; %s]", fmt(ic[1]), fmt(ic[2])), greater = sprintf("%s", fmt(ic[1])), less = sprintf("%s", fmt(ic[2])))
  list(titre = if (deux) "Test Z — deux échantillons (σ connus)" else "Test Z — un échantillon (σ connu)",
       lignes = c(h$h0, h$h1, regle(), sprintf("Écart(s)-type(s) connu(s) (σ) : %s", paste(fmt(if (deux) c(s1, s2) else s1), collapse = " ; ")),
                  sprintf("%s : %s", if (deux) "Différence estimée" else "Moyenne estimée", fmt(centre)),
                  sprintf("Statistique Z : %s", fmt(z)), sprintf("p-value : %s", fmt_p(pv)), sprintf("%s : %s", lib_ic, val_ic), dec),
       conclusion = sub("^Décision : ", "", sub("\\.$", "", dec)),
       tables = list(Series = tab), figures = c(fig),
       avertissements = list("Le test Z suppose l'écart-type de la population connu ; à défaut, utiliser le test t. Il suppose aussi des observations indépendantes et une loi normale (ou un grand échantillon)."))
}

# ---- Poisson ---------------------------------------------------------------
ic_poisson <- function(k, T, niv) { r <- stats::poisson.test(k, T, conf.level = niv)$conf.int; as.numeric(r) }
lire_poisson <- function(d, p, suf = "") {
  src <- p[[paste0("source", suf)]] %||% p$source %||% "col"
  if (identical(src, "resume")) {
    k <- num(p[[paste0("total", suf)]]); T <- num(p[[paste0("duree_tot", suf)]])
    if (is.na(k) || k < 0 || abs(k - round(k)) > 1e-8) stop("Le nombre total d'événements doit être un entier positif ou nul.", call. = FALSE)
    val_pos(T, "La taille totale de l'échantillon (durée, surface…)")
    return(list(k = round(k), T = T, nom = if (nzchar(suf)) "échantillon 2" else "échantillon"))
  }
  nm <- nom_colonne(d, p[[paste0("x", suf)]], "Colonne de comptages"); v <- sans_na(col_num(d, nm))
  if (!length(v)) stop(sprintf("La colonne « %s » ne contient aucune valeur.", nm), call. = FALSE)
  if (any(v < 0) || any(abs(v - round(v)) > 1e-8)) stop(sprintf("La colonne « %s » doit contenir des nombres d'événements entiers, positifs ou nuls.", nm), call. = FALSE)
  L <- num(p[[paste0("duree", suf)]]); if (is.na(L)) L <- 1
  val_pos(L, "La taille de chaque observation (durée, surface…)")
  list(k = sum(v), T = length(v) * L, nom = nm)
}
cmd_poisson <- function(d, p) {
  alt <- alt_lire(p$alternative); deux <- identical(as.character(p$mode), "2")
  a <- lire_poisson(d, p)
  la <- a$k / a$T
  if (!deux) {
    l0 <- val_pos(num(p$lambda0), "Le taux de référence")
    r <- stats::poisson.test(a$k, a$T, r = l0, alternative = alt, conf.level = 1 - ALPHA)
    h <- hyp_textes(alt, sprintf("le taux d'événements de « %s »", a$nom), fmt(l0))
    ic <- as.numeric(r$conf.int); pv <- r$p.value
    lib <- sprintf("Intervalle de confiance exact à %s du taux", conf_txt())
    lignes <- c(h$h0, h$h1, regle(), sprintf("Nombre total d'événements : %s ; taille totale observée : %s", fmt(a$k), fmt(a$T)),
                sprintf("Taux estimé : %s", fmt(la)), sprintf("%s : [%s ; %s]", lib, fmt(ic[1]), fmt(ic[2])), sprintf("p-value (test exact de Poisson) : %s", fmt_p(pv)))
    tab <- data.frame(Echantillon = a$nom, Evenements = a$k, Taille = a$T, Taux = la, Borne_inf = ic[1], Borne_sup = ic[2], stringsAsFactors = FALSE)
    fig <- lire_png("test_poisson.png", largeur = 700, hauteur = 520)
    par(mar = c(4, 5, 3, 1)); ymax <- max(c(la, l0, ic[is.finite(ic)])) * 1.2
    b <- barplot(la, ylim = c(0, ymax), col = "#cfe0f5", names.arg = a$nom, ylab = "Taux d'événements", main = "Test de Poisson — 1 échantillon")
    segments(b, max(ic[1], 0), b, min(ic[2], ymax), lwd = 2); abline(h = l0, lty = 2, col = "#b03030")
    legend("topright", bty = "n", lty = c(2, 1), lwd = 2, col = c("#b03030", "black"), legend = c(sprintf("taux de référence = %s", fmt(l0)), "intervalle de confiance"), cex = 0.85)
    invisible(dev.off())
  } else {
    b2 <- lire_poisson(d, p, "2"); lb <- b2$k / b2$T
    if (identical(a$nom, b2$nom)) { a$nom <- paste0(a$nom, " (1)"); b2$nom <- paste0(b2$nom, " (2)") }
    ratio0 <- num(p$ratio0); if (is.na(ratio0)) ratio0 <- 1; val_pos(ratio0, "Le rapport de référence")
    r <- stats::poisson.test(c(a$k, b2$k), c(a$T, b2$T), r = ratio0, alternative = alt, conf.level = 1 - ALPHA)
    h <- hyp_textes(alt, sprintf("le rapport des taux (« %s » / « %s »)", a$nom, b2$nom), fmt(ratio0))
    ic <- as.numeric(r$conf.int); pv <- r$p.value
    dif <- la - lb; sed <- sqrt(a$k / a$T^2 + b2$k / b2$T^2); zc <- stats::qnorm(1 - ALPHA / 2)
    lignes <- c(h$h0, h$h1, regle(),
                sprintf("%s : %s événement(s) sur une taille de %s → taux %s", a$nom, fmt(a$k), fmt(a$T), fmt(la)),
                sprintf("%s : %s événement(s) sur une taille de %s → taux %s", b2$nom, fmt(b2$k), fmt(b2$T), fmt(lb)),
                sprintf("Différence des taux : %s (intervalle approché à %s : [%s ; %s])", fmt(dif), conf_txt(), fmt(dif - zc * sed), fmt(dif + zc * sed)),
                sprintf("Rapport des taux estimé : %s ; intervalle exact à %s : [%s ; %s]", fmt(unname(r$estimate)), conf_txt(), fmt(ic[1]), fmt(ic[2])),
                sprintf("p-value (test exact conditionnel de Poisson) : %s", fmt_p(pv)))
    ci1 <- ic_poisson(a$k, a$T, 1 - ALPHA); ci2 <- ic_poisson(b2$k, b2$T, 1 - ALPHA)
    tab <- data.frame(Echantillon = c(a$nom, b2$nom), Evenements = c(a$k, b2$k), Taille = c(a$T, b2$T), Taux = c(la, lb),
                      Borne_inf = c(ci1[1], ci2[1]), Borne_sup = c(ci1[2], ci2[2]), stringsAsFactors = FALSE)
    fig <- lire_png("test_poisson.png", largeur = 700, hauteur = 520)
    par(mar = c(4, 5, 3, 1)); ymax <- max(c(la, lb, ci1, ci2)) * 1.15
    bb <- barplot(c(la, lb), ylim = c(0, ymax), col = c("#cfe0f5", "#f5dcc0"), names.arg = c(a$nom, b2$nom), ylab = "Taux d'événements", main = "Test de Poisson — 2 échantillons")
    segments(bb, c(ci1[1], ci2[1]), bb, c(ci1[2], ci2[2]), lwd = 2)
    invisible(dev.off())
  }
  dec <- decision(pv, "aucune différence n'est mise en évidence.", "la différence est statistiquement significative.")
  list(titre = if (deux) "Test de Poisson — deux échantillons" else "Test de Poisson — un échantillon",
       lignes = c(lignes, dec), conclusion = sub("^Décision : ", "", sub("\\.$", "", dec)), tables = list(Taux = tab), figures = c(fig),
       avertissements = list("Le taux est le nombre d'événements par unité de taille (durée, surface, lot…) ; les échantillons doivent être exprimés dans la même unité. Hypothèse : événements indépendants et taux constant (loi de Poisson)."))
}

# ---- Estimation d'une moyenne (loi normale) ------------------------------------
cmd_estimation <- function(d, p) {
  niv <- num(p$niveau); if (is.na(niv)) niv <- 100 * (1 - ALPHA)
  if (niv > 1) niv <- niv / 100
  if (niv <= 0 || niv >= 1) stop("Le niveau de confiance doit être compris entre 0 et 100 %.", call. = FALSE)
  cote <- if (isTRUE(p$cote %in% c("lower", "upper"))) p$cote else "two.sided"
  crit_q <- if (cote == "two.sided") 1 - (1 - niv) / 2 else niv
  sig <- num(p$sigma); if (!is.na(sig)) val_pos(sig, "L'écart-type connu (σ)")
  E <- num(p$marge); if (!is.na(E)) val_pos(E, "La marge d'erreur souhaitée")
  if (identical(p$source, "resume")) {
    n <- num(p$n); m <- num(p$moyenne); s <- num(p$ecart_type)
    if (is.na(n) || n < 1 || abs(n - round(n)) > 1e-8) stop("La taille de l'échantillon doit être un entier positif.", call. = FALSE)
    if (is.na(m)) stop("Saisir la moyenne de l'échantillon.", call. = FALSE)
    if (is.na(sig)) { val_pos(s, "L'écart-type de l'échantillon") }
    nom <- "échantillon"
  } else {
    nom <- nom_colonne(d, p$x, "Série X"); x <- sans_na(col_num(d, nom))
    n <- length(x); if (n < 2 && is.na(sig)) stop("Il faut au moins 2 valeurs pour estimer l'écart-type.", call. = FALSE)
    m <- mean(x); s <- if (n >= 2) stats::sd(x) else NA_real_
  }
  sigma_connu <- !is.na(sig)
  sg <- if (sigma_connu) sig else s
  se <- sg / sqrt(n)
  q <- if (sigma_connu) stats::qnorm(crit_q) else stats::qt(crit_q, n - 1)
  marge <- q * se
  ic <- switch(cote, two.sided = c(m - marge, m + marge), lower = c(m - marge, Inf), upper = c(-Inf, m + marge))
  nec <- NA
  if (!is.na(E)) {
    nec <- ceiling((stats::qnorm(crit_q) * sg / E)^2)
    if (!sigma_connu) for (i in 1:100) { nn <- max(2, ceiling((stats::qt(crit_q, max(nec - 1, 1)) * sg / E)^2)); if (nn == nec) break; nec <- nn }
    nec <- max(2, nec)
  }
  lib_c <- switch(cote, two.sided = "bilatéral", lower = "unilatéral (borne inférieure)", upper = "unilatéral (borne supérieure)")
  lignes <- c(sprintf("Estimation de la moyenne de « %s » — loi normale, intervalle %s", nom, lib_c),
              sprintf("Niveau de confiance : %s", pct_txt(niv)),
              sprintf("Taille de l'échantillon : %s ; moyenne : %s ; écart-type %s : %s", fmt(n), fmt(m), if (sigma_connu) "connu (σ)" else "de l'échantillon (s)", fmt(sg)),
              sprintf("Erreur standard : %s ; loi utilisée : %s", fmt(se), if (sigma_connu) "normale (σ connu)" else sprintf("t de Student à %s ddl (σ inconnu)", fmt(n - 1))),
              sprintf("Marge d'erreur obtenue : %s", fmt(marge)))
  lignes <- c(lignes, switch(cote,
              two.sided = sprintf("Intervalle de confiance à %s : [%s ; %s]", pct_txt(niv), fmt(ic[1]), fmt(ic[2])),
              lower = sprintf("Borne inférieure de confiance à %s : %s (la moyenne est supérieure à cette valeur)", pct_txt(niv), fmt(ic[1])),
              upper = sprintf("Borne supérieure de confiance à %s : %s (la moyenne est inférieure à cette valeur)", pct_txt(niv), fmt(ic[2]))))
  if (!is.na(E)) lignes <- c(lignes, sprintf("Pour une marge d'erreur de %s : taille d'échantillon nécessaire = %s (avec l'écart-type %s = %s)", fmt(E), fmt(nec), if (sigma_connu) "connu" else "de l'échantillon", fmt(sg)))
  fig <- lire_png("estimation.png", largeur = 760, hauteur = 480)
  par(mar = c(4.5, 4.5, 3, 1))
  lim <- if (is.finite(marge)) max(4 * se, 1.6 * marge) else 4 * se
  xs <- seq(m - lim, m + lim, length.out = 500); ys <- stats::dnorm(xs, m, se)
  plot(xs, ys, type = "l", lwd = 2, xlab = "Moyenne", ylab = "Densité (loi de la moyenne estimée)", main = "Estimation de la moyenne")
  lo <- max(ic[1], min(xs)); hi <- min(ic[2], max(xs)); sel <- xs >= lo & xs <= hi
  polygon(c(lo, xs[sel], hi), c(0, ys[sel], 0), col = "#cfe0f5", border = NA); lines(xs, ys, lwd = 2)
  abline(v = m, col = "#b03030", lwd = 2); abline(v = ic[is.finite(ic)], lty = 2, col = "#305090", lwd = 2)
  legend("topright", bty = "n", cex = 0.85, lty = c(1, 2), lwd = 2, col = c("#b03030", "#305090"), legend = c(sprintf("moyenne = %s", fmt(m)), sprintf("limite(s) à %s", pct_txt(niv))))
  invisible(dev.off())
  tab <- data.frame(Serie = nom, n = n, Moyenne = m, Ecart_type = sg, Erreur_standard = se, Marge = marge,
                    Borne_inf = ic[1], Borne_sup = ic[2], stringsAsFactors = FALSE)
  avert <- list("L'estimation suppose des observations indépendantes issues d'une loi normale (voir le test de normalité), ou un échantillon assez grand.")
  list(titre = "Estimation d'une moyenne — loi normale", lignes = lignes,
       conclusion = switch(cote, two.sided = sprintf("Moyenne estimée %s, intervalle à %s : [%s ; %s]", fmt(m), pct_txt(niv), fmt(ic[1]), fmt(ic[2])),
                           lower = sprintf("Moyenne estimée %s, supérieure à %s avec %s de confiance", fmt(m), fmt(ic[1]), pct_txt(niv)),
                           upper = sprintf("Moyenne estimée %s, inférieure à %s avec %s de confiance", fmt(m), fmt(ic[2]), pct_txt(niv))),
       tables = list(Estimation = tab), figures = c(fig), avertissements = avert)
}

# ---- Identification de loi -------------------------------------------------------
GUMBEL_EULER <- 0.5772156649
LOIS_ID <- list(
  normale = list(lib = "Normale", noms = c("moyenne", "écart-type"), ok = function(x) TRUE,
    fit = function(x) c(mean(x), sqrt(mean((x - mean(x))^2))),
    cdf = function(x, q) stats::pnorm(x, q[1], q[2]), dens = function(x, q) stats::dnorm(x, q[1], q[2]),
    quant = function(u, q) stats::qnorm(u, q[1], q[2]), rng = function(n, q) stats::rnorm(n, q[1], q[2])),
  lognormale = list(lib = "Log-normale", noms = c("moyenne des log", "écart-type des log"), ok = function(x) all(x > 0),
    fit = function(x) { l <- log(x); c(mean(l), sqrt(mean((l - mean(l))^2))) },
    cdf = function(x, q) stats::plnorm(x, q[1], q[2]), dens = function(x, q) stats::dlnorm(x, q[1], q[2]),
    quant = function(u, q) stats::qlnorm(u, q[1], q[2]), rng = function(n, q) stats::rlnorm(n, q[1], q[2])),
  exponentielle = list(lib = "Exponentielle", noms = c("moyenne"), ok = function(x) all(x >= 0) && mean(x) > 0,
    fit = function(x) mean(x),
    cdf = function(x, q) stats::pexp(x, 1 / q[1]), dens = function(x, q) stats::dexp(x, 1 / q[1]),
    quant = function(u, q) stats::qexp(u, 1 / q[1]), rng = function(n, q) stats::rexp(n, 1 / q[1])),
  gamma = list(lib = "Gamma", noms = c("forme", "échelle"), ok = function(x) all(x > 0),
    fit = function(x) {
      s <- log(mean(x)) - mean(log(x)); if (!is.finite(s) || s <= 1e-12) stop("gamma")
      k <- (3 - s + sqrt((s - 3)^2 + 24 * s)) / (12 * s)
      for (i in 1:30) { kn <- k - (log(k) - digamma(k) - s) / (1 / k - trigamma(k)); if (!is.finite(kn) || kn <= 0) break; if (abs(kn - k) < 1e-10 * k) { k <- kn; break }; k <- kn }
      c(k, mean(x) / k) },
    cdf = function(x, q) stats::pgamma(x, shape = q[1], scale = q[2]), dens = function(x, q) stats::dgamma(x, shape = q[1], scale = q[2]),
    quant = function(u, q) stats::qgamma(u, shape = q[1], scale = q[2]), rng = function(n, q) stats::rgamma(n, shape = q[1], scale = q[2])),
  weibull = list(lib = "Weibull", noms = c("forme", "échelle"), ok = function(x) all(x > 0),
    fit = function(x) {
      lx <- log(x); mlx <- mean(lx)
      f <- function(k) { xk <- exp(k * (lx - max(lx))); sum(xk * lx) / sum(xk) - 1 / k - mlx }
      k <- stats::uniroot(f, c(0.01, 200), tol = 1e-9)$root
      c(k, (mean(x^k))^(1 / k)) },
    cdf = function(x, q) stats::pweibull(x, shape = q[1], scale = q[2]), dens = function(x, q) stats::dweibull(x, shape = q[1], scale = q[2]),
    quant = function(u, q) stats::qweibull(u, shape = q[1], scale = q[2]), rng = function(n, q) stats::rweibull(n, shape = q[1], scale = q[2])),
  logistique = list(lib = "Logistique", noms = c("position", "échelle"), ok = function(x) TRUE,
    fit = function(x) {
      s0 <- stats::sd(x) * sqrt(3) / pi; m0 <- stats::median(x)
      nll <- function(t) -sum(stats::dlogis(x, t[1], exp(t[2]), log = TRUE))
      o <- stats::optim(c(m0, log(s0)), nll, method = "BFGS"); c(o$par[1], exp(o$par[2])) },
    cdf = function(x, q) stats::plogis(x, q[1], q[2]), dens = function(x, q) stats::dlogis(x, q[1], q[2]),
    quant = function(u, q) stats::qlogis(u, q[1], q[2]), rng = function(n, q) stats::rlogis(n, q[1], q[2])),
  gumbel = list(lib = "Valeur extrême (Gumbel, maximum)", noms = c("position", "échelle"), ok = function(x) TRUE,
    fit = function(x) {
      b0 <- stats::sd(x) * sqrt(6) / pi; a0 <- mean(x) - GUMBEL_EULER * b0
      nll <- function(t) { b <- exp(t[2]); z <- (x - t[1]) / b; sum(log(b) + z + exp(-z)) }
      o <- stats::optim(c(a0, log(b0)), nll, method = "BFGS"); c(o$par[1], exp(o$par[2])) },
    cdf = function(x, q) exp(-exp(-(x - q[1]) / q[2])), dens = function(x, q) { z <- (x - q[1]) / q[2]; exp(-z - exp(-z)) / q[2] },
    quant = function(u, q) q[1] - q[2] * log(-log(u)), rng = function(n, q) q[1] - q[2] * log(-log(stats::runif(n)))))

ad_stat <- function(u) {            # statistique d'Anderson-Darling A² à partir des valeurs F(x(i)) triées
  n <- length(u); u <- pmin(pmax(u, 1e-12), 1 - 1e-12); i <- seq_len(n)
  -n - sum((2 * i - 1) * (log(u) + log(1 - rev(u)))) / n
}
ajuster_loi <- function(id, x, B, graine) {
  L <- LOIS_ID[[id]]
  if (!L$ok(x)) return(list(id = id, lib = L$lib, ok = FALSE, raison = "données incompatibles avec le domaine de la loi"))
  q <- tryCatch(L$fit(x), error = function(e) NULL)
  if (is.null(q) || any(!is.finite(q)) || (length(q) > 1 && q[2] <= 0)) return(list(id = id, lib = L$lib, ok = FALSE, raison = "ajustement impossible"))
  xs <- sort(x); A2 <- ad_stat(L$cdf(xs, q))
  ll <- sum(log(pmax(L$dens(x, q), 1e-300)))
  pv <- NA_real_
  if (B > 0 && is.finite(A2)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(graine)
    sim <- vapply(seq_len(B), function(b) {
      xb <- L$rng(length(x), q)
      qb <- tryCatch(L$fit(xb), error = function(e) NULL)
      if (is.null(qb) || any(!is.finite(qb))) return(NA_real_)
      ad_stat(L$cdf(sort(xb), qb)) }, numeric(1))
    sim <- sim[is.finite(sim)]
    if (length(sim) >= 20) pv <- (1 + sum(sim >= A2)) / (1 + length(sim))
  }
  list(id = id, lib = L$lib, ok = TRUE, q = q, A2 = A2, ll = ll, aic = 2 * length(q) - 2 * ll, p = pv, noms = L$noms)
}
# Limites de confiance ponctuelles des centiles de la loi ajustée (au niveau 1 − α), comme sur les diagrammes de probabilité de Minitab.
# Normale et log-normale : formule asymptotique du maximum de vraisemblance ; autres lois : bootstrap paramétrique (B tirages).
# Renvoie une liste (inf, sup) de valeurs de la variable aux probabilités cumulées « pg ».
bandes_probabilite <- function(id, x, q, pg, B = 250L, graine = 1L) {
  L <- LOIS_ID[[id]]; n <- length(x); zc <- stats::qnorm(1 - ALPHA / 2)
  if (id %in% c("normale", "lognormale")) {
    u <- stats::qnorm(pg); ec <- zc * q[2] * sqrt((1 + u^2 / 2) / n); m <- q[1] + q[2] * u
    return(if (id == "normale") list(inf = m - ec, sup = m + ec) else list(inf = exp(m - ec), sup = exp(m + ec)))
  }
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(graine)
  Q <- NULL
  for (b in seq_len(B)) {
    xb <- L$rng(n, q); qb <- tryCatch(L$fit(xb), error = function(e) NULL)
    if (is.null(qb) || any(!is.finite(qb))) next
    v <- suppressWarnings(L$quant(pg, qb)); if (all(is.finite(v))) Q <- rbind(Q, v)
  }
  if (is.null(Q) || nrow(Q) < 30) return(NULL)
  list(inf = apply(Q, 2, stats::quantile, probs = ALPHA / 2, names = FALSE), sup = apply(Q, 2, stats::quantile, probs = 1 - ALPHA / 2, names = FALSE))
}
fmt_par <- function(r) paste(sprintf("%s = %s", r$noms, fmt(r$q, 5)), collapse = " ; ")

cmd_identification <- function(d, p) {
  cols <- noms_numeriques(d, unlist(p$colonnes))
  ids <- unlist(p$lois); if (is.null(ids) || !length(ids)) ids <- names(LOIS_ID)
  ids <- intersect(ids, names(LOIS_ID)); if (!length(ids)) stop("Choisir au moins une loi.", call. = FALSE)
  B <- suppressWarnings(as.integer(num(p$simulations))); if (is.na(B)) B <- 200L; B <- max(0L, min(B, 2000L))
  lignes <- c(sprintf("Identification de loi par ajustement (maximum de vraisemblance) et test d'Anderson-Darling ; p-value par simulation (%d tirages).", B),
              "H0 : les données suivent la loi testée. H1 : elles ne la suivent pas.", regle())
  tabs <- list(); figs <- character(0); avert <- list(); concl <- character(0)
  for (j in seq_along(cols)) {
    nm <- cols[j]; x <- sans_na(d[[nm]]); n <- length(x)
    if (n < 8) { avert <- c(avert, list(sprintf("« %s » : au moins 8 valeurs sont nécessaires (%d trouvée(s)).", nm, n))); next }
    if (stats::sd(x) == 0) { avert <- c(avert, list(sprintf("« %s » : toutes les valeurs sont identiques.", nm))); next }
    Bj <- if (n > 3000) min(B, 50L) else B
    res <- lapply(ids, function(id) ajuster_loi(id, x, Bj, 20240601 + n + j))
    ok <- Filter(function(r) r$ok, res)
    lignes <- c(lignes, "", sprintf("── %s (n = %d)", nm, n))
    for (r in res) if (!r$ok) lignes <- c(lignes, sprintf("  %-34s non testée : %s.", r$lib, r$raison))
    if (!length(ok)) { avert <- c(avert, list(sprintf("« %s » : aucune loi n'a pu être ajustée.", nm))); next }
    pvs <- vapply(ok, function(r) if (is.na(r$p)) -1 else r$p, numeric(1)); aics <- vapply(ok, function(r) r$aic, numeric(1))
    ordre <- order(-pvs, aics); ok <- ok[ordre]
    for (r in ok) lignes <- c(lignes, sprintf("  %-34s AD = %-9s p-value %-9s AIC = %-10s %s", r$lib, fmt(r$A2, 4), fmt_p(r$p), fmt(r$aic, 6), fmt_par(r)))
    admis <- Filter(function(r) !is.na(r$p) && r$p >= ALPHA, ok)
    if (length(admis)) {
      c1 <- sprintf("« %s » : loi(s) non rejetée(s) (p ≥ α = %s) : %s ; meilleur ajustement : %s.", nm, alpha_txt(), paste(vapply(admis, function(r) r$lib, ""), collapse = ", "), admis[[1]]$lib)
    } else c1 <- sprintf("« %s » : aucune des lois testées n'est compatible avec les données (toutes les p-values < α = %s).", nm, alpha_txt())
    lignes <- c(lignes, "  Décision : " %+% c1); concl <- c(concl, c1)
    tabs[[nm]] <- data.frame(Loi = vapply(ok, function(r) r$lib, ""), AD = vapply(ok, function(r) r$A2, numeric(1)),
                             p_value = vapply(ok, function(r) r$p, numeric(1)), AIC = vapply(ok, function(r) r$aic, numeric(1)),
                             Parametres = vapply(ok, fmt_par, ""), stringsAsFactors = FALSE)
    # densités ajustées
    f1 <- lire_png(sprintf("identification_%d_densites.png", j), largeur = 900, hauteur = 600)
    par(mar = c(4.5, 4.5, 3, 1)); h <- hist(x, plot = FALSE, breaks = "FD")
    xs <- seq(min(x), max(x), length.out = 400); cols_l <- PAL(length(ok))
    dens <- lapply(ok, function(r) LOIS_ID[[r$id]]$dens(xs, r$q)); ymax <- max(h$density, unlist(lapply(dens, function(v) max(v[is.finite(v)], 0)))) * 1.1
    plot(h, freq = FALSE, col = "#e6edf7", border = "white", ylim = c(0, min(ymax, max(h$density) * 3)), main = sprintf("Lois ajustées — %s", nm), xlab = nm, ylab = "Densité")
    for (k in seq_along(ok)) lines(xs, dens[[k]], col = cols_l[k], lwd = 2)
    legend("topright", bty = "n", cex = 0.8, lwd = 2, col = cols_l, legend = vapply(ok, function(r) sprintf("%s (p %s)", r$lib, fmt_p(r$p)), ""))
    invisible(dev.off())
    # diagrammes de probabilité
    f2 <- lire_png(sprintf("identification_%d_probabilites.png", j), largeur = 900, hauteur = 640)
    kk <- length(ok); nc <- min(3, kk); nl <- ceiling(kk / nc)
    par(mfrow = c(nl, nc), mar = c(3.5, 3.8, 2.6, 0.8), mgp = c(2.2, 0.7, 0)); xs_ <- sort(x); pp <- (seq_len(n) - 0.5) / n
    for (r in ok) {
      th <- LOIS_ID[[r$id]]$quant(pp, r$q)
      plot(th, xs_, pch = 16, cex = 0.6, col = "#305090", xlab = "Quantiles théoriques", ylab = "Données", main = sprintf("%s  (p %s)", r$lib, fmt_p(r$p)), cex.main = 0.95)
      abline(0, 1, col = "#b03030", lwd = 2)
    }
    invisible(dev.off())
    figs <- c(figs, f1, f2)
  }
  if (!length(tabs)) stop(if (length(avert)) paste(unlist(avert), collapse = " ") else "Aucune série exploitable.", call. = FALSE)
  list(titre = "Identification de loi", lignes = lignes, conclusion = paste(concl, collapse = " "), tables = tabs, figures = figs,
       avertissements = c(avert, list("Les paramètres sont estimés à partir des données : la p-value est donc calculée par simulation (bootstrap paramétrique). Un échantillon petit ne distingue pas bien plusieurs lois voisines ; utiliser aussi les diagrammes de probabilité.")))
}
`%+%` <- function(a, b) paste0(a, b)

# =============================================================================
#  Lot 3 — Corrélation, régression, séries chronologiques
# =============================================================================
# ---- Corrélation ---------------------------------------------------------------
cmd_correlation <- function(d, p) {
  cols <- noms_numeriques(d, unlist(p$colonnes))
  if (length(cols) < 2) stop("Sélectionner au moins deux séries numériques.", call. = FALSE)
  meth <- if (isTRUE(p$methode %in% c("spearman", "kendall"))) p$methode else "pearson"
  lib_m <- c(pearson = "Pearson (linéaire)", spearman = "Spearman (rangs, monotone)", kendall = "Kendall (rangs)")[[meth]]
  k <- length(cols); paires <- list(); R <- matrix(NA_real_, k, k, dimnames = list(cols, cols)); P <- R; diag(R) <- 1; diag(P) <- 0
  lignes <- c(sprintf("Corrélation de %s — %d séries.", lib_m, k),
              if (meth == "pearson") "H0 : ρ = 0 (pas de corrélation linéaire entre les deux séries). H1 : ρ ≠ 0." else "H0 : pas d'association monotone entre les deux séries. H1 : association monotone.",
              regle())
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    ok <- is.finite(d[[cols[i]]]) & is.finite(d[[cols[j]]]); x <- d[[cols[i]]][ok]; y <- d[[cols[j]]][ok]; n <- length(x)
    if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) { paires[[length(paires) + 1]] <- data.frame(Serie_1 = cols[i], Serie_2 = cols[j], n = n, r = NA_real_, p_value = NA_real_, Borne_inf = NA_real_, Borne_sup = NA_real_, Decision = "non calculable", stringsAsFactors = FALSE); next }
    ct <- suppressWarnings(stats::cor.test(x, y, method = meth, exact = FALSE, conf.level = 1 - ALPHA))
    r <- unname(ct$estimate); pv <- ct$p.value; R[i, j] <- R[j, i] <- r; P[i, j] <- P[j, i] <- pv
    ic <- if (meth == "pearson") as.numeric(ct$conf.int) else c(NA_real_, NA_real_)
    forc <- if (abs(r) >= 0.8) "forte" else if (abs(r) >= 0.5) "modérée" else if (abs(r) >= 0.3) "faible" else "très faible"
    dec <- if (pv < ALPHA) sprintf("H0 rejetée : corrélation %s, %s", forc, if (r > 0) "positive" else "négative") else "H0 non rejetée : aucune corrélation mise en évidence"
    lignes <- c(lignes, sprintf("%s × %s (n = %d) : r = %s ; p-value %s%s → %s.", cols[i], cols[j], n, fmt(r, 4), fmt_p(pv),
                                if (meth == "pearson") sprintf(" ; IC à %s [%s ; %s]", conf_txt(), fmt(ic[1], 4), fmt(ic[2], 4)) else "", dec))
    paires[[length(paires) + 1]] <- data.frame(Serie_1 = cols[i], Serie_2 = cols[j], n = n, r = r, p_value = pv, Borne_inf = ic[1], Borne_sup = ic[2],
                                               Decision = if (pv < ALPHA) "H0 rejetée" else "H0 non rejetée", stringsAsFactors = FALSE)
  }
  mat <- data.frame(Serie = cols, as.data.frame(R), stringsAsFactors = FALSE, check.names = FALSE)
  nfig <- min(k, 8); cf <- cols[seq_len(nfig)]
  fig <- lire_png("correlation.png", largeur = 900, hauteur = 800)
  panneau_haut <- function(x, y, ...) {
    ok <- is.finite(x) & is.finite(y); r <- if (sum(ok) > 2) stats::cor(x[ok], y[ok], method = meth) else NA
    usr <- par("usr"); on.exit(par(usr = usr)); par(usr = c(0, 1, 0, 1))
    text(0.5, 0.5, fmt(r, 3), cex = 1.2 + 1.2 * abs(ifelse(is.na(r), 0, r)), col = if (!is.na(r) && abs(r) >= 0.5) "#b03030" else "#444444")
  }
  panneau_bas <- function(x, y, ...) { points(x, y, pch = 19, cex = 0.5, col = "#2a5d9f80"); ok <- is.finite(x) & is.finite(y); if (sum(ok) > 4) lines(stats::lowess(x[ok], y[ok]), col = "#b03030", lwd = 1.5) }
  graphics::pairs(d[cf], upper.panel = panneau_haut, lower.panel = panneau_bas, gap = 0.4, main = sprintf("Matrice de corrélation — %s", lib_m))
  invisible(dev.off())
  avert <- list("Une corrélation n'établit pas une relation de cause à effet. Examiner le nuage de points : un point isolé ou une relation courbe peuvent fausser r.")
  if (k > nfig) avert <- c(avert, list(sprintf("Matrice graphique limitée aux %d premières séries.", nfig)))
  list(titre = sprintf("Corrélation — %s", lib_m), lignes = lignes,
       conclusion = sprintf("%d paire(s) analysée(s), %d corrélation(s) significative(s) au risque α = %s.", length(paires), sum(vapply(paires, function(t) isTRUE(t$p_value < ALPHA), logical(1))), alpha_txt()),
       tables = list(Paires = do.call(rbind, paires), Matrice_r = mat), figures = c(fig), avertissements = avert)
}

# ---- Régression --------------------------------------------------------------------
cmd_regression <- function(d, p) {
  yn <- nom_colonne(d, p$y, "Réponse (Y)"); xs <- unlist(p$x)
  if (is.null(xs) || !length(xs)) stop("Choisir au moins une variable explicative (X).", call. = FALSE)
  if (yn %in% xs) stop("La réponse ne peut pas être aussi une variable explicative.", call. = FALSE)
  for (nm in c(yn, xs)) col_num(d, nm)
  deg <- suppressWarnings(as.integer(num(p$degre))); if (is.na(deg)) deg <- 1L
  if (deg < 1 || deg > 3) stop("Le degré du polynôme doit être 1, 2 ou 3.", call. = FALSE)
  if (deg > 1 && length(xs) > 1) stop("Un polynôme (degré 2 ou 3) n'est possible qu'avec une seule variable explicative.", call. = FALSE)
  ok <- stats::complete.cases(d[c(yn, xs)]) & is.finite(rowSums(d[c(yn, xs)]))
  df <- d[ok, c(yn, xs), drop = FALSE]; n <- nrow(df)
  X <- if (deg == 1) as.matrix(df[xs]) else sapply(seq_len(deg), function(k) df[[xs]]^k)
  noms_t <- if (deg == 1) xs else c(xs, paste0(xs, c("²", "³")[seq_len(deg - 1)]))
  colnames(X) <- noms_t
  q <- ncol(X)
  if (n < q + 3) stop(sprintf("Trop peu d'observations complètes (%d) pour %d coefficient(s) et la constante.", n, q), call. = FALSE)
  dm <- data.frame(Y = df[[yn]], X, check.names = FALSE); names(dm)[-1] <- paste0("V", seq_len(q))
  fit <- stats::lm(Y ~ ., data = dm)
  sm <- summary(fit); co <- sm$coefficients; rownames(co) <- c("Constante", noms_t)
  qt_ <- stats::qt(1 - ALPHA / 2, fit$df.residual); ic <- cbind(co[, 1] - qt_ * co[, 2], co[, 1] + qt_ * co[, 2])
  s <- sm$sigma; r2 <- sm$r.squared; r2a <- sm$adj.r.squared
  fs <- sm$fstatistic; pF <- stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
  sct <- sum((dm$Y - mean(dm$Y))^2); sce <- sum(fit$residuals^2); scr <- sct - sce
  anova_t <- data.frame(Source = c("Régression", "Erreur résiduelle", "Total"), ddl = c(q, fit$df.residual, n - 1), SC = c(scr, sce, sct),
                        CM = c(scr / q, sce / fit$df.residual, NA), F = c(unname(fs[1]), NA, NA), p_value = c(pF, NA, NA), stringsAsFactors = FALSE)
  res <- fit$residuals; hat <- stats::hatvalues(fit); rst <- stats::rstandard(fit); cook <- stats::cooks.distance(fit)
  dw <- sum(diff(res)^2) / sum(res^2)
  vif <- if (q > 1 && deg == 1) vapply(seq_len(q), function(j) { r2j <- summary(stats::lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared; 1 / (1 - r2j) }, numeric(1)) else rep(NA_real_, q)
  coef_t <- data.frame(Terme = rownames(co), Coefficient = co[, 1], Erreur_standard = co[, 2], t = co[, 3], p_value = co[, 4],
                       Borne_inf = ic[, 1], Borne_sup = ic[, 2], VIF = c(NA, vif), Significatif = ifelse(co[, 4] < ALPHA, "oui", "non"), stringsAsFactors = FALSE)
  eq_terms <- paste(sprintf("%s %s × %s", ifelse(co[-1, 1] < 0, "−", "+"), fmt(abs(co[-1, 1]), 5), noms_t))
  equation <- sprintf("%s = %s %s", yn, fmt(co[1, 1], 5), paste(eq_terms, collapse = " "))
  sw <- if (n >= 3 && n <= 5000) stats::shapiro.test(res)$p.value else NA_real_
  atyp <- which(abs(rst) > 2 | hat > 3 * (q + 1) / n | cook > 4 / n)
  lignes <- c(sprintf("Régression %s de « %s » en fonction de %s (n = %d).", if (deg > 1) sprintf("polynomiale de degré %d", deg) else if (q > 1) "linéaire multiple" else "linéaire simple", yn, paste0("« ", xs, " »", collapse = ", "), n),
              "Test global — H0 : tous les coefficients (hors constante) sont nuls ; H1 : au moins un coefficient est différent de 0.", regle(),
              sprintf("Équation : %s", equation),
              sprintf("S (écart-type résiduel) = %s ; R² = %s ; R² ajusté = %s", fmt(s, 5), pct_txt(r2), pct_txt(r2a)),
              sprintf("F = %s (ddl %d ; %d) ; p-value %s", fmt(unname(fs[1])), q, fit$df.residual, fmt_p(pF)),
              decision(pF, "le modèle n'explique pas significativement la réponse.", "le modèle explique significativement la réponse."),
              "Coefficients — H0 : le coefficient est nul ; H1 : il est différent de 0.")
  for (i in seq_len(nrow(co))) lignes <- c(lignes, sprintf("  %-22s %12s   p-value %-9s → %s", rownames(co)[i], fmt(co[i, 1], 5), fmt_p(co[i, 4]), if (co[i, 4] < ALPHA) "H0 rejetée (coefficient significatif)" else "H0 non rejetée"))
  lignes <- c(lignes, sprintf("Durbin-Watson = %s (proche de 2 : pas d'autocorrélation des résidus dans l'ordre des données).", fmt(dw, 4)),
              if (!is.na(sw)) sprintf("Normalité des résidus (Shapiro-Wilk) : p-value %s → %s", fmt_p(sw), if (sw >= ALPHA) "H0 non rejetée : normalité non rejetée" else "H0 rejetée : les résidus s'écartent de la loi normale") else "Normalité des résidus : non testée.",
              if (length(atyp)) sprintf("%d observation(s) inhabituelle(s) (résidu standardisé > 2 en valeur absolue, levier élevé ou distance de Cook élevée) : lignes %s.", length(atyp), paste(utils::head(which(ok)[atyp], 12), collapse = ", ")) else "Aucune observation inhabituelle.")
  if (any(vif > 10, na.rm = TRUE)) lignes <- c(lignes, "VIF > 10 : variables explicatives fortement liées entre elles (multicolinéarité), coefficients peu fiables.")
  # --- graphique d'ajustement
  f1 <- lire_png("regression_ajustement.png", largeur = 900, hauteur = 600)
  par(mar = c(4.5, 4.5, 3, 1))
  if (length(xs) == 1) {
    x <- df[[xs]]; g <- seq(min(x), max(x), length.out = 200)
    nd <- data.frame(sapply(seq_len(deg), function(k) g^k)); names(nd) <- paste0("V", seq_len(deg))
    pc <- stats::predict(fit, nd, interval = "confidence", level = 1 - ALPHA); pp <- stats::predict(fit, nd, interval = "prediction", level = 1 - ALPHA)
    plot(x, df[[yn]], pch = 19, col = "#2a5d9f", xlab = xs, ylab = yn, main = sprintf("Ajustement — %s", yn), ylim = range(c(df[[yn]], pp), finite = TRUE))
    polygon(c(g, rev(g)), c(pp[, 2], rev(pp[, 3])), col = "#f3e3cc", border = NA); polygon(c(g, rev(g)), c(pc[, 2], rev(pc[, 3])), col = "#cfe0f5", border = NA)
    points(x, df[[yn]], pch = 19, col = "#2a5d9f"); lines(g, pc[, 1], col = "#b03030", lwd = 2)
    legend("topleft", bty = "n", cex = 0.8, fill = c("#cfe0f5", "#f3e3cc"), border = NA, legend = c(sprintf("intervalle de confiance à %s", conf_txt()), sprintf("intervalle de prédiction à %s", conf_txt())))
  } else {
    pr <- stats::fitted(fit); plot(pr, df[[yn]], pch = 19, col = "#2a5d9f", xlab = "Valeurs ajustées", ylab = yn, main = "Valeurs observées et ajustées"); abline(0, 1, col = "#b03030", lwd = 2)
  }
  invisible(dev.off())
  # --- résidus (4 graphiques)
  f2 <- lire_png("regression_residus.png", largeur = 900, hauteur = 700)
  par(mfrow = c(2, 2), mar = c(4, 4.2, 2.6, 1))
  qq <- stats::qqnorm(rst, plot.it = FALSE); plot(qq$x, qq$y, pch = 19, cex = 0.7, col = "#2a5d9f", xlab = "Quantiles normaux", ylab = "Résidus standardisés", main = "Droite de Henry des résidus"); abline(0, 1, col = "#b03030", lwd = 2)
  plot(stats::fitted(fit), rst, pch = 19, cex = 0.7, col = "#2a5d9f", xlab = "Valeurs ajustées", ylab = "Résidus standardisés", main = "Résidus et valeurs ajustées"); abline(h = c(-2, 0, 2), lty = c(3, 2, 3), col = "#b03030")
  hist(rst, col = "#cfe0f5", border = "white", main = "Histogramme des résidus", xlab = "Résidus standardisés", ylab = "Effectif")
  plot(seq_len(n), rst, type = "b", pch = 19, cex = 0.6, col = "#2a5d9f", xlab = "Ordre des observations", ylab = "Résidus standardisés", main = "Résidus et ordre"); abline(h = c(-2, 0, 2), lty = c(3, 2, 3), col = "#b03030")
  invisible(dev.off())
  dec <- decision(pF, "le modèle n'explique pas significativement la réponse.", "le modèle explique significativement la réponse.")
  list(titre = "Régression", lignes = lignes,
       conclusion = sprintf("%s ; R² = %s, R² ajusté = %s.", sub("^Décision : ", "", sub("\\.$", "", dec)), pct_txt(r2), pct_txt(r2a)),
       tables = list(Coefficients = coef_t, Analyse_de_variance = anova_t), figures = c(f1, f2),
       avertissements = list("Vérifier les hypothèses sur les résidus (indépendance, normalité, variance constante) avec les graphiques. Une relation statistique n'est pas une relation de cause à effet ; éviter d'extrapoler hors de l'étendue des données."))
}

# ---- Séries chronologiques et tendances ------------------------------------------------
MODELES_TENDANCE <- c(lineaire = "Linéaire", quadratique = "Quadratique", exponentielle = "Exponentielle (croissance)", logistique = "Logistique (courbe en S)")
ajuster_tendance <- function(id, t, y) {
  if (id == "lineaire") { f <- stats::lm(y ~ t); sm <- summary(f); co <- sm$coefficients
    list(predict = function(tt, interval = FALSE) { if (interval) stats::predict(f, data.frame(t = tt), interval = "prediction", level = 1 - ALPHA) else as.numeric(stats::predict(f, data.frame(t = tt))) },
         eq = sprintf("Y = %s %s %s × t", fmt(co[1, 1], 5), if (co[2, 1] < 0) "−" else "+", fmt(abs(co[2, 1]), 5)),
         test = list(lib = "pente (tendance linéaire)", p = co[2, 4], coef = co[2, 1]), k = 2) }
  else if (id == "quadratique") { f <- stats::lm(y ~ t + I(t^2)); sm <- summary(f); co <- sm$coefficients
    list(predict = function(tt, interval = FALSE) { if (interval) stats::predict(f, data.frame(t = tt), interval = "prediction", level = 1 - ALPHA) else as.numeric(stats::predict(f, data.frame(t = tt))) },
         eq = sprintf("Y = %s %s %s × t %s %s × t²", fmt(co[1, 1], 5), if (co[2, 1] < 0) "−" else "+", fmt(abs(co[2, 1]), 5), if (co[3, 1] < 0) "−" else "+", fmt(abs(co[3, 1]), 5)),
         test = list(lib = "terme quadratique (courbure)", p = co[3, 4], coef = co[3, 1]), k = 3) }
  else if (id == "exponentielle") { if (any(y <= 0)) stop("toutes les valeurs doivent être strictement positives.", call. = FALSE)
    f <- stats::lm(log(y) ~ t); sm <- summary(f); co <- sm$coefficients; s2 <- sm$sigma^2
    list(predict = function(tt, interval = FALSE) { if (interval) { pr <- stats::predict(f, data.frame(t = tt), interval = "prediction", level = 1 - ALPHA); exp(pr) } else exp(as.numeric(stats::predict(f, data.frame(t = tt)))) },
         eq = sprintf("Y = %s × exp(%s × t)  (taux de croissance par période : %s)", fmt(exp(co[1, 1]), 5), fmt(co[2, 1], 5), pct_txt(exp(co[2, 1]) - 1)),
         test = list(lib = "pente du logarithme (croissance exponentielle)", p = co[2, 4], coef = co[2, 1]), k = 2) }
  else { if (any(y <= 0)) stop("toutes les valeurs doivent être strictement positives.", call. = FALSE)
    K0 <- max(y) * 1.05; z <- log(y / (K0 - y)); l0 <- stats::lm(z ~ t); r0 <- unname(stats::coef(l0)[2]); t00 <- -unname(stats::coef(l0)[1]) / r0
    if (!is.finite(r0) || r0 == 0 || !is.finite(t00)) stop("ajustement impossible.", call. = FALSE)
    par_ <- c(K = K0, r = r0, t0 = t00); approx <- TRUE
    fn <- tryCatch(stats::nls(y ~ K / (1 + exp(-r * (t - t0))), start = as.list(par_), control = stats::nls.control(maxiter = 300, warnOnly = FALSE)), error = function(e) NULL)
    if (!is.null(fn)) { par_ <- stats::coef(fn); approx <- FALSE }
    list(predict = function(tt, interval = FALSE) { v <- par_[["K"]] / (1 + exp(-par_[["r"]] * (tt - par_[["t0"]]))); if (interval) cbind(fit = v, lwr = NA_real_, upr = NA_real_) else v },
         eq = sprintf("Y = %s / (1 + exp(−%s × (t − %s)))  (asymptote %s)%s", fmt(par_[["K"]], 5), fmt(par_[["r"]], 5), fmt(par_[["t0"]], 5), fmt(par_[["K"]], 5), if (approx) " — ajustement approché" else ""),
         test = NULL, k = 3) }
}
cmd_serie <- function(d, p) {
  yn <- nom_colonne(d, p$y, "Série"); col_num(d, yn)
  nom_o <- NULL; lab <- NULL
  if (!is.null(p$ordre) && nzchar(p$ordre)) { nom_o <- nom_colonne(d, p$ordre, "Colonne d'ordre"); d <- d[order(d[[nom_o]], na.last = TRUE), , drop = FALSE]; lab <- etiq_ordre(d[[nom_o]]) }
  y_all <- d[[yn]]; t_all <- seq_along(y_all); ok <- is.finite(y_all); y <- y_all[ok]; t <- t_all[ok]; n <- length(y)
  if (n < 6) stop(sprintf("Au moins 6 valeurs sont nécessaires pour analyser une tendance (%d trouvée(s)).", n), call. = FALSE)
  ids <- intersect(unlist(p$modeles), names(MODELES_TENDANCE)); if (!length(ids)) ids <- "lineaire"
  h <- suppressWarnings(as.integer(num(p$horizon))); if (is.na(h) || h < 0) h <- 0L; h <- min(h, 500L)
  lignes <- c(sprintf("Analyse de tendance de « %s » (%d points%s).", yn, n, if (!is.null(nom_o)) sprintf(", ordonnés selon « %s »", nom_o) else ", dans l'ordre des données"),
              "Test de tendance — H0 : le coefficient de tendance est nul (pas de tendance) ; H1 : il est différent de 0.", regle())
  mod <- list(); avert <- list(); tab <- list()
  for (id in ids) {
    m <- tryCatch(ajuster_tendance(id, t, y), error = function(e) { avert <<- c(avert, list(sprintf("%s : %s", MODELES_TENDANCE[[id]], conditionMessage(e)))); NULL })
    if (is.null(m)) next
    fit <- m$predict(t); e <- y - fit
    mape <- if (any(y == 0)) NA_real_ else mean(abs(e / y)) * 100; mad <- mean(abs(e)); msd <- mean(e^2); r2 <- 1 - sum(e^2) / sum((y - mean(y))^2)
    mod[[id]] <- m
    tab[[length(tab) + 1]] <- data.frame(Modele = MODELES_TENDANCE[[id]], MAPE = mape, MAD = mad, MSD = msd, R2 = r2, Equation = m$eq, stringsAsFactors = FALSE)
    lignes <- c(lignes, "", sprintf("── Modèle %s", sub("^(.)", "\\L\\1", MODELES_TENDANCE[[id]], perl = TRUE)), sprintf("  Équation : %s", m$eq),
                sprintf("  Précision : MAPE = %s %% ; MAD = %s ; MSD = %s ; R² = %s", fmt(mape, 4), fmt(mad, 5), fmt(msd, 5), pct_txt(r2)))
    if (!is.null(m$test)) lignes <- c(lignes, sprintf("  Test — %s : p-value %s → %s", m$test$lib, fmt_p(m$test$p),
                                                      if (m$test$p < ALPHA) sprintf("H0 rejetée : %s (coefficient %s)", if (id == "quadratique") "courbure significative" else "tendance significative", fmt(m$test$coef, 4)) else "H0 non rejetée : tendance non démontrée"))
  }
  if (!length(mod)) stop(paste(unlist(avert), collapse = " "), call. = FALSE)
  tabm <- do.call(rbind, tab); meilleur <- tabm$Modele[which.min(tabm$MSD)]
  if (length(mod) > 1) lignes <- c(lignes, "", sprintf("Meilleur ajustement (MSD le plus faible) : %s. Un modèle plus souple ajuste toujours mieux : juger aussi la forme du graphique et la logique du phénomène.", meilleur))
  # prévisions
  tabs <- list(Modeles = tabm)
  if (h > 0) {
    tf <- max(t_all) + seq_len(h)
    pv <- data.frame(Periode = paste0("+", seq_len(h)), stringsAsFactors = FALSE)
    for (id in names(mod)) { pr <- mod[[id]]$predict(tf, TRUE); pv[[MODELES_TENDANCE[[id]]]] <- pr[, 1]; if (!anyNA(pr[, 2])) { pv[[paste0(MODELES_TENDANCE[[id]], " (borne inf.)")]] <- pr[, 2]; pv[[paste0(MODELES_TENDANCE[[id]], " (borne sup.)")]] <- pr[, 3] } }
    tabs$Previsions <- pv
    lignes <- c(lignes, "", sprintf("Prévisions sur %d période(s) : voir le tableau « Previsions » (intervalles de prédiction à %s pour les modèles linéaire, quadratique et exponentiel).", h, conf_txt()))
  }
  # graphique
  ntot <- length(y_all) + h; cols_m <- PAL(length(mod) + 1)
  longues <- !is.null(lab) && max(nchar(lab), 0, na.rm = TRUE) > 8
  fig <- lire_png("serie_tendance.png", largeur = 950, hauteur = 600 + if (longues) 160 else 0)
  par(mar = c(if (longues) 9 else 4.5, 4.5, 3, 1))
  gt <- seq(min(t), max(t_all) + h, length.out = 300)
  courbes <- lapply(mod, function(m) m$predict(gt))
  yl <- range(c(y, unlist(courbes)), finite = TRUE); mg <- diff(yl) * 0.08; yl <- yl + c(-mg, mg)
  plot(t_all, y_all, type = "n", xaxt = "n", xlab = if (longues) "" else if (is.null(nom_o)) "Période" else nom_o, ylab = yn, main = sprintf("Tendance — %s", yn), xlim = c(1, ntot), ylim = yl)
  etq <- if (!is.null(lab)) c(lab, rep(NA, h)) else as.character(seq_len(ntot)); etq[length(y_all) + seq_len(h)] <- if (h > 0) paste0("+", seq_len(h)) else character(0)
  idx <- if (ntot <= (if (longues) 20 else 30)) seq_len(ntot) else unique(round(seq(1, ntot, length.out = if (longues) 14 else 15)))
  axis(1, at = idx, labels = etq[idx], las = if (ntot > 15 || longues) 2 else 1, cex.axis = 0.8)
  if (longues) mtext(if (is.null(nom_o)) "Période" else nom_o, side = 1, line = par("mar")[1] - 1.3)
  if (h > 0) abline(v = length(y_all) + 0.5, lty = 3, col = "#888888")
  lines(t_all, y_all, col = "#2a5d9f", lwd = 1.2); points(t, y, pch = 19, cex = 0.8, col = "#2a5d9f")
  if (length(mod) == 1 && h > 0) { pr <- mod[[1]]$predict(max(t_all) + seq_len(h), TRUE); if (!anyNA(pr[, 2])) polygon(c(max(t_all) + seq_len(h), rev(max(t_all) + seq_len(h))), c(pr[, 2], rev(pr[, 3])), col = "#f3e3cc80", border = NA) }
  k <- 0; for (id in names(mod)) { k <- k + 1; lines(gt, courbes[[id]], col = cols_m[k], lwd = 2) }
  legend("topleft", bty = "n", cex = 0.8, lwd = c(1.2, rep(2, length(mod))), col = c("#2a5d9f", cols_m[seq_along(mod)]), legend = c("Données", MODELES_TENDANCE[names(mod)]))
  invisible(dev.off())
  conc <- sprintf("%d modèle(s) de tendance ajusté(s) ; meilleur MSD : %s.", length(mod), meilleur)
  if (!is.null(mod$lineaire$test)) conc <- paste(conc, if (mod$lineaire$test$p < ALPHA) sprintf("Tendance linéaire significative (p-value %s < α = %s).", fmt_p(mod$lineaire$test$p), alpha_txt()) else sprintf("Tendance linéaire non démontrée (p-value %s ≥ α = %s).", fmt_p(mod$lineaire$test$p), alpha_txt()))
  list(titre = "Séries chronologiques — tendances", lignes = lignes, conclusion = conc, tables = tabs, figures = c(fig),
       avertissements = c(avert, list("Les tendances ignorent la saisonnalité et l'autocorrélation : à utiliser comme description générale. Éviter de prévoir trop loin de la dernière observation.")))
}

# =============================================================================
#  Lot 4 — Cartes de contrôle aux attributs (p, np, c et u), avec phases
# =============================================================================
cmd_carte_attributs <- function(d, p) {
  type <- if (isTRUE(p$type %in% c("p", "u", "np", "c"))) p$type else "p"
  tests <- suppressWarnings(as.integer(unlist(p$tests))); tests <- tests[!is.na(tests) & tests >= 1 & tests <= 8]
  if (!length(p$tests)) tests <- 1:4
  lab <- NULL; nom_o <- NULL
  if (!is.null(p$ordre) && nzchar(p$ordre)) { nom_o <- nom_colonne(d, p$ordre, "Colonne d'ordre"); d <- d[order(d[[nom_o]], na.last = TRUE), , drop = FALSE]; lab <- etiq_ordre(d[[nom_o]]) }
  dn <- nom_colonne(d, p$defauts, if (type %in% c("p", "np")) "Colonne des unités défectueuses" else "Colonne des défauts"); x <- col_num(d, dn)
  en <- NULL
  if (type == "c") { n <- rep(1, length(x))              # carte c : unités de même taille, aucun effectif à saisir
    if (!is.null(p$effectif) && nzchar(p$effectif) && length(unique(col_num(d, nom_colonne(d, p$effectif, "Colonne des effectifs")))) > 1)
      stop("La carte c suppose des unités de même taille ; avec des effectifs variables, utiliser la carte u (défauts par unité).", call. = FALSE)
  } else if (!is.null(p$effectif) && nzchar(p$effectif)) { en <- nom_colonne(d, p$effectif, "Colonne des effectifs"); n <- col_num(d, en)
  } else { nc <- num(p$n); if (is.na(nc) || nc <= 0) stop("Choisir la colonne des effectifs contrôlés, ou saisir un effectif constant.", call. = FALSE); en <- NULL; n <- rep(nc, length(x)) }
  ph_rows <- NULL
  if (!is.null(p$phase) && nzchar(p$phase)) { pn <- nom_colonne(d, p$phase, "Colonne de phases"); ph_rows <- as.character(d[[pn]]) }
  ok <- is.finite(x) & is.finite(n)
  x <- x[ok]; n <- n[ok]; k <- length(x)
  etiq <- if (!is.null(lab)) lab[ok] else as.character(seq_len(k))
  ph <- if (!is.null(ph_rows)) ph_rows[ok] else NULL
  if (k < 2) stop("Il faut au moins 2 points pour construire une carte de contrôle.", call. = FALSE)
  if (any(x < 0) || any(abs(x - round(x)) > 1e-8)) stop("Les nombres de défauts ou d'unités défectueuses doivent être des entiers positifs ou nuls.", call. = FALSE)
  if (any(n <= 0)) stop("Les effectifs contrôlés doivent être strictement positifs.", call. = FALSE)
  if (type %in% c("p", "np") && any(x > n)) stop(sprintf("Ligne %d : le nombre d'unités défectueuses dépasse l'effectif contrôlé.", which(x > n)[1]), call. = FALSE)
  rupt <- num(p$rupture)
  if (is.null(ph) && !is.na(rupt)) ph <- ifelse(seq_len(k) < rupt, "Avant", "Après")
  if (!is.null(ph)) { ph <- as.character(ph); ph[is.na(ph)] <- "(vide)" }
  niv <- if (is.null(ph)) NULL else unique(ph)
  idx <- if (is.null(ph)) list(seq_len(k)) else lapply(niv, function(l) which(ph == l))
  if (!is.null(ph) && length(niv) < 2) { ph <- NULL; niv <- NULL; idx <- list(seq_len(k)) }
  y <- if (type %in% c("np", "c")) x else x / n; cl <- lcl <- ucl <- sg <- rep(NA_real_, k)
  for (j in seq_along(idx)) {
    ix <- idx[[j]]
    if (length(ix) < 2) stop(sprintf("La phase « %s » ne contient qu'un point : il en faut au moins 2 par phase.", niv[j]), call. = FALSE)
    cb <- sum(x[ix]) / sum(n[ix])
    qui <- if (is.null(ph)) "" else sprintf("Phase « %s » : ", niv[j])
    if (type %in% c("p", "np")) { if (cb <= 0 || cb >= 1) stop(sprintf("%sla proportion moyenne est %s, aucune limite de contrôle n'est calculable.", qui, fmt(cb)), call. = FALSE)
      if (type == "p") { cl[ix] <- cb; sg[ix] <- sqrt(cb * (1 - cb) / n[ix]) } else { cl[ix] <- n[ix] * cb; sg[ix] <- sqrt(n[ix] * cb * (1 - cb)) } }
    else if (type == "u") { if (cb <= 0) stop(sprintf("%saucun défaut observé, aucune limite de contrôle n'est calculable.", qui), call. = FALSE); cl[ix] <- cb; sg[ix] <- sqrt(cb / n[ix]) }
    else { cb <- mean(x[ix]); if (cb <= 0) stop(sprintf("%saucun défaut observé, aucune limite de contrôle n'est calculable.", qui), call. = FALSE); cl[ix] <- cb; sg[ix] <- sqrt(cb) }
    ucl[ix] <- cl[ix] + 3 * sg[ix]; lcl[ix] <- cl[ix] - 3 * sg[ix]
  }
  lcl <- pmax(lcl, 0); if (type == "p") ucl <- pmin(ucl, 1); if (type == "np") ucl <- pmin(ucl, n)
  sig1 <- list()
  fusion <- function(a, b) { for (nm in names(b)) a[[nm]] <- c(a[[nm]], b[[nm]]); a }
  for (ix in idx) { r <- tests_causes_speciales(y[ix], cl[ix], sg[ix], tests); sig1 <- fusion(sig1, lapply(r, function(v) ix[v])) }
  sig1 <- sig1[order(as.integer(names(sig1)))]
  nsig <- length(unique(unlist(sig1)))
  marque <- function(s, kk) { r <- character(kk); for (t in names(s)) r[s[[t]]] <- ifelse(nzchar(r[s[[t]]]), paste0(r[s[[t]]], ",", t), t); r }
  nom <- switch(type, p = "Proportion d'unités défectueuses (carte p)", np = "Nombre d'unités défectueuses (carte np)", u = "Défauts par unité (carte u)", c = "Nombre de défauts (carte c)")
  longues <- !is.null(lab) && max(nchar(lab), 0, na.rm = TRUE) > 8
  fig <- lire_png("carte_attributs.png", largeur = max(900, min(2000, 20 * k + 500)), hauteur = 560 + if (longues) 160 else 0)
  par(mar = c(4.5, 4.5, 2.5, 7))
  trace_carte(nom, etiq, y, cl, lcl, ucl, sg, sig1, switch(type, p = "Proportion", np = "Unités défectueuses", u = "Défauts par unité", c = "Défauts"), any(tests %in% 5:8), if (!is.null(nom_o)) nom_o else "Échantillon", ph)
  invisible(dev.off())
  lignes <- c(sprintf("Carte %s — %d points%s.", type, k, if (type == "c") ", unités de taille constante" else if (!is.null(en)) sprintf(", effectif variable (colonne « %s »)", en) else sprintf(", effectif constant de %s", fmt(n[1]))))
  if (is.null(ph)) lignes <- c(lignes, sprintf("%s : %s ; LIC = %s ; LSC = %s%s", switch(type, p = "Proportion moyenne", np = "Nombre moyen d'unités défectueuses", u = "Taux moyen", c = "Nombre moyen de défauts"), fmt(cl[1], 6), fmt(lcl[1], 6), fmt(ucl[1], 6), if (length(unique(n)) > 1) " (limites variables, valeurs du premier point)" else ""))
  else for (j in seq_along(idx)) { i1 <- idx[[j]][1]; lignes <- c(lignes, sprintf("Phase « %s » (%d points) : moyenne = %s ; LIC = %s ; LSC = %s", niv[j], length(idx[[j]]), fmt(cl[i1], 6), fmt(lcl[i1], 6), fmt(ucl[i1], 6))) }
  concl <- if (nsig == 0) sprintf("Aucun signal de cause spéciale (tests %s) : le procédé paraît sous contrôle statistique.", paste(tests, collapse = ", "))
           else sprintf("%d point(s) en signal de cause spéciale (tests %s) : le procédé n'est pas stable.", nsig, paste(names(sig1), collapse = ", "))
  lignes <- c(lignes, "", concl, if (length(sig1)) vapply(names(sig1), function(t) sprintf("Test %s : %s", t, paste(etiq[sig1[[t]]], collapse = ", ")), ""))
  hors <- sum(y > ucl | y < lcl)
  avert <- list()
  if (length(unique(n)) > 1 && max(n) / min(n) > 4) avert <- c(avert, list("Les effectifs varient beaucoup : les limites varient d'un point à l'autre (carte à limites variables)."))
  if (k < 20) avert <- c(avert, list(sprintf("Seulement %d points : on recommande au moins 20 à 25 points pour établir des limites de contrôle fiables.", k)))
  if (hors > 0.1 * k && k >= 20) avert <- c(avert, list("Beaucoup de points hors limites : une variabilité supérieure à la loi binomiale/de Poisson (surdispersion) est possible ; en tenir compte avant de conclure à des causes spéciales."))
  if (type == "p") avert <- c(avert, list("Carte p : unités défectueuses par échantillon (loi binomiale). Pour compter plusieurs défauts par unité, utiliser la carte u."))
  if (type == "np") avert <- c(avert, list("Carte np : nombre d'unités défectueuses par échantillon (loi binomiale) ; elle convient surtout à des échantillons de taille constante, sinon préférer la carte p."))
  if (type == "c") avert <- c(avert, list("Carte c : nombre de défauts par unité de contrôle de taille constante (loi de Poisson) ; si la taille varie, utiliser la carte u."))
  tab <- if (type == "c") data.frame(Echantillon = etiq, Comptage = x, LIC = lcl, LC = cl, LSC = ucl, Tests = marque(sig1, k), stringsAsFactors = FALSE)
         else data.frame(Echantillon = etiq, n = n, Comptage = x, Valeur = y, LIC = lcl, LC = cl, LSC = ucl, Tests = marque(sig1, k), stringsAsFactors = FALSE)
  if (!is.null(ph)) tab <- cbind(Phase = ph, tab)
  tt <- switch(type, p = "Carte de contrôle p (proportion d'unités défectueuses)", np = "Carte de contrôle np (nombre d'unités défectueuses)", u = "Carte de contrôle u (défauts par unité)", c = "Carte de contrôle c (nombre de défauts)")
  if (!is.null(ph)) tt <- paste0(tt, sprintf(" — %d phases (%s)", length(idx), paste(niv, collapse = " / ")))
  list(titre = tt, lignes = lignes, conclusion = concl, tables = list(Carte = tab), figures = c(fig), avertissements = avert)
}

# =============================================================================
#  Lot 4 — Graphiques
# =============================================================================
# Séries à représenter : colonnes numériques sélectionnées, ou « valeur » découpée selon une colonne de groupes.
series_graphique <- function(d, p) {
  gn <- if (!is.null(p$groupe) && nzchar(p$groupe)) nom_colonne(d, p$groupe, "Colonne de groupes") else NULL
  if (!is.null(gn) && !is.null(p$valeur) && nzchar(p$valeur)) {
    v <- col_num(d, nom_colonne(d, p$valeur, "Colonne des valeurs")); g <- ch_cat(d[[gn]]); ok <- is.finite(v) & !is.na(g)
    if (!any(ok)) stop("Aucune valeur exploitable.", call. = FALSE)
    niv <- unique(g[ok]); s <- lapply(niv, function(l) v[ok & g == l]); names(s) <- niv
    return(list(s = s, nom_val = p$valeur, nom_grp = gn))
  }
  cols <- noms_numeriques(d, unlist(p$colonnes))
  s <- lapply(cols, function(nm) sans_na(d[[nm]])); names(s) <- cols
  list(s = s, nom_val = if (length(cols) == 1) cols else "Valeur", nom_grp = NULL)
}
decrire <- function(s) data.frame(Serie = names(s), n = vapply(s, length, integer(1)), Moyenne = vapply(s, function(v) if (length(v)) mean(v) else NA_real_, numeric(1)),
                                  Ecart_type = vapply(s, function(v) if (length(v) > 1) stats::sd(v) else NA_real_, numeric(1)),
                                  Minimum = vapply(s, function(v) if (length(v)) min(v) else NA_real_, numeric(1)),
                                  Mediane = vapply(s, function(v) if (length(v)) stats::median(v) else NA_real_, numeric(1)),
                                  Maximum = vapply(s, function(v) if (length(v)) max(v) else NA_real_, numeric(1)), stringsAsFactors = FALSE, row.names = NULL)
# Résumé par catégorie (effectif, somme ou moyenne), le surplus de catégories étant regroupé en « Autres ».
resume_categories <- function(d, p, max_cat = 12) {
  cn <- nom_colonne(d, p$categorie, "Colonne des catégories"); g <- ch_cat(d[[cn]])
  fct <- if (isTRUE(p$fonction %in% c("somme", "moyenne"))) p$fonction else "effectif"
  if (fct == "effectif" || is.null(p$valeur) || !nzchar(p$valeur)) { fct <- "effectif"; v <- rep(1, length(g)); vn <- NULL }
  else { vn <- nom_colonne(d, p$valeur, "Colonne des valeurs"); v <- col_num(d, vn) }
  ok <- !is.na(g) & is.finite(v)
  if (!any(ok)) stop("Aucune donnée exploitable.", call. = FALSE)
  val <- tapply(v[ok], g[ok], if (fct == "moyenne") mean else sum)
  val <- sort(val, decreasing = TRUE)
  if (length(val) > max_cat) { reste <- sum(val[-seq_len(max_cat - 1)]); val <- c(val[seq_len(max_cat - 1)], Autres = reste) }
  list(val = val, nom = cn, lib = if (fct == "effectif") "Effectif" else sprintf("%s de %s", if (fct == "somme") "Somme" else "Moyenne", vn), fct = fct)
}

cmd_graphique <- function(d, p) {
  type <- p$type_graphique
  figs <- character(0); lignes <- character(0); tabs <- list(); avert <- list(); titre <- "Graphique"
  if (type == "histogramme") {
    S <- series_graphique(d, p); s <- S$s; titre <- "Histogramme"
    cl <- suppressWarnings(as.integer(num(p$classes)))
    for (j in seq_along(s)[seq_len(min(length(s), 12))]) {
      x <- s[[j]]; if (length(x) < 2) { avert <- c(avert, list(sprintf("« %s » : au moins 2 valeurs sont nécessaires.", names(s)[j]))); next }
      f <- lire_png(sprintf("histogramme_%d.png", j), largeur = 800, hauteur = 560); par(mar = c(4.5, 4.5, 3, 1))
      h <- hist(x, breaks = if (!is.na(cl) && cl >= 2) cl else "FD", plot = FALSE)
      ymax <- max(h$counts) * 1.12
      if (vrai(p$normale) && stats::sd(x) > 0) { larg <- diff(h$breaks)[1]; ymax <- max(max(h$counts), stats::dnorm(mean(x), mean(x), stats::sd(x)) * length(x) * larg) * 1.15 }
      plot(h, col = "#cfe0f5", border = "white", main = sprintf("Histogramme — %s", names(s)[j]), xlab = names(s)[j], ylab = "Effectif", ylim = c(0, ymax))
      if (vrai(p$normale) && stats::sd(x) > 0) { xs <- seq(min(h$breaks), max(h$breaks), length.out = 300); lines(xs, stats::dnorm(xs, mean(x), stats::sd(x)) * length(x) * diff(h$breaks)[1], col = "#b03030", lwd = 2) }
      invisible(dev.off()); figs <- c(figs, f)
    }
    tabs$Series <- decrire(s); lignes <- c(sprintf("Histogramme de %d série(s).", length(s)), sprintf("Classes : %s.", if (!is.na(cl) && cl >= 2) paste(cl, "demandées") else "choisies automatiquement (règle de Freedman-Diaconis)"))
  } else if (type %in% c("secteurs", "barres")) {
    R <- resume_categories(d, p, if (type == "secteurs") 10 else 30); val <- R$val; tot <- sum(val)
    if (type == "secteurs" && any(val < 0)) stop("Un graphique en secteurs exige des valeurs positives.", call. = FALSE)
    titre <- if (type == "secteurs") "Graphique en secteurs" else "Diagramme en barres"
    gn <- if (type == "barres" && !is.null(p$groupe) && nzchar(p$groupe)) nom_colonne(d, p$groupe, "Colonne de groupes") else NULL
    f <- lire_png(if (type == "secteurs") "secteurs.png" else "barres.png", largeur = 800, hauteur = 560)
    if (type == "secteurs") {
      par(mar = c(1, 1, 3, 1)); pc <- 100 * val / tot
      pie(val, labels = sprintf("%s\n%s %%", names(val), sub("\\.", ",", formatC(pc, format = "f", digits = 1))), col = PAL(length(val)), border = "white", main = sprintf("%s — %s", R$lib, R$nom), cex = 0.85)
    } else if (is.null(gn)) {
      par(mar = c(if (max(nchar(names(val))) > 8) 8 else 4.5, 4.5, 3, 1))
      b <- barplot(val, col = "#cfe0f5", border = "#5b7fb0", las = if (max(nchar(names(val))) > 8 || length(val) > 8) 2 else 1, main = sprintf("%s par %s", R$lib, R$nom), ylab = R$lib, ylim = c(0, max(val, 0) * 1.12))
      text(b, val, labels = fmt(val, 5), pos = 3, cex = 0.75)
    } else {
      cn <- nom_colonne(d, p$categorie); g1 <- ch_cat(d[[cn]]); g2 <- ch_cat(d[[gn]])
      vv <- if (R$fct == "effectif") rep(1, nrow(d)) else col_num(d, p$valeur); ok <- !is.na(g1) & !is.na(g2) & is.finite(vv)
      M <- tapply(vv[ok], list(g1[ok], g2[ok]), if (R$fct == "moyenne") mean else sum); M[is.na(M)] <- 0
      M <- M[names(sort(rowSums(M), decreasing = TRUE))[seq_len(min(nrow(M), 12))], , drop = FALSE]
      par(mar = c(if (max(nchar(rownames(M))) > 8) 8 else 4.5, 4.5, 3, 1))
      barplot(t(M), beside = TRUE, col = PAL(ncol(M)), las = if (max(nchar(rownames(M))) > 8) 2 else 1, main = sprintf("%s par %s et %s", R$lib, R$nom, gn), ylab = R$lib, legend.text = colnames(M), args.legend = list(x = "topright", bty = "n", cex = 0.8))
      tabs$Tableau <- data.frame(Categorie = rownames(M), as.data.frame(M, check.names = FALSE), stringsAsFactors = FALSE, check.names = FALSE)
    }
    invisible(dev.off()); figs <- c(figs, f)
    if (is.null(tabs$Tableau)) tabs$Categories <- data.frame(Categorie = names(val), Valeur = as.numeric(val), Part_pct = 100 * as.numeric(val) / tot, stringsAsFactors = FALSE)
    lignes <- c(sprintf("%s — %s : %d catégorie(s), total = %s.", titre, R$lib, length(val), fmt(tot, 8)))
  } else if (type %in% c("bulles", "nuage")) {
    xn <- nom_colonne(d, p$x, "Axe X"); yn <- nom_colonne(d, p$y, "Axe Y"); x <- col_num(d, xn); y <- col_num(d, yn)
    couleur <- if (!is.null(p$couleur) && nzchar(p$couleur)) ch_cat(d[[nom_colonne(d, p$couleur, "Colonne de couleur")]]) else NULL
    if (type == "bulles") { tn <- nom_colonne(d, p$taille, "Taille des bulles"); tz <- col_num(d, tn) } else tz <- rep(1, length(x))
    ok <- is.finite(x) & is.finite(y) & is.finite(tz) & (if (type == "bulles") tz > 0 else TRUE) & (if (is.null(couleur)) TRUE else !is.na(couleur))
    if (sum(ok) < 2) stop("Au moins 2 points complets sont nécessaires.", call. = FALSE)
    x <- x[ok]; y <- y[ok]; tz <- tz[ok]; couleur <- if (is.null(couleur)) NULL else couleur[ok]
    niv <- if (is.null(couleur)) NULL else unique(couleur); pal <- PAL(max(length(niv), 1))
    col_pt <- if (is.null(couleur)) "#2a5d9f" else pal[match(couleur, niv)]
    titre <- if (type == "bulles") "Graphique à bulles" else "Nuage de points"
    f <- lire_png(if (type == "bulles") "bulles.png" else "nuage.png", largeur = 850, hauteur = 600); par(mar = c(4.5, 4.5, 3, 1))
    plot(x, y, type = "n", xlab = xn, ylab = yn, main = sprintf("%s : %s selon %s", titre, yn, xn), xlim = range(x) + c(-1, 1) * diff(range(x)) * (if (type == "bulles") 0.12 else 0.03), ylim = range(y) + c(-1, 1) * diff(range(y)) * (if (type == "bulles") 0.12 else 0.03))
    if (type == "bulles") symbols(x, y, circles = sqrt(tz / max(tz)), inches = 0.3, add = TRUE, bg = paste0(col_pt, "99"), fg = col_pt)
    else points(x, y, pch = 19, cex = 0.9, col = paste0(col_pt, "CC"))
    lignes <- c(sprintf("%s : %d points.", titre, length(x)))
    if (type == "nuage" && vrai(p$ajust) && stats::sd(x) > 0) {
      fit <- stats::lm(y ~ x); abline(fit, col = "#b03030", lwd = 2); ct <- stats::cor.test(x, y)
      legend("topleft", bty = "n", cex = 0.85, text.col = "#b03030", legend = c(sprintf("Y = %s %s %s × X", fmt(coef(fit)[1], 4), if (coef(fit)[2] < 0) "−" else "+", fmt(abs(coef(fit)[2]), 4)), sprintf("r = %s ; R² = %s", fmt(ct$estimate, 4), pct_txt(ct$estimate^2))))
      lignes <- c(lignes, "H0 : ρ = 0 (pas de corrélation linéaire) ; H1 : ρ ≠ 0.", regle(), sprintf("Droite ajustée : Y = %s %s %s × X ; r = %s ; p-value %s", fmt(coef(fit)[1], 5), if (coef(fit)[2] < 0) "−" else "+", fmt(abs(coef(fit)[2]), 5), fmt(ct$estimate, 4), fmt_p(ct$p.value)),
                  decision(ct$p.value, "aucune corrélation linéaire mise en évidence.", "corrélation linéaire significative."))
    }
    if (!is.null(niv)) legend("topright", bty = "n", pch = 19, col = pal, legend = niv, cex = 0.8, title = p$couleur)
    if (type == "bulles") lignes <- c(lignes, sprintf("Surface des bulles proportionnelle à « %s » (de %s à %s).", tn, fmt(min(tz)), fmt(max(tz))))
    invisible(dev.off()); figs <- c(figs, f)
  } else if (type %in% c("individuelles", "intervalles", "boite")) {
    S <- series_graphique(d, p); s <- S$s; s <- s[vapply(s, length, integer(1)) > 0]
    if (!length(s)) stop("Aucune valeur exploitable.", call. = FALSE)
    titre <- c(individuelles = "Diagramme des valeurs individuelles", intervalles = "Graphique d'intervalles", boite = "Boîte à moustaches")[[type]]
    f <- lire_png(c(individuelles = "valeurs_individuelles.png", intervalles = "intervalles.png", boite = "boite_moustaches.png")[[type]], largeur = 800, hauteur = 580)
    long <- max(nchar(names(s))) > 8 || length(s) > 8; par(mar = c(if (long) 8 else 4.5, 4.5, 3, 1))
    ylab <- S$nom_val
    if (type == "boite") {
      boxplot(s, col = "#cfe0f5", border = "#5b7fb0", las = if (long) 2 else 1, ylab = ylab, main = titre); points(seq_along(s), vapply(s, mean, numeric(1)), pch = 18, cex = 1.4, col = "#b03030")
      legend("topright", bty = "n", pch = 18, col = "#b03030", legend = "moyenne", cex = 0.8)
    } else if (type == "individuelles") {
      stripchart(s, vertical = TRUE, method = "jitter", jitter = 0.18, pch = 19, cex = 0.8, col = "#2a5d9f99", las = if (long) 2 else 1, ylab = ylab, main = titre)
      m <- vapply(s, mean, numeric(1)); segments(seq_along(s) - 0.3, m, seq_along(s) + 0.3, m, col = "#b03030", lwd = 3)
      legend("topright", bty = "n", lwd = 3, col = "#b03030", legend = "moyenne", cex = 0.8)
    } else {
      m <- vapply(s, mean, numeric(1)); n <- vapply(s, length, integer(1)); sd_ <- vapply(s, function(v) if (length(v) > 1) stats::sd(v) else NA_real_, numeric(1))
      h <- ifelse(n > 1, stats::qt(1 - ALPHA / 2, pmax(n - 1, 1)) * sd_ / sqrt(n), NA_real_)
      yl <- range(c(m - h, m + h), na.rm = TRUE); if (!all(is.finite(yl))) yl <- range(m); mg <- diff(yl) * 0.15 + 1e-9
      plot(seq_along(s), m, ylim = yl + c(-mg, mg), xlim = c(0.5, length(s) + 0.5), pch = 19, cex = 1.3, col = "#2a5d9f", xaxt = "n", xlab = "", ylab = ylab, main = sprintf("Intervalles de confiance à %s des moyennes", conf_txt()), cex.main = 1.05)
      axis(1, at = seq_along(s), labels = names(s), las = if (long) 2 else 1)
      arrows(seq_along(s), m - h, seq_along(s), m + h, angle = 90, code = 3, length = 0.08, lwd = 2, col = "#2a5d9f")
      tabs$Intervalles <- data.frame(Serie = names(s), n = n, Moyenne = m, Ecart_type = sd_, Borne_inf = m - h, Borne_sup = m + h, stringsAsFactors = FALSE)
    }
    invisible(dev.off()); figs <- c(figs, f)
    tabs$Series <- decrire(s)
    lignes <- c(sprintf("%s : %d série(s) ou groupe(s).", titre, length(s)))
    if (type == "intervalles") lignes <- c(lignes, sprintf("Intervalle de confiance à %s de chaque moyenne (loi t de Student) ; des intervalles qui ne se chevauchent pas suggèrent une différence de moyennes (à confirmer par un test ou une ANOVA).", conf_txt()))
  } else if (type == "probabilite") {
    S <- series_graphique(d, p); s <- S$s; id <- if (isTRUE(p$loi %in% names(LOIS_ID))) p$loi else "normale"; L <- LOIS_ID[[id]]; titre <- sprintf("Diagramme de probabilité — %s", L$lib)
    U <- function(pp, q) switch(id, normale = stats::qnorm(pp), lognormale = stats::qnorm(pp), exponentielle = -log(1 - pp), weibull = log(-log(1 - pp)),
                                logistique = stats::qlogis(pp), gumbel = -log(-log(pp)), gamma = stats::qgamma(pp, shape = q[1]))
    lg <- id %in% c("lognormale", "weibull")
    lignes <- c(sprintf("%s. H0 : les données suivent la loi « %s » ; H1 : elles ne la suivent pas. Test d'Anderson-Darling, p-value par simulation.", titre, L$lib), regle())
    for (j in seq_along(s)[seq_len(min(length(s), 12))]) {
      x <- s[[j]]; nm <- names(s)[j]
      if (length(x) < 8) { avert <- c(avert, list(sprintf("« %s » : au moins 8 valeurs sont nécessaires.", nm))); next }
      r <- ajuster_loi(id, x, 300L, 20240601 + length(x) + j)
      if (!r$ok) { avert <- c(avert, list(sprintf("« %s » : %s.", nm, r$raison))); next }
      xs <- sort(x); pp <- (seq_along(xs) - 0.3) / (length(xs) + 0.4)
      f <- lire_png(sprintf("probabilite_%d.png", j), largeur = 800, hauteur = 580); par(mar = c(4.5, 4.5, 3, 1))
      yy <- U(pp, r$q); g <- seq(min(xs), max(xs), length.out = 200); if (lg) g <- exp(seq(log(min(xs)), log(max(xs)), length.out = 200))
      yref <- U(pmin(pmax(L$cdf(g, r$q), 1e-9), 1 - 1e-9), r$q)
      tick_p <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
      pg <- seq(0.003, 0.997, length.out = 120); bd <- bandes_probabilite(id, x, r$q, pg, graine = 20240601 + j)
      yl <- range(yy, U(c(0.01, 0.99), r$q)); xl <- range(xs)
      if (!is.null(bd)) { vis <- U(pg, r$q) >= yl[1] & U(pg, r$q) <= yl[2]; xl <- range(xl, bd$inf[vis], bd$sup[vis], finite = TRUE) }
      plot(xs, yy, pch = 19, col = "#2a5d9f", cex = 0.9, yaxt = "n", log = if (lg) "x" else "", xlim = xl, ylim = yl, xlab = nm, ylab = "Pourcentage cumulé", main = sprintf("%s — %s", titre, nm))
      axis(2, at = U(tick_p, r$q), labels = c("1", "5", "10", "25", "50", "75", "90", "95", "99"), las = 1)
      abline(h = U(tick_p, r$q), col = "#eeeeee"); lines(g, yref, col = "#b03030", lwd = 2)
      if (!is.null(bd)) { yb <- U(pg, r$q); lines(bd$inf, yb, col = "#b03030", lty = 2, lwd = 1.3); lines(bd$sup, yb, col = "#b03030", lty = 2, lwd = 1.3) }
      points(xs, yy, pch = 19, col = "#2a5d9f", cex = 0.9)
      legend("bottomright", bty = "n", cex = 0.85, legend = c(sprintf("AD = %s", fmt(r$A2, 4)), sprintf("p-value = %s", fmt_p(r$p)), fmt_par(r),
                                                              if (!is.null(bd)) sprintf("Limites de confiance à %s", conf_txt())))
      invisible(dev.off()); figs <- c(figs, f)
      lignes <- c(lignes, sprintf("%s (n = %d) : AD = %s ; p-value %s ; %s. %s", nm, length(x), fmt(r$A2, 4), fmt_p(r$p), fmt_par(r),
                                  decision(r$p, sprintf("la loi « %s » n'est pas rejetée.", L$lib), sprintf("les données ne suivent pas la loi « %s ».", L$lib))))
    }
    if (!length(figs)) stop(paste(unlist(avert), collapse = " "), call. = FALSE)
  } else stop("Type de graphique inconnu.", call. = FALSE)
  list(titre = titre, lignes = lignes, conclusion = lignes[1], tables = tabs, figures = figs, avertissements = avert)
}

# ---- Puissance et taille d'échantillon (à la manière de Minitab) -----------------
# Exactement une des trois grandeurs « taille d'échantillon », « différence » ou « puissance » est laissée vide :
# elle est calculée à partir des deux autres. Une courbe de puissance (puissance en fonction de la différence,
# pour plusieurs tailles d'échantillon) est tracée.
PUISSANCE_TESTS <- c(t1 = "test t à 1 échantillon", tpaire = "test t apparié", t2 = "test t à 2 échantillons",
                     z1 = "test z à 1 échantillon (σ connu)", p1 = "test de 1 proportion (loi binomiale exacte)",
                     p2 = "test de 2 proportions", anova = "ANOVA à 1 facteur")

cmd_puissance <- function(d, p) {
  test <- as.character(p$test); if (!isTRUE(test %in% names(PUISSANCE_TESTS))) stop("Choisir le test à étudier.", call. = FALSE)
  alt <- alt_lire(p$alternative); un <- alt != "two.sided"
  if (test == "anova") { alt <- "two.sided"; un <- FALSE }
  n <- num(p$n); dif <- num(p$difference); pu <- num(p$puissance)
  if (!is.na(pu) && pu > 1) pu <- pu / 100
  vides <- c(n = is.na(n), difference = is.na(dif), puissance = is.na(pu))
  if (sum(vides) != 1) stop("Laisser vide exactement une des trois cases : taille d'échantillon, différence ou puissance (elle sera calculée).", call. = FALSE)
  if (!is.na(pu) && (pu <= ALPHA || pu >= 1)) stop("La puissance doit être comprise entre le risque α et 1 (exclus).", call. = FALSE)
  if (!is.na(dif) && dif == 0) stop("La différence à détecter doit être non nulle.", call. = FALSE)
  nmin <- switch(test, p1 = 1, anova = 2, 2)
  if (!is.na(n) && (n < nmin || abs(n - round(n)) > 1e-8)) stop(sprintf("La taille d'échantillon doit être un entier supérieur ou égal à %d.", nmin), call. = FALSE)
  s <- num(p$ecart_type); p0 <- num(p$p0); k <- num(p$k)
  if (test %in% c("t1", "tpaire", "t2", "z1", "anova")) val_pos(s, "L'écart-type")
  if (test %in% c("p1", "p2")) { if (is.na(p0) || p0 <= 0 || p0 >= 1) stop("La proportion de référence doit être strictement comprise entre 0 et 1.", call. = FALSE) }
  if (test == "anova") { if (is.na(k) || k < 2 || abs(k - round(k)) > 1e-8) stop("Le nombre de niveaux doit être un entier supérieur ou égal à 2.", call. = FALSE) }
  # sens de la différence (proportions : p = p0 + différence ; pour « inférieur » la différence est négative)
  sens <- if (alt == "less") -1 else 1
  if (test %in% c("p1", "p2") && !is.na(dif)) {
    dif <- sens * abs(dif) * (if (alt == "two.sided" && dif < 0) -1 else 1)
    if (p0 + dif <= 0 || p0 + dif >= 1) stop("La proportion de référence + la différence doit rester strictement comprise entre 0 et 1.", call. = FALSE)
  }
  a <- ALPHA
  tt <- c(t1 = "one.sample", tpaire = "paired", t2 = "two.sample")
  pw <- function(n, dd) {                       # puissance pour n (vectorisé sur n pour p1) et une différence dd
    ad <- abs(dd)
    switch(test,
      t1 = , tpaire = , t2 = vapply(n, function(m) stats::power.t.test(n = m, delta = ad, sd = s, sig.level = a, type = tt[[test]], alternative = if (un) "one.sided" else "two.sided")$power, 0),
      z1 = { z <- ad * sqrt(n) / s; if (un) stats::pnorm(-stats::qnorm(1 - a) + z) else stats::pnorm(-stats::qnorm(1 - a / 2) + z) + stats::pnorm(-stats::qnorm(1 - a / 2) - z) },
      p2 = vapply(n, function(m) stats::power.prop.test(n = m, p1 = p0, p2 = p0 + dd, sig.level = a, alternative = if (un) "one.sided" else "two.sided")$power, 0),
      anova = { mu <- c(-ad / 2, ad / 2, rep(0, k - 2)); vapply(n, function(m) stats::power.anova.test(groups = k, n = m, between.var = stats::var(mu), within.var = s^2, sig.level = a)$power, 0) },
      p1 = { pr <- p0 + dd
        if (alt == "greater") { cu <- stats::qbinom(1 - a, n, p0) + 1; 1 - stats::pbinom(cu - 1, n, pr) }
        else if (alt == "less") { x <- stats::qbinom(a, n, p0); cl <- ifelse(stats::pbinom(x, n, p0) <= a + 1e-12, x, x - 1); stats::pbinom(cl, n, pr) }
        else { cu <- stats::qbinom(1 - a / 2, n, p0) + 1; x <- stats::qbinom(a / 2, n, p0); cl <- ifelse(stats::pbinom(x, n, p0) <= a / 2 + 1e-12, x, x - 1)
               stats::pbinom(cl, n, pr) + 1 - stats::pbinom(cu - 1, n, pr) } })
  }
  dmax_p <- function() if (alt == "less") -(p0 - 1e-6) else (1 - p0 - 1e-6)
  # résolution
  if (vides[["puissance"]]) {
    pu <- as.numeric(pw(n, dif))
  } else if (vides[["n"]]) {
    cible <- pu; dd <- dif
    if (test == "p1") {
      N <- 1:100000; pv <- pw(N, dd); i <- which(pv >= cible)
      if (!length(i)) stop("La taille d'échantillon nécessaire dépasse 100 000 : la différence à détecter est trop petite.", call. = FALSE)
      n <- N[i[1]]
    } else {
      hi <- max(nmin, 4); while (pw(hi, dd) < cible) { hi <- hi * 2; if (hi > 1e7) stop("La taille d'échantillon nécessaire dépasse 10 millions : la différence à détecter est trop petite.", call. = FALSE) }
      lo <- nmin - 1
      while (hi - lo > 1) { m <- (lo + hi) %/% 2; if (pw(m, dd) >= cible) hi <- m else lo <- m }
      n <- hi
    }
    pu <- as.numeric(pw(n, dd))
  } else {
    cible <- pu
    if (test %in% c("p1", "p2")) { lim <- dmax_p(); f <- function(x) as.numeric(pw(n, x)) - cible
      if (f(lim) < 0) stop("Cette puissance est inatteignable avec cette taille d'échantillon.", call. = FALSE)
      dif <- stats::uniroot(f, sort(c(sens * 1e-6, lim)), tol = 1e-10)$root
    } else { f <- function(x) as.numeric(pw(n, x)) - cible; hi <- 50 * s
      if (f(hi) < 0) stop("Cette puissance est inatteignable avec cette taille d'échantillon.", call. = FALSE)
      dif <- stats::uniroot(f, c(1e-8 * s, hi), tol = 1e-10)$root }
  }
  ad <- abs(dif)
  # tailles d'échantillon de la courbe
  nc <- paste(as.character(p$n_courbe), collapse = " ")
  ns <- suppressWarnings(as.numeric(strsplit(trimws(gsub("[;,]", " ", nc)), "[[:space:]]+")[[1]]))
  ns <- ns[!is.na(ns) & ns >= nmin]; ns <- unique(round(ns))
  if (!length(ns)) ns <- unique(pmax(nmin, round(n * c(0.5, 1, 1.5))))
  ns <- sort(unique(c(ns)))
  # texte
  cote <- switch(alt, greater = "unilatéral (supérieur)", less = "unilatéral (inférieur)", "bilatéral")
  eff <- if (test == "t2") sprintf("par groupe : %d", as.integer(n)) else sprintf("%d", as.integer(n))
  diftxt <- switch(test, t1 = "Différence à détecter (écart entre la moyenne réelle et la valeur de référence)", tpaire = "Différence à détecter (moyenne des différences appariées)",
                   t2 = "Différence à détecter entre les deux moyennes", z1 = "Différence à détecter (écart entre la moyenne réelle et la valeur de référence)",
                   p1 = "Différence à détecter (proportion réelle − proportion de référence)", p2 = "Différence à détecter (proportion du groupe 2 − proportion de référence)",
                   anova = "Différence à détecter (écart maximal entre deux moyennes)")
  lignes <- c(sprintf("Analyse de puissance : %s", PUISSANCE_TESTS[[test]]),
              sprintf("Risque α : %s ; test %s", fmt(a), cote))
  if (test %in% c("t1", "tpaire", "t2", "z1", "anova")) lignes <- c(lignes, sprintf("Écart-type supposé : %s", fmt(s)))
  if (test %in% c("p1", "p2")) lignes <- c(lignes, sprintf("Proportion de référence : %s", fmt(p0)))
  if (test == "anova") lignes <- c(lignes, sprintf("Nombre de niveaux du facteur : %d", as.integer(k)))
  lignes <- c(lignes, "",
              sprintf("%s : %s", diftxt, fmt(dif)),
              sprintf("Taille d'échantillon (%s)", eff),
              sprintf("Puissance : %s (risque β = %s)", fmt(pu), fmt(1 - pu)))
  if (test == "p1") {
    ca <- if (alt == "greater") { cu <- stats::qbinom(1 - a, n, p0) + 1; 1 - stats::pbinom(cu - 1, n, p0) }
          else if (alt == "less") { x <- stats::qbinom(a, n, p0); cl <- if (stats::pbinom(x, n, p0) <= a + 1e-12) x else x - 1; stats::pbinom(cl, n, p0) }
          else { cu <- stats::qbinom(1 - a / 2, n, p0) + 1; x <- stats::qbinom(a / 2, n, p0); cl <- if (stats::pbinom(x, n, p0) <= a / 2 + 1e-12) x else x - 1; stats::pbinom(cl, n, p0) + 1 - stats::pbinom(cu - 1, n, p0) }
    lignes <- c(lignes, sprintf("Risque α réellement atteint (loi binomiale discrète) : %s", fmt(ca)))
  }
  what <- if (vides[["n"]]) sprintf("Taille d'échantillon nécessaire : %d", as.integer(n)) else if (vides[["difference"]]) sprintf("Plus petite différence détectable : %s", fmt(ad)) else sprintf("Puissance obtenue : %s", fmt(pu))
  lignes <- c(lignes, "", what)
  # tableau : puissance pour chaque taille de la courbe à la différence étudiée
  tab <- data.frame(Taille = ns, Difference = dif, Puissance = as.numeric(vapply(ns, function(m) pw(m, dif), 0)), stringsAsFactors = FALSE)
  # figure
  fig <- lire_png("puissance.png", largeur = 800, hauteur = 560)
  par(mar = c(4.5, 4.8, 3.2, 1))
  xmax <- if (test %in% c("p1", "p2")) min(2 * ad, abs(dmax_p())) else 2 * ad
  xs <- seq(0, xmax, length.out = 200)[-1]
  cols <- c("#1f6fb5", "#c0392b", "#2e8b57", "#8e44ad", "#d68910", "#555555")[seq_along(ns) %% 6 + 1]
  plot(NA, xlim = c(0, xmax), ylim = c(0, 1), xlab = "Différence", ylab = "Puissance", main = "Courbe de puissance")
  grid(col = "#e5e5e5")
  for (i in seq_along(ns)) lines(xs, vapply(xs, function(x) as.numeric(pw(ns[i], sens * x)), 0), col = cols[i], lwd = 2)
  abline(h = pu, lty = 2, col = "#888888"); abline(v = ad, lty = 3, col = "#888888")
  abline(h = a, lty = 3, col = "#bbbbbb")
  points(ad, pu, pch = 19, cex = 1.3)
  legend("bottomright", bty = "n", lwd = 2, col = cols, cex = 0.85, title = if (test == "t2") "Taille par groupe" else "Taille d'échantillon", legend = ns)
  invisible(dev.off())
  list(titre = "Puissance et taille d'échantillon", lignes = lignes, conclusion = what, tables = list(Puissance = tab), tables_libres = "Puissance", figures = c(fig),
       avertissements = list("La puissance est la probabilité de mettre en évidence une différence réelle de la taille indiquée. Elle dépend du risque α, de la dispersion (écart-type) et de la taille d'échantillon ; la courbe montre son évolution avec la différence."))
}
