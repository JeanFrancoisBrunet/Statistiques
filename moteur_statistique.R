# moteur_statistique.R
# Moteur de calcul statistique pour Raspberry Pi 5
# Compatible avec R de base ; les fonctions de graphiques utilisent les fonctions R natives.
# Les fonctions renvoient des listes/data.frames afin d'être réutilisées dans Shiny ou en script.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

verifier_numerique <- function(x, nom = "x", minimum = 1L) {
  if (!is.numeric(x)) stop(sprintf("'%s' doit être numérique.", nom), call. = FALSE)
  x <- x[is.finite(x)]
  if (length(x) < minimum) stop(sprintf("Il faut au moins %d valeur(s) numérique(s) valide(s) pour '%s'.", minimum, nom), call. = FALSE)
  x
}

resume_serie <- function(x, nom = "Série") {
  x <- verifier_numerique(x, nom, 1L)
  n <- length(x)
  sdv <- if (n >= 2) stats::sd(x) else NA_real_
  p_norm <- if (n >= 3 && n <= 5000 && length(unique(x)) > 2 && sdv > 0) {
    tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
  } else NA_real_
  interpretation <- if (is.na(p_norm)) {
    "Test de Shapiro-Wilk non calculable (n < 3, n > 5000, données constantes ou trop peu de valeurs distinctes)."
  } else if (p_norm < 0.05) {
    "La normalité est rejetée au seuil de 5 %."
  } else {
    "La normalité n'est pas rejetée au seuil de 5 % (cela ne prouve pas la normalité)."
  }
  data.frame(
    Serie = nom, n = n, Moyenne = mean(x), Mediane = stats::median(x),
    Ecart_type = sdv, Variance = if (n >= 2) stats::var(x) else NA_real_,
    Minimum = min(x), Q1 = unname(stats::quantile(x, .25)),
    Q3 = unname(stats::quantile(x, .75)), Maximum = max(x),
    P_Shapiro_Wilk = p_norm, Interpretation_normalite = interpretation,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

resume_multi_series <- function(donnees) {
  if (is.data.frame(donnees)) {
    colonnes <- names(donnees)[vapply(donnees, is.numeric, logical(1))]
    if (!length(colonnes)) stop("Aucune colonne numérique trouvée.", call. = FALSE)
    do.call(rbind, lapply(colonnes, function(nm) resume_serie(donnees[[nm]], nm)))
  } else if (is.list(donnees)) {
    do.call(rbind, lapply(names(donnees) %||% paste0("Série_", seq_along(donnees)),
                          function(nm) resume_serie(donnees[[nm]], nm)))
  } else stop("Fournir un data.frame ou une liste de séries numériques.", call. = FALSE)
}

test_t <- function(x, y = NULL, mu = 0, paired = FALSE, alternative = "two.sided",
                   var_equal = FALSE, conf_level = 0.95) {
  if (!alternative %in% c("two.sided", "less", "greater")) stop("Alternative invalide.", call. = FALSE)
  if (!is.null(y) && paired) {
    # Appariement : on retire les PAIRES incomplètes (et non les valeurs séparément, ce qui désalignerait les paires).
    if (!is.numeric(x) || !is.numeric(y)) stop("x et y doivent être numériques.", call. = FALSE)
    if (length(x) != length(y)) stop("Pour un test apparié, x et y doivent avoir la même longueur.", call. = FALSE)
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 2L) stop("Il faut au moins 2 paires complètes pour un test apparié.", call. = FALSE)
  } else {
    x <- verifier_numerique(x, "x", 2L)
  }
  if (is.null(y)) {
    res <- stats::t.test(x, mu = mu, alternative = alternative, conf.level = conf_level)
    type <- "t à un échantillon"
  } else {
    if (!paired) y <- verifier_numerique(y, "y", 2L)
    res <- stats::t.test(x, y, paired = paired, var.equal = var_equal,
                         alternative = alternative, conf.level = conf_level)
    type <- if (paired) "t apparié" else if (var_equal) "t indépendant (variances supposées égales)" else "t indépendant de Welch"
  }
  list(type = type, statistic = unname(res$statistic), ddl = unname(res$parameter),
       p_value = res$p.value, intervalle_confiance = unname(res$conf.int),
       estimation = res$estimate, conclusion_5pct = if (res$p.value < 0.05)
         "Différence statistiquement significative au seuil de 5 %."
       else "Pas de différence statistiquement significative mise en évidence au seuil de 5 %.",
       objet_test = res)
}

test_anova <- function(donnees, valeur, groupe) {
  if (!all(c(valeur, groupe) %in% names(donnees))) stop("Colonnes valeur/groupe introuvables.", call. = FALSE)
  d <- donnees[, c(valeur, groupe), drop = FALSE]
  names(d) <- c("valeur", "groupe")
  d <- d[is.finite(d$valeur) & !is.na(d$groupe), , drop = FALSE]
  d$groupe <- as.factor(d$groupe)
  if (nlevels(d$groupe) < 2) stop("L'ANOVA exige au moins deux groupes.", call. = FALSE)
  fit <- stats::aov(valeur ~ groupe, data = d)
  tab <- summary(fit)[[1]]
  list(table = as.data.frame(tab), modele = fit,
       p_value = tab[["Pr(>F)"]][1],
       conclusion_5pct = if (tab[["Pr(>F)"]][1] < 0.05)
         "Au moins une moyenne de groupe diffère statistiquement (ANOVA, seuil 5 %)."
       else "Aucune différence de moyenne n'est mise en évidence par l'ANOVA au seuil de 5 %.",
       avertissement = "Vérifier indépendance, normalité approximative des résidus et homogénéité des variances. Une ANOVA significative ne précise pas quels groupes diffèrent.")
}

indices_capabilite <- function(x, LSL = NA_real_, USL = NA_real_, cible = NA_real_,
                               sigma_within = NA_real_) {
  x <- verifier_numerique(x, "mesures", 2L)
  if (is.na(LSL) && is.na(USL)) stop("Saisir au moins une limite de spécification (LSL ou USL).", call. = FALSE)
  if (!is.na(LSL) && !is.na(USL) && LSL >= USL) stop("LSL doit être strictement inférieur à USL.", call. = FALSE)
  mu <- mean(x)
  sigma_overall <- stats::sd(x)
  sigma_w <- if (!is.na(sigma_within) && sigma_within > 0) sigma_within else NA_real_
  if (is.na(sigma_w)) {
    warning("Sigma intra-sous-groupe non fourni : Cm/Cmk/Cp/Cpk ne sont pas calculés. Pp/Ppk utilisent l'écart-type global.", call. = FALSE)
  }
  calc_cp <- function(sig) if (is.na(sig) || sig <= 0 || is.na(LSL) || is.na(USL)) NA_real_ else (USL - LSL) / (6 * sig)
  calc_cpk <- function(sig) {
    if (is.na(sig) || sig <= 0) return(NA_real_)
    vals <- c(if (!is.na(USL)) (USL - mu) / (3 * sig) else NA_real_,
              if (!is.na(LSL)) (mu - LSL) / (3 * sig) else NA_real_)
    min(vals, na.rm = TRUE)
  }
  # Cm/Cmk sont souvent utilisés pour études machine courtes ; calculés ici avec sigma intra fourni.
  Cm <- calc_cp(sigma_w)
  Cmk <- calc_cpk(sigma_w)
  Cp <- calc_cp(sigma_w)
  Cpk <- calc_cpk(sigma_w)
  Pp <- calc_cp(sigma_overall)
  Ppk <- calc_cpk(sigma_overall)
  ppm_below <- if (is.na(LSL)) NA_real_ else stats::pnorm(LSL, mean = mu, sd = sigma_overall) * 1e6
  ppm_above <- if (is.na(USL)) NA_real_ else (1 - stats::pnorm(USL, mean = mu, sd = sigma_overall)) * 1e6
  data.frame(
    Effectif = length(x), Moyenne = mu, Sigma_global = sigma_overall,
    Sigma_intra_fourni = sigma_w, LSL = LSL, Cible = cible, USL = USL,
    Cm = Cm, Cmk = Cmk, Cp = Cp, Cpk = Cpk, Pp = Pp, Ppk = Ppk,
    PPM_estimes_sous_LSL = ppm_below, PPM_estimes_sur_USL = ppm_above,
    stringsAsFactors = FALSE
  )
}

kappa_agreement <- function(reference, evaluateur, poids = c("unweighted", "squared")) {
  if (length(reference) != length(evaluateur)) stop("Les deux évaluations doivent avoir la même longueur.", call. = FALSE)
  ok <- !is.na(reference) & !is.na(evaluateur)
  reference <- as.factor(reference[ok])
  evaluateur <- as.factor(evaluateur[ok])
  if (!length(reference)) stop("Aucune paire valide.", call. = FALSE)
  niveaux <- union(levels(reference), levels(evaluateur))
  reference <- factor(reference, levels = niveaux)
  evaluateur <- factor(evaluateur, levels = niveaux)
  tab <- table(reference, evaluateur)
  n <- sum(tab)
  po <- sum(diag(tab)) / n
  marg_l <- rowSums(tab) / n
  marg_c <- colSums(tab) / n
  pe <- sum(marg_l * marg_c)
  k <- if (abs(1 - pe) < .Machine$double.eps) NA_real_ else (po - pe) / (1 - pe)
  list(effectif = n, matrice = tab, accord_observe = po, accord_attendu = pe,
       kappa_cohen = k,
       interpretation = if (is.na(k)) "Kappa non défini (accord attendu égal à 1)." else if (k < 0) "Accord inférieur à celui attendu par hasard." else if (k < .20) "Accord faible." else if (k < .40) "Accord léger." else if (k < .60) "Accord modéré." else if (k < .80) "Accord important." else "Accord très important.",
       note = "Kappa non pondéré. Pour des catégories ordinales, une version pondérée peut être préférable.")
}

donnees_longues <- function(donnees) {
  nums <- names(donnees)[vapply(donnees, is.numeric, logical(1))]
  if (length(nums) < 1) stop("Aucune colonne numérique.", call. = FALSE)
  data.frame(Valeur = unlist(donnees[nums], use.names = FALSE),
             Serie = factor(rep(nums, vapply(donnees[nums], function(z) sum(is.finite(z)), integer(1))),
                            levels = nums))
}

exporter_resultats_csv <- function(objet, fichier) {
  utils::write.csv(objet, fichier, row.names = FALSE, na = "")
  invisible(fichier)
}
