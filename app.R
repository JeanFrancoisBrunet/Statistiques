# app.R
# Interface Shiny mobile-first pour Samsung S23 / Raspberry Pi 5.
# Lancer dans le dossier contenant app.R et moteur_statistique.R :
#   R -e "shiny::runApp(host='0.0.0.0', port=3838)"
# Depuis le S23, ouvrir http://ADRESSE_IP_DU_PI:3838
# Dépendances : shiny, DT, DBI, RSQLite, ggplot2, jsonlite
# Installer une fois : install.packages(c("shiny","DT","DBI","RSQLite","ggplot2","jsonlite"))
# Les analyses avancées (tests t, Z, Poisson, estimation, identification de loi, régression, séries, ANOVA, test F, khi², cartes de contrôle, R&R, graphiques) utilisent le même moteur
# que l'application de bureau : cli_statistique.R et commandes_avancees.R doivent être dans le même dossier.

required <- c("shiny", "DT", "DBI", "RSQLite", "ggplot2", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Packages R manquants : ", paste(missing, collapse = ", "),
                          ". Installez-les avec install.packages().")
source("moteur_statistique.R", local = TRUE)

# ---- Moteur partagé avec l'application de bureau (cli_statistique.R) -------------------------
LIBELLES_WEB <- c(Ecart_type = "Écart-type", Etendue = "Étendue", Ecart = "Écart", Sous_groupe = "Sous-groupe",
                  Modalite = "Modalité", Observe = "Observé", Theorique = "Théorique", Serie = "Série",
                  Composante_variance = "Composante de variance", Pct_contribution = "% contribution",
                  Pct_variation_etude = "% variation d'étude", Pct_tolerance = "% tolérance")
libelle_colonne <- function(n) { n <- unname(ifelse(n %in% names(LIBELLES_WEB), LIBELLES_WEB[n], n)); gsub("_", " ", n, fixed = TRUE) }


# ---- Type « date » (comme l'application de bureau) : dates stockées en texte ISO « AAAA-MM-JJ [hh:mm[:ss]] » ------------
MANQUANTS_DATE <- c("", "na", "nan", "n/a", "nd")
RE_DATE_ISO <- "^(\\d{4})-(\\d{1,2})-(\\d{1,2})(?:[ T]+(\\d{1,2}):(\\d{2})(?::(\\d{2}))?(?:[.,]\\d+)?)?$"
RE_DATE_JMA <- "^(\\d{1,2})[/.-](\\d{1,2})[/.-](\\d{2}|\\d{4})(?:[ T]+(\\d{1,2})[:h](\\d{2})(?::(\\d{2}))?)?$"
analyser_date <- function(x, jour_d_abord = TRUE) {      # liste (a, mo, j, h, mi, s) ou NULL
  x <- trimws(x)
  g <- function(m, i) { s <- substr(x, m[i], m[i] + attr(m, "match.length")[i] - 1L); s }
  r <- regexec(RE_DATE_ISO, x, perl = TRUE)[[1]]
  if (r[1] != -1) {
    z <- regmatches(x, list(r))[[1]]; v <- suppressWarnings(as.integer(z[-1])); v[is.na(v)] <- 0L
    a <- v[1]; mo <- v[2]; j <- v[3]
  } else {
    r <- regexec(RE_DATE_JMA, x, perl = TRUE)[[1]]
    if (r[1] == -1) return(NULL)
    z <- regmatches(x, list(r))[[1]]; zz <- z[-1]
    v <- suppressWarnings(as.integer(zz)); v[is.na(v)] <- 0L
    a <- v[3] + if (nchar(zz[3]) == 4) 0L else if (v[3] < 70) 2000L else 1900L
    if (jour_d_abord) { j <- v[1]; mo <- v[2] } else { j <- v[2]; mo <- v[1] }
  }
  h <- v[4]; mi <- v[5]; s <- v[6]
  if (is.na(suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", a, mo, j), optional = TRUE))) || h > 23 || mi > 59 || s > 59) return(NULL)
  list(a = a, mo = mo, j = j, h = h, mi = mi, s = s)
}
colonne_date <- function(v) {                             # vecteur ISO (NA si manquant) si toute la colonne est faite de dates, sinon NULL
  v <- as.character(v)
  pleine <- !is.na(v) & !(tolower(trimws(v)) %in% MANQUANTS_DATE)
  if (!any(pleine)) return(NULL)
  for (jf in c(TRUE, FALSE)) {
    res <- lapply(v[pleine], analyser_date, jour_d_abord = jf)
    if (all(!vapply(res, is.null, logical(1)))) {
      heure <- any(vapply(res, function(z) z$h > 0 || z$mi > 0 || z$s > 0, logical(1)))
      sec <- any(vapply(res, function(z) z$s > 0, logical(1)))
      f <- vapply(res, function(z) {
        b <- sprintf("%04d-%02d-%02d", z$a, z$mo, z$j)
        if (sec) sprintf("%s %02d:%02d:%02d", b, z$h, z$mi, z$s) else if (heure) sprintf("%s %02d:%02d", b, z$h, z$mi) else b
      }, "")
      out <- rep(NA_character_, length(v)); out[pleine] <- f
      return(out)
    }
  }
  NULL
}
detecter_dates <- function(d) {                           # colonnes de texte dont toutes les valeurs sont des dates -> texte ISO
  dates <- character(0)
  for (nm in names(d)) if (is.character(d[[nm]])) {
    z <- colonne_date(d[[nm]])
    if (!is.null(z)) { d[[nm]] <- z; dates <- c(dates, nm) }
  }
  list(d = d, dates = dates)
}
date_affichee <- function(v) {                            # « AAAA-MM-JJ hh:mm » -> « JJ/MM/AAAA hh:mm »
  ok <- !is.na(v) & grepl("^\\d{4}-\\d{2}-\\d{2}", v)
  v[ok] <- paste0(substr(v[ok], 9, 10), "/", substr(v[ok], 6, 7), "/", substr(v[ok], 1, 4), substring(v[ok], 11))
  v
}
PERIODES_DATE <- c("Jour" = "jour", "Mois" = "mois", "Année" = "annee")
etiquettes_groupe <- function(d, colonne, dates = character(0), periode = "jour") {
  v <- d[[colonne]]
  if (colonne %in% dates) { n <- c(annee = 4, mois = 7, jour = 10)[[if (periode %in% c("annee", "mois")) periode else "jour"]]; substr(as.character(v), 1, n) }
  else as.character(v)
}

executer_cli <- function(data, commande, params, alpha = 0.05, groupe = NULL) {
  if (is.null(data)) stop("Aucune donnée chargée.")
  script <- normalizePath("cli_statistique.R", mustWork = FALSE)
  if (!file.exists(script) || !file.exists("commandes_avancees.R"))
    stop("cli_statistique.R et commandes_avancees.R doivent se trouver dans le dossier de l'application.")
  rs <- file.path(R.home("bin"), "Rscript")
  dossier <- tempfile("stat_web_"); dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  if (!is.null(groupe)) {              # analyse groupe par groupe : colonne technique « __groupe__ » lue par le moteur
    data[["__groupe__"]] <- if (is.null(groupe$etiquettes)) as.character(data[[groupe$colonne]]) else groupe$etiquettes
    params <- c(params, list(groupes = as.list(groupe$valeurs), groupe_nom = if (is.null(groupe$nom)) groupe$colonne else groupe$nom))
  }
  utils::write.csv(data, file.path(dossier, "donnees.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  writeLines(jsonlite::toJSON(c(list(commande = commande), params, list(echelle = 1, alpha_test = alpha)),
                              auto_unbox = TRUE, na = "null", null = "null"),
             file.path(dossier, "params.json"), useBytes = TRUE)
  sortie <- system2(rs, c(shQuote(script), shQuote(file.path(dossier, "donnees.csv")), shQuote(dossier)),
                    stdout = TRUE, stderr = TRUE, env = c("LC_ALL=C.UTF-8", "LANG=C.UTF-8"))
  f <- file.path(dossier, "resultat.json")
  if (!file.exists(f)) stop("Le calcul n'a produit aucun résultat. ", paste(utils::tail(sortie, 1), collapse = ""))
  res <- jsonlite::fromJSON(f, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  if (!isTRUE(res$ok)) stop(if (is.null(res$erreur)) "Erreur de calcul." else res$erreur)
  res$images <- lapply(as.character(unlist(res$figures)), function(n) {
    g <- file.path(dossier, n)
    if (file.exists(g)) jsonlite::base64_enc(readBin(g, "raw", file.size(g))) else NULL
  })
  res
}

formate_cellule <- function(v) {
  if (is.null(v)) return("")
  if (is.numeric(v)) return(format(signif(v, 5), decimal.mark = ",", trim = TRUE))
  as.character(v)
}
table_html <- function(t, libre = FALSE) {
  noms <- names(t)
  n <- max(lengths(t), 0)
  tags$div(style = "overflow-x:auto;",
    tags$table(class = "table table-striped table-condensed", style = "width:auto;",
      tags$thead(tags$tr(lapply(noms, function(k) tags$th(if (libre) k else libelle_colonne(k))))),
      tags$tbody(lapply(seq_len(n), function(i)
        tags$tr(lapply(noms, function(k) { v <- t[[k]]; tags$td(formate_cellule(if (length(v) >= i) v[[i]] else NULL)) }))))))
}
rendre_resultat <- function(r) {
  if (!is.null(r$erreur_web)) return(div(class = "stat-note", paste("Erreur :", r$erreur_web)))
  tagList(
    if (!is.null(r$titre)) h4(r$titre),
    if (!is.null(r$conclusion)) div(class = "stat-note", strong(r$conclusion)),
    if (length(r$lignes)) tags$pre(paste(unlist(r$lignes), collapse = "\n")),
    lapply(names(r$tables), function(nm) tagList(h5(gsub("_", " ", nm, fixed = TRUE)),
                                                table_html(r$tables[[nm]], libre = nm %in% unlist(r$tables_libres)))),
    lapply(r$images, function(b) if (!is.null(b)) tags$img(src = paste0("data:image/png;base64,", b), style = "max-width:100%;height:auto;margin:8px 0;")),
    lapply(unlist(r$avertissements), function(a) tags$p(class = "help-block", paste("Attention :", a)))
  )
}

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(ggplot2)

dir.create("data", showWarnings = FALSE)
dir.create("illustrations", showWarnings = FALSE)
addResourcePath("illus", normalizePath("illustrations"))
db <- DBI::dbConnect(RSQLite::SQLite(), "data/statistiques_pi5.sqlite")
DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS projets (id INTEGER PRIMARY KEY AUTOINCREMENT, nom TEXT NOT NULL, date_creation TEXT NOT NULL)")
DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS jeux_donnees (id INTEGER PRIMARY KEY AUTOINCREMENT, projet_id INTEGER, nom TEXT, csv TEXT, date_creation TEXT)")
onStop(function() DBI::dbDisconnect(db))

LOIS_GEN <- list(
  list(code = "aleatoire", lib = "Aléatoire (random)", desc = "Nombres tirés au hasard entre deux bornes, sans loi particulière : toutes les valeurs sont équiprobables (mettre 0 décimale pour obtenir des entiers).", params = list(list(cle = "min", lib = "Minimum", defaut = "0"), list(cle = "max", lib = "Maximum", defaut = "100"))),
  list(code = "normale", lib = "Normale (Gauss)", desc = "Distribution la plus importante en statistique : décrit des phénomènes naturels dont les valeurs se concentrent autour d'une moyenne, avec une symétrie parfaite.", params = list(list(cle = "moyenne", lib = "Moyenne", defaut = "0"), list(cle = "ecart_type", lib = "Écart-type", defaut = "1"))),
  list(code = "uniforme", lib = "Uniforme", desc = "Toutes les valeurs comprises entre deux bornes sont équiprobables.", params = list(list(cle = "min", lib = "Minimum", defaut = "0"), list(cle = "max", lib = "Maximum", defaut = "1"))),
  list(code = "poisson", lib = "Poisson", desc = "Nombre d'événements qui se produisent dans un intervalle de temps ou d'espace donné, quand ils arrivent de manière aléatoire et indépendante.", params = list(list(cle = "lambda", lib = "Moyenne λ", defaut = "3"))),
  list(code = "student", lib = "Student (t)", desc = "Distribution d'une moyenne d'échantillon quand l'écart-type est estimé à partir de données limitées.", params = list(list(cle = "df", lib = "Degrés de liberté", defaut = "10"))),
  list(code = "fisher", lib = "Fisher-Snedecor (F)", desc = "Rapport de deux variances estimées à partir d'échantillons.", params = list(list(cle = "df1", lib = "ddl du numérateur", defaut = "5"), list(cle = "df2", lib = "ddl du dénominateur", defaut = "10"))),
  list(code = "khi2", lib = "Khi²", desc = "Somme des carrés de variables normales centrées réduites indépendantes.", params = list(list(cle = "df", lib = "Degrés de liberté", defaut = "4"))),
  list(code = "binomiale", lib = "Binomiale", desc = "Nombre de succès obtenus lors d'un nombre fixe d'expériences indépendantes.", params = list(list(cle = "essais", lib = "Nombre d'essais", defaut = "10"), list(cle = "p", lib = "Probabilité de succès", defaut = "0,5"))),
  list(code = "geometrique", lib = "Géométrique", desc = "Nombre d'essais nécessaires pour obtenir le premier succès dans une série d'expériences indépendantes (au moins 1).", params = list(list(cle = "p", lib = "Probabilité de succès", defaut = "0,2"))),
  list(code = "hypergeometrique", lib = "Hypergéométrique", desc = "Nombre de succès obtenus lors d'un tirage sans remise dans une population finie.", params = list(list(cle = "N", lib = "Taille de la population", defaut = "50"), list(cle = "K", lib = "Succès dans la population", defaut = "20"), list(cle = "m", lib = "Taille du tirage", defaut = "10"))),
  list(code = "exponentielle", lib = "Exponentielle", desc = "Temps d'attente avant qu'un événement ne se produise dans un processus continu.", params = list(list(cle = "moyenne", lib = "Moyenne (1 / taux)", defaut = "1"))),
  list(code = "gamma", lib = "Gamma", desc = "Somme de temps d'attente exponentiels ; modélise des durées et des quantités positives asymétriques.", params = list(list(cle = "forme", lib = "Forme", defaut = "2"), list(cle = "echelle", lib = "Échelle", defaut = "1"))),
  list(code = "logistique", lib = "Logistique", desc = "Proche de la loi normale, avec des queues plus lourdes.", params = list(list(cle = "position", lib = "Position", defaut = "0"), list(cle = "echelle", lib = "Échelle", defaut = "1"))),
  list(code = "weibull", lib = "Weibull", desc = "Durées de vie et de défaillance : la forme règle le taux de panne (décroissant, constant ou croissant).", params = list(list(cle = "forme", lib = "Forme", defaut = "1,5"), list(cle = "echelle", lib = "Échelle", defaut = "1"))),
  list(code = "lognormale", lib = "Log-normale", desc = "Variable dont le logarithme suit une loi normale : valeurs positives, asymétrie à droite.", params = list(list(cle = "mu", lib = "Moyenne du logarithme", defaut = "0"), list(cle = "sigma", lib = "Écart-type du logarithme", defaut = "1"))),
  list(code = "beta", lib = "Bêta", desc = "Valeurs comprises entre 0 et 1 (proportions, taux).", params = list(list(cle = "a", lib = "Forme a", defaut = "2"), list(cle = "b", lib = "Forme b", defaut = "5")))
)
ILLUS_TITRES <- c("demarche_6sigma" = "Démarche 6 Sigma", "dpmo" = "Défauts par Million d'Opportunités", "multi_variations" = "Variations multiples", "stat_descriptive" = "Statistiques Descriptives", "stat_inferentielle" = "Statistiques Inférentielles", "interpreter_distribution" = "Interpréter une distribution", "moyenne_ecart_type" = "Moyenne et écart-type", "loi_normale" = "Loi normale", "droite_henry" = "Interpréter une droite d'Henry", "boite_moustaches" = "Boîte à moustaches", "pareto" = "Pareto", "carte_controle" = "Cartes de Contrôle", "tests_hypotheses" = "Tests d'hypothèses", "risques_alpha_beta" = "Risques α et β", "khi2" = "Test du khi²", "test_z" = "Test Z", "test_t_1" = "Test t à 1 échantillon", "test_t_2" = "Test t à 2 échantillons", "test_t_apparie" = "Test t apparié", "bartlett" = "Test de Bartlett", "levene" = "Test de Levene", "mann_whitney" = "Test de Mann-Whitney", "kruskal_wallis" = "Test de Kruskal-Wallis")

# ---- Onglets décrits par des champs (même contenu que l'application de bureau) -------------------
# type : choix | col | cols | liste | num | case ; si : condition écrite avec v$champ (valable en R et en JavaScript)
ALT3 <- c("Différent de" = "two.sided", "Supérieur à" = "greater", "Inférieur à" = "less")
LOIS_ID <- c("Normale" = "normale", "Log-normale" = "lognormale", "Exponentielle" = "exponentielle", "Gamma" = "gamma", "Weibull" = "weibull", "Logistique" = "logistique", "Valeur extrême (Gumbel)" = "gumbel")
TYPES_GRAPH <- c("Histogramme" = "histogramme", "Secteurs (camembert)" = "secteurs", "Barres" = "barres", "Bulles" = "bulles", "Nuage de points" = "nuage", "Valeurs individuelles" = "individuelles", "Intervalles" = "intervalles", "Boîte à moustaches" = "boite", "Diagramme de probabilité" = "probabilite")
SRC_POI <- c("Colonne de comptages" = "col", "Totaux saisis" = "resume")
fc <- function(id, lib, type, ...) c(list(id = id, lib = lib, type = type), list(...))
ti <- function(...) paste0("(", paste0("v$type_graphique == '", c(...), "'", collapse = " || "), ")")   # type de graphique parmi...
SERIES_GR <- c("histogramme", "individuelles", "intervalles", "boite", "probabilite")
DECL <- list(
  ident = list(titre = "Identification de loi", commande = "identification", champs = list(
    fc("colonnes", "Séries à analyser", "cols"),
    fc("lois", "Lois à comparer", "liste", opts = LOIS_ID),
    fc("simulations", "Tirages de simulation (p-value)", "num", defaut = 200)),
    aide = "Chaque loi est ajustée aux données ; H0 : les données suivent cette loi, conservée si p-value ≥ α. Avec « Analyser par groupe », l'identification est faite groupe par groupe."),
  z = list(titre = "Test Z", commande = "z", champs = list(
    fc("mode", "Test", "choix", opts = c("1 échantillon" = "1", "2 échantillons" = "2")),
    fc("x", "Série X", "col"),
    fc("sigma", "Écart-type connu de X (σ)", "num"),
    fc("y", "Série Y", "col", rang = 2, si = "v$mode == '2'"),
    fc("sigma2", "Écart-type connu de Y (σ₂)", "num", si = "v$mode == '2'"),
    fc("mu", "Valeur de référence (1 échantillon) ou différence supposée (2 échantillons)", "num", defaut = 0),
    fc("alternative", "Hypothèse alternative", "choix", opts = ALT3)),
    aide = "σ est l'écart-type de la population, connu par ailleurs (pas celui de l'échantillon)."),
  poisson = list(titre = "Test de Poisson", commande = "poisson", libre = "v$source == 'resume' && (v$mode != '2' || v$source2 == 'resume')", champs = list(
    fc("mode", "Test", "choix", opts = c("1 échantillon" = "1", "2 échantillons" = "2")),
    fc("source", "Données (échantillon 1)", "choix", opts = SRC_POI),
    fc("x", "Colonne de comptages", "col", si = "v$source == 'col'"),
    fc("duree", "Taille de chaque observation", "num", defaut = 1, si = "v$source == 'col'"),
    fc("total", "Nombre total d'événements", "num", si = "v$source == 'resume'"),
    fc("duree_tot", "Taille totale observée", "num", si = "v$source == 'resume'"),
    fc("source2", "Données (échantillon 2)", "choix", opts = SRC_POI, si = "v$mode == '2'"),
    fc("x2", "Colonne de comptages 2", "col", rang = 2, si = "v$mode == '2' && v$source2 == 'col'"),
    fc("duree2", "Taille de chaque observation 2", "num", defaut = 1, si = "v$mode == '2' && v$source2 == 'col'"),
    fc("total2", "Nombre total d'événements 2", "num", si = "v$mode == '2' && v$source2 == 'resume'"),
    fc("duree_tot2", "Taille totale observée 2", "num", si = "v$mode == '2' && v$source2 == 'resume'"),
    fc("lambda0", "Taux de référence (λ₀)", "num", si = "v$mode == '1'"),
    fc("ratio0", "Rapport de référence λ₁ / λ₂ (1 = taux égaux)", "num", defaut = 1, si = "v$mode == '2'"),
    fc("alternative", "Hypothèse alternative", "choix", opts = ALT3)),
    aide = "Tests exacts de Poisson : nombre d'événements par observation (durée, surface, lot…)."),
  estim = list(titre = "Estimation", commande = "estimation", libre = "v$source == 'resume'", champs = list(
    fc("source", "Données", "choix", opts = c("Colonne de données" = "col", "Résumé saisi (n, moyenne, écart-type)" = "resume")),
    fc("x", "Série", "col", si = "v$source == 'col'"),
    fc("n", "Taille de l'échantillon (n)", "num", si = "v$source == 'resume'"),
    fc("moyenne", "Moyenne de l'échantillon", "num", si = "v$source == 'resume'"),
    fc("ecart_type", "Écart-type de l'échantillon (s)", "num", option = TRUE, si = "v$source == 'resume'"),
    fc("sigma", "Écart-type connu (σ), facultatif", "num", option = TRUE),
    fc("niveau", "Niveau de confiance (%)", "num", defaut = 95),
    fc("cote", "Intervalle", "choix", opts = c("Bilatéral" = "two.sided", "Unilatéral — borne inférieure" = "lower", "Unilatéral — borne supérieure" = "upper")),
    fc("marge", "Marge d'erreur souhaitée (donne la taille d'échantillon nécessaire)", "num", option = TRUE)),
    aide = "Loi normale si σ est connu, loi t de Student sinon."),
  corr = list(titre = "Corrélation", commande = "correlation", champs = list(
    fc("colonnes", "Séries à corréler (au moins deux)", "cols"),
    fc("methode", "Coefficient", "choix", opts = c("Pearson (relation linéaire)" = "pearson", "Spearman (rangs)" = "spearman", "Kendall (rangs)" = "kendall")))),
  reg = list(titre = "Régression", commande = "regression", champs = list(
    fc("y", "Réponse (Y)", "col"),
    fc("x", "Variables explicatives (X)", "cols", defaut_sel = FALSE, exclure = "y"),
    fc("degre", "Modèle", "choix", opts = c("Linéaire (degré 1)" = "1", "Polynôme de degré 2" = "2", "Polynôme de degré 3" = "3"))),
    aide = "Degré 2 ou 3 : une seule variable X ; plusieurs variables : régression multiple."),
  serie = list(titre = "Série chronologique", commande = "serie", champs = list(
    fc("y", "Série", "col"),
    fc("ordre", "Ordre des données (facultatif)", "col", cols = "all", option = TRUE),
    fc("modeles", "Modèles de tendance", "liste", opts = c("Linéaire" = "lineaire", "Quadratique" = "quadratique", "Exponentielle" = "exponentielle", "Logistique (courbe en S)" = "logistique")),
    fc("horizon", "Périodes à prévoir", "num", defaut = 0)),
    aide = "Avec « Analyser par groupe », la tendance est ajustée groupe par groupe."),
  attr = list(titre = "Cartes p et u", commande = "carte_attributs", fixes = list(tests = list(1, 2, 3, 4)), champs = list(
    fc("type", "Type de carte", "choix", opts = c("Carte p — proportion d'unités défectueuses" = "p", "Carte u — défauts par unité" = "u")),
    fc("defauts", "Colonne des comptages", "col"),
    fc("effectif", "Colonne des effectifs (facultatif)", "col", option = TRUE),
    fc("n", "Effectif constant (si pas de colonne d'effectifs)", "num", option = TRUE),
    fc("ordre", "Ordre des données (facultatif)", "col", cols = "all", option = TRUE),
    fc("phase", "Phases avant / après (colonne, facultatif)", "col", cols = "all", option = TRUE),
    fc("rupture", "ou « Après » à partir du point n°", "num", option = TRUE)),
    aide = "Limites calculées séparément dans chaque phase."),
  graph = list(titre = "Graphiques", commande = "graphique", champs = list(
    fc("type_graphique", "Graphique", "choix", opts = TYPES_GRAPH),
    fc("colonnes", "Séries", "cols", si = ti(SERIES_GR)),
    fc("valeur", "Valeurs (colonne, facultatif)", "col", option = TRUE, si = ti(SERIES_GR, "secteurs", "barres")),
    fc("groupe", "Groupes (colonne, facultatif)", "col", cols = "all", option = TRUE, si = ti(SERIES_GR, "barres")),
    fc("categorie", "Catégories (colonne)", "col", cols = "all", si = "v$type_graphique == 'secteurs' || v$type_graphique == 'barres'"),
    fc("fonction", "Mesure par catégorie", "choix", opts = c("Effectif (nombre de lignes)" = "effectif", "Somme des valeurs" = "somme", "Moyenne des valeurs" = "moyenne"), si = "v$type_graphique == 'secteurs' || v$type_graphique == 'barres'"),
    fc("x", "Axe X", "col", si = "v$type_graphique == 'bulles' || v$type_graphique == 'nuage'"),
    fc("y", "Axe Y", "col", rang = 2, si = "v$type_graphique == 'bulles' || v$type_graphique == 'nuage'"),
    fc("taille", "Taille des bulles", "col", rang = 3, si = "v$type_graphique == 'bulles'"),
    fc("couleur", "Couleur selon (facultatif)", "col", cols = "all", option = TRUE, si = "v$type_graphique == 'bulles' || v$type_graphique == 'nuage'"),
    fc("ajust", "Droite d'ajustement", "case", defaut = TRUE, si = "v$type_graphique == 'nuage'"),
    fc("classes", "Nombre de classes (facultatif)", "num", option = TRUE, si = "v$type_graphique == 'histogramme'"),
    fc("normale", "Courbe normale", "case", defaut = FALSE, si = "v$type_graphique == 'histogramme'"),
    fc("loi", "Loi de référence", "choix", opts = LOIS_ID, si = "v$type_graphique == 'probabilite'")),
    aide = "Avec « Analyser par groupe », un graphique est produit pour chaque groupe.")
)
for (k in names(DECL)) for (j in seq_along(DECL[[k]]$champs)) DECL[[k]]$champs[[j]]$ns <- paste0(k, "_", DECL[[k]]$champs[[j]]$id)

# Condition « si » : évaluée en R (valeurs v) ou convertie en JavaScript (input.<préfixe>_<champ>)
si_js <- function(si, pref) gsub("v\\$([A-Za-z0-9_]+)", paste0("input.", pref, "_\\1"), si)
champ_ui <- function(pref, ch) {
  id <- paste0(pref, "_", ch$id)
  w <- switch(ch$type,
    choix = selectInput(id, ch$lib, choices = ch$opts),
    col = selectInput(id, ch$lib, choices = if (isTRUE(ch$option)) c("— aucun —" = "") else character(0)),
    cols = selectInput(id, ch$lib, choices = NULL, multiple = TRUE),
    liste = selectInput(id, ch$lib, choices = ch$opts, selected = unname(ch$opts), multiple = TRUE),
    num = numericInput(id, ch$lib, value = if (is.null(ch$defaut)) NA_real_ else ch$defaut),
    case = checkboxInput(id, ch$lib, value = isTRUE(ch$defaut)))
  if (!is.null(ch$si)) conditionalPanel(si_js(ch$si, pref), w) else w
}
decl_tab <- function(pref) {
  d <- DECL[[pref]]
  tabPanel(d$titre, br(), if (!is.null(d$aide)) helpText(d$aide),
           lapply(d$champs, function(ch) champ_ui(pref, ch)),
           actionButton(paste0("run_", pref), "Lancer", class = "btn-primary"),
           uiOutput(paste0(pref, "_out")))
}

tab_illustrations <- tabPanel("Illustrations", br(),
  helpText("Images de référence (PNG) du dossier « illustrations » : <sujet>.png, <sujet>_1.png, <sujet>_2.png…"),
  selectInput("illus_sujet", "Sujet", choices = setNames(names(ILLUS_TITRES), unname(ILLUS_TITRES))),
  uiOutput("illus_out"))
panneau_generation <- wellPanel(
  h4("Générer des données aléatoires"),
  selectInput("g_loi", "Loi", choices = setNames(vapply(LOIS_GEN, `[[`, "", "code"), vapply(LOIS_GEN, `[[`, "", "lib"))),
  uiOutput("g_params"),
  fluidRow(column(4, numericInput("g_n", "Nombre de valeurs", 30, min = 1, max = 100000)),
           column(4, numericInput("g_dec", "Décimales", 3, min = 0, max = 10)),
           column(4, numericInput("g_graine", "Valeur d'initialisation (facultatif)", NA_real_))),
  textInput("g_nom", "Nom de la variable", "Serie_1"),
  checkboxInput("g_rempl", "Remplacer les données actuelles (sinon la variable est ajoutée)", FALSE),
  actionButton("g_go", "Générer", class = "btn-primary"),
  helpText("Une même valeur d'initialisation redonne exactement le même tirage."))

ui <- fluidPage(
  tags$head(
    tags$title("Statistiques - Pi 5"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body {background:#f5f7fa; color:#202733;}
      .container-fluid {max-width:1250px; padding:12px;}
      .well {background:white; border:1px solid #dce3ec; border-radius:12px; box-shadow:0 2px 8px #00000008;}
      .btn {border-radius:8px; margin:3px 0;}
      h2 {font-size:24px;} h3 {font-size:19px;}
      .help-block {font-size:13px;}
      .stat-note {background:#fff7df;padding:10px;border-radius:8px;}
      @media(max-width:700px){ .container-fluid {padding:7px;} h2 {font-size:21px;} }
    "))
  ),
  titlePanel("Statistiques — Raspberry Pi 5"),
  p("Saisie et analyse de séries de mesures depuis un téléphone ou un ordinateur."),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("project_name", "Nom du projet", value = "Nouveau projet"),
      actionButton("save_project", "Créer / sélectionner le projet", class = "btn-primary"),
      uiOutput("project_status"),
      hr(),
      numericInput("alpha", "Risque α des tests", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
      helpText("H0 est rejetée si la p-value est inférieure à α, et conservée si elle est supérieure ou égale à α (0,05 = 5 %). Les intervalles de confiance suivent le même risque."),
      selectInput("grp_col", "Analyser par groupe", choices = c("— aucun —" = "")),
      uiOutput("grp_per_ui"),
      conditionalPanel("input.grp_col != ''",
        checkboxGroupInput("grp_vals", "Groupes à analyser", choices = NULL),
        helpText("Chaque analyse est lancée groupe par groupe (résultats et graphiques par groupe).")),
      hr(),
      h4("Données"),
      radioButtons("entry_mode", "Mode de saisie", choices = c("Coller / saisir des données" = "paste", "Importer un CSV" = "csv"), selected = "paste"),
      conditionalPanel("input.entry_mode == 'paste'",
        textAreaInput("data_text", "Une colonne par série, noms en première ligne", value = "Serie_A,Serie_B\n10.1,10.4\n9.9,10.2\n10.3,10.1\n10.0,10.5\n10.2,10.3", rows = 8),
        helpText("Exemple : séparateur virgule. Utilisez un point pour les décimales.")
      ),
      conditionalPanel("input.entry_mode == 'csv'",
        fileInput("csv_file", "Fichier CSV", accept = c(".csv", ".txt")),
        checkboxInput("header", "Première ligne = noms de colonnes", TRUE),
        selectInput("sep", "Séparateur", choices = c("Virgule" = ",", "Point-virgule" = ";", "Tabulation" = "\t"), selected = ";"),
        selectInput("dec", "Décimales", choices = c("Virgule" = ",", "Point" = "."), selected = ",")
      ),
      actionButton("load_data", "Charger les données", class = "btn-primary"),
      actionButton("save_data", "Enregistrer dans le projet"),
      downloadButton("download_csv", "Exporter CSV"),
      hr(),
      helpText("Les données sont conservées sur le Pi dans data/statistiques_pi5.sqlite. Faites régulièrement une copie de sauvegarde.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(id = "onglets",
        tabPanel("Données", br(), panneau_generation, DTOutput("data_table"), br(), textOutput("data_status")),
        navbarMenu("Description",
          tabPanel("Descriptif", br(),
            p("Statistiques descriptives, boîtes à moustaches et histogrammes des séries choisies."),
            selectInput("d_cols", "Séries à analyser", choices = NULL, multiple = TRUE),
            actionButton("run_desc", "Lancer le descriptif", class = "btn-primary"),
            uiOutput("desc_out")
          ),
          tabPanel("Normalité", br(),
            p("Tests de normalité sur une ou plusieurs séries ; un graphique (histogramme et droite de Henry) par série."),
            selectInput("n_cols", "Séries à tester", choices = NULL, multiple = TRUE),
            checkboxGroupInput("n_tests", "Tests de normalité", selected = c("sw", "ad"), choices = c(
              "Shapiro-Wilk" = "sw", "Anderson-Darling" = "ad", "Kolmogorov-Smirnov (Lilliefors)" = "lf",
              "Cramér-von Mises" = "cvm", "Jarque-Bera" = "jb", "Ryan-Joiner (Minitab)" = "rj")),
            actionButton("run_norm", "Lancer les tests", class = "btn-primary"),
            uiOutput("norm_out")
          ),
          decl_tab("ident")
        ),
        navbarMenu("Tests",
          tabPanel("Tests t", br(),
            p("Test t de Student : comparer deux séries indépendantes (Welch par défaut, Student si les variances sont supposées égales), deux séries appariées, ou une série à une valeur cible."),
            selectInput("t_x", "Série X", choices = NULL),
            selectInput("t_y", "Série Y (laisser vide pour un test à un échantillon)", choices = c("— test à un échantillon —" = "")),
            numericInput("t_mu", "Valeur de référence (test à un échantillon)", value = 0),
            checkboxInput("t_paired", "Mesures appariées", FALSE),
            checkboxInput("t_var_equal", "Supposer les variances égales (test indépendant)", FALSE),
            selectInput("t_alt", "Hypothèse alternative", choices = c("Différent de" = "two.sided", "Supérieur à" = "greater", "Inférieur à" = "less")),
            actionButton("run_t", "Lancer le test t", class = "btn-primary"),
            uiOutput("t_out")
          ),
          decl_tab("z"),
          decl_tab("poisson"),
          decl_tab("estim"),
          tabPanel("Test F", br(),
            p("Compare les variances de deux séries ; le résultat indique aussi s'il faut préférer le test t de Student ou celui de Welch."),
            selectInput("f_x", "Série X", choices = NULL),
            selectInput("f_y", "Série Y", choices = NULL),
            selectInput("f_alt", "Hypothèse alternative", choices = c("Variances différentes" = "two.sided",
              "Variance de X supérieure à celle de Y" = "greater", "Variance de X inférieure à celle de Y" = "less")),
            actionButton("run_f", "Lancer le test F", class = "btn-primary"),
            uiOutput("f_out")
          ),
          tabPanel("ANOVA", br(),
            p("Pour l'ANOVA, le fichier doit comporter une colonne de mesures et une colonne de groupes. Le tableau de saisie standard en colonnes séparées convient si chaque colonne représente un groupe ; utilisez alors le bouton ci-dessous."),
            selectInput("anova_mode", "Format", choices = c("Chaque colonne numérique est un groupe" = "wide", "Données longues (mesure + groupe)" = "long")),
            uiOutput("anova_fields"),
            actionButton("run_anova", "Lancer l'ANOVA", class = "btn-primary"),
            uiOutput("anova_out")
          ),
          tabPanel("Khi²", br(),
            selectInput("x_test", "Test", choices = c("Indépendance de deux variables" = "independance",
              "Homogénéité de plusieurs populations" = "homogeneite", "Conformité à une répartition ou à une loi" = "conformite")),
            conditionalPanel("input.x_test == 'conformite'",
              selectInput("x_loi", "Loi testée", choices = c("Répartition attendue (modalités)" = "repartition", "Loi normale" = "normale"))),
            conditionalPanel("!(input.x_test == 'conformite' && input.x_loi == 'normale')",
              radioButtons("x_format", "Données", choices = c("Données brutes (une ligne par individu)" = "brut",
                "Tableau d'effectifs (cases déjà comptées)" = "table"))),
            conditionalPanel("input.x_test != 'conformite' && input.x_format == 'brut'",
              selectInput("x_a", "Variable A (lignes) / populations", choices = NULL),
              selectInput("x_b", "Variable B (colonnes) / variable étudiée", choices = NULL)),
            conditionalPanel("!(input.x_test == 'conformite' && input.x_loi == 'normale') && input.x_format == 'table'",
              selectInput("x_cols", "Colonnes d'effectifs (une par modalité)", choices = NULL, multiple = TRUE),
              selectInput("x_lab", "Colonne des libellés (facultatif)", choices = c("— aucune —" = ""))),
            conditionalPanel("input.x_test == 'conformite' && input.x_format == 'brut' && input.x_loi == 'repartition'",
              selectInput("x_var", "Variable testée", choices = NULL)),
            conditionalPanel("input.x_test == 'conformite' && input.x_loi == 'normale'",
              selectInput("x_var_n", "Variable numérique testée", choices = NULL),
              numericInput("x_mu", "Moyenne de la loi (vide = estimée)", value = NA_real_),
              numericInput("x_sd", "Écart-type de la loi (vide = estimé)", value = NA_real_, min = 0),
              numericInput("x_k", "Nombre de classes (vide = automatique)", value = NA_real_, min = 3)),
            conditionalPanel("input.x_test == 'conformite' && input.x_loi == 'repartition'",
              textInput("x_prop", "Proportions attendues, séparées par des espaces (vide = égales)", value = "")),
            helpText("Tableau d'effectifs : une ligne par ligne du tableau, une colonne par modalité. Données brutes : colonnes de catégories, une ligne par individu."),
            actionButton("run_khi2", "Lancer le test du khi²", class = "btn-primary"),
            uiOutput("khi2_out")
          )
        ),
        navbarMenu("Modélisation",
          decl_tab("corr"),
          decl_tab("reg"),
          decl_tab("serie")
        ),
        navbarMenu("Qualité",
          tabPanel("Capabilité", br(),
            p("Indices calculés selon les limites de spécification saisies. Pour Cm/Cmk/Cp/Cpk, fournir un écart-type intra-sous-groupe validé ; sinon seuls Pp/Ppk sont calculés."),
            selectInput("cap_series", "Série de mesures", choices = NULL),
            numericInput("lsl", "LSL — limite inférieure (laisser vide si absente)", value = NA_real_),
            numericInput("target", "Cible (facultatif)", value = NA_real_),
            numericInput("usl", "USL — limite supérieure (laisser vide si absente)", value = NA_real_),
            numericInput("sigma_within", "Écart-type intra-sous-groupe (facultatif)", value = NA_real_, min = 0),
            actionButton("run_cap", "Calculer la capabilité", class = "btn-primary"),
            uiOutput("cap_out")
          ),
          tabPanel("Cartes de contrôle", br(),
            selectInput("c_type", "Type de carte", choices = c("Automatique (selon la taille des sous-groupes)" = "auto",
              "X-barre / R (moyennes et étendues)" = "xbar_r", "X-barre / S (moyennes et écarts-types)" = "xbar_s",
              "Valeurs individuelles / étendues mobiles (I-MR)" = "imr")),
            radioButtons("c_fmt", "Organisation des données", choices = c(
              "Chaque ligne est un sous-groupe ; les colonnes choisies en sont les valeurs" = "colonnes",
              "Une colonne de mesures et une colonne qui désigne le sous-groupe" = "long")),
            conditionalPanel("input.c_fmt == 'colonnes'",
              selectInput("c_cols", "Colonnes (valeurs individuelles : une seule colonne)", choices = NULL, multiple = TRUE),
              helpText("Les cellules vides donnent des sous-groupes de tailles différentes.")),
            conditionalPanel("input.c_fmt == 'long'",
              selectInput("c_val", "Mesures", choices = NULL),
              selectInput("c_grp", "Sous-groupes", choices = NULL),
              helpText("Des sous-groupes de tailles différentes donnent des limites de contrôle variables.")),
            selectInput("c_ordre", "Ordre des données (facultatif : date ou numéro d'ordre)", choices = c("— aucun —" = "")),
          selectInput("c_phase", "Phases avant / après (colonne, facultatif)", choices = c("— aucun —" = "")),
          numericInput("c_rupt", "ou « Après » à partir du point n° (facultatif)", value = NA_real_),
          helpText("Avec des phases, les limites de contrôle sont calculées séparément dans chaque phase."),
          checkboxGroupInput("c_tests", "Tests de causes spéciales", inline = TRUE, choices = 1:8, selected = 1:4),
            helpText("1 : point hors limites — 2 : 9 points du même côté — 3 : 6 points en tendance — 4 : 14 points alternés — 5 à 8 : règles des zones (Minitab / Nelson)."),
            actionButton("run_carte", "Tracer la carte", class = "btn-primary"),
            uiOutput("carte_out")
          ),
          decl_tab("attr"),
          tabPanel("R&R", br(),
            radioButtons("rr_mode", "Étude", choices = c("R&R croisée : pièces × opérateurs × répétitions (une ligne par mesure)" = "croisee",
              "R&R de type 1 : une seule pièce étalon mesurée de nombreuses fois" = "type1")),
            conditionalPanel("input.rr_mode == 'croisee'",
              selectInput("rc_p", "Pièces", choices = NULL),
              selectInput("rc_o", "Opérateurs", choices = NULL),
              selectInput("rc_m", "Mesures", choices = NULL),
              numericInput("rc_tol", "Tolérance (facultative)", value = NA_real_, min = 0),
              numericInput("rc_l", "L (écarts-types de la variation d'étude)", value = 6, min = 1),
              numericInput("rc_a", "Seuil de l'interaction", value = 0.05, min = 0, max = 1, step = 0.01)),
            conditionalPanel("input.rr_mode == 'type1'",
              selectInput("r1_col", "Mesures", choices = NULL),
              numericInput("r1_ref", "Valeur de référence", value = NA_real_),
              numericInput("r1_tol", "Tolérance", value = NA_real_, min = 0),
              numericInput("r1_k", "K (% de la tolérance)", value = 20, min = 0),
              numericInput("r1_l", "L (écarts-types)", value = 6, min = 1)),
            helpText("Valeurs par défaut : conventions de Minitab (méthode ANOVA, L = 6, interaction retirée au seuil 0,05 ; type 1 : K = 20 %, L = 6, Cg et Cgk ≥ 1,33). La norme AIAG retient 0,25 pour le seuil de l'interaction. La tolérance est l'écart entre les limites de spécification."),
            actionButton("run_rr", "Calculer", class = "btn-primary"),
            uiOutput("rr_out")
          ),
          tabPanel("Kappa", br(),
            p("Comparer deux colonnes d'évaluations catégorielles de même longueur (par exemple opérateur A et opérateur B)."),
            selectInput("kappa_a", "Évaluation de référence", choices = NULL),
            selectInput("kappa_b", "Deuxième évaluation", choices = NULL),
            actionButton("run_kappa", "Calculer Kappa de Cohen", class = "btn-primary"),
            uiOutput("kappa_out")
          )
        ),
        decl_tab("graph"),
        tab_illustrations,
        tabPanel("Aide & limites", br(),
          h4("Interprétation responsable"),
          tags$ul(
            tags$li("Groupes : « Analyser par groupe » (colonne de gauche) lance chaque analyse une fois par groupe retenu, avec les résultats et les graphiques de chaque groupe. Les colonnes de dates (JJ/MM/AAAA ou AAAA-MM-JJ, avec ou sans heure) sont reconnues au chargement ; regroupées par jour, mois ou année quand elles servent de groupes."),
            tags$li("Décision : le risque α (0,05 par défaut) se règle dans la colonne de gauche. H0 est rejetée si la p-value est inférieure à α, et conservée si elle lui est supérieure ou égale. Pour un test de normalité (quel que soit le test), la normalité n'est validée que si la p-value est supérieure ou égale à α."),
            tags$li("Organisation : les onglets sont regroupés (Description, Tests, Modélisation, Qualité) ; « Générer des données aléatoires » (onglet Données) propose 15 lois et le tirage aléatoire entre deux bornes ; l'onglet Illustrations affiche les images PNG du dossier « illustrations »."),
            tags$li("Test Z : écart(s)-type(s) de population connu(s), un ou deux échantillons. Test de Poisson : un échantillon (taux de référence) ou deux échantillons (rapport des taux), tests exacts. Estimation : intervalle de confiance d'une moyenne (loi normale si σ est connu, t de Student sinon), bilatéral ou unilatéral, et taille d'échantillon pour une marge d'erreur donnée."),
            tags$li("Identification de loi : normale, log-normale, exponentielle, gamma, Weibull, logistique, valeur extrême ; H0 : les données suivent la loi testée (conservée si p-value ≥ α). Corrélation (Pearson, Spearman, Kendall), régression (linéaire, multiple, polynôme de degré 2 ou 3) et séries chronologiques (tendances linéaire, quadratique, exponentielle, logistique)."),
            tags$li("Cartes de contrôle : phases avant / après (colonne de phases ou numéro de point), limites calculées séparément dans chaque phase ; cartes p et u pour les attributs. Graphiques : histogramme, secteurs, barres, bulles, nuage de points, valeurs individuelles, intervalles, boîtes à moustaches, diagramme de probabilité."),
            tags$li("p-value : elle ne mesure ni l'importance pratique de l'effet ni la probabilité que l'hypothèse soit vraie."),
            tags$li("Shapiro-Wilk : avec de grands effectifs, de petits écarts à la normalité sans importance pratique peuvent devenir significatifs ; examiner aussi les graphiques."),
            tags$li("Test t : vérifier l'indépendance, la forme des données et les conditions du test choisi. Le test de Welch est le choix par défaut pour deux échantillons indépendants."),
            tags$li("ANOVA : vérifier les résidus, l'indépendance et l'homogénéité des variances ; une ANOVA significative nécessite des comparaisons post-hoc pour identifier les groupes concernés."),
            tags$li("Test F : suppose des échantillons indépendants issus de lois normales, et y est sensible ; le test de Levene (Brown-Forsythe) est fourni en complément."),
            tags$li("Khi² : les effectifs théoriques doivent être suffisamment grands (en général au moins 5 par case) ; sinon des regroupements ou le test exact de Fisher (tableaux 2 × 2) sont préférables. La taille d'effet (V de Cramér) complète la p-value."),
            tags$li("Cartes de contrôle : une carte n'a de sens que si les sous-groupes sont rationnels ; les limites sont calculées sur les données, et varient si les sous-groupes n'ont pas tous la même taille."),
            tags$li("R&R : la variation de mesure est comparée à la variation d'étude et, si elle est fournie, à la tolérance ; moins de 10 % est acceptable, de 10 à 30 % conditionnel, plus de 30 % inacceptable, avec au moins 5 catégories distinctes."),
            tags$li("Capabilité : Cp/Cpk et Cm/Cmk ne sont interprétables que si la méthode de calcul de la variation intra est appropriée et le procédé suffisamment stable. Pp/Ppk utilisent la dispersion globale."),
            tags$li("Kappa : la valeur dépend notamment de la prévalence des catégories ; examiner aussi la matrice de confusion et l'accord observé. Interprétation : Landis et Koch (1977) ; systèmes de mesure (AIAG, MSA) : Kappa > 0,75 bon à excellent, Kappa < 0,40 insuffisant.")
          ),
          div(class = "stat-note", "Cette version est un outil d'aide à l'analyse, pas une validation métrologique ou qualité automatique. Vérifiez les données, les hypothèses et les unités avant toute décision.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(data = NULL, dates = character(0), project_id = NULL, project_name = NULL, status = "Aucune donnée chargée.")

  parse_pasted <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) stop("Saisissez des données avant de charger.")
    # Lecture flexible CSV, séparateur virgule ; la saisie par défaut utilise le point décimal.
    d <- utils::read.csv(text = txt, header = TRUE, stringsAsFactors = FALSE,
                         check.names = FALSE, na.strings = c("", "NA", "NaN"))
    if (!ncol(d)) stop("Aucune colonne détectée.")
    for (nm in names(d)) {
      if (is.character(d[[nm]]) && is.null(colonne_date(d[[nm]]))) {
        converted <- suppressWarnings(as.numeric(gsub(",", ".", trimws(d[[nm]]), fixed = TRUE)))
        if (sum(!is.na(d[[nm]])) > 0 && sum(!is.na(converted)) >= max(1, floor(sum(!is.na(d[[nm]])) * .7))) d[[nm]] <- converted
      }
    }
    d
  }

  charger_donnees <- function(d) {
    state$data <- d
      state$status <- sprintf("%d ligne(s), %d colonne(s) chargée(s).", nrow(d), ncol(d))
      numeric_names <- names(d)[vapply(d, is.numeric, logical(1))]
      updateSelectInput(session, "grp_col", choices = c("— aucun —" = "", names(d)), selected = "")
      updateSelectInput(session, "d_cols", choices = numeric_names, selected = numeric_names)
      updateSelectInput(session, "n_cols", choices = numeric_names, selected = numeric_names)
      updateSelectInput(session, "t_x", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      updateSelectInput(session, "t_y", choices = c("— test à un échantillon —" = "", numeric_names), selected = "")
      updateSelectInput(session, "cap_series", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      updateSelectInput(session, "kappa_a", choices = names(d), selected = names(d)[1])
      updateSelectInput(session, "kappa_b", choices = names(d), selected = names(d)[min(2, ncol(d))])
      tous <- names(d)
      cat_names <- tous[!vapply(d, is.numeric, logical(1))]
      cle <- function(x, y) if (length(x)) x[1] else y
      updateSelectInput(session, "f_x", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      updateSelectInput(session, "f_y", choices = numeric_names, selected = numeric_names[min(2, length(numeric_names))] %||% character(0))
      updateSelectInput(session, "x_a", choices = tous, selected = cle(cat_names, tous[1]))
      updateSelectInput(session, "x_b", choices = tous, selected = if (length(cat_names) >= 2) cat_names[2] else tous[min(2, ncol(d))])
      updateSelectInput(session, "x_cols", choices = numeric_names, selected = numeric_names)
      updateSelectInput(session, "x_lab", choices = c("— aucune —" = "", cat_names), selected = "")
      updateSelectInput(session, "x_var", choices = tous, selected = tous[1])
      updateSelectInput(session, "x_var_n", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      updateSelectInput(session, "c_cols", choices = numeric_names, selected = numeric_names)
      updateSelectInput(session, "c_val", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      updateSelectInput(session, "c_grp", choices = tous, selected = tous[min(2, ncol(d))])
      updateSelectInput(session, "rc_p", choices = tous, selected = tous[1])
      updateSelectInput(session, "rc_o", choices = tous, selected = tous[min(2, ncol(d))])
      updateSelectInput(session, "rc_m", choices = numeric_names, selected = numeric_names[length(numeric_names)] %||% character(0))
      updateSelectInput(session, "r1_col", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      state$status <- paste(state$status, "Colonnes numériques :", if (length(numeric_names)) paste(numeric_names, collapse = ", ") else "aucune",
                            if (length(state$dates)) paste0("— dates : ", paste(state$dates, collapse = ", ")) else "")
    tous <- names(d)
    updateSelectInput(session, "c_ordre", choices = c("— aucun —" = "", tous), selected = if (length(state$dates)) state$dates[1] else "")
    updateSelectInput(session, "c_phase", choices = c("— aucun —" = "", tous), selected = "")
    maj_champs_decl(d)
  }

  # Champs des onglets décrits par DECL : listes de colonnes mises à jour à chaque chargement
  maj_champs_decl <- function(d) {
    noms <- list(num = names(d)[vapply(d, is.numeric, logical(1))], all = names(d))
    for (k in names(DECL)) for (ch in DECL[[k]]$champs) {
      if (!ch$type %in% c("col", "cols")) next
      vals <- noms[[if (is.null(ch$cols)) "num" else ch$cols]]
      id <- paste0(k, "_", ch$id)
      if (ch$type == "col") {
        rang <- if (is.null(ch$rang)) 1 else ch$rang
        sel <- if (identical(ch$id, "ordre") && length(state$dates)) state$dates[1] else if (isTRUE(ch$option)) "" else vals[min(rang, length(vals))]
        updateSelectInput(session, id, choices = c(if (isTRUE(ch$option)) c("— aucun —" = ""), vals), selected = sel)
      } else {
        updateSelectInput(session, id, choices = vals, selected = if (isFALSE(ch$defaut_sel)) character(0) else vals)
      }
    }
  }

  observeEvent(input$load_data, {
    tryCatch({
      if (identical(input$entry_mode, "paste")) {
        d <- parse_pasted(input$data_text)
      } else {
        req(input$csv_file)
        d <- utils::read.csv(input$csv_file$datapath, header = input$header,
                             sep = input$sep, dec = input$dec, stringsAsFactors = FALSE,
                             check.names = FALSE, na.strings = c("", "NA", "NaN"))
      }
      if (!nrow(d)) stop("Le fichier ne contient aucune ligne de données.")
      r_d <- detecter_dates(d); d <- r_d$d; state$dates <- r_d$dates
      charger_donnees(d)
    }, error = function(e) {
      state$status <- paste("Erreur :", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$save_project, {
    req(input$project_name)
    nom <- trimws(input$project_name)
    if (!nzchar(nom)) return()
    DBI::dbExecute(db, "INSERT INTO projets(nom, date_creation) VALUES(?, ?)",
                   params = list(nom, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    state$project_id <- DBI::dbGetQuery(db, "SELECT last_insert_rowid() AS id")$id[1]
    state$project_name <- nom
    showNotification(paste("Projet créé :", nom), type = "message")
  })

  observeEvent(input$save_data, {
    req(state$data)
    if (is.null(state$project_id)) {
      showNotification("Créez d'abord un projet.", type = "warning")
      return()
    }
    csv_text <- paste(capture.output(utils::write.csv(state$data, row.names = FALSE, na = "")), collapse = "\n")
    DBI::dbExecute(db, "INSERT INTO jeux_donnees(projet_id, nom, csv, date_creation) VALUES(?, ?, ?, ?)",
                   params = list(state$project_id, "Jeu de données", csv_text, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    showNotification("Données enregistrées dans le projet.", type = "message")
  })

  output$project_status <- renderUI({
    if (is.null(state$project_id)) p("Aucun projet sélectionné.")
    else p(strong(paste("Projet actif :", state$project_name)))
  })
  output$data_status <- renderText(state$status)
  output$data_table <- renderDT({
    req(state$data)
    aff <- state$data
    for (nm in intersect(state$dates, names(aff))) aff[[nm]] <- date_affichee(aff[[nm]])
    datatable(aff, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  # ---- Analyses calculées par le moteur partagé (cli_statistique.R) ----
  cli_res <- reactiveValues()
  lancer <- function(id, commande, params, libre = FALSE) {
    if (!libre) req(state$data)
    a <- input$alpha
    if (is.null(a) || is.na(a) || a <= 0 || a >= 0.5) {
      cli_res[[id]] <- list(erreur_web = "Le risque α doit être compris entre 0 et 0,5 (par exemple 0,05).")
      return(invisible())
    }
    groupe <- NULL
    if (!libre && !is.null(input$grp_col) && nzchar(input$grp_col)) {
      if (!length(input$grp_vals)) {
        cli_res[[id]] <- list(erreur_web = "Aucun groupe sélectionné : cocher au moins un groupe, ou choisir « — aucun — ».")
        return(invisible())
      }
      per <- periode_groupe()
      groupe <- list(colonne = input$grp_col, valeurs = input$grp_vals,
                     etiquettes = etiquettes_groupe(state$data, input$grp_col, state$dates, per %||% "jour"),
                     nom = paste0(input$grp_col, if (!is.null(per)) c(jour = " (par jour)", mois = " (par mois)", annee = " (par année)")[[per]] else ""))
    }
    withProgress(message = "Calcul en cours…", value = 0.5, {
      cli_res[[id]] <- tryCatch(executer_cli(if (libre) data.frame(x = 0) else state$data, commande, params, alpha = a, groupe = groupe),
                                error = function(e) list(erreur_web = conditionMessage(e)))
    })
  }
  for (id in c("desc", "norm", "t", "anova", "f", "khi2", "carte", "rr", "cap", "kappa", names(DECL))) local({
    i <- id
    output[[paste0(i, "_out")]] <- renderUI({ r <- cli_res[[i]]; if (is.null(r)) NULL else rendre_resultat(r) })
  })
  vide <- function(v) if (is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v))) NULL else v

  # Groupes : liste des valeurs de la colonne choisie (tout est coché par défaut)
  output$grp_per_ui <- renderUI({
    if (!is.null(input$grp_col) && nzchar(input$grp_col) && input$grp_col %in% state$dates)
      selectInput("grp_per", "Regrouper les dates par", choices = PERIODES_DATE)
  })
  periode_groupe <- function() if (!is.null(input$grp_col) && input$grp_col %in% state$dates) (if (is.null(input$grp_per)) "jour" else input$grp_per) else NULL
  observeEvent(list(input$grp_col, input$grp_per), {
    if (is.null(input$grp_col) || !nzchar(input$grp_col) || is.null(state$data)) {
      updateCheckboxGroupInput(session, "grp_vals", choices = character(0)); return()
    }
    v <- unique(etiquettes_groupe(state$data, input$grp_col, state$dates, periode_groupe() %||% "jour")); v <- v[!is.na(v)]
    n <- suppressWarnings(as.numeric(gsub(",", ".", v)))
    v <- if (!anyNA(n)) v[order(n)] else sort(v)
    if (length(v) > 30) {
      showNotification(sprintf("La colonne « %s » compte %d groupes : le maximum est 30.", input$grp_col, length(v)), type = "warning")
      updateSelectInput(session, "grp_col", selected = ""); return()
    }
    updateCheckboxGroupInput(session, "grp_vals", choices = v, selected = v)
  }, ignoreInit = TRUE)

  observeEvent(input$run_desc, {
    lancer("desc", "descriptif", list(colonnes = as.list(input$d_cols)))
  })
  observeEvent(input$run_norm, {
    lancer("norm", "normalite", list(colonnes = as.list(input$n_cols), tests = as.list(input$n_tests)))
  })

  observeEvent(input$run_t, {
    req(input$t_x)
    y <- input$t_y
    lancer("t", "t", list(x = input$t_x, y = if (is.null(y) || !nzchar(y)) NULL else y, mu = vide(input$t_mu) %||% 0,
                          apparie = isTRUE(input$t_paired), variances_egales = isTRUE(input$t_var_equal),
                          alternative = input$t_alt))
  })

  observeEvent(input$run_anova, {
    if (identical(input$anova_mode, "wide")) {
      lancer("anova", "anova", list(mode = "large", colonnes = as.list(input$anova_wide_cols)))
    } else {
      lancer("anova", "anova", list(mode = "long", valeur = input$anova_value, groupe = input$anova_group))
    }
  })

  observeEvent(input$run_f, {
    lancer("f", "ftest", list(x = input$f_x, y = input$f_y, alternative = input$f_alt))
  })

  observeEvent(input$run_khi2, {
    tl <- input$x_test
    normale <- identical(tl, "conformite") && identical(input$x_loi, "normale")
    fmt <- if (normale) "brut" else input$x_format
    cols <- as.list(input$x_cols)
    if (identical(tl, "conformite")) {
      p <- list(test = "conformite", loi = if (normale) "normale" else "repartition", format = fmt,
                variable = if (normale) input$x_var_n else input$x_var, colonnes = cols, libelles = input$x_lab %||% "",
                proportions = if (normale) "" else input$x_prop,
                moyenne = if (normale) vide(input$x_mu) else NULL, ecart_type = if (normale) vide(input$x_sd) else NULL,
                classes = if (normale) vide(input$x_k) else NULL)
    } else {
      p <- list(test = tl, format = fmt, a = input$x_a, b = input$x_b, colonnes = cols, libelles = input$x_lab %||% "")
    }
    lancer("khi2", "khi2", p)
  })

  observeEvent(input$run_carte, {
    tests <- as.integer(input$c_tests)
    lancer("carte", "carte", list(type = input$c_type, format = input$c_fmt, colonnes = as.list(input$c_cols),
                                  valeur = input$c_val, groupe = input$c_grp, tests = as.list(if (length(tests)) tests else 0L),
                                  ordre = input$c_ordre %||% "", phase = input$c_phase %||% "", rupture = vide(input$c_rupt)))
  })

  observeEvent(input$run_rr, {
    if (identical(input$rr_mode, "type1")) {
      lancer("rr", "rr1", list(colonne = input$r1_col, reference = vide(input$r1_ref), tolerance = vide(input$r1_tol),
                               k = vide(input$r1_k), l = vide(input$r1_l)))
    } else {
      lancer("rr", "rr_croisee", list(piece = input$rc_p, operateur = input$rc_o, mesure = input$rc_m,
                                      tolerance = vide(input$rc_tol), l = vide(input$rc_l), alpha = vide(input$rc_a)))
    }
  })

  observeEvent(input$run_cap, {
    req(input$cap_series)
    lancer("cap", "capabilite", list(colonne = input$cap_series, lsl = vide(input$lsl), usl = vide(input$usl),
                                     cible = vide(input$target), sigma_intra = vide(input$sigma_within)))
  })

  observeEvent(input$run_kappa, {
    req(input$kappa_a, input$kappa_b)
    lancer("kappa", "kappa", list(a = input$kappa_a, b = input$kappa_b))
  })


  # ---- Onglets décrits par DECL : paramètres lus dans les champs visibles (comme l'application de bureau) ----
  params_decl <- function(k) {
    d <- DECL[[k]]
    v <- list()
    for (ch in d$champs) {
      x <- input[[paste0(k, "_", ch$id)]]
      v[[ch$id]] <- if (is.null(x)) "" else x
    }
    visible <- function(ch) is.null(ch$si) || isTRUE(tryCatch(eval(parse(text = ch$si), list(v = v)), error = function(e) FALSE))
    p <- list()
    for (ch in d$champs) {
      if (!visible(ch)) next
      x <- v[[ch$id]]; i <- ch$id
      if (ch$type == "choix") p[[i]] <- x
      else if (ch$type == "case") p[[i]] <- isTRUE(x)
      else if (ch$type == "col") {
        if (is.null(x) || !nzchar(x)) { if (!isTRUE(ch$option)) stop(sprintf("Choisir : %s.", ch$lib)); p[i] <- list(NULL) } else p[[i]] <- x
      } else if (ch$type == "num") {
        n <- suppressWarnings(as.numeric(x))
        if (length(n) != 1 || is.na(n)) { if (!isTRUE(ch$option) && is.null(ch$defaut)) stop(sprintf("Saisir : %s.", ch$lib)); p[i] <- list(NULL) } else p[[i]] <- n
      } else {
        sel <- as.character(x)
        if (!is.null(ch$exclure)) sel <- setdiff(sel, v[[ch$exclure]])
        if (!length(sel)) stop(sprintf("Sélectionner au moins un élément dans « %s ».", ch$lib))
        p[[i]] <- as.list(sel)
      }
    }
    if (!is.null(d$fixes)) p <- c(p, d$fixes)
    libre <- !is.null(d$libre) && isTRUE(tryCatch(eval(parse(text = d$libre), list(v = v)), error = function(e) FALSE))
    list(params = p, libre = libre)
  }
  for (k in names(DECL)) local({
    kk <- k
    observeEvent(input[[paste0("run_", kk)]], {
      r <- tryCatch(params_decl(kk), error = function(e) conditionMessage(e))
      if (is.character(r)) { cli_res[[kk]] <- list(erreur_web = r); return() }
      lancer(kk, DECL[[kk]]$commande, r$params, libre = r$libre)
    })
  })

  # ---- Génération de données aléatoires ----
  output$g_params <- renderUI({
    l <- Filter(function(z) z$code == input$g_loi, LOIS_GEN)[[1]]
    tagList(helpText(l$desc),
            fluidRow(lapply(seq_along(l$params), function(i) column(4, textInput(paste0("g_p", i), l$params[[i]]$lib, value = l$params[[i]]$defaut)))))
  })
  observeEvent(input$g_loi, updateNumericInput(session, "g_dec", value = if (identical(input$g_loi, "aleatoire")) 0 else 3), ignoreInit = TRUE)
  observeEvent(input$g_go, {
    tryCatch({
      l <- Filter(function(z) z$code == input$g_loi, LOIS_GEN)[[1]]
      q <- list()
      for (i in seq_along(l$params)) q[[l$params[[i]]$cle]] <- suppressWarnings(as.numeric(gsub(",", ".", input[[paste0("g_p", i)]], fixed = TRUE)))
      nom <- trimws(input$g_nom)
      if (!nzchar(nom)) stop("Saisir le nom de la variable.")
      a <- input$alpha; if (is.null(a) || is.na(a)) a <- 0.05
      r <- executer_cli(data.frame(x = 0), "generer", list(loi = l$code, parametres = q, n = input$g_n, decimales = input$g_dec,
                                                          graine = if (is.na(input$g_graine)) NULL else input$g_graine), alpha = a)
      v <- as.numeric(unlist(r$valeurs))
      if (isTRUE(input$g_rempl) || is.null(state$data)) state$dates <- character(0)
      d <- if (isTRUE(input$g_rempl) || is.null(state$data)) data.frame(v) else state$data
      if (!isTRUE(input$g_rempl) && !is.null(state$data)) {
        if (nom %in% names(d)) stop("Ce nom de variable existe déjà.")
        n0 <- nrow(d); length(v) <- max(n0, length(v)); d <- d[seq_len(length(v)), , drop = FALSE]; d[[nom]] <- v
        rownames(d) <- NULL
      } else names(d) <- nom
      charger_donnees(d)
      state$status <- paste(sprintf("Variable « %s » générée (%d valeurs).", nom, length(unlist(r$valeurs))), state$status)
      updateTextInput(session, "g_nom", value = paste0("Serie_", ncol(d) + 1))
      showNotification(sprintf("%d valeur(s) générée(s) : variable « %s ».", length(unlist(r$valeurs)), nom), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
  })

  # ---- Illustrations ----
  output$illus_out <- renderUI({
    cle <- input$illus_sujet; req(cle)
    f <- sort(list.files("illustrations", pattern = "\\.png$", ignore.case = TRUE))
    stem <- tolower(sub("\\.png$", "", f, ignore.case = TRUE))
    f <- f[stem == cle | startsWith(stem, paste0(cle, "_"))]
    if (!length(f)) return(div(class = "stat-note", sprintf("Aucune image pour ce sujet : placer « %s.png » (ou %s_1.png, %s_2.png…) dans le dossier « illustrations ».", cle, cle, cle)))
    tagList(lapply(f, function(x) tags$img(src = paste0("illus/", utils::URLencode(x), "?", as.integer(file.mtime(file.path("illustrations", x)))), style = "max-width:100%;height:auto;margin:8px 0;")))
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("donnees_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(state$data)
      utils::write.csv(state$data, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
