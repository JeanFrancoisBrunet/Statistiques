# app.R
# Interface Shiny mobile-first pour Samsung S23 / Raspberry Pi 5.
# Lancer dans le dossier contenant app.R et moteur_statistique.R :
#   R -e "shiny::runApp(host='0.0.0.0', port=3838)"
# Depuis le S23, ouvrir http://ADRESSE_IP_DU_PI:3838
# Dépendances : shiny, DT, DBI, RSQLite, ggplot2, jsonlite
# Installer une fois : install.packages(c("shiny","DT","DBI","RSQLite","ggplot2","jsonlite"))
# Les analyses avancées (tests t, ANOVA, test F, khi², cartes de contrôle, R&R) utilisent le même moteur
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

executer_cli <- function(data, commande, params, alpha = 0.05, groupe = NULL) {
  if (is.null(data)) stop("Aucune donnée chargée.")
  script <- normalizePath("cli_statistique.R", mustWork = FALSE)
  if (!file.exists(script) || !file.exists("commandes_avancees.R"))
    stop("cli_statistique.R et commandes_avancees.R doivent se trouver dans le dossier de l'application.")
  rs <- file.path(R.home("bin"), "Rscript")
  dossier <- tempfile("stat_web_"); dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  if (!is.null(groupe)) {              # analyse groupe par groupe : colonne technique « __groupe__ » lue par le moteur
    data[["__groupe__"]] <- as.character(data[[groupe$colonne]])
    params <- c(params, list(groupes = as.list(groupe$valeurs), groupe_nom = groupe$colonne))
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
db <- DBI::dbConnect(RSQLite::SQLite(), "data/statistiques_pi5.sqlite")
DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS projets (id INTEGER PRIMARY KEY AUTOINCREMENT, nom TEXT NOT NULL, date_creation TEXT NOT NULL)")
DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS jeux_donnees (id INTEGER PRIMARY KEY AUTOINCREMENT, projet_id INTEGER, nom TEXT, csv TEXT, date_creation TEXT)")
onStop(function() DBI::dbDisconnect(db))

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
      tabsetPanel(
        tabPanel("Données", br(), DTOutput("data_table"), br(), textOutput("data_status")),
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
        tabPanel("ANOVA", br(),
          p("Pour l'ANOVA, le fichier doit comporter une colonne de mesures et une colonne de groupes. Le tableau de saisie standard en colonnes séparées convient si chaque colonne représente un groupe ; utilisez alors le bouton ci-dessous."),
          selectInput("anova_mode", "Format", choices = c("Chaque colonne numérique est un groupe" = "wide", "Données longues (mesure + groupe)" = "long")),
          uiOutput("anova_fields"),
          actionButton("run_anova", "Lancer l'ANOVA", class = "btn-primary"),
          uiOutput("anova_out")
        ),
        tabPanel("Test F", br(),
          p("Compare les variances de deux séries ; le résultat indique aussi s'il faut préférer le test t de Student ou celui de Welch."),
          selectInput("f_x", "Série X", choices = NULL),
          selectInput("f_y", "Série Y", choices = NULL),
          selectInput("f_alt", "Hypothèse alternative", choices = c("Variances différentes" = "two.sided",
            "Variance de X supérieure à celle de Y" = "greater", "Variance de X inférieure à celle de Y" = "less")),
          actionButton("run_f", "Lancer le test F", class = "btn-primary"),
          uiOutput("f_out")
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
          checkboxGroupInput("c_tests", "Tests de causes spéciales", inline = TRUE, choices = 1:8, selected = 1:4),
          helpText("1 : point hors limites — 2 : 9 points du même côté — 3 : 6 points en tendance — 4 : 14 points alternés — 5 à 8 : règles des zones (Minitab / Nelson)."),
          actionButton("run_carte", "Tracer la carte", class = "btn-primary"),
          uiOutput("carte_out")
        ),
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
        tabPanel("Kappa", br(),
          p("Comparer deux colonnes d'évaluations catégorielles de même longueur (par exemple opérateur A et opérateur B)."),
          selectInput("kappa_a", "Évaluation de référence", choices = NULL),
          selectInput("kappa_b", "Deuxième évaluation", choices = NULL),
          actionButton("run_kappa", "Calculer Kappa de Cohen", class = "btn-primary"),
          uiOutput("kappa_out")
        ),
        tabPanel("Aide & limites", br(),
          h4("Interprétation responsable"),
          tags$ul(
            tags$li("Groupes : « Analyser par groupe » (colonne de gauche) lance chaque analyse une fois par groupe retenu, avec les résultats et les graphiques de chaque groupe. Les dates ne sont reconnues comme telles que dans l'application de bureau."),
            tags$li("Décision : le risque α (0,05 par défaut) se règle dans la colonne de gauche. H0 est rejetée si la p-value est inférieure à α, et conservée si elle lui est supérieure ou égale. Pour un test de normalité (quel que soit le test), la normalité n'est validée que si la p-value est supérieure ou égale à α."),
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
  state <- reactiveValues(data = NULL, project_id = NULL, project_name = NULL, status = "Aucune donnée chargée.")

  parse_pasted <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) stop("Saisissez des données avant de charger.")
    # Lecture flexible CSV, séparateur virgule ; la saisie par défaut utilise le point décimal.
    d <- utils::read.csv(text = txt, header = TRUE, stringsAsFactors = FALSE,
                         check.names = FALSE, na.strings = c("", "NA", "NaN"))
    if (!ncol(d)) stop("Aucune colonne détectée.")
    for (nm in names(d)) {
      if (is.character(d[[nm]])) {
        converted <- suppressWarnings(as.numeric(gsub(",", ".", trimws(d[[nm]]), fixed = TRUE)))
        if (sum(!is.na(d[[nm]])) > 0 && sum(!is.na(converted)) >= max(1, floor(sum(!is.na(d[[nm]])) * .7))) d[[nm]] <- converted
      }
    }
    d
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
      state$status <- paste(state$status, "Colonnes numériques :", if (length(numeric_names)) paste(numeric_names, collapse = ", ") else "aucune")
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
    datatable(state$data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  # ---- Analyses calculées par le moteur partagé (cli_statistique.R) ----
  cli_res <- reactiveValues()
  lancer <- function(id, commande, params) {
    req(state$data)
    a <- input$alpha
    if (is.null(a) || is.na(a) || a <= 0 || a >= 0.5) {
      cli_res[[id]] <- list(erreur_web = "Le risque α doit être compris entre 0 et 0,5 (par exemple 0,05).")
      return(invisible())
    }
    groupe <- NULL
    if (!is.null(input$grp_col) && nzchar(input$grp_col)) {
      if (!length(input$grp_vals)) {
        cli_res[[id]] <- list(erreur_web = "Aucun groupe sélectionné : cocher au moins un groupe, ou choisir « — aucun — ».")
        return(invisible())
      }
      groupe <- list(colonne = input$grp_col, valeurs = input$grp_vals)
    }
    withProgress(message = "Calcul en cours…", value = 0.5, {
      cli_res[[id]] <- tryCatch(executer_cli(state$data, commande, params, alpha = a, groupe = groupe),
                                error = function(e) list(erreur_web = conditionMessage(e)))
    })
  }
  for (id in c("desc", "norm", "t", "anova", "f", "khi2", "carte", "rr", "cap", "kappa")) local({
    i <- id
    output[[paste0(i, "_out")]] <- renderUI({ r <- cli_res[[i]]; if (is.null(r)) NULL else rendre_resultat(r) })
  })
  vide <- function(v) if (is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v))) NULL else v

  # Groupes : liste des valeurs de la colonne choisie (tout est coché par défaut)
  observeEvent(input$grp_col, {
    if (is.null(input$grp_col) || !nzchar(input$grp_col) || is.null(state$data)) {
      updateCheckboxGroupInput(session, "grp_vals", choices = character(0)); return()
    }
    v <- unique(as.character(state$data[[input$grp_col]])); v <- v[!is.na(v)]
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
                                  valeur = input$c_val, groupe = input$c_grp, tests = as.list(if (length(tests)) tests else 0L)))
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

  output$download_csv <- downloadHandler(
    filename = function() paste0("donnees_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(state$data)
      utils::write.csv(state$data, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
