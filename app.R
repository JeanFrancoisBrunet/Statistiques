# app.R
# Interface Shiny mobile-first pour Samsung S23 / Raspberry Pi 5.
# Lancer dans le dossier contenant app.R et moteur_statistique.R :
#   R -e "shiny::runApp(host='0.0.0.0', port=3838)"
# Depuis le S23, ouvrir http://ADRESSE_IP_DU_PI:3838
# Dépendances : shiny, DT, DBI, RSQLite, ggplot2
# Installer une fois : install.packages(c("shiny","DT","DBI","RSQLite","ggplot2"))

required <- c("shiny", "DT", "DBI", "RSQLite", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Packages R manquants : ", paste(missing, collapse = ", "),
                          ". Installez-les avec install.packages().")
source("moteur_statistique.R", local = TRUE)

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
        tabPanel("Descriptif", br(), DTOutput("summary_table"), plotOutput("boxplot", height = "330px"), plotOutput("histogram", height = "300px")),
        tabPanel("Tests t", br(),
          p("Comparer deux séries indépendantes, deux séries appariées, ou une série à une valeur cible."),
          selectInput("t_x", "Série X", choices = NULL),
          selectInput("t_y", "Série Y (laisser vide pour un test à un échantillon)", choices = c("— test à un échantillon —" = "")),
          numericInput("t_mu", "Valeur de référence (test à un échantillon)", value = 0),
          checkboxInput("t_paired", "Mesures appariées", FALSE),
          checkboxInput("t_var_equal", "Supposer les variances égales (test indépendant)", FALSE),
          selectInput("t_alt", "Hypothèse alternative", choices = c("Différent de" = "two.sided", "Supérieur à" = "greater", "Inférieur à" = "less")),
          actionButton("run_t", "Lancer le test t", class = "btn-primary"),
          verbatimTextOutput("t_result")
        ),
        tabPanel("ANOVA", br(),
          p("Pour l'ANOVA, le fichier doit comporter une colonne de mesures et une colonne de groupes. Le tableau de saisie standard en colonnes séparées convient si chaque colonne représente un groupe ; utilisez alors le bouton ci-dessous."),
          selectInput("anova_mode", "Format", choices = c("Chaque colonne numérique est un groupe" = "wide", "Données longues (mesure + groupe)" = "long")),
          uiOutput("anova_fields"),
          actionButton("run_anova", "Lancer l'ANOVA", class = "btn-primary"),
          verbatimTextOutput("anova_result")
        ),
        tabPanel("Capabilité", br(),
          p("Indices calculés selon les limites de spécification saisies. Pour Cm/Cmk/Cp/Cpk, fournir un écart-type intra-sous-groupe validé ; sinon seuls Pp/Ppk sont calculés."),
          selectInput("cap_series", "Série de mesures", choices = NULL),
          numericInput("lsl", "LSL — limite inférieure (laisser vide si absente)", value = NA_real_),
          numericInput("target", "Cible (facultatif)", value = NA_real_),
          numericInput("usl", "USL — limite supérieure (laisser vide si absente)", value = NA_real_),
          numericInput("sigma_within", "Écart-type intra-sous-groupe (facultatif)", value = NA_real_, min = 0),
          actionButton("run_cap", "Calculer la capabilité", class = "btn-primary"),
          DTOutput("cap_table"),
          plotOutput("cap_plot", height = "320px")
        ),
        tabPanel("Kappa", br(),
          p("Comparer deux colonnes d'évaluations catégorielles de même longueur (par exemple opérateur A et opérateur B)."),
          selectInput("kappa_a", "Évaluation de référence", choices = NULL),
          selectInput("kappa_b", "Deuxième évaluation", choices = NULL),
          actionButton("run_kappa", "Calculer Kappa de Cohen", class = "btn-primary"),
          verbatimTextOutput("kappa_result"),
          DTOutput("kappa_table")
        ),
        tabPanel("Aide & limites", br(),
          h4("Interprétation responsable"),
          tags$ul(
            tags$li("p-value : elle ne mesure ni l'importance pratique de l'effet ni la probabilité que l'hypothèse soit vraie."),
            tags$li("Shapiro-Wilk : avec de grands effectifs, de petits écarts à la normalité sans importance pratique peuvent devenir significatifs ; examiner aussi les graphiques."),
            tags$li("Test t : vérifier l'indépendance, la forme des données et les conditions du test choisi. Le test de Welch est le choix par défaut pour deux échantillons indépendants."),
            tags$li("ANOVA : vérifier les résidus, l'indépendance et l'homogénéité des variances ; une ANOVA significative nécessite des comparaisons post-hoc pour identifier les groupes concernés."),
            tags$li("Capabilité : Cp/Cpk et Cm/Cmk ne sont interprétables que si la méthode de calcul de la variation intra est appropriée et le procédé suffisamment stable. Pp/Ppk utilisent la dispersion globale."),
            tags$li("Kappa : la valeur dépend notamment de la prévalence des catégories ; examiner aussi la matrice de confusion et l'accord observé. Interprétation : Landis et Koch (1977) ; systèmes de mesure (AIAG, MSA) : Kappa > 0,75 bon à excellent, Kappa < 0,40 insuffisant.")
          ),
          div(class = "stat-note", "Cette première version est un outil d'aide à l'analyse, pas une validation métrologique ou qualité automatique. Vérifiez les données, les hypothèses et les unités avant toute décision.")
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
      updateSelectInput(session, "t_x", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      updateSelectInput(session, "t_y", choices = c("— test à un échantillon —" = "", numeric_names), selected = "")
      updateSelectInput(session, "cap_series", choices = numeric_names, selected = numeric_names[1] %||% character(0))
      updateSelectInput(session, "kappa_a", choices = names(d), selected = names(d)[1])
      updateSelectInput(session, "kappa_b", choices = names(d), selected = names(d)[min(2, ncol(d))])
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

  output$summary_table <- renderDT({
    req(state$data)
    d <- state$data[, vapply(state$data, is.numeric, logical(1)), drop = FALSE]
    validate(need(ncol(d) > 0, "Aucune colonne numérique à résumer."))
    datatable(resume_multi_series(d), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  output$boxplot <- renderPlot({
    req(state$data)
    d <- state$data[, vapply(state$data, is.numeric, logical(1)), drop = FALSE]
    validate(need(ncol(d) > 0, "Aucune colonne numérique."))
    long <- stack(d)
    names(long) <- c("Valeur", "Serie")
    ggplot(long, aes(x = Serie, y = Valeur)) + geom_boxplot(na.rm = TRUE) +
      labs(title = "Boîtes à moustaches par série", x = "Série", y = "Mesure") + theme_minimal()
  })

  output$histogram <- renderPlot({
    req(state$data)
    d <- state$data[, vapply(state$data, is.numeric, logical(1)), drop = FALSE]
    validate(need(ncol(d) > 0, "Aucune colonne numérique."))
    long <- stack(d)
    names(long) <- c("Valeur", "Serie")
    ggplot(long, aes(x = Valeur)) + geom_histogram(bins = 20, na.rm = TRUE) +
      facet_wrap(~Serie, scales = "free") + labs(title = "Distribution des mesures", x = "Mesure", y = "Effectif") + theme_minimal()
  })

  observeEvent(input$run_t, {
    req(state$data, input$t_x)
    tryCatch({
      x <- state$data[[input$t_x]]
      yname <- input$t_y
      res <- if (is.null(yname) || !nzchar(yname)) {
        test_t(x, mu = input$t_mu, alternative = input$t_alt)
      } else {
        test_t(x, state$data[[yname]], paired = input$t_paired,
               var_equal = input$t_var_equal, alternative = input$t_alt)
      }
      output$t_result <- renderPrint({
        cat("Type :", res$type, "\n")
        cat("Statistique t :", signif(res$statistic, 6), "\n")
        cat("Degrés de liberté :", signif(res$ddl, 6), "\n")
        cat("p-value :", signif(res$p_value, 6), "\n")
        cat("Intervalle de confiance :", paste(signif(res$intervalle_confiance, 6), collapse = " ; "), "\n")
        cat(res$conclusion_5pct, "\n")
      })
    }, error = function(e) output$t_result <- renderPrint(cat("Erreur :", conditionMessage(e))))
  })

  output$anova_fields <- renderUI({
    req(state$data)
    if (identical(input$anova_mode, "wide")) {
      nums <- names(state$data)[vapply(state$data, is.numeric, logical(1))]
      selectInput("anova_wide_cols", "Colonnes de groupes", choices = nums, selected = nums, multiple = TRUE)
    } else {
      tagList(
        selectInput("anova_value", "Colonne mesure", choices = names(state$data)),
        selectInput("anova_group", "Colonne groupe", choices = names(state$data))
      )
    }
  })

  observeEvent(input$run_anova, {
    req(state$data)
    tryCatch({
      if (identical(input$anova_mode, "wide")) {
        cols <- input$anova_wide_cols
        validate(need(length(cols) >= 2, "Sélectionnez au moins deux colonnes numériques."))
        d <- stack(state$data[, cols, drop = FALSE])
        names(d) <- c("valeur", "groupe")
        fit <- stats::aov(valeur ~ as.factor(groupe), data = d)
        tab <- summary(fit)[[1]]
        p <- tab[["Pr(>F)"]][1]
        msg <- if (p < .05) "Au moins une moyenne diffère au seuil de 5 %." else "Pas de différence mise en évidence au seuil de 5 %."
        res <- list(table = as.data.frame(tab), p_value = p, conclusion_5pct = msg,
                    avertissement = "Vérifier les hypothèses ANOVA. Pour le format en colonnes, les observations manquantes sont ignorées.")
      } else {
        res <- test_anova(state$data, input$anova_value, input$anova_group)
      }
      output$anova_result <- renderPrint({
        print(res$table)
        cat("\np-value :", signif(res$p_value, 6), "\n", res$conclusion_5pct, "\n")
        cat(res$avertissement, "\n")
      })
    }, error = function(e) output$anova_result <- renderPrint(cat("Erreur :", conditionMessage(e))))
  })

  cap_result <- eventReactive(input$run_cap, {
    req(state$data, input$cap_series)
    indices_capabilite(state$data[[input$cap_series]],
                       LSL = input$lsl, USL = input$usl, cible = input$target,
                       sigma_within = input$sigma_within)
  })
  output$cap_table <- renderDT({
    req(cap_result())
    datatable(cap_result(), options = list(scrollX = TRUE), rownames = FALSE)
  })
  output$cap_plot <- renderPlot({
    req(state$data, input$cap_series, cap_result())
    x <- state$data[[input$cap_series]]
    x <- x[is.finite(x)]
    d <- data.frame(Mesure = x)
    p <- ggplot(d, aes(x = Mesure)) + geom_histogram(aes(y = after_stat(density)), bins = 20, na.rm = TRUE) +
      geom_density(na.rm = TRUE) + labs(title = paste("Distribution :", input$cap_series), x = "Mesure", y = "Densité") + theme_minimal()
    if (is.finite(input$lsl)) p <- p + geom_vline(xintercept = input$lsl, linetype = "dashed")
    if (is.finite(input$usl)) p <- p + geom_vline(xintercept = input$usl, linetype = "dashed")
    if (is.finite(input$target)) p <- p + geom_vline(xintercept = input$target, linetype = "dotted")
    p
  })

  observeEvent(input$run_kappa, {
    req(state$data, input$kappa_a, input$kappa_b)
    tryCatch({
      res <- kappa_agreement(state$data[[input$kappa_a]], state$data[[input$kappa_b]])
      output$kappa_result <- renderPrint({
        cat("Effectif valide :", res$effectif, "\n")
        cat("Accord observé :", round(res$accord_observe * 100, 2), "%\n")
        cat("Accord attendu :", round(res$accord_attendu * 100, 2), "%\n")
        cat("Kappa de Cohen :", signif(res$kappa_cohen, 6), "\n")
        cat(res$interpretation, "\n")
        cat(res$note, "\n\n")
        cat(res$reference, sep = "\n")
      })
      output$kappa_table <- renderDT(datatable(as.data.frame.matrix(res$matrice), options = list(scrollX = TRUE), rownames = TRUE))
    }, error = function(e) {
      output$kappa_result <- renderPrint(cat("Erreur :", conditionMessage(e)))
    })
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
