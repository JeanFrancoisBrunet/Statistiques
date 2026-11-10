# Statistiques — Raspberry Pi 5 + Samsung S23

Programme de statistiques du Pi 5, utilisable de 4 façons : application de bureau sur le Pi, bot Telegram depuis le S23, interface web Shiny depuis le Pi ou depuis le S23. Les calculs sont réalisés par **R** ; l'application de bureau et le bot sont en **Python**.

- Dossier du projet : `/home/jfbrunet/Projects/Statistiques`
- Dossier du bot Telegram : `/home/jfbrunet/Projects/Telegram`

## Les quatre accès

| Appareil | Accès                                                              | Démarrage après un redémarrage du Pi |
|---       |---                                                                 |---                                   |
| Pi 5     | Application de bureau (icône du bureau)                            | Cliquer sur l'icône                  |
| S23      | Bot Telegram                                                       | Automatique (service systemd)        |
| Pi 5     | Interface web Shiny : `http://localhost:3838`                      | Taper `statweb`                      |
| S23      | Interface web Shiny : `http://10.221.90.1:3838` (WireGuard activé) | Taper `statweb` sur le Pi            |

## Contenu du dossier

| Fichier                        | Rôle                                                                                                                                                                                                                                                                          |
|---                             |---                                                                                                                                                                                                                                                                            |
| `statistiques.py`              | Application de bureau (Tkinter) avec écran d'accueil ; contient aussi le cœur partagé avec le bot (lecture des données, appel de R, mise en forme)                                                                                                                            |
| `telegram_bot_statistiques.py` | Bot Telegram (dans le dossier du bot)                                                                                                                                                                                                                                         |
| `rapport_statistique.py`       | Rapport de synthèse PDF (reportlab), appelé par l'application de bureau                                                                                                                                                                                                       |
| `cli_statistique.R`            | Passerelle en ligne de commande vers le moteur R (résultats JSON + graphiques PNG)                                                                                                                                                                                            |
| `commandes_avancees.R`         | Analyses avancées chargées par `cli_statistique.R` : test F, khi², ANOVA à deux facteurs, cartes de contrôle (dont p, np, c, u), R&R (croisée, emboîtée, type 1), tests Z et de Poisson, puissance et taille d'échantillon, estimation, lois, régression, séries, graphiques  |
| `moteur_statistique.R`         | Fonctions de calcul R réutilisables                                                                                                                                                                                                                                           |
| `app.R`                        | Interface web Shiny, facultative (voir plus bas) ; elle appelle `cli_statistique.R` et `commandes_avancees.R`, qui doivent rester dans le même dossier                                                                                                                        |
| `Statistiques.desktop`         | Lanceur pour le bureau (non versionné : il reste sur le bureau du Pi)                                                                                                                                                                                                         |
| `vocabulaire.txt`              | Glossaire (« Terme : définition », une entrée par ligne), à compléter au fur et à mesure ; lu par l'application de bureau, l'interface web et le bot                                                                                                                          |
| `icons/6Sigma.png`             | Icône de l'écran d'accueil et du lanceur                                                                                                                                                                                                                                      |

## Fonctionnalités

- **Données** : saisie ou collage de texte, ouverture d'un fichier `txt`, `csv`, `tsv`, `dat` ou `xlsx`, export CSV.
- **Risque α et décision** : le risque α (0,05 par défaut) se règle en haut de la fenêtre (colonne de gauche dans l'interface web) et s'applique à tous les tests ; il est conservé dans les projets `.statproj`. Chaque résultat rappelle H0 et H1 et la règle de décision : **p-value ≥ α → H0 non rejetée ; p-value < α → H0 rejetée**. Pour la normalité (quel que soit le test), H0 est « la série suit une loi normale » : elle n'est validée que si la p-value est supérieure ou égale à α. Les intervalles de confiance des tests suivent le même risque (niveau 1 − α). Le seuil de l'interaction de la R&R croisée reste un réglage propre à cette analyse.
- **Dates et groupes** : une colonne de dates (JJ/MM/AAAA ou AAAA-MM-JJ, avec ou sans heure, y compris dans un fichier `xlsx`) est reconnue comme un type « date », distinct du texte et du nombre. « Analyser par groupe » (en haut de la fenêtre ; colonne de gauche dans l'interface web) lance **chaque analyse groupe par groupe** pour les groupes retenus (colonne de texte, de nombres ou de dates regroupées par jour, mois ou année) : un bloc de résultats, des tableaux et des graphiques par groupe, avec le groupe inscrit sur chaque graphique. Pour les cartes de contrôle, « Ordre des données » trie les points par date et l'affiche sur l'axe.
- **Statistiques descriptives** : effectif, moyenne, médiane, écart-type, variance, quartiles, extrêmes, Shapiro-Wilk, boîtes à moustaches, histogrammes.
- **Normalité** : au choix Shapiro-Wilk, Anderson-Darling, Kolmogorov-Smirnov (Lilliefors), Cramér-von Mises, Jarque-Bera, Ryan-Joiner (test de Minitab), sur une ou plusieurs séries à la fois ; pour chaque série, histogramme (courbe normale) et droite de Henry.
- **Tests t (Student)** : un échantillon, deux échantillons indépendants (Welch par défaut, variances égales en option), échantillons appariés ; hypothèse bilatérale ou unilatérale ; intervalle de confiance et taille d'effet (d de Cohen).
- **Test F** de comparaison de deux variances : rapport des variances, intervalle de confiance, hypothèse bilatérale ou unilatérale, test de Levene (plus robuste) et conseil sur le choix entre le test t de Student et celui de Welch (application de bureau).
- **ANOVA à deux facteurs** (application de bureau et interface web) : une colonne de mesures et deux colonnes de facteurs, avec ou sans interaction ; sommes des carrés ajustées (type III, comme Minitab), η² partiel, moyennes par niveau et par combinaison, graphiques des effets principaux et de l'interaction, contrôle des résidus (Shapiro-Wilk, Fligner-Killeen), comparaisons de Tukey des facteurs significatifs si le plan est équilibré.
- **ANOVA à un facteur** : colonnes = groupes ou format long (colonne de mesures + colonne de groupes) ; taille d'effet η² ; contrôle des résidus (Shapiro-Wilk) et de l'homogénéité des variances (Levene, Fligner-Killeen) ; ANOVA de Welch et Kruskal-Wallis en alternative ; comparaisons deux à deux de Tukey si le test global est significatif, ou de Welch avec ajustement de Holm si les variances diffèrent ; graphique des moyennes et de leurs intervalles de confiance.
- **Khi²** (application de bureau) : tests d'**indépendance**, d'**homogénéité** et de **conformité** (à une répartition attendue ou à une loi normale), à partir de données brutes (deux colonnes de catégories) ou d'un tableau d'effectifs déjà compté. Effectifs observés et théoriques, résidus standardisés, V de Cramér, profils des lignes, graphiques ; correction de Yates et test exact de Fisher pour un tableau 2 × 2 ou quand les effectifs théoriques sont trop petits. La conformité à la loi normale utilise des classes équiprobables (au moins 5 valeurs attendues par classe).
- **Cartes de contrôle** (application de bureau) : X-barre/R, X-barre/S et valeurs individuelles/étendues mobiles (I-MR), avec choix automatique du type (valeurs seules : I-MR ; 2 à 7 valeurs par sous-groupe : X-barre/R ; 8 et plus : X-barre/S). Données en colonnes (une ligne par sous-groupe) ou en format long (colonne de mesures + colonne de sous-groupes) ; des sous-groupes de tailles différentes donnent des limites de contrôle variables. Limites calculées d'après les données à 3 écarts-types (constantes d2, d3 et c4 calculées exactement), 8 tests de causes spéciales (règles de Minitab / Nelson, 1 à 4 par défaut), écart-type intra-sous-groupe estimé réutilisable pour la capabilité.
- **R&R** (application de bureau, interface web et bot Telegram), conventions de **Minitab** par défaut, rappelées dans chaque résultat : **R&R emboîtée** (chaque pièce n'est mesurée que par un seul opérateur, essai destructif : pièces emboîtées dans les opérateurs, plan équilibré, mêmes indicateurs que la croisée sans interaction) ; **R&R croisée** (pièces × opérateurs × répétitions, plan équilibré, méthode ANOVA, interaction retirée si sa p-value dépasse 0,05 — la norme AIAG retient 0,25 —, variation d'étude sur 6 écarts-types, composantes de variance, % contribution, % variation d'étude, % tolérance, nombre de catégories distinctes, graphiques et cartes par opérateur) et **R&R de type 1** (une pièce étalon mesurée de nombreuses fois : biais et test t, Cg et Cgk avec K = 20 % de la tolérance et L = 6, seuil 1,33).
- **Capabilité** : Pp/Ppk à partir de l'écart-type global ; Cp/Cpk/Cm/Cmk uniquement si un écart-type intra-sous-groupe est fourni ; PPM estimés ; histogramme avec limites de spécification.
- **Kappa de Cohen** non pondéré, avec matrice de confusion, en trois modes : comparaison de deux évaluations, ou comparaison d'un ou plusieurs évaluateurs à une **colonne de référence (norme)**, avec une colonne par évaluateur ou avec des données en **format long** (une ligne par évaluation : réponse, évaluateur, échantillon, essai, référence ; application de bureau). Dans les modes avec référence : Kappa de chaque évaluateur contre la référence avec un verdict (satisfaisant, à améliorer, insuffisant), accord en % et intervalle de confiance, accord de tous les évaluateurs avec la référence, Kappa des évaluateurs deux à deux et graphique. Interprétation selon l'échelle de Landis et Koch (1977) ; repères AIAG (MSA) pour les systèmes de mesure : Kappa > 0,75 bon à excellent, Kappa < 0,40 insuffisant. Les références sont rappelées avec chaque résultat.
- **Génération de données** (bouton « Générer des données… » de l'onglet Données ; aussi dans l'interface web et le bot) : variable aléatoire « aléatoire (random) » entre deux bornes, sans loi particulière, ou selon 15 lois (normale, uniforme, Poisson, Student, Fisher, khi², binomiale, géométrique, hypergéométrique, exponentielle, gamma, logistique, Weibull, log-normale, bêta), avec les paramètres de la loi, le nombre de valeurs, le nom de la variable, le nombre de décimales et une valeur d'initialisation facultative pour retrouver le même tirage. La variable s'ajoute aux données chargées ou les remplace ; on peut en générer plusieurs à la suite.
- **Puissance et taille d'échantillon** (onglet « Puissance » du groupe Tests, comme dans Minitab ; aussi dans l'interface web et le bot, sans données chargées) : pour un test t à 1 échantillon, t apparié, t à 2 échantillons, z (σ connu), 1 proportion (loi binomiale exacte), 2 proportions ou ANOVA à 1 facteur, on laisse vide **une seule** des trois cases « taille d'échantillon », « différence à détecter » et « puissance » : elle est calculée à partir des deux autres (avec le risque α et l'écart-type ou la proportion de référence). Le résultat s'accompagne de la **courbe de puissance** (puissance en fonction de la différence, pour plusieurs tailles d'échantillon ; tailles facultativement choisies).
- **Test Z** (un ou deux échantillons, écart(s)-type(s) connu(s)), **tests de Poisson** exacts (un échantillon, ou deux échantillons : rapport des taux) et **estimation** d'une moyenne (loi normale ; niveau de confiance, bilatéral ou unilatéral ; taille d'échantillon pour une marge d'erreur donnée), à partir d'une colonne ou de valeurs saisies (application de bureau).
- **Identification de loi** : ajustement de la normale, log-normale, exponentielle, gamma, Weibull, logistique et valeur extrême, test d'Anderson-Darling (p-value par simulation), AIC, densités et diagrammes de probabilité, série par série et groupe par groupe.
- **Corrélation et régression** : Pearson, Spearman ou Kendall avec p-value et matrice graphique ; régression linéaire simple ou multiple, polynôme de degré 2 ou 3 (ANOVA, coefficients, VIF, Durbin-Watson, résidus, observations inhabituelles, bandes de confiance et de prédiction).
- **Séries chronologiques** : tendance linéaire, quadratique, exponentielle ou logistique, indicateurs MAPE/MAD/MSD, test de tendance et prévisions ; avec groupes (un résultat par groupe).
- **Cartes de contrôle aux attributs p, np, c et u** (p : proportion d'unités défectueuses ; np : nombre d'unités défectueuses ; c : nombre de défauts par unité de contrôle de taille constante ; u : défauts par unité), effectifs variables (sauf carte c), et **phases avant / après** sur toutes les cartes (p, np, c, u, X-barre/R, X-barre/S, I-MR) : limites calculées séparément dans chaque phase, d'après une colonne de phases ou un numéro de point de rupture.
- **Graphiques** : histogramme, secteurs, barres, bulles, nuage de points, valeurs individuelles, intervalles, boîtes à moustaches, diagramme de probabilité. Le diagramme de probabilité (normale ou autre loi) et la droite de Henry de l'onglet Normalité tracent, comme Minitab, des **limites de confiance ponctuelles** (niveau 1 − α, 95 % par défaut) autour de la droite ajustée ; formule asymptotique pour la normale et la log-normale, bootstrap paramétrique pour les autres lois.
- **Illustrations** : onglet d'images de référence (PNG) rangées dans le dossier `illustrations/` (17 sujets : moyenne et écart-type, loi normale, risques α et β, tests d'hypothèses, khi², tests Z, t, Bartlett, Levene, Mann-Whitney, Kruskal-Wallis, droite de Henry, distribution, boîte à moustaches…). Un sujet peut compter plusieurs images (`<sujet>_1.png`, `<sujet>_2.png`) ; « Ajouter une image… » copie un fichier avec le bon nom, et le bouton « Illustration… » des onglets d'analyse ouvre le sujet associé.
- **Vocabulaire** (onglet entre Rapport et Aide) : glossaire des termes de statistique et de Lean Six Sigma, lu dans le fichier `vocabulaire.txt` du dossier du projet. Recherche par mot (accents et majuscules ignorés, dans les termes puis dans les définitions) ; boutons **Ajouter**, **Modifier** (ou double-clic) et **Supprimer**, qui réécrivent le fichier trié par ordre alphabétique ; **Ouvrir le fichier** l'ouvre dans l'éditeur de texte, et **Recharger** (ou le retour sur l'onglet) relit les modifications faites à la main. Format : une entrée par ligne, `Terme : définition` ; une définition peut continuer sur les lignes suivantes si elles commencent par deux espaces ; les lignes qui commencent par `#` sont des commentaires. Le même fichier est affiché dans l'interface web (onglet Vocabulaire) et interrogé par le bot (`/vocabulaire`).
- **Projet** (onglet Rapport) : journal de toutes les analyses calculées ; enregistrement des données, des résultats (texte, tableaux, graphiques) et des réglages des onglets dans un fichier `.statproj`, pour reprendre le travail plus tard.
- **Rapport de synthèse PDF** (onglet Rapport) : titre, auteur, conclusions saisies, synthèse des analyses retenues (case à cocher dans le journal), description des données, puis détail de chaque analyse avec tableaux, graphiques et remarques.

## Installation sur le Pi

```bash
sudo apt update
sudo apt install -y r-base r-cran-jsonlite python3-tk python3-requests
```

Recommandé, pour un affichage optimal des graphiques dans l'application de bureau :

```bash
sudo apt install -y python3-pil.imagetk
```

Pour copier une image (bouton « Copier image » des onglets Graphiques et Illustrations) et la coller dans LibreOffice Writer ou un courriel (Ctrl+V) :

```bash
sudo apt install -y xclip
```

(`wl-clipboard` si le bureau utilise Wayland ; `echo $XDG_SESSION_TYPE` indique `x11` ou `wayland`.) Sans cet outil, le bouton « Enregistrer image… » écrit l'image dans un fichier PNG, à insérer ensuite par Insertion > Image.

Pour le rapport de synthèse PDF :

```bash
sudo apt install -y python3-reportlab
```

La police DejaVu (paquet `fonts-dejavu-core`) est utilisée si elle est présente, ce qui est le cas par défaut sur Raspberry Pi OS ; sinon le rapport emploie les polices standard du PDF.

Facultatif, pour lire les fichiers `.xlsx` :

```bash
pip install openpyxl --break-system-packages
```

La liste des dépendances est aussi rappelée dans l'en-tête de `statistiques.py`.

Vérification rapide :

```bash
Rscript -e 'library(jsonlite); cat("R et jsonlite : OK\n")'
```

## Application de bureau

```bash
cd /home/jfbrunet/Projects/Statistiques
python3 statistiques.py
```

Lanceur sur le bureau : créer `~/Desktop/Statistiques.desktop` avec le contenu suivant, puis le rendre exécutable (`chmod +x ~/Desktop/Statistiques.desktop`) :

```ini
[Desktop Entry]
Type=Application
Name=Statistiques
Exec=python3 /home/jfbrunet/Projects/Statistiques/statistiques.py
Icon=/home/jfbrunet/Projects/Statistiques/icons/6Sigma.png
Terminal=false
Categories=Education;Science;
```

Selon le gestionnaire de fichiers, un clic droit sur l'icône puis « Autoriser le lancement » peut être nécessaire la première fois.

Utilisation : onglet **Données** pour saisir/coller des données ou ouvrir un fichier, puis un onglet par analyse. La fenêtre s'ouvre agrandie ; le séparateur entre le texte des résultats et les graphiques se déplace à la souris, et un clic sur un graphique l'ouvre en grand. Chaque résultat peut être enregistré (texte, avec les graphiques PNG à côté). Les options de lecture (séparateur, décimale, ligne de noms de colonnes) sont en mode automatique par défaut et peuvent être forcées. L'onglet **Rapport** garde le journal des analyses (double-clic ou « Revoir l'analyse sélectionnée » pour réafficher un résultat dans son onglet), enregistre ou rouvre un projet et génère le rapport PDF ; il décrit les données actuellement chargées, chaque analyse portant sur les données au moment de son calcul.

Les fichiers `.R` sont appelés par `Rscript` : ils n'ont pas besoin du droit d'exécution (`chmod +x`), contrairement aux `.py` lancés directement.

## Bot Telegram

1. Créer le bot avec `@BotFather` et récupérer son jeton.
2. Dans `~/.telegram_config`, section `[telegram]`, ajouter le jeton (la ligne `chat_id` et, au besoin, `allowed_ids` sont partagées avec les autres bots) :

   ```ini
   [telegram]
   token_statistiques = <jeton du bot>
   chat_id = <votre identifiant>
   allowed_ids = <identifiants autorisés, séparés par des virgules>
   ```

   Le jeton peut aussi être fourni par la variable `TELEGRAM_TOKEN`, et les accès par `ALLOWED_IDS`. Un utilisateur refusé reçoit son identifiant Telegram à ajouter à la liste.
3. Lancer à la main (test) :

   ```bash
   cd /home/jfbrunet/Projects/Statistiques
   python3 /home/jfbrunet/Projects/Telegram/telegram_bot_statistiques.py
   ```

   Au démarrage, le bot envoie un message de confirmation (heure, état de R).

### Service systemd (démarrage automatique)

À enregistrer dans `/etc/systemd/system/telegram-bot-statistiques.service` :

```ini
[Unit]
Description=Bot Telegram Statistiques
After=network-online.target
Wants=network-online.target

[Service]
User=jfbrunet
WorkingDirectory=/home/jfbrunet/Projects/Statistiques
ExecStart=/usr/bin/python3 /home/jfbrunet/Projects/Telegram/telegram_bot_statistiques.py
Restart=on-failure
RestartSec=10
Environment=PYTHONUNBUFFERED=1
Environment=PYTHONPATH=/home/jfbrunet/Projects/Statistiques
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

`PYTHONPATH` permet au bot (situé dans le dossier Telegram) de trouver `statistiques.py`. Chaque directive est sur sa propre ligne, sans espace en fin de ligne.

Activation (une seule fois) :

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now telegram-bot-statistiques
```

Commandes utiles :

```bash
systemctl status telegram-bot-statistiques
journalctl -u telegram-bot-statistiques -n 50 --no-pager
sudo systemctl restart telegram-bot-statistiques   # après modification du bot ou de statistiques.py
```

### Utilisation depuis le S23

- **Saisir des données** : envoyer un message texte (une série par colonne, noms de colonnes en première ligne), ou joindre un fichier `csv`, `txt`, `tsv`, `dat` ou `xlsx` (20 Mo au maximum). Un message Telegram étant limité à 4096 caractères, utiliser un fichier pour les gros volumes.
- Chaque nouvel envoi **remplace** les données précédentes.
- **Analyser** : boutons du menu ou commandes ; les colonnes se désignent par leur numéro (affiché après le chargement et par `/etat`) ou par leur nom.
- Les résultats arrivent en texte à chasse fixe, suivis des graphiques en images. Quand un argument manque, le bot propose des boutons (colonnes, lois, types de graphique) ou demande la suite de la commande ; les boutons du menu lancent les commandes sans arguments.

| Commande                                                                            | Rôle                                                                                                                    |
|---                                                                                  |---                                                                                                                      |
| `/donnees`, `/etat`                                                                 | Aide au chargement ; colonnes et effectifs                                                                              |
| `/descriptif`                                                                       | Statistiques descriptives                                                                                               |
| `/normalite [colonne]`                                                              | Test de normalité                                                                                                       |
| `/t [X] [Y ou 0] [welch\|apparie\|egales] [sup\|inf] [mu=valeur]`                   | Test t (`0` à la place de Y : un échantillon)                                                                           |
| `/anova [colonnes \| mesures groupes]`                                              | ANOVA                                                                                                                   |
| `/anova2 [mesures facteurA facteurB] [interaction=non]`                             | ANOVA à deux facteurs                                                                                                   |
| `/rr croisee\|emboitee [pièces opérateurs mesures] [tol=…] [l=6]`                   | R&R croisée ou emboîtée (`alpha=…` : seuil de l'interaction, croisée) ; `/rr type1 mesures ref=… tol=… [k=20] [l=6]`    |
| `/capa [colonne] [LSL] [USL] [cible] [σ intra]`                                     | Capabilité (`-` pour une valeur absente)                                                                                |
| `/kappa [évaluation 1] [évaluation 2]`                                              | Kappa de Cohen entre deux colonnes (mode référence : application de bureau)                                             |
| `/f [X] [Y] [sup\|inf]`                                                             | Test F de comparaison de deux variances                                                                                 |
| `/z X σ [mu=…]` ou `/z X Y σX σY`                                                   | Test Z, un ou deux échantillons (écart-type connu)                                                                      |
| `/puissance t2 diff=… puissance=… sd=…` (tests : t1, tpaire, t2, z1, p1, p2, anova) | Puissance, taille d'échantillon ou différence détectable (la grandeur laissée vide) + courbe de puissance               |
| `/poisson X lambda0=…` · `X Y` · `total=… duree=…`                                  | Test de Poisson, un ou deux échantillons (colonne de comptages ou totaux)                                               |
| `/estimation [X] [niveau=95] [inf\|sup] [marge=…] [sigma=…]`                        | Intervalle de confiance d'une moyenne ; `n=… moyenne=… s=…` pour un résumé                                              |
| `/identification [colonnes] [lois=…] [sim=…]`                                       | Identification de loi (normale, gamma, Weibull…)                                                                        |
| `/correlation [colonnes] [spearman\|kendall]`                                       | Corrélation                                                                                                             |
| `/regression Y X1 [X2…] [deg=1\|2\|3]`                                              | Régression linéaire, multiple ou polynomiale                                                                            |
| `/serie Y [ordre=col] [modeles=…] [horizon=n]`                                      | Série chronologique : tendances linéaire, quadratique, exponentielle, logistique                                        |
| `/khi2 A B`                                                                         | Khi² d'indépendance (données brutes)                                                                                    |
| `/carte colonnes [type=…] [ordre=col] [phase=col] [rupture=n]`                      | Carte de contrôle X-barre/R, X-barre/S ou I-MR, avec avant/après                                                        |
| `/cartep p\|np\|c\|u comptages [effectifs] [n=…] [phase=col]`                       | Cartes p, np, c et u (attributs ; c sans effectif), avec avant/après                                                    |
| `/graphique type colonnes`                                                          | Histogramme, secteurs, barres, bulles, nuage, individuelles, intervalles, boîte, probabilité                            |
| `/generer loi [paramètres] [n=…] [dec=…] [graine=…] [nom=…]`                        | Données aléatoires (aléatoire, normale, Poisson, Weibull… 15 lois)                                                      |
| `/illustration [sujet]`                                                             | Illustrations de référence du dossier `illustrations`                                                                   |
| `/vocabulaire [mot]`                                                                | Définition d'un terme du fichier `vocabulaire.txt` (sans argument : liste par initiale)                                 |
| `/groupe [colonne] [jour\|mois\|annee] [valeurs=a\|b]`                              | Analyse par groupe : toutes les analyses suivantes sont lancées groupe par groupe (`aucun` pour arrêter)                |
| `/alpha [valeur]`                                                                   | Risque α des tests (0,05 par défaut)                                                                                    |
| `/lecture`                                                                          | Forcer séparateur, décimale, ligne de noms ; relit les données déjà reçues                                              |
| `/export`, `/effacer`, `/annuler`                                                   | Récupérer les données en CSV ; effacer ; annuler une saisie en attente                                                  |

Les données d'une conversation ne sont conservées **qu'en mémoire du bot** : rien n'est écrit sur disque, et elles disparaissent à l'arrêt du bot.

## Interface web Shiny (à la demande)

`app.R` propose une interface web adaptée au téléphone, avec sauvegarde SQLite des projets (dossier `data/`). Elle partage `moteur_statistique.R` avec le reste du projet et confie les analyses avancées au même moteur que l'application de bureau (`cli_statistique.R` et `commandes_avancees.R`). Onglets regroupés en menus : Données (avec la génération de données aléatoires), Description (Descriptif, Normalité, Identification de loi), Tests (Tests t, Test Z, Test de Poisson, Puissance, Estimation, Test F, ANOVA, ANOVA 2 facteurs, Khi²), Modélisation (Corrélation, Régression, Série chronologique), Qualité (Capabilité, Cartes de contrôle avec phases avant / après, Cartes d'attributs p, np, c et u, R&R croisée, emboîtée et de type 1, Kappa), Graphiques, Illustrations, Vocabulaire et Aide. Le package R `jsonlite` est requis. Toutes les analyses y passent par le moteur commun : elles peuvent être lancées groupe par groupe (les colonnes de dates sont reconnues, avec regroupement par jour, mois ou année). Les illustrations sont lues dans le dossier `illustrations/` du projet. Elle n'est **pas** lancée automatiquement : on la démarre quand on en a besoin.

Installation des paquets R (une seule fois) :

```bash
R -e "install.packages(c('shiny','DT','DBI','RSQLite','ggplot2'), repos='https://cloud.r-project.org')"
```

### Règle pare-feu (une seule fois)

L'application écoute sur toutes les interfaces ; le pare-feu n'autorise le port 3838 que depuis le réseau WireGuard :

```bash
sudo ufw allow in on wg0 from 10.221.90.0/24 to any port 3838 proto tcp comment 'shiny-statistiques'
```

Le port n'est donc pas joignable depuis le réseau local ni depuis Internet.

### Fonctions de lancement

À ajouter à la fin de `~/.bashrc` (puis `source ~/.bashrc`) :

```bash
statweb() {
  if pgrep -f "shiny::runApp" >/dev/null; then echo "Shiny déjà en marche."
  else (cd /home/jfbrunet/Projects/Statistiques && nohup R -q -e "shiny::runApp('.', host='0.0.0.0', port=3838)" > shiny.log 2>&1 &); sleep 3; fi
  echo "Pi 5 : http://localhost:3838"; echo "S23 (WireGuard activé) : http://10.221.90.1:3838"
}
statweb_stop() { pkill -f "shiny::runApp" && echo "Shiny arrêté." || echo "Shiny n'était pas lancé."; }
statweb_etat() { ss -ltn | grep 3838 || echo "Shiny n'écoute pas."; }
```

| Commande       | Rôle                                                                 |
|---             |---                                                                   |
| `statweb`      | Démarre Shiny en arrière-plan (il survit à la fermeture du terminal) |
| `statweb_etat` | Indique si Shiny écoute sur le port 3838                             |
| `statweb_stop` | Arrête Shiny                                                         |

Accès : `http://localhost:3838` sur le Pi ; `http://10.221.90.1:3838` sur le S23, WireGuard activé. En cas de problème de démarrage, consulter `shiny.log` dans le dossier du projet.

L'application n'a pas d'authentification : l'arrêter avec `statweb_stop` quand elle n'est plus utilisée, et ne jamais ouvrir ce port sur Internet. Les fichiers `shiny.log` et `data/` n'ont pas vocation à être versionnés (`.gitignore`).

## Après un redémarrage du Pi

- **Bot Telegram** : rien à faire, le service démarre seul. Vérification : `systemctl status telegram-bot-statistiques`.
- **Application de bureau** : cliquer sur l'icône.
- **Shiny** : taper `statweb` dans un terminal si l'interface web est souhaitée ; `statweb_stop` pour l'arrêter ensuite.
- **WireGuard** : le serveur du Pi doit être actif pour l'accès depuis le S23 ; activer le tunnel sur le téléphone.

## Format des données

- Une série de mesures par colonne ; la première ligne contient les noms de colonnes (elle peut être omise si toutes ses valeurs sont numériques).
- Séparateurs reconnus : tabulation, point-virgule, virgule, espace. Décimale : point ou virgule.
- Les cellules vides et les mentions `NA` sont des valeurs manquantes ; des séries de longueurs différentes sont acceptées (colonnes complétées par des valeurs manquantes).
- Une colonne dont toutes les valeurs sont des dates (JJ/MM/AAAA, AAAA-MM-JJ, avec heure facultative) est de type date ; les jours sont lus avant les mois.
- Une colonne contenant au moins une valeur non numérique, qui n'est pas une date, est traitée comme du texte (groupes, évaluations catégorielles) et un avertissement l'indique.
- Pour un test apparié, les paires incomplètes sont écartées ensemble.
- Cartes de contrôle : une ligne par sous-groupe (une colonne par valeur, cellules vides admises) ou une ligne par mesure avec une colonne qui désigne le sous-groupe. R&R croisée : une ligne par mesure, avec une colonne pour la pièce, une pour l'opérateur et une pour la mesure ; chaque opérateur mesure chaque pièce le même nombre de fois (au moins 2). Khi² : deux colonnes de catégories (une ligne par individu), ou un tableau d'effectifs (une ligne par ligne du tableau, une colonne par modalité, éventuellement une colonne de libellés).
- Pour un Kappa avec référence : soit une ligne par échantillon, une colonne de référence (reconnue automatiquement si son nom évoque une référence ou une norme) et une colonne par évaluateur ou par essai ; soit une ligne par évaluation (format long : colonnes réponse, évaluateur, échantillon, essai facultatif et référence, reconnues à leur nom). Les évaluateurs entre eux permettent de juger la répétabilité quand chaque essai est une colonne.

## Précautions statistiques importantes

- Le test de Shapiro-Wilk est disponible pour 3 à 5000 observations et n'est pas calculable si les données sont constantes.
- Les tests et l'ANOVA exigent de vérifier les hypothèses et le plan d'échantillonnage (indépendance des observations en particulier).
- **Cp/Cpk et Cm/Cmk ne sont pas calculés sans écart-type intra-sous-groupe** : l'écart-type global n'est jamais substitué automatiquement. Pp/Ppk utilisent l'écart-type global.
- Les PPM reposent sur une loi normale ajustée aux données : ils sont indicatifs et peuvent être trompeurs si le procédé n'est pas stable ou les données non normales.
- Le Kappa est non pondéré ; pour des catégories ordonnées, un Kappa pondéré peut être préférable. Sa valeur dépend de la prévalence des catégories.
- Les comparaisons de Tukey ne sont fournies que lorsque l'ANOVA globale est significative.
- Le test F est très sensible à la non-normalité : se référer au test de Levene dans ce cas.
- Le khi² suppose des effectifs théoriques suffisants (au moins 5 dans plus de 80 % des cases, aucun inférieur à 1) ; sinon regrouper des modalités ou utiliser le test exact de Fisher.
- Les limites d'une carte de contrôle sont calculées sur l'ensemble des données saisies : un procédé instable les élargit. On recommande au moins 20 à 25 sous-groupes.
- R&R : on recommande au moins 10 pièces représentatives de l'étendue du procédé, 3 opérateurs et 2 à 3 répétitions ; moins de 10 % de variation d'étude est acceptable, de 10 à 30 % acceptable sous conditions, plus de 30 % inacceptable, avec au moins 5 catégories distinctes.

## Limites de cette version

Pas de gestion de comptes, pas de R&R par la méthode des moyennes et étendues, pas de R&R sur plan déséquilibré (croisée ou emboîtée), pas d'ANOVA à plus de deux facteurs ni de modèle à effets aléatoires ou emboîtés (hors R&R). 
Les projets (`.statproj`), le journal des résultats et le rapport PDF sont propres à l'application de bureau ; les analyses ajoutées depuis (Test Z, Poisson, estimation, identification de loi, régression, séries, cartes p/u, graphiques), la génération de données et les illustrations existent aussi dans l'interface web, et pour la plupart dans le bot.
L'interface Shiny a sa propre base SQLite et ne produit pas de rapport PDF. Le bot Telegram ne conserve rien sur disque.
