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

| Fichier                        | Rôle                                                                                                                                               |
|---                             |---                                                                                                                                                 |
| `statistiques.py`              | Application de bureau (Tkinter) avec écran d'accueil ; contient aussi le cœur partagé avec le bot (lecture des données, appel de R, mise en forme) |
| `telegram_bot_statistiques.py` | Bot Telegram (dans le dossier du bot)                                                                                                              |
| `cli_statistique.R`            | Passerelle en ligne de commande vers le moteur R (résultats JSON + graphiques PNG)                                                                 |
| `moteur_statistique.R`         | Fonctions de calcul R réutilisables                                                                                                                |
| `app.R`                        | Interface web Shiny, facultative (voir plus bas)                                                                                                   |
| `Statistiques.desktop`         | Lanceur pour le bureau (non versionné : il reste sur le bureau du Pi)                                                                              |
| `icons/6Sigma.png`             | Icône de l'écran d'accueil et du lanceur                                                                                                           |

## Fonctionnalités

- **Données** : saisie ou collage de texte, ouverture d'un fichier `txt`, `csv`, `tsv`, `dat` ou `xlsx`, export CSV.
- **Statistiques descriptives** : effectif, moyenne, médiane, écart-type, variance, quartiles, extrêmes, Shapiro-Wilk, boîtes à moustaches, histogrammes.
- **Normalité** : Shapiro-Wilk, histogramme avec courbe normale, droite de Henry.
- **Tests t** : un échantillon, deux échantillons indépendants (Welch par défaut, variances égales en option), échantillons appariés ; hypothèse bilatérale ou unilatérale ; intervalle de confiance et taille d'effet (d de Cohen).
- **ANOVA à un facteur** : colonnes = groupes ou format long (colonne de mesures + colonne de groupes) ; contrôle des résidus (Shapiro-Wilk) et de l'homogénéité des variances (Fligner-Killeen) ; comparaisons deux à deux de Tukey si le test global est significatif.
- **Capabilité** : Pp/Ppk à partir de l'écart-type global ; Cp/Cpk/Cm/Cmk uniquement si un écart-type intra-sous-groupe est fourni ; PPM estimés ; histogramme avec limites de spécification.
- **Kappa de Cohen** non pondéré, avec matrice de confusion, en deux modes : comparaison de deux évaluations, ou comparaison d'un ou plusieurs évaluateurs à une **colonne de référence (norme)** (application de bureau). Dans ce second mode : Kappa de chaque évaluateur contre la référence avec un verdict (satisfaisant, à améliorer, insuffisant), accord en % et intervalle de confiance, accord de tous les évaluateurs avec la référence, Kappa des évaluateurs deux à deux et graphique. Interprétation selon l'échelle de Landis et Koch (1977) ; repères AIAG (MSA) pour les systèmes de mesure : Kappa > 0,75 bon à excellent, Kappa < 0,40 insuffisant. Les références sont rappelées avec chaque résultat.

## Installation sur le Pi

```bash
sudo apt update
sudo apt install -y r-base r-cran-jsonlite python3-tk python3-requests
```

Recommandé, pour un affichage optimal des graphiques dans l'application de bureau :

```bash
sudo apt install -y python3-pil.imagetk
```

Facultatif, pour lire les fichiers `.xlsx` :

```bash
pip install openpyxl --break-system-packages
```

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
Name=Atelier statistique
Exec=python3 /home/jfbrunet/Projects/Statistiques/statistiques.py
Icon=/home/jfbrunet/Projects/Statistiques/icons/6Sigma.png
Terminal=false
Categories=Education;Science;
```

Selon le gestionnaire de fichiers, un clic droit sur l'icône puis « Autoriser le lancement » peut être nécessaire la première fois.

Utilisation : onglet **Données** pour saisir/coller des données ou ouvrir un fichier, puis un onglet par analyse. La fenêtre s'ouvre agrandie ; le séparateur entre le texte des résultats et les graphiques se déplace à la souris, et un clic sur un graphique l'ouvre en grand. Chaque résultat peut être enregistré (texte, avec les graphiques PNG à côté). Les options de lecture (séparateur, décimale, ligne de noms de colonnes) sont en mode automatique par défaut et peuvent être forcées.

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
- Les résultats arrivent en texte à chasse fixe, suivis des graphiques en images.

| Commande                                                          | Rôle                                                                        |
|---                                                                |---                                                                          |
| `/donnees`, `/etat`                                               | Aide au chargement ; colonnes et effectifs                                  |
| `/descriptif`                                                     | Statistiques descriptives                                                   |
| `/normalite [colonne]`                                            | Test de normalité                                                           |
| `/t [X] [Y ou 0] [welch\|apparie\|egales] [sup\|inf] [mu=valeur]` | Test t (`0` à la place de Y : un échantillon)                               |
| `/anova [colonnes \| mesures groupes]`                            | ANOVA                                                                       |
| `/capa [colonne] [LSL] [USL] [cible] [σ intra]`                   | Capabilité (`-` pour une valeur absente)                                    |
| `/kappa [évaluation 1] [évaluation 2]`                            | Kappa de Cohen entre deux colonnes (mode référence : application de bureau) |
| `/lecture`                                                        | Forcer séparateur, décimale, ligne de noms ; relit les données déjà reçues  |
| `/export`, `/effacer`, `/annuler`                                 | Récupérer les données en CSV ; effacer ; annuler une saisie en attente      |

Les données d'une conversation ne sont conservées **qu'en mémoire du bot** : rien n'est écrit sur disque, et elles disparaissent à l'arrêt du bot.

## Interface web Shiny (à la demande)

`app.R` propose une interface web adaptée au téléphone, avec sauvegarde SQLite des projets (dossier `data/`). Elle partage `moteur_statistique.R` avec le reste du projet. Elle n'est **pas** lancée automatiquement : on la démarre quand on en a besoin.

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
- Une colonne contenant au moins une valeur non numérique est traitée comme du texte (groupes, évaluations catégorielles) et un avertissement l'indique.
- Pour un test apparié, les paires incomplètes sont écartées ensemble.
- Pour un Kappa avec référence : une ligne par échantillon, une colonne de référence (reconnue automatiquement si son nom évoque une référence ou une norme) et une colonne par évaluateur ou par essai. Les évaluateurs entre eux permettent de juger la répétabilité quand chaque essai est une colonne.

## Précautions statistiques importantes

- Le test de Shapiro-Wilk est disponible pour 3 à 5000 observations et n'est pas calculable si les données sont constantes.
- Les tests et l'ANOVA exigent de vérifier les hypothèses et le plan d'échantillonnage (indépendance des observations en particulier).
- **Cp/Cpk et Cm/Cmk ne sont pas calculés sans écart-type intra-sous-groupe** : l'écart-type global n'est jamais substitué automatiquement. Pp/Ppk utilisent l'écart-type global.
- Les PPM reposent sur une loi normale ajustée aux données : ils sont indicatifs et peuvent être trompeurs si le procédé n'est pas stable ou les données non normales.
- Le Kappa est non pondéré ; pour des catégories ordonnées, un Kappa pondéré peut être préférable. Sa valeur dépend de la prévalence des catégories.
- Les comparaisons de Tukey ne sont fournies que lorsque l'ANOVA globale est significative.

## Limites de cette version

Pas de gestion de comptes, pas de rapports PDF, pas de cartes de contrôle, pas de tests non paramétriques, pas d'étude R&R complète, ANOVA à un seul facteur. Les versions Python (bureau et bot) n'enregistrent pas de projets : seules les données et résultats exportés sont conservés, à la demande.
