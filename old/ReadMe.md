# Atelier statistique — Raspberry Pi 5 + Samsung S23

Ce dossier contient deux programmes R complémentaires :

1. **`moteur_statistique.R`** : fonctions de calcul réutilisables.
2. **`app.R`** : interface web Shiny adaptée au téléphone, pour saisir/importer des données, consulter les analyses et enregistrer les projets.

## Fonctionnalités incluses

- Import CSV et saisie de séries en colonnes.
- Statistiques descriptives : effectif, moyenne, médiane, écart-type, variance, quartiles, min/max.
- Test de normalité de Shapiro-Wilk avec p-value et avertissement d'interprétation.
- Tests t : un échantillon, deux échantillons (Welch par défaut), apparié en option.
- ANOVA à un facteur, données en colonnes ou format long.
- Graphiques : boîtes à moustaches, histogrammes et distribution pour capabilité.
- Capabilité : Cm/Cmk/Cp/Cpk si l'écart-type intra-sous-groupe est fourni, Pp/Ppk à partir de l'écart-type global.
- Kappa de Cohen non pondéré pour deux évaluations catégorielles.
- Sauvegarde SQLite des projets et jeux de données, export CSV.

## Installation sur le Pi

Dans un terminal :

```bash
sudo apt update
sudo apt install -y r-base
```

Puis, dans le dossier de l'application :

```bash
R -e "install.packages(c('shiny','DT','DBI','RSQLite','ggplot2'), repos='https://cloud.r-project.org')"
```

Lancer l'application :

```bash
R -e "shiny::runApp('.', host='0.0.0.0', port=3838)"
```

Trouver l'adresse IP locale du Pi :

```bash
hostname -I
```

Sur le Samsung S23 connecté au même réseau Wi-Fi, ouvrir dans Chrome :

```text
http://ADRESSE_IP_DU_PI:3838
```

Remplacer `ADRESSE_IP_DU_PI` par l'adresse affichée par `hostname -I`.

## Démarrage rapide

1. Décompresser le dossier.
2. Se placer dans le dossier contenant `app.R` et `moteur_statistique.R`.
3. Installer les dépendances.
4. Lancer la commande Shiny ci-dessus.
5. Ouvrir l'adresse du Pi depuis le téléphone.
6. Donner un nom au projet, charger les données, puis enregistrer le jeu de données.

## Format de saisie

Le mode « coller / saisir » attend un CSV simple avec une ligne d'en-tête, des virgules comme séparateurs de colonnes et un point comme séparateur décimal, par exemple :

```csv
Lot_A,Lot_B
10.1,10.4
9.9,10.2
10.3,10.1
```

Pour des fichiers français, l'import CSV permet de choisir le point-virgule comme séparateur et la virgule comme décimale.

## Précautions statistiques importantes

- La p-value de Shapiro-Wilk n'est pas une preuve de normalité. Le test est disponible pour 3 à 5000 observations et peut ne pas être calculable si les données sont constantes.
- Les tests et l'ANOVA exigent de vérifier les hypothèses et le plan d'échantillonnage.
- **Cp/Cpk et Cm/Cmk ne sont pas calculés sans sigma intra-sous-groupe**. Il ne faut pas substituer automatiquement l'écart-type global au sigma intra.
- Pp/Ppk sont calculés à partir de l'écart-type global.
- Les estimations de PPM utilisent une loi normale ajustée aux données et sont indicatives ; elles peuvent être trompeuses si le procédé n'est pas stable ou si les données ne sont pas approximativement normales.
- Kappa est non pondéré dans cette première version. Pour des catégories ordonnées, un Kappa pondéré peut être préférable.
- La sauvegarde SQLite est locale au Pi. Prévoir une copie régulière du dossier `data/`.

## Limites de cette première version

Cette version est une base fonctionnelle à tester sur le matériel réel. Elle n'inclut pas encore la gestion des comptes utilisateurs, l'accès depuis Internet, les rapports PDF, les cartes de contrôle, les tests non paramétriques, les comparaisons post-hoc ANOVA ni une étude R&R complète. Ne pas exposer le port Shiny directement sur Internet sans mettre en place une authentification et une configuration sécurisée.
