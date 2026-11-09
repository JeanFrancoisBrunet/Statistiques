#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# =============================================================================
#  Statistiques — Raspberry Pi 5
#  Application de bureau (Tkinter) et cœur partagé avec telegram_bot_statistiques.py.
#  - Données : saisie / collage de texte, ou fichier (txt, csv, tsv, dat, xlsx)
#  - Calculs : délégués à R (moteur_statistique.R via cli_statistique.R)
#      statistiques descriptives, normalité (Shapiro-Wilk), tests t, Z, F, Poisson, ANOVA à un et à deux
#      facteurs (+ Tukey), khi², capabilité (Cp/Cpk/Pp/Ppk), cartes de contrôle (X-barre, I-MR, p, np, c, u),
#      R&R (croisée, emboîtée, type 1), Kappa de Cohen, régression, séries chronologiques, génération de données
#  - Au lancement : écran d'accueil avec icons/6Sigma.png
#
#  Dépendances (Raspberry Pi OS / Debian) :
#    Obligatoires : python3 + Tkinter, R + jsonlite
#                   sudo apt install -y r-base r-cran-jsonlite python3-tk
#    Recommandées : Pillow avec ImageTk (affichage des graphiques)      sudo apt install -y python3-pil.imagetk
#                   xclip (bouton « Copier image » sous X11 ; wl-clipboard sous Wayland)
#                                                                       sudo apt install -y xclip
#                   reportlab (rapport PDF)                             sudo apt install -y python3-reportlab
#    Facultatives : openpyxl (lecture des fichiers .xlsx)               pip install openpyxl --break-system-packages
#                   python3-requests (uniquement pour le bot Telegram)  sudo apt install -y python3-requests
#  Lancement   : python3 statistiques.py
#  Le bot Telegram importe ce module : tkinter n'est chargé que pour l'interface.
#  Auteur : Jean-François BRUNET – JFBConseils – Novembre 2026
#  Rev.   :
# =============================================================================

import atexit
import csv
import datetime
import json
import math
import os
import queue
import random
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import zipfile
from pathlib import Path

DOSSIER = Path(__file__).resolve().parent
ICONE = DOSSIER / "icons" / "6Sigma.png"
SCRIPT_R = DOSSIER / "cli_statistique.R"
TITRE = "Statistiques"
MAX_LIGNES = 200_000
MAX_GENERE = 100_000                     # nombre maximal de valeurs générées en une fois
EXT_PROJET = ".statproj"                 # fichier de projet (archive zip : données, résultats, graphiques)
FORMAT_PROJET = "Statistiques"
VERSION_PROJET = 1
TITRE_RAPPORT = "Rapport d'analyse statistiques"

MANQUANTS = {"", "na", "n/a", "nan", "null", "none"}
MOTS_REFERENCE = ("ref", "réf", "norme", "standard", "etalon", "étalon", "expert", "vrai")   # noms de colonne de référence
MOTS_EVALUATEUR = ("evalu", "évalu", "apprais", "operat", "opérat", "juge", "controleur", "contrôleur", "inspect")
MOTS_REPONSE = ("reponse", "réponse", "resultat", "résultat", "jugement", "decision", "décision", "verdict")
MOTS_ECHANTILLON = ("piece", "pièce", "echantillon", "échantillon", "sample", "part", "unite", "unité")
MOTS_ESSAI = ("essai", "trial", "repetition", "répétition", "replic", "réplic")

RE_DEC_VIRGULE = re.compile(r"^[-+]?\d+,\d+$")
RE_DEC_POINT = re.compile(r"^[-+]?\d+\.\d+$")
RE_ENTIER = re.compile(r"^[-+]?\d+$")


class ErreurStat(Exception):
    """Erreur destinée à l'utilisateur (message lisible)."""


# =============================================================================
#  Lecture des données
# =============================================================================
def _convertir(texte, dec):
    """(valeur, ok) : ok=False si la chaîne n'est pas un nombre ; valeur=None si manquante."""
    t = str(texte).strip().replace(" ", "").replace(" ", "")
    if t.lower() in MANQUANTS:
        return None, True
    if "_" in t:
        return None, False
    if dec == ",":
        t = t.replace(",", ".")
    try:
        v = float(t)
    except ValueError:
        return None, False
    if math.isnan(v) or math.isinf(v):
        return None, False
    return v, True


RE_DATE_ISO = re.compile(r"^(\d{4})-(\d{1,2})-(\d{1,2})(?:[ T]+(\d{1,2}):(\d{2})(?::(\d{2}))?(?:[.,]\d+)?)?$")
RE_DATE_JMA = re.compile(r"^(\d{1,2})[/.\-](\d{1,2})[/.\-](\d{2}|\d{4})(?:[ T]+(\d{1,2})[:h](\d{2})(?::(\d{2}))?)?$")
RE_DATE_STOCKEE = re.compile(r"^\d{4}-\d{2}-\d{2}(?: \d{2}:\d{2}(?::\d{2})?)?$")
# Lois de génération : (libellé, code R, [(clé, libellé du paramètre, valeur proposée)], description)
LOIS_GENERATION = [
    ("Aléatoire (random)", "aleatoire", [("min", "Minimum", "0"), ("max", "Maximum", "100")],
     "Nombres tirés au hasard entre deux bornes, sans loi particulière : toutes les valeurs sont équiprobables (mettre 0 décimale pour obtenir des entiers)."),
    ("Normale (Gauss)", "normale", [("moyenne", "Moyenne", "0"), ("ecart_type", "Écart-type", "1")],
     "Distribution la plus importante en statistique : décrit des phénomènes naturels dont les valeurs se concentrent autour d'une moyenne, avec une symétrie parfaite."),
    ("Uniforme", "uniforme", [("min", "Minimum", "0"), ("max", "Maximum", "1")],
     "Toutes les valeurs comprises entre deux bornes sont équiprobables."),
    ("Poisson", "poisson", [("lambda", "Moyenne λ", "3")],
     "Nombre d'événements qui se produisent dans un intervalle de temps ou d'espace donné, quand ils arrivent de manière aléatoire et indépendante."),
    ("Student (t)", "student", [("df", "Degrés de liberté", "10")],
     "Distribution d'une moyenne d'échantillon quand l'écart-type est estimé à partir de données limitées."),
    ("Fisher-Snedecor (F)", "fisher", [("df1", "ddl du numérateur", "5"), ("df2", "ddl du dénominateur", "10")],
     "Rapport de deux variances estimées à partir d'échantillons."),
    ("Khi²", "khi2", [("df", "Degrés de liberté", "4")],
     "Somme des carrés de variables normales centrées réduites indépendantes."),
    ("Binomiale", "binomiale", [("essais", "Nombre d'essais", "10"), ("p", "Probabilité de succès", "0,5")],
     "Nombre de succès obtenus lors d'un nombre fixe d'expériences indépendantes."),
    ("Géométrique", "geometrique", [("p", "Probabilité de succès", "0,2")],
     "Nombre d'essais nécessaires pour obtenir le premier succès dans une série d'expériences indépendantes (au moins 1)."),
    ("Hypergéométrique", "hypergeometrique", [("N", "Taille de la population", "50"), ("K", "Succès dans la population", "20"), ("m", "Taille du tirage", "10")],
     "Nombre de succès obtenus lors d'un tirage sans remise dans une population finie."),
    ("Exponentielle", "exponentielle", [("moyenne", "Moyenne (1 / taux)", "1")],
     "Temps d'attente avant qu'un événement ne se produise dans un processus continu."),
    ("Gamma", "gamma", [("forme", "Forme", "2"), ("echelle", "Échelle", "1")],
     "Somme de temps d'attente exponentiels ; modélise des durées et des quantités positives asymétriques."),
    ("Logistique", "logistique", [("position", "Position", "0"), ("echelle", "Échelle", "1")],
     "Proche de la loi normale, avec des queues plus lourdes."),
    ("Weibull", "weibull", [("forme", "Forme", "1,5"), ("echelle", "Échelle", "1")],
     "Durées de vie et de défaillance : la forme règle le taux de panne (décroissant, constant ou croissant)."),
    ("Log-normale", "lognormale", [("mu", "Moyenne du logarithme", "0"), ("sigma", "Écart-type du logarithme", "1")],
     "Variable dont le logarithme suit une loi normale : valeurs positives, asymétrie à droite."),
    ("Bêta", "beta", [("a", "Forme a", "2"), ("b", "Forme b", "5")],
     "Valeurs comprises entre 0 et 1 (proportions, taux)."),
]
DOSSIER_ILLUSTRATIONS = DOSSIER / "illustrations"
# (clé, titre) : les images d'un thème se nomment <clé>.png, ou <clé>_1.png, <clé>_2.png… (dans le dossier « illustrations »)
ILLUSTRATIONS = [
    ("demarche_6sigma", "Démarche 6 Sigma"), 
    ("types_donnees", "Données Variables Vs Attributs"),
    ("dpmo", "Défauts par Million d'Opportunités"), 
    ("multi_variations", "Variations multiples"), 
    ("causes_commune_speciale", "Causes Communes Vs Spéciales"), 
    ("stat_descriptive", "Statistiques Descriptives"),  
    ("stat_inferentielle", "Statistiques Inférentielles"),
    ("interpreter_distribution", "Interpréter une distribution"), 
    ("interpreter_p_value", "Interpréter une P-value"), 
    ("moyenne_ecart_type", "Moyenne et écart-type"),
    ("loi_normale", "Loi normale"),
    ("droite_henry", "Interpréter une droite d'Henry"),
    ("boite_moustaches", "Boîte à moustaches"),
    ("pareto", "Pareto"),
    ("msa", "Analyse Système Mesure"),
    ("carte_controle", "Cartes de Contrôle"),
    ("tests_hypotheses", "Tests d'hypothèses"),
    ("risques_alpha_beta", "Risques α et β"),
    ("khi2", "Test du khi²"),
    ("test_z", "Test Z"),
    ("test_t_1", "Test t à 1 échantillon"),
    ("test_t_2", "Test t à 2 échantillons"),
    ("test_t_apparie", "Test t apparié"),
    ("bartlett", "Test de Bartlett"),
    ("levene", "Test de Levene"),
    ("mann_whitney", "Test de Mann-Whitney"),
    ("kruskal_wallis", "Test de Kruskal-Wallis"),
]
# thème proposé par le bouton « Illustration… » de chaque onglet d'analyse
ILLUSTRATION_ONGLET = {"Descriptif": "stat_descriptive", "Normalité": "loi_normale", "Test t": "test_t_1", "Test Z": "test_z", "Khi²": "khi2",
                       "Identification de loi": "interpreter_distribution", "Graphiques": "boite_moustaches",
                       "ANOVA": "tests_hypotheses", "ANOVA 2 facteurs": "tests_hypotheses", "R&R": "msa",
                       "Cartes de contrôle": "carte_controle", "Cartes d'attributs": "carte_controle", "Données": "types_donnees", "Rapport": "demarche_6sigma"}


def images_illustration(cle):
    """Fichiers PNG du sujet, triés (<clé>.png, <clé>_1.png, <clé>_2.png…)."""
    if not DOSSIER_ILLUSTRATIONS.is_dir():
        return []
    return sorted((f for f in DOSSIER_ILLUSTRATIONS.iterdir()
                   if f.suffix.lower() == ".png" and (f.stem.lower() == cle or f.stem.lower().startswith(cle + "_"))),
                  key=lambda f: f.name.lower())


# ---- Vocabulaire : fichier texte « Terme : définition », complété au fur et à mesure (lu aussi par le bot et l'interface web) ----
FICHIER_VOCABULAIRE = DOSSIER / "vocabulaire.txt"
ENTETE_VOCABULAIRE = [
    "# Vocabulaire du programme Statistiques — un terme par entrée, au format « Terme : définition ».",
    "# Pour compléter la liste : ajouter une ligne (ou utiliser le bouton « Ajouter… » de l'onglet Vocabulaire), puis « Recharger » dans le programme.",
    "# Une définition peut continuer sur les lignes suivantes, si elles commencent par deux espaces.",
    "# Les lignes qui commencent par # sont ignorées. Ce fichier est lu par l'application de bureau, l'interface web et le bot Telegram."]


def cle_tri_texte(s):
    """Clé de tri et de recherche : sans accents ni majuscules."""
    import unicodedata
    return "".join(c for c in unicodedata.normalize("NFD", str(s).lower()) if unicodedata.category(c) != "Mn")


def lire_vocabulaire(chemin=None):
    """Liste triée de (terme, définition) lue dans le fichier de vocabulaire ; [] s'il n'existe pas.
    Format : « Terme : définition » ; les lignes suivantes commençant par une espace prolongent la définition ; # = commentaire."""
    chemin = Path(chemin) if chemin else FICHIER_VOCABULAIRE
    try:
        lignes = chemin.read_text(encoding="utf-8-sig").splitlines()
    except OSError:
        return []
    termes = []
    for lg in lignes:
        if not lg.strip() or lg.lstrip().startswith("#") and not lg[:1].isspace():
            continue
        if lg[:1].isspace():
            if termes:
                termes[-1][1].append(lg.strip())
            continue
        m = re.match(r"^(.*?)\s:(?:\s+(.*))?$", lg.replace("\xa0", " "))
        if m:
            termes.append([m.group(1).strip(), [m.group(2).strip()] if m.group(2) and m.group(2).strip() else []])
    return sorted(((t, "\n".join(d)) for t, d in termes), key=lambda e: cle_tri_texte(e[0]))


def ecrire_vocabulaire(termes, chemin=None):
    """Réécrit le fichier de vocabulaire (trié) en conservant les lignes de commentaires de tête."""
    chemin = Path(chemin) if chemin else FICHIER_VOCABULAIRE
    entete = []
    try:
        for lg in chemin.read_text(encoding="utf-8-sig").splitlines():
            if lg.lstrip().startswith("#") or (not lg.strip() and not entete):
                entete.append(lg)
            elif lg.strip():
                break
    except OSError:
        pass
    entete = [l for l in entete if l.strip()] or list(ENTETE_VOCABULAIRE)
    sortie = list(entete) + [""]
    for terme, definition in sorted(termes, key=lambda e: cle_tri_texte(e[0])):
        morceaux = [l.strip() for l in str(definition).splitlines() if l.strip()]
        sortie.append(f"{terme.strip()} : {morceaux[0] if morceaux else ''}".rstrip())
        sortie += ["  " + l for l in morceaux[1:]]
    tmp = chemin.with_name(chemin.name + ".tmp")
    tmp.write_text("\n".join(sortie) + "\n", encoding="utf-8")
    tmp.replace(chemin)


def chercher_vocabulaire(termes, requete, dans_definitions=True):
    """Termes correspondant à la requête (tous les mots, sans tenir compte des accents ni de la casse) :
    d'abord ceux dont le nom correspond, puis ceux dont seule la définition correspond."""
    mots = cle_tri_texte(requete).split()
    if not mots:
        return list(termes)
    dans_nom = [e for e in termes if all(m in cle_tri_texte(e[0]) for m in mots)]
    if not dans_definitions:
        return dans_nom
    reste = [e for e in termes if e not in dans_nom and all(m in cle_tri_texte(e[0] + " " + e[1]) for m in mots)]
    return dans_nom + reste


AUCUN_GROUPE = "— aucun —"
AUCUN_ORDRE = "— ordre de saisie —"
PERIODES_DATE = {"Jour": "jour", "Mois": "mois", "Année": "annee"}
MAX_GROUPES = 30


def _analyser_date(texte, jour_d_abord=True):
    """datetime.datetime ou None : formats AAAA-MM-JJ, JJ/MM/AAAA (ou - ou .), avec heure facultative (hh:mm[:ss])."""
    t = str(texte).strip()
    try:
        m = RE_DATE_ISO.match(t)
        if m:
            a, mo, j, h, mi, s = m.groups()
            return datetime.datetime(int(a), int(mo), int(j), int(h or 0), int(mi or 0), int(s or 0))
        m = RE_DATE_JMA.match(t)
        if m:
            x, y, a, h, mi, s = m.groups()
            an = int(a) + (0 if len(a) == 4 else (2000 if int(a) < 70 else 1900))
            j, mo = (int(x), int(y)) if jour_d_abord else (int(y), int(x))
            return datetime.datetime(an, mo, j, int(h or 0), int(mi or 0), int(s or 0))
    except ValueError:
        return None
    return None


def _colonne_date(brut):
    """Liste de dates ISO (« AAAA-MM-JJ [hh:mm[:ss]] », None si manquante) si toutes les valeurs non vides de la colonne
    sont des dates ; sinon None. Le jour est lu avant le mois, sauf si cela est impossible pour une valeur au moins."""
    pleines = [v for v in brut if v.strip().lower() not in MANQUANTS]
    if not pleines:
        return None
    for jour_d_abord in (True, False):
        dts = [None if v.strip().lower() in MANQUANTS else _analyser_date(v, jour_d_abord) for v in brut]
        if all(d is not None for v, d in zip(brut, dts) if v.strip().lower() not in MANQUANTS):
            vus = [d for d in dts if d is not None]
            heure = any(d.hour or d.minute or d.second for d in vus)
            sec = any(d.second for d in vus)
            f = "%Y-%m-%d %H:%M:%S" if sec else ("%Y-%m-%d %H:%M" if heure else "%Y-%m-%d")
            return [None if d is None else d.strftime(f) for d in dts]
    return None


def date_affichee(iso):
    """« AAAA-MM-JJ hh:mm » -> « JJ/MM/AAAA hh:mm »."""
    if not iso or not RE_DATE_STOCKEE.match(str(iso)):
        return "" if iso is None else str(iso)
    a, m, j = iso[0:4], iso[5:7], iso[8:10]
    return f"{j}/{m}/{a}" + (iso[10:] if len(iso) > 10 else "")


def _cle_tri(valeur):
    try:
        return (0, float(str(valeur).replace(",", ".")), "")
    except ValueError:
        return (1, 0.0, str(valeur).casefold())


def _decouper(ligne, sep):
    if sep is None:
        return [ligne.strip()]
    if sep == " ":
        return ligne.split()
    try:
        return [c.strip() for c in next(csv.reader([ligne], delimiter=sep, skipinitialspace=True))]
    except Exception:
        return [c.strip() for c in ligne.split(sep)]


def _separateur(lignes):
    """Détecte le séparateur : '\\t', ';', ',', ' ' ou None (une seule colonne)."""
    ech = lignes[:50]
    moitie = len(ech) / 2
    for sep in ("\t", ";"):
        if sep in ech[0] and sum(sep in l for l in ech) >= moitie:
            return sep
    # Ni tabulation ni point-virgule : virgule décimale (données séparées par des espaces) ou virgule séparateur ?
    jetons = [t for l in ech for t in l.split()]
    if (jetons and any(RE_DEC_VIRGULE.match(t) for t in jetons)
            and all(RE_DEC_VIRGULE.match(t) or RE_ENTIER.match(t) for t in jetons)):
        return " " if any(len(l.split()) > 1 for l in ech) else None
    if "," in ech[0] and sum("," in l for l in ech) >= moitie:
        return ","
    if all(len(l.split()) > 1 for l in ech) and len({len(l.split()) for l in ech}) == 1:
        return " "
    if len(ech) == 1 and len(ech[0].split()) > 1:
        return " "
    return None


def _decimale(champs, sep):
    if sep == ",":
        return "."
    jetons = [c for ligne in champs for c in ligne]
    virg = sum(bool(RE_DEC_VIRGULE.match(t)) for t in jetons)
    pt = sum(bool(RE_DEC_POINT.match(t)) for t in jetons)
    return "," if virg > pt else "."


def _nom_propre(nom):
    """Nom de colonne sans séparateur ni guillemet ni saut de ligne."""
    return re.sub(r"[;\t\r\n\"]+", " ", str(nom)).strip()


class Donnees:
    """Jeu de données : colonnes nommées, numériques (float / None) ou texte (str / None)."""

    def __init__(self, colonnes, types, avertissements=None, source="saisie"):
        self.colonnes = colonnes          # dict nom -> liste
        self.types = types                # dict nom -> "num" | "txt"
        self.avertissements = list(avertissements or [])
        self.source = source

    @property
    def noms(self):
        return list(self.colonnes)

    @property
    def noms_num(self):
        return [n for n in self.colonnes if self.types[n] == "num"]

    @property
    def noms_txt(self):
        return [n for n in self.colonnes if self.types[n] == "txt"]

    @property
    def noms_date(self):
        return [n for n in self.colonnes if self.types[n] == "date"]

    def groupe_etiquettes(self, colonne, periode=None):
        """Étiquette de groupe de chaque ligne (None si la case est vide) : texte, nombre ou, pour une date, la période
        (« jour » AAAA-MM-JJ, « mois » AAAA-MM, « annee » AAAA)."""
        if colonne not in self.colonnes:
            raise ErreurStat(f"Colonne de groupes introuvable : « {colonne} ».")
        t = self.types[colonne]
        out = []
        for v in self.colonnes[colonne]:
            if v is None:
                out.append(None)
            elif t == "num":
                out.append(fmt_nombre(v, 15))
            elif t == "date":
                out.append(str(v)[:{"annee": 4, "mois": 7}.get(periode or "jour", 10)])
            else:
                out.append(str(v))
        return out

    def groupe_valeurs(self, colonne, periode=None):
        """Groupes distincts de la colonne (ordre naturel : nombres puis textes)."""
        return sorted({e for e in self.groupe_etiquettes(colonne, periode) if e is not None}, key=_cle_tri)

    def avec_groupe(self, colonne, periode=None):
        """Copie des données avec la colonne technique « __groupe__ » (groupe de chaque ligne) ; l'original n'est pas modifié."""
        cols = dict(self.colonnes)
        types = dict(self.types)
        cols["__groupe__"] = self.groupe_etiquettes(colonne, periode)
        types["__groupe__"] = "txt"
        return Donnees(cols, types, self.avertissements, self.source)

    @property
    def nb_lignes(self):
        return max((len(v) for v in self.colonnes.values()), default=0)

    def resume(self):
        num, dat = len(self.noms_num), len(self.noms_date)
        return f"{self.nb_lignes} ligne(s), {len(self.noms)} colonne(s) dont {num} numérique(s)" + (f" et {dat} de dates." if dat else ".")

    def lignes(self, limite=None):
        n = self.nb_lignes if limite is None else min(limite, self.nb_lignes)
        cols = [self.colonnes[k] for k in self.noms]
        return [[(c[i] if i < len(c) else None) for c in cols] for i in range(n)]

    def ajouter_colonne(self, nom, valeurs, type_="num"):
        """Ajoute une colonne ; le nom est rendu unique. Retourne le nom effectivement utilisé."""
        base = _nom_propre(nom) or "Serie"
        n, k = base, 2
        while n in self.colonnes:
            n = f"{base}_{k}"
            k += 1
        self.colonnes[n] = list(valeurs)
        self.types[n] = type_
        return n

    def vers_dict(self):
        return {"colonnes": self.colonnes, "types": self.types,
                "avertissements": self.avertissements, "source": self.source}

    @classmethod
    def depuis_dict(cls, d):
        """Reconstruit un jeu de données à partir d'un fichier de projet (contrôle du contenu)."""
        invalide = ErreurStat("Fichier de projet invalide : données illisibles.")
        if not isinstance(d, dict) or not isinstance(d.get("colonnes"), dict) or not isinstance(d.get("types"), dict):
            raise invalide
        colonnes, types = {}, {}
        for nom, vals in d["colonnes"].items():
            t = d["types"].get(nom)
            if t not in ("num", "txt", "date") or not isinstance(vals, list) or len(vals) > MAX_LIGNES:
                raise invalide
            if t == "date":
                if not all(v is None or (isinstance(v, str) and RE_DATE_STOCKEE.match(v)) for v in vals):
                    raise invalide
            if t == "num":
                if not all(v is None or (isinstance(v, (int, float)) and not isinstance(v, bool)) for v in vals):
                    raise invalide
                colonnes[str(nom)] = [None if v is None else float(v) for v in vals]
            else:
                if not all(v is None or isinstance(v, str) for v in vals):
                    raise invalide
                colonnes[str(nom)] = list(vals)
            types[str(nom)] = t
        if not colonnes:
            raise invalide
        avert = d.get("avertissements")
        avert = [a for a in avert if isinstance(a, str)] if isinstance(avert, list) else []
        return cls(colonnes, types, avert, str(d.get("source") or "projet"))

    def resume_rapport(self):
        """Description des données pour le rapport PDF."""
        cols = []
        for n in self.noms:
            valides = sum(1 for x in self.colonnes[n] if x is not None)
            cols.append((n, {"num": "numérique", "date": "date"}.get(self.types[n], "texte"), valides, self.nb_lignes - valides))
        return {"source": self.source, "nb_lignes": self.nb_lignes, "colonnes": cols, "avertissements": list(self.avertissements)}

    def ecrire_csv(self, chemin):
        with open(chemin, "w", encoding="utf-8", newline="") as f:
            w = csv.writer(f)
            w.writerow(self.noms)
            for ligne in self.lignes():
                w.writerow(["" if v is None else (repr(v) if isinstance(v, float) else v) for v in ligne])      # dates : ISO, lues par R

    def vers_csv_texte(self):
        sortie = []
        sortie.append(";".join(self.noms))
        dates = [self.types[n] == "date" for n in self.noms]
        for ligne in self.lignes():
            sortie.append(";".join("" if v is None else (fmt_nombre(v, 15) if isinstance(v, float) else (date_affichee(v) if dt else str(v)))
                                   for v, dt in zip(ligne, dates)))
        return "\n".join(sortie) + "\n"


def lire_texte(texte, sep="auto", dec="auto", entete="auto"):
    """Interprète un texte (saisie ou contenu de fichier) : retourne un objet Donnees.

    sep    : 'auto', '\\t', ';', ',' ou ' '
    dec    : 'auto', '.' ou ','
    entete : 'auto', True ou False (la première ligne contient les noms de colonnes)
    """
    if texte is None:
        raise ErreurStat("Aucune donnée à lire.")
    lignes = [l for l in texte.replace("\r\n", "\n").replace("\r", "\n").replace("﻿", "").split("\n") if l.strip()]
    if not lignes:
        raise ErreurStat("Aucune donnée à lire.")
    if len(lignes) > MAX_LIGNES:
        raise ErreurStat(f"Trop de lignes (maximum {MAX_LIGNES}).")

    s = _separateur(lignes) if sep == "auto" else (None if sep in (None, "") else sep)
    champs = [_decouper(l, s) for l in lignes]
    d = _decimale(champs, s) if dec == "auto" else dec

    # Une seule ligne de nombres : c'est une série saisie à l'horizontale
    if len(champs) == 1 and entete in ("auto", False) and all(_convertir(c, d)[1] and c.strip() for c in champs[0]):
        champs = [[c] for c in champs[0]]
        entete = False

    premiere = champs[0]
    if entete == "auto":
        entete = any((not _convertir(c, d)[1]) for c in premiere)
    nc = max(len(r) for r in champs)
    if entete:
        noms = list(premiere) + [""] * (nc - len(premiere))
        corps = champs[1:]
    else:
        noms = [""] * nc
        corps = champs
    if not corps:
        raise ErreurStat("Aucune ligne de données : seule une ligne de noms de colonnes a été trouvée.")

    propres, vus = [], {}
    for i, n in enumerate(noms):
        n = n.strip().strip('"') or (f"Serie_{i + 1}" if not entete else f"Colonne_{i + 1}")
        base, k = n, 2
        while n in vus:
            n = f"{base}_{k}"
            k += 1
        vus[n] = True
        propres.append(n)

    colonnes, types, avert = {}, {}, []
    for j, nom in enumerate(propres):
        brut = [(r[j].strip() if j < len(r) else "") for r in corps]
        conv = [_convertir(v, d) for v in brut]
        non_manquantes = [v for v in brut if v.strip().lower() not in MANQUANTS]
        if not non_manquantes:
            avert.append(f"Colonne « {nom} » entièrement vide : ignorée.")
            continue
        iso = None if all(ok for _, ok in conv) else _colonne_date(brut)
        if all(ok for _, ok in conv):
            colonnes[nom] = [v for v, _ in conv]
            types[nom] = "num"
        elif iso is not None:
            colonnes[nom] = iso
            types[nom] = "date"
        else:
            colonnes[nom] = [None if v.strip().lower() in MANQUANTS else v for v in brut]
            types[nom] = "txt"
            nb_num = sum(1 for (v, ok), b in zip(conv, brut) if ok and v is not None)
            if nb_num >= 0.5 * len(non_manquantes):
                mauvais = [b for (v, ok), b in zip(conv, brut) if not ok][:3]
                avert.append(f"Colonne « {nom} » : valeurs non numériques ({', '.join(mauvais)}…) — colonne traitée comme du texte.")
    if not colonnes:
        raise ErreurStat("Aucune colonne exploitable.")
    return Donnees(colonnes, types, avert)


def lire_fichier(chemin, sep="auto", dec="auto", entete="auto"):
    """Lit un fichier de données (txt, csv, tsv, dat, xlsx)."""
    p = Path(chemin)
    if not p.is_file():
        raise ErreurStat(f"Fichier introuvable : {p}")
    ext = p.suffix.lower()
    if ext == ".xlsx":
        return _lire_xlsx(p, entete)
    if ext in (".xls", ".ods", ".xlsm"):
        raise ErreurStat("Format non pris en charge : enregistrer le classeur en .xlsx ou en .csv.")
    brut = p.read_bytes()
    for enc in ("utf-8-sig", "cp1252", "latin-1"):
        try:
            texte = brut.decode(enc)
            break
        except UnicodeDecodeError:
            continue
    d = lire_texte(texte, sep, dec, entete)
    d.source = p.name
    return d


def lire_texte_fichier(chemin):
    """Contenu texte d'un fichier (pour l'afficher dans la zone de saisie) ; None si illisible."""
    p = Path(chemin)
    if p.suffix.lower() in (".xlsx", ".xls", ".ods", ".xlsm") or p.stat().st_size > 1_000_000:
        return None
    brut = p.read_bytes()
    for enc in ("utf-8-sig", "cp1252", "latin-1"):
        try:
            return brut.decode(enc)
        except UnicodeDecodeError:
            continue
    return None


def _lire_xlsx(p, entete):
    try:
        import openpyxl
    except ImportError:
        raise ErreurStat("Lecture .xlsx indisponible : installer openpyxl (pip install openpyxl --break-system-packages) "
                         "ou enregistrer le classeur en .csv.")
    wb = openpyxl.load_workbook(p, read_only=True, data_only=True)
    ws = wb.worksheets[0]
    lignes = []
    for row in ws.iter_rows(values_only=True):
        if row is None or all(c is None or str(c).strip() == "" for c in row):
            continue
        lignes.append("\t".join("" if c is None else str(c).replace("\t", " ").replace("\n", " ") for c in row))
    wb.close()
    d = lire_texte("\n".join(lignes), sep="\t", dec=".", entete=entete)
    d.source = p.name
    return d


# =============================================================================
#  Génération de données
# =============================================================================
def _entier_borne(valeur, nom, minimum, maximum):
    try:
        n = int(str(valeur).strip())
    except (TypeError, ValueError):
        raise ErreurStat(f"{nom} : saisir un nombre entier.")
    if not minimum <= n <= maximum:
        raise ErreurStat(f"{nom} : la valeur doit être comprise entre {minimum} et {maximum}.")
    return n


def _generateur(graine):
    if graine is None or str(graine).strip() == "":
        return random.Random()
    try:
        return random.Random(int(str(graine).strip()))
    except ValueError:
        raise ErreurStat("La valeur d'initialisation doit être un nombre entier (ou rester vide).")


def generer_uniforme(n, minimum=0.0, maximum=1.0, decimales=3, graine=None):
    """n valeurs aléatoires uniformes entre deux bornes ; 'valeur init.' (valeur d'initialisation, entier) rend le tirage reproductible."""
    n = _entier_borne(n, "Nombre de valeurs", 1, MAX_GENERE)
    dec = _entier_borne(decimales, "Nombre de décimales", 0, 10)
    if not (math.isfinite(minimum) and math.isfinite(maximum)) or minimum >= maximum:
        raise ErreurStat("Le minimum doit être strictement inférieur au maximum.")
    rng = _generateur(graine)
    return [round(rng.uniform(minimum, maximum), dec) for _ in range(n)]


def generer_normale(n, moyenne, ecart_type, decimales=3, graine=None):
    """n valeurs aléatoires issues d'une loi normale (moyenne, écart-type) ; 'la valeur init. ou graine' rend le tirage reproductible."""
    n = _entier_borne(n, "Nombre de valeurs", 1, MAX_GENERE)
    dec = _entier_borne(decimales, "Nombre de décimales", 0, 10)
    if not (math.isfinite(moyenne) and math.isfinite(ecart_type)) or ecart_type <= 0:
        raise ErreurStat("L'écart-type doit être un nombre strictement positif.")
    rng = _generateur(graine)
    return [round(rng.gauss(moyenne, ecart_type), dec) for _ in range(n)]


# =============================================================================
#  Mise en forme
# =============================================================================
def fmt_nombre(v, chiffres=5):
    if v is None:
        return "—"
    if isinstance(v, bool):
        return "oui" if v else "non"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, float):
        if math.isnan(v):
            return "—"
        if v == int(v) and abs(v) < 1e9:
            return str(int(v))
        return f"{v:.{chiffres}g}".replace(".", ",")
    return str(v)


LIBELLES = {"Serie": "Série", "Mediane": "Médiane", "Ecart_type": "Écart-type", "Ecart": "Écart", "P_Shapiro_Wilk": "p Shapiro-Wilk",
            "Sigma_global": "σ global", "Sigma_intra_fourni": "σ intra fourni", "Borne_inf": "Borne inf.", "Borne_sup": "Borne sup.",
            "p_value": "p-value", "p_ajustee": "p ajustée", "Somme_carres": "Somme des carrés", "Carre_moyen": "Carré moyen",
            "PPM_estimes_sous_LSL": "PPM estimés sous LSL", "PPM_estimes_sur_USL": "PPM estimés sur USL", "ddl": "ddl",
            "Reference": "Référence \\ Évaluation", "Series": "Séries",
            "Evaluateur": "Évaluateur", "Evaluateur_1": "Évaluateur 1", "Evaluateur_2": "Évaluateur 2",
            "Accord_pct": "Accord (%)", "IC_inf": "IC 95 % inf. (%)", "IC_sup": "IC 95 % sup. (%)",
            "Evaluateurs_vs_reference": "Évaluateurs / référence (norme)",
            "Evaluateurs_entre_eux": "Évaluateurs entre eux (Kappa deux à deux)",
            "Decision": "Décision", "Sous_groupe": "Sous-groupe", "LIC": "LIC", "LC": "LC", "LSC": "LSC", "Modalite": "Modalité", "Observe": "Observé",
            "Theorique": "Théorique", "Proportion_attendue_pct": "Proportion attendue (%)", "Residu_standardise": "Résidu standardisé",
            "Contribution": "Contribution au khi²", "Statistique": "Statistique",
            "Composante_variance": "Composante de variance", "Contribution_pct": "Contribution (%)", "Variation_etude": "Variation d'étude",
            "Pct_variation_etude": "% variation d'étude", "Pct_tolerance": "% tolérance"}


def libelle(c):
    if " — " in str(c):                      # titre déjà composé (ex. matrice de confusion d'un évaluateur)
        return str(c)
    return LIBELLES.get(c, str(c).replace("_", " "))


def fmt_cellule(nom, v):
    if nom == "Indicateur" and isinstance(v, str):
        return libelle(v)
    if isinstance(v, (int, float)) and not isinstance(v, bool) and str(nom).lower().startswith("p_"):
        return "< 0,0001" if v < 1e-4 else f"{v:.4f}".replace(".", ",")
    return fmt_nombre(v)


def table_texte(cols, rows, mode="large", libre=False):
    """Tableau en texte à chasse fixe (mode 'large') ou en fiches « Colonne : valeur » (mode 'fiche')."""
    cellules = [[fmt_cellule(c, v) for c, v in zip(cols, r)] for r in rows]
    if mode == "fiche" and list(cols) == ["Indicateur", "Valeur"]:
        return "\n".join(f"  {r[0]} : {r[1]}" for r in cellules)
    if mode == "fiche":
        blocs = []
        for r in cellules:
            blocs.append("\n".join(f"  {c if libre else libelle(c)} : {v}" for c, v in zip(cols, r)))
        return "\n\n".join(blocs)
    ent = [str(c) if libre else libelle(c) for c in cols]
    larg = [max(len(ent[i]), *(len(r[i]) for r in cellules)) if cellules else len(ent[i]) for i in range(len(cols))]
    def ligne(vals):
        return "  ".join(v.ljust(larg[i]) if i == 0 else v.rjust(larg[i]) for i, v in enumerate(vals))
    sortie = [ligne(ent), "  ".join("-" * w for w in larg)] + [ligne(r) for r in cellules]
    return "\n".join(sortie)


def _liste(v):
    if v is None:
        return []
    if isinstance(v, (list, tuple)):
        return list(v)
    if isinstance(v, dict):
        return list(v.values())
    return [v]


class Resultat:
    """Résultat d'une analyse R : titre, lignes de texte, tableaux, figures PNG, remarques."""

    def __init__(self, brut, dossier):
        self.brut = brut                                  # contenu d'origine (conservé pour l'enregistrement du projet)
        self.dossier = Path(dossier)
        self.commande = brut.get("commande", "")
        self.titre = str(brut.get("titre", ""))
        self.conclusion = str(brut.get("conclusion") or "")
        self.lignes = [str(x) for x in _liste(brut.get("lignes"))]
        self.avertissements = [str(x) for x in _liste(brut.get("avertissements"))]
        self.figures = [self.dossier / str(f) for f in _liste(brut.get("figures"))
                        if Path(str(f)).name == str(f) and (self.dossier / str(f)).is_file()]
        self.tables = {}
        self.libres = {str(x) for x in _liste(brut.get("tables_libres"))}   # tableaux dont les en-têtes sont des données (à ne pas « traduire »)
        tabs = brut.get("tables")
        if isinstance(tabs, dict):
            for nom, t in tabs.items():
                if not isinstance(t, dict) or not t:
                    continue
                cols = list(t.keys())
                vals = [_liste(t[c]) for c in cols]
                self.tables[nom] = (cols, [list(r) for r in zip(*vals)])

    def texte(self, mode="large", titres=True):
        parts = []
        if titres and self.titre:
            parts.append(self.titre + "\n" + "=" * len(self.titre))
        if self.lignes:
            parts.append("\n".join(self.lignes))
        for nom, (cols, rows) in self.tables.items():
            parts.append(libelle(nom) + "\n" + table_texte(cols, rows, mode, libre=nom in self.libres))
        if self.avertissements:
            parts.append("\n".join("Remarque : " + a for a in self.avertissements))
        return "\n\n".join(parts)

    def pour_rapport(self):
        """Contenu mis en forme pour le rapport PDF (tableaux déjà formatés)."""
        tables = []
        for nom, (cols, rows) in self.tables.items():
            tables.append((libelle(nom), [str(c) if nom in self.libres else libelle(c) for c in cols],
                           [[fmt_cellule(c, v) for c, v in zip(cols, r)] for r in rows]))
        return {"titre": self.titre, "conclusion": self.conclusion, "lignes": list(self.lignes), "tables": tables,
                "figures": [str(f) for f in self.figures], "remarques": list(self.avertissements)}

    def liberer(self):
        shutil.rmtree(self.dossier, ignore_errors=True)


# =============================================================================
#  Appel de R
# =============================================================================
_dossiers_temp = []


@atexit.register
def _nettoyer_temp():
    for d in _dossiers_temp:
        shutil.rmtree(d, ignore_errors=True)


def verifier_r():
    """(ok, message) : R et jsonlite sont-ils utilisables ?"""
    rs = shutil.which("Rscript")
    if not rs:
        return False, "R est introuvable. Installer : sudo apt install -y r-base r-cran-jsonlite"
    if not SCRIPT_R.is_file() or not (DOSSIER / "moteur_statistique.R").is_file() or not (DOSSIER / "commandes_avancees.R").is_file():
        return False, "cli_statistique.R, moteur_statistique.R ou commandes_avancees.R manquant dans le dossier du programme."
    try:
        r = subprocess.run([rs, "-e", "library(jsonlite)"], capture_output=True, text=True, timeout=60)
    except Exception as e:
        return False, f"R inutilisable : {e}"
    if r.returncode != 0:
        return False, "Le package R « jsonlite » est absent. Installer : sudo apt install -y r-cran-jsonlite"
    return True, "R prêt."


def executer_r(donnees, commande, params, timeout=180, groupe=None):
    """groupe : None ou {"colonne", "periode", "valeurs"} -> l'analyse est lancée une fois par groupe retenu (réunies en un résultat)."""
    if groupe:
        if not groupe.get("valeurs"):
            raise ErreurStat("Aucun groupe sélectionné : choisir au moins un groupe, ou « — aucun — » pour analyser l'ensemble des données.")
        nom = groupe["colonne"] + ({"jour": " (par jour)", "mois": " (par mois)", "annee": " (par année)"}.get(groupe.get("periode") or "", "")
                                   if donnees.types.get(groupe["colonne"]) == "date" else "")
        params = {**params, "groupes": list(groupe["valeurs"]), "groupe_nom": nom}
        donnees = donnees.avec_groupe(groupe["colonne"], groupe.get("periode"))
    rs = shutil.which("Rscript")
    if not rs:
        raise ErreurStat("R est introuvable. Installer : sudo apt install -y r-base r-cran-jsonlite")
    dossier = Path(tempfile.mkdtemp(prefix="stat_"))
    _dossiers_temp.append(dossier)
    try:
        donnees.ecrire_csv(dossier / "donnees.csv")
        (dossier / "params.json").write_text(json.dumps({"commande": commande, **params}, ensure_ascii=False), encoding="utf-8")
        env = dict(os.environ, LC_ALL="C.UTF-8", LANG="C.UTF-8")
        try:
            p = subprocess.run([rs, str(SCRIPT_R), str(dossier / "donnees.csv"), str(dossier)],
                               capture_output=True, text=True, timeout=timeout, env=env)
        except subprocess.TimeoutExpired:
            raise ErreurStat("Le calcul R a dépassé le délai autorisé.")
        sortie = dossier / "resultat.json"
        if not sortie.is_file():
            detail = (p.stderr or p.stdout or "").strip().splitlines()
            raise ErreurStat("R n'a produit aucun résultat. " + (detail[-1] if detail else ""))
        brut = json.loads(sortie.read_text(encoding="utf-8"))
        if not brut.get("ok"):
            raise ErreurStat(str(brut.get("erreur", "Erreur de calcul.")))
        return Resultat(brut, dossier)
    except Exception:
        shutil.rmtree(dossier, ignore_errors=True)
        raise


def lire_nombre(texte, nom="valeur"):
    """Nombre saisi (virgule ou point décimal) ; chaîne vide -> None."""
    if texte is None:
        return None
    t = str(texte).strip().replace(" ", "")
    if t == "" or t == "-":
        return None
    try:
        v = float(t.replace(",", "."))
    except ValueError:
        raise ErreurStat(f"Valeur numérique invalide pour {nom} : « {texte} ».")
    if math.isnan(v) or math.isinf(v):
        raise ErreurStat(f"Valeur numérique invalide pour {nom} : « {texte} ».")
    return v


def lire_alpha(texte):
    """Risque alpha saisi (0,05 ou 5 % ; virgule ou point) : doit être strictement compris entre 0 et 0,5."""
    t = str(texte or "").strip()
    pct = t.endswith("%")
    v = lire_nombre(t.rstrip("%").strip(), "le risque α")
    if v is None:
        raise ErreurStat("Saisir le risque α (par exemple 0,05).")
    if pct:
        v /= 100
    elif v >= 1:
        v /= 100                                  # « 5 » est lu comme 5 %
    if not (0 < v < 0.5):
        raise ErreurStat("Le risque α doit être compris entre 0 et 0,5 (par exemple 0,05 ou 5 %).")
    return v


# =============================================================================
#  Projet : enregistrement et reprise du travail
# =============================================================================
RE_FIGURE_PROJET = re.compile(r"^figures/(\d{1,5})/([A-Za-z0-9_.\-]{1,80}\.png)$")
LIMITE_JSON_PROJET = 200_000_000
LIMITE_PNG_PROJET = 30_000_000


def enregistrer_projet(chemin, donnees, entrees, meta=None, ui=None):
    """Écrit un projet (archive zip) : données, résultats complets, graphiques PNG, réglages de l'interface.

    entrees : liste de (onglet, heure_iso, Resultat, inclus_dans_le_rapport)."""
    projet = {"format": FORMAT_PROJET, "version": VERSION_PROJET,
              "enregistre": datetime.datetime.now().isoformat(timespec="seconds"),
              "meta": meta or {}, "ui": ui or {},
              "donnees": donnees.vers_dict() if donnees is not None else None, "journal": []}
    fichiers = []
    for i, (onglet, heure, res, inclus) in enumerate(entrees):
        for f in res.figures:
            fichiers.append((f"figures/{i}/{f.name}", f))
        projet["journal"].append({"onglet": onglet, "heure": heure, "inclus": bool(inclus), "brut": res.brut})
    chemin = Path(chemin)
    tmp = chemin.with_name(chemin.name + ".tmp")
    try:
        with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as z:
            z.writestr("projet.json", json.dumps(projet, ensure_ascii=False))
            for nom, f in fichiers:
                z.write(f, nom)
        os.replace(tmp, chemin)
    except BaseException:
        tmp.unlink(missing_ok=True)
        raise


def lire_projet(chemin):
    """Lit un projet : retourne (meta, donnees ou None, entrees, ui). Le contenu du fichier est contrôlé."""
    invalide = ErreurStat("Ce fichier n'est pas un projet valide du Prog. Statistiques.")
    try:
        z = zipfile.ZipFile(chemin)
    except (zipfile.BadZipFile, OSError):
        raise invalide
    with z:
        infos = {i.filename: i for i in z.infolist()}
        if "projet.json" not in infos or infos["projet.json"].file_size > LIMITE_JSON_PROJET or len(infos) > 20_000:
            raise invalide
        try:
            projet = json.loads(z.read("projet.json").decode("utf-8"))
        except (ValueError, UnicodeDecodeError):
            raise invalide
        if not isinstance(projet, dict) or projet.get("format") != FORMAT_PROJET:
            raise invalide
        version = projet.get("version")
        if not isinstance(version, int) or version > VERSION_PROJET:
            raise ErreurStat("Ce projet a été créé par une version plus récente du programme.")
        donnees = Donnees.depuis_dict(projet["donnees"]) if projet.get("donnees") else None
        journal = projet.get("journal")
        entrees = []
        for i, e in enumerate(journal if isinstance(journal, list) else []):
            if not isinstance(e, dict) or not isinstance(e.get("brut"), dict):
                raise invalide
            dossier = Path(tempfile.mkdtemp(prefix="stat_"))
            _dossiers_temp.append(dossier)
            for nom, info in infos.items():
                m = RE_FIGURE_PROJET.match(nom)
                if m and int(m.group(1)) == i and info.file_size <= LIMITE_PNG_PROJET:
                    (dossier / m.group(2)).write_bytes(z.read(nom))
            entrees.append((str(e.get("onglet", "")), str(e.get("heure", "")), Resultat(e["brut"], dossier), bool(e.get("inclus", True))))
    meta = projet.get("meta") if isinstance(projet.get("meta"), dict) else {}
    ui = projet.get("ui") if isinstance(projet.get("ui"), dict) else {}
    return meta, donnees, entrees, ui


class Etude:
    """Jeu de données courant + analyses. Utilisé par l'interface et par le bot (une instance par utilisateur)."""

    def __init__(self):
        self.donnees = None
        self.echelle = 1.0                     # taille des graphiques R (1 = 900 px de large) ; réglée par l'interface
        self.groupe = None                     # analyse par groupe : {"colonne", "periode", "valeurs"} ; réglé par l'interface
        self.alpha = 0.05                      # risque alpha des tests (décision : p-value < alpha → H0 rejetée) ; réglé par l'interface

    def _r(self, commande, params):
        return executer_r(self._d(), commande, {**params, "echelle": self.echelle, "alpha_test": self.alpha}, groupe=self.groupe)

    def _d(self):
        if self.donnees is None:
            raise ErreurStat("Aucune donnée chargée.")
        return self.donnees

    def charger_texte(self, texte, **kw):
        self.donnees = lire_texte(texte, **kw)
        return self.donnees

    def charger_fichier(self, chemin, **kw):
        self.donnees = lire_fichier(chemin, **kw)
        return self.donnees

    def generer(self, nom, valeurs, remplacer=False):
        """Ajoute une variable numérique aux données (ou, avec remplacer, repart de zéro).
        Retourne le nom effectivement utilisé (rendu unique si besoin)."""
        nom = _nom_propre(nom)
        if not nom:
            raise ErreurStat("Donner un nom à la variable.")
        if remplacer or self.donnees is None:
            self.donnees = Donnees({}, {}, source="données générées")
        return self.donnees.ajouter_colonne(nom, valeurs)

    def generer_loi(self, loi, parametres, n, decimales=3, graine=None):
        """Valeurs aléatoires d'une loi quelconque de LOIS_GENERATION (tirées par R) ; retourne la liste des valeurs."""
        n = _entier_borne(n, "Nombre de valeurs", 1, MAX_GENERE)
        dec = _entier_borne(decimales, "Nombre de décimales", 0, 10)
        gr = None
        if graine is not None and str(graine).strip() != "":
            try:
                gr = int(str(graine).strip())
            except ValueError:
                raise ErreurStat("La valeur d'initialisation doit être un nombre entier (ou rester vide).")
        res = executer_r(Donnees({"x": [0.0]}, {"x": "num"}), "generer",
                         {"loi": loi, "parametres": parametres, "n": n, "decimales": dec, "graine": gr})
        try:
            vals = res.brut.get("valeurs")
            vals = [float(v) for v in vals] if isinstance(vals, list) else ([float(vals)] if vals is not None else [])
        finally:
            res.liberer()
        if len(vals) != n:
            raise ErreurStat("La génération n'a pas produit le nombre de valeurs attendu.")
        return vals

    def descriptif(self, colonnes=None):
        return self._r("descriptif", {"colonnes": list(colonnes) if colonnes else None})

    def normalite(self, colonnes, tests=None):
        """Tests de normalité d'une ou de plusieurs séries (une colonne ou une liste de colonnes) ;
        tests : liste d'identifiants parmi TESTS_NORMALITE (Shapiro-Wilk par défaut)."""
        cols = [colonnes] if isinstance(colonnes, str) else list(colonnes or [])
        if not cols:
            raise ErreurStat("Choisir au moins une série.")
        ids = [t for t in (tests or ["sw"]) if t in TESTS_NORMALITE]
        if not ids:
            raise ErreurStat("Choisir au moins un test de normalité.")
        return self._r("normalite", {"colonnes": cols, "tests": ids})

    def test_t(self, x, y=None, mu=0.0, apparie=False, variances_egales=False, alternative="two.sided"):
        return self._r("t", {"x": x, "y": y or None, "mu": mu, "apparie": bool(apparie),
                             "variances_egales": bool(variances_egales), "alternative": alternative})

    def anova(self, mode, colonnes=None, valeur=None, groupe=None):
        return self._r("anova", {"mode": mode, "colonnes": list(colonnes) if colonnes else None,
                                 "valeur": valeur, "groupe": groupe})

    def capabilite(self, colonne, lsl=None, usl=None, cible=None, sigma_intra=None):
        if lsl is None and usl is None:
            raise ErreurStat("Saisir au moins une limite de spécification (LSL ou USL).")
        return self._r("capabilite", {"colonne": colonne, "lsl": lsl, "usl": usl,
                                      "cible": cible, "sigma_intra": sigma_intra})

    def kappa(self, a, b):
        return self._r("kappa", {"a": a, "b": b})

    def kappa_reference(self, reference, evaluateurs):
        """Kappa de chaque évaluateur contre une colonne de référence (norme), et des évaluateurs entre eux."""
        if not reference:
            raise ErreurStat("Choisir la colonne de référence (norme).")
        evaluateurs = [e for e in evaluateurs if e != reference]
        if not evaluateurs:
            raise ErreurStat("Sélectionner au moins un évaluateur, différent de la colonne de référence.")
        return self._r("kappa", {"reference": reference, "evaluateurs": evaluateurs})

    def commande(self, nom, params):
        """Commande R quelconque (analyses avancées : test F, khi², cartes de contrôle, R&R)."""
        return self._r(nom, params)

    def commande_libre(self, nom, params):
        """Commande R qui peut se passer des données chargées (valeurs saisies à la main)."""
        if self.donnees is not None:
            return self._r(nom, params)
        return executer_r(Donnees({"x": [0.0]}, {"x": "num"}), nom, {**params, "echelle": self.echelle, "alpha_test": self.alpha})

    def test_f(self, x, y, alternative="two.sided"):
        if not x or not y:
            raise ErreurStat("Choisir les deux séries à comparer.")
        return self._r("ftest", {"x": x, "y": y, "alternative": alternative})

    def kappa_long(self, reponse, evaluateur, echantillon, essai, reference):
        """Kappa à partir de données en format long (une ligne par évaluation) : les données sont remises en
        colonnes (une ligne par échantillon et par essai, une colonne par évaluateur) avant le calcul.
        Les données chargées ne sont pas modifiées."""
        d = self._d()
        for nom, lib in ((reponse, "la colonne des réponses"), (evaluateur, "la colonne des évaluateurs"),
                         (echantillon, "la colonne des échantillons"), (reference, "la colonne de référence (norme)")):
            if not nom or nom not in d.colonnes:
                raise ErreurStat(f"Choisir {lib}.")
        essai = essai if essai and essai in d.colonnes else None
        if len({reponse, evaluateur, echantillon, reference, essai or ""} - {""}) < (5 if essai else 4):
            raise ErreurStat("Les colonnes choisies doivent être toutes différentes.")

        def txt(v):
            if v is None:
                return None
            if isinstance(v, float) and v == int(v):
                return str(int(v))
            s = str(v).strip()
            return s or None

        cles, evals, rep_ev, ref_cle, vus = [], [], {}, {}, set()
        col = d.colonnes
        for i in range(d.nb_lignes):
            def g(c):
                return txt(col[c][i]) if c and i < len(col[c]) else None
            ech, ev, rep, ref = g(echantillon), g(evaluateur), g(reponse), g(reference)
            if ech is None or ev is None:
                continue
            cle = (ech, g(essai) or "")
            if cle not in vus:
                vus.add(cle)
                cles.append(cle)
            if ev not in evals:
                evals.append(ev)
            if ref is not None:
                if ref_cle.get(cle, ref) != ref:
                    raise ErreurStat(f"La référence de l'échantillon « {ech} » n'est pas toujours la même (« {ref_cle[cle]} » et « {ref} »).")
                ref_cle[cle] = ref
            if rep is not None:
                if (cle, ev) in rep_ev:
                    raise ErreurStat(f"Plusieurs réponses de l'évaluateur « {ev} » pour l'échantillon « {ech} »"
                                     + ("." if essai else " : indiquer la colonne des essais."))
                rep_ev[(cle, ev)] = rep
        if len(evals) < 1 or not cles:
            raise ErreurStat("Aucune évaluation exploitable dans les colonnes choisies.")
        if len(evals) > 50:
            raise ErreurStat("Trop d'évaluateurs distincts : vérifier la colonne des évaluateurs.")
        colonnes = {reference: [ref_cle.get(c) for c in cles]}
        types = {reference: "txt"}
        for ev in evals:
            nom = ev if ev not in colonnes else f"Évaluateur {ev}"
            colonnes[nom] = [rep_ev.get((c, ev)) for c in cles]
            types[nom] = "txt"
        large = Donnees(colonnes, types, source="format long")
        res = executer_r(large, "kappa", {"reference": reference, "evaluateurs": [k for k in colonnes if k != reference],
                                          "echelle": self.echelle})
        return res


TEXTE_AIDE = """\
Statistiques — aide

Données :
• Une série de mesures par colonne ; la première ligne contient les noms de colonnes.
• Pour une analyse par groupes, utiliser une colonne de mesures et une colonne de groupes.
• Séparateurs reconnus : tabulation, point-virgule, virgule, espace. Décimale : point ou virgule.
• Les cases vides et les mentions NA sont traitées comme des valeurs manquantes.
• Fichiers : txt, csv, tsv, dat, xlsx. Les options de lecture permettent de forcer le séparateur, la décimale ou la présence d'une ligne de noms.
• Une saisie sans ligne de noms est possible si toutes les valeurs de la première ligne sont numériques.
• Pour un Kappa avec référence : soit une ligne par échantillon, une colonne de référence (reconnue automatiquement si son nom évoque une référence ou une norme)
  et une colonne par évaluateur ou par essai ; soit une ligne par évaluation (colonnes réponse, évaluateur, échantillon, essai, référence : format long, reconnu à ses noms de colonnes).
• « Générer des données… » crée une variable aléatoire : tirage « aléatoire (random) » entre deux bornes, sans loi particulière, ou loi au choix (normale, uniforme, Poisson, Student, Fisher, khi², binomiale, géométrique, hypergéométrique, exponentielle, gamma, logistique, Weibull, log-normale, bêta), avec les paramètres de la loi,
  le nombre de valeurs, le nom de la variable, le nombre de décimales et, si on le souhaite, une valeur d'initialisation pour retrouver exactement le même tirage. La variable s'ajoute aux données chargées, ou les remplace si on le demande.

Organisation : les onglets sont regroupés (Description, Tests, Modélisation, Qualité, Graphiques) ; l'onglet Illustrations affiche les images de référence (PNG) du dossier « illustrations », et le bouton « Illustration… » de chaque analyse ouvre le sujet associé.
• Vocabulaire : glossaire des termes du programme, lu dans le fichier « vocabulaire.txt » (« Terme : définition », une entrée par ligne). Recherche par mot (aussi dans les définitions), et boutons Ajouter, Modifier, Supprimer ; le fichier peut aussi être complété à la main, puis rechargé (« Recharger », ou en revenant sur l'onglet). Le même fichier sert à l'interface web et au bot Telegram (/vocabulaire).

Dates et groupes :
• Une colonne dont toutes les valeurs sont des dates (JJ/MM/AAAA, AAAA-MM-JJ, avec ou sans heure hh:mm[:ss] ; les dates d'un fichier xlsx sont reconnues aussi) est de type « date » : elle est affichée « (date) » dans le tableau et traitée comme un repère de temps, non comme un nombre.
• « Analyser par groupe » (en haut de la fenêtre) : choisir une colonne de groupes (texte, nombre ou date par jour, mois ou année), puis éventuellement « Choisir les groupes… » pour ne garder que certains groupes.
  Toutes les analyses des onglets (descriptif, normalité, tests, ANOVA, capabilité, cartes, R&R, khi², Kappa en deux colonnes ou avec colonne de référence) sont alors lancées groupe par groupe : un bloc de résultats, des tableaux et des graphiques par groupe (le nom du groupe est inscrit en haut de chaque graphique).
  « — aucun — » ramène à l'analyse de l'ensemble des données. Un groupe où l'analyse est impossible (trop peu de valeurs, par exemple) est signalé et les autres groupes sont traités. Le Kappa en format long n'est pas découpé par groupe.
• Cartes de contrôle : « Ordre des données » classe les points par date (ou par numéro d'ordre) et l'utilise comme étiquette de l'axe ; une colonne de dates est proposée par défaut quand elle existe.

Décision statistique :
• Le risque α (en haut de la fenêtre, 0,05 par défaut) est le seuil de décision de tous les tests. Chaque résultat rappelle H0 et H1, puis la règle :
  p-value ≥ α → H0 n'est pas rejetée ; p-value < α → H0 est rejetée au profit de H1. Les intervalles de confiance suivent le même risque (niveau 1 − α).
• Pour un test de normalité (quel que soit le test), H0 est « la série suit une loi normale » : la normalité n'est validée que si la p-value est supérieure ou égale à α.
  De même, pour les contrôles de l'ANOVA (normalité des résidus, homogénéité des variances), H0 est l'hypothèse requise.
• Le seuil de l'interaction de la R&R croisée (0,05 par défaut dans Minitab, 0,25 dans la norme AIAG) est un réglage propre à cette analyse.

Analyses : 
• Descriptif : effectif, moyenne, médiane, écart-type, variance, quartiles, extrêmes, Shapiro-Wilk.
• Normalité : tests de Shapiro-Wilk, Anderson-Darling, Kolmogorov-Smirnov (Lilliefors), Cramér-von Mises, Jarque-Bera et Ryan-Joiner (Minitab) au choix, histogramme et droite de Henry, pour une ou plusieurs séries à la fois (un graphique par série).
• Test t (Student) : un échantillon, deux échantillons indépendants (Welch par défaut, Student si les variances sont supposées égales) ou appariés.
• Test Z : un échantillon (moyenne comparée à une valeur de référence) ou deux échantillons (différence des moyennes), avec écart(s)-type(s) connu(s) de la population ; hypothèse alternative bilatérale ou unilatérale.
• Test de Poisson : un échantillon (taux comparé à un taux de référence) ou deux échantillons (rapport des taux) ; les données sont une colonne de comptages (avec la taille de chaque observation) ou des totaux saisis. Tests exacts de Poisson.
• Estimation : intervalle de confiance d'une moyenne (loi normale si σ est connu, t de Student sinon) pour un niveau de confiance choisi, bilatéral ou unilatéral ; avec une marge d'erreur souhaitée, la taille d'échantillon nécessaire est calculée. Les données peuvent être une colonne ou un résumé saisi.
• Identification de loi : ajuste plusieurs lois (normale, log-normale, exponentielle, gamma, Weibull, logistique, valeur extrême) à chaque série, avec le test d'Anderson-Darling (p-value par simulation), le critère AIC, les densités ajustées et les diagrammes de probabilité ; H0 : la série suit la loi testée.
• Corrélation : coefficients de Pearson, Spearman ou Kendall pour toutes les paires de séries, avec p-value (H0 : pas de corrélation), intervalle de confiance (Pearson) et matrice graphique.
• Régression : linéaire simple ou multiple, ou polynôme de degré 2 ou 3 pour une variable ; équation, R², analyse de variance, coefficients testés (H0 : coefficient nul), VIF, Durbin-Watson, normalité des résidus, observations inhabituelles, graphiques d'ajustement (bandes de confiance et de prédiction) et de résidus.
• Série chronologique : tendance linéaire, quadratique, exponentielle ou logistique (courbe en S), avec MAPE, MAD, MSD, test de tendance et prévisions (intervalles de prédiction pour les modèles linéaire, quadratique et exponentiel).
• Test F : compare les variances de deux séries (avec le test de Levene, plus robuste) et indique s'il faut préférer le test t de Student ou celui de Welch.
• ANOVA : un facteur, colonnes = groupes ou format long ; taille d'effet η², contrôle des résidus, tests de Levene et de Fligner-Killeen,
  ANOVA de Welch et Kruskal-Wallis en alternative ; comparaisons de Tukey si le test global est significatif (de Welch avec ajustement de Holm si les variances diffèrent).
• ANOVA 2 facteurs : une colonne de mesures et deux colonnes de facteurs, avec ou sans interaction ; sommes des carrés ajustées (type III), taille d'effet η² partiel,
  moyennes par niveau et par combinaison, graphiques des effets principaux et de l'interaction, contrôle des résidus, comparaisons de Tukey des facteurs significatifs (plan équilibré).
• Khi² : trois tests, avec données brutes (deux colonnes de catégories) ou tableau d'effectifs déjà compté :
  – indépendance de deux variables ; homogénéité de plusieurs populations (même calcul, interprétation différente) ; effectifs observés et théoriques, résidus, V de Cramér,
    correction de Yates et test exact de Fisher pour un tableau 2 × 2 ou si les effectifs théoriques sont trop petits ;
  – conformité à une répartition attendue (proportions saisies, égales par défaut) ou à une loi normale (classes équiprobables ; moyenne et écart-type estimés ou donnés).
• Capabilité : Pp/Ppk à partir de l'écart-type global ; Cp/Cpk/Cm/Cmk uniquement si un écart-type intra-sous-groupe est fourni. Au moins une limite de spécification est requise.
• Kappa de Cohen (non pondéré), trois modes :
  – deux évaluations : accord entre deux colonnes de catégories ;
  – référence (norme) : une colonne de référence et une colonne par évaluateur (ou par essai) ; le Kappa de chaque évaluateur contre la référence dit s'il est satisfaisant (> 0,75),
    à améliorer (0,40 à 0,75) ou insuffisant (< 0,40), avec l'accord en % et son intervalle de confiance, et l'accord des évaluateurs entre eux ;
  – référence en format long : une ligne par évaluation (réponse, évaluateur, échantillon, essai éventuel et référence de l'échantillon, comme dans une analyse de concordance Minitab) ; 
    les colonnes sont remises en forme automatiquement, sans modifier les données chargées, et le résultat est le même que dans le mode précédent.
  Interprétation selon l'échelle de Landis et Koch (1977) ; repères AIAG (MSA) pour les systèmes de mesure.
• Cartes de contrôle aux attributs (Qualité) : carte p (proportion d'unités défectueuses), carte np (nombre d'unités défectueuses), carte c (nombre de défauts par unité de contrôle de taille constante) et carte u (défauts par unité), avec des effectifs d'inspection variables (limites variables, sauf carte c) et, au choix, une colonne de phases ou un numéro de point à partir duquel commence la phase « Après » (limites calculées séparément dans chaque phase).
• Graphiques : histogramme (courbe normale facultative), secteurs, barres, bulles, nuage de points (droite d'ajustement), valeurs individuelles, intervalles, boîtes à moustaches et diagramme de probabilité (normale ou autre loi, avec limites de confiance ponctuelles au niveau 1 − α, comme Minitab ; la droite de Henry de l'onglet Normalité les porte aussi) ; un graphique par groupe avec « Analyser par groupe ».
• Cartes de contrôle : X-barre/R, X-barre/S et valeurs individuelles/étendues mobiles (I-MR) ; le choix automatique retient I-MR pour des valeurs seules, X-barre/R de 2 à 7 valeurs
  par sous-groupe et X-barre/S à partir de 8. Données : une ligne par sous-groupe (une colonne par valeur) ou une colonne de mesures et une colonne qui désigne le sous-groupe ;
  des sous-groupes de tailles différentes donnent des limites variables. Limites calculées d'après les données (3 écarts-types), 8 tests de causes spéciales au choix (1 à 4 par défaut),
  écart-type intra-sous-groupe estimé, utilisable dans l'onglet Capabilité.
  Avant / après : une colonne de phases, ou un numéro de point de rupture, calcule les limites séparément dans chaque phase et les trace en escalier.
• R&R, conventions de Minitab par défaut (à vérifier dans les résultats, qui les rappellent) :
  – croisée : pièces × opérateurs × répétitions, une ligne par mesure, plan équilibré ; méthode ANOVA, interaction retirée si sa p-value dépasse 0,05 (AIAG : 0,25), composantes de variance,
    % contribution, % variation d'étude (L = 6), % tolérance si la tolérance est donnée, nombre de catégories distinctes ; moins de 10 % acceptable, 10 à 30 % acceptable sous conditions, plus de 30 % inacceptable ;
  – emboîtée : chaque pièce n'est mesurée que par un seul opérateur (essai destructif) ; pièces emboîtées dans les opérateurs, plan équilibré (même nombre de pièces par opérateur, même nombre de répétitions) ; mêmes indicateurs que la croisée, sans interaction ;
  – type 1 : une pièce étalon mesurée de nombreuses fois ; biais et test t, Cg et Cgk (K = 20 % de la tolérance, L = 6, seuil 1,33).

Projet et rapport (onglet Rapport) :
• Chaque analyse calculée est conservée dans le journal de l'onglet Rapport.
• « Enregistrer le projet » sauvegarde les données, tous les résultats (texte, tableaux, graphiques) et les réglages des onglets dans un fichier .statproj ; 
  « Ouvrir un projet » reprend le travail où il s'était arrêté.
• « Générer le rapport PDF » produit un rapport de synthèse : titre, auteur, conclusions saisies, synthèse des analyses cochées, description des données,
  puis le détail de chaque analyse. Nécessite python3-reportlab.
• Le rapport décrit les données actuellement chargées ; chaque analyse porte sur les données au moment du calcul.

Précautions :
• Vérifier l'indépendance des observations, la stabilité du procédé (la normalité) et les unités avant toute décision.
• Les PPM reposent sur une loi normale ajustée : ils sont indicatifs.
"""


# =============================================================================
#  Interface Tkinter
# =============================================================================
def charger_image(chemin, largeur_max, hauteur_max=None, agrandir=False):
    """PhotoImage ajustée à largeur_max × hauteur_max (proportions conservées).

    Sans agrandir=True, l'image n'est jamais agrandie au-delà de sa taille d'origine.
    Pillow (avec ImageTk) donne un redimensionnement lissé ; sans lui, seule une réduction
    par pas entiers est possible (sudo apt install python3-pil.imagetk)."""
    import tkinter as tk
    try:
        from PIL import Image, ImageTk
        im = Image.open(chemin)
        ratio = largeur_max / im.width
        if hauteur_max:
            ratio = min(ratio, hauteur_max / im.height)
        if not agrandir:
            ratio = min(ratio, 1.0)
        if abs(ratio - 1.0) > 0.01:
            im = im.resize((max(1, int(im.width * ratio)), max(1, int(im.height * ratio))), Image.LANCZOS)
        return ImageTk.PhotoImage(im)
    except Exception:
        pass
    photo = tk.PhotoImage(file=str(chemin))
    ratio = largeur_max / photo.width()
    if hauteur_max:
        ratio = min(ratio, hauteur_max / photo.height())
    if ratio < 0.99:
        photo = photo.subsample(math.ceil(1 / ratio))
    return photo


def echelle_ecran(root):
    """Échelle de rendu des graphiques R (1 = 900 × 600 px), choisie pour que les graphiques restent nets
    en plein écran sur l'écran utilisé."""
    return max(1.0, min(2.0, root.winfo_screenwidth() * 0.96 / 900, root.winfo_screenheight() * 0.88 / 600))


class PanneauGraphiques:
    """Affiche des graphiques PNG ajustés à la place disponible ; ◀ ▶ pour passer de l'un à l'autre."""

    def __init__(self, parent, plein_ecran=None, fermer=None, objet="Graphique"):
        import tkinter as tk
        from tkinter import ttk
        self.objet = objet
        self.frame = ttk.Frame(parent)
        self.figures = []
        self.i = 0
        self.photo = None
        self._job = None
        self._taille = (0, 0)
        barre = ttk.Frame(self.frame)
        barre.pack(fill="x", pady=(0, 4))
        ttk.Button(barre, text="◀", width=3, command=lambda: self.aller(-1)).pack(side="left")
        self.lbl = ttk.Label(barre, text="", width=16, anchor="center")
        self.lbl.pack(side="left")
        ttk.Button(barre, text="▶", width=3, command=lambda: self.aller(1)).pack(side="left")
        ttk.Button(barre, text=f"Copier {objet.lower()}", command=self.copier).pack(side="left", padx=(10, 0))
        ttk.Button(barre, text=f"Enregistrer {objet.lower()}…", command=self.enregistrer).pack(side="left", padx=6)
        if plein_ecran:
            ttk.Button(barre, text="Plein écran", command=plein_ecran).pack(side="left")
        if fermer:
            ttk.Button(barre, text="Fermer", command=fermer).pack(side="right")
        self.zone = tk.Canvas(self.frame, background="white", highlightthickness=1, highlightbackground="#999999")
        self.zone.pack(fill="both", expand=True)
        self.zone.bind("<Configure>", self._reajuster)

    def definir(self, figures):
        self.figures = list(figures)
        self.i = 0
        self.afficher()

    def aller(self, pas):
        if len(self.figures) > 1:
            self.i = (self.i + pas) % len(self.figures)
            self.afficher()

    def afficher(self):
        self.zone.delete("all")
        self.photo = None
        w, h = self.zone.winfo_width(), self.zone.winfo_height()
        self._taille = (w, h)
        if not self.figures:
            self.lbl.configure(text="")
            return
        self.lbl.configure(text=f"{self.objet} {self.i + 1} / {len(self.figures)}")
        try:
            self.photo = charger_image(self.figures[self.i], max(200, w - 8), max(150, h - 8), agrandir=True)
            self.zone.create_image(w // 2, h // 2, image=self.photo)
        except Exception as e:
            self.zone.create_text(10, 10, anchor="nw", text=f"Affichage impossible : {e}")

    def _reajuster(self, _evt=None):
        if self._job:
            self.frame.after_cancel(self._job)
        self._job = self.frame.after(200, self._maj)

    def _maj(self):
        self._job = None
        w, h = self.zone.winfo_width(), self.zone.winfo_height()
        if self.figures and (abs(w - self._taille[0]) > 20 or abs(h - self._taille[1]) > 20):
            self.afficher()

    def enregistrer(self):
        """Enregistre une copie du PNG affiché dans le dossier choisi."""
        from tkinter import messagebox
        if not self.figures:
            messagebox.showinfo(TITRE, "Rien à enregistrer : aucune image n'est affichée.")
            return
        src = Path(self.figures[self.i])
        parent = self.frame.winfo_toplevel()
        chemin = choisir_fichier_ecriture(parent, title=f"Enregistrer {self.objet.lower()}", defaultextension=".png", initialfile=src.name,
                                          filetypes=[("Image PNG", "*.png")], initialdir=str(Path.home()))
        if not chemin:
            return
        try:
            shutil.copy(src, chemin)
        except OSError as e:
            messagebox.showerror(TITRE, f"Enregistrement impossible : {e}", parent=parent)
            return
        messagebox.showinfo(TITRE, f"Image enregistrée :\n{chemin}", parent=parent)

    def copier(self):
        """Place l'image affichée dans le presse-papiers (pour la coller dans LibreOffice Writer, etc.)."""
        from tkinter import messagebox
        parent = self.frame.winfo_toplevel()
        if not self.figures:
            messagebox.showinfo(TITRE, "Rien à copier : aucune image n'est affichée.")
            return
        src = str(self.figures[self.i])
        for cmd in (["wl-copy", "--type", "image/png"], ["xclip", "-selection", "clipboard", "-t", "image/png"]):
            if shutil.which(cmd[0]):
                try:
                    if cmd[0] == "wl-copy":
                        with open(src, "rb") as f:
                            subprocess.run(cmd, stdin=f, check=True, timeout=10)
                    else:
                        subprocess.Popen(cmd + ["-i", src], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)   # xclip reste actif pour servir le presse-papiers
                    messagebox.showinfo(TITRE, "Image copiée\n\nColler avec Ctrl+V", parent=parent)
                    return
                except Exception as e:
                    messagebox.showerror(TITRE, f"Copie impossible : {e}", parent=parent)
                    return
        messagebox.showinfo(TITRE, "La copie dans le presse-papiers demande l'outil « xclip » (ou « wl-clipboard » sous Wayland) :\n"
                                   "sudo apt install -y xclip\n\nEn attendant, « Enregistrer… » crée un fichier PNG à insérer (Insertion > Image dans Writer).", parent=parent)


class Visionneuse:
    """Fenêtre plein écran pour voir les graphiques en grand."""

    def __init__(self, parent, figures):
        import tkinter as tk
        self.win = tk.Toplevel(parent)
        self.win.title(f"{TITRE} — graphiques")
        sw, sh = self.win.winfo_screenwidth(), self.win.winfo_screenheight()
        self.win.geometry(f"{int(sw * 0.96)}x{int(sh * 0.90)}+{int(sw * 0.02)}+0")
        self.panneau = PanneauGraphiques(self.win, fermer=self.win.destroy)
        self.panneau.frame.pack(fill="both", expand=True, padx=8, pady=8)
        self.win.bind("<Left>", lambda e: self.panneau.aller(-1))
        self.win.bind("<Right>", lambda e: self.panneau.aller(1))
        self.win.bind("<Escape>", lambda e: self.win.destroy())
        self.win.update_idletasks()
        self.panneau.definir(figures)


class DialogueGeneration:
    """Fenêtre de génération de données aléatoires : loi normale (moyenne, écart-type) ou valeurs uniformes."""

    def __init__(self, app):
        import tkinter as tk
        from tkinter import ttk
        self.app = app
        self.win = tk.Toplevel(app.root)
        self.win.title(f"{TITRE} — générer des données")
        self.win.transient(app.root)
        self.win.resizable(False, False)
        cadre = ttk.Frame(self.win, padding=12)
        cadre.pack(fill="both", expand=True)
        self.v_loi = tk.StringVar(value=LOIS_GENERATION[0][0])
        self.v_nom = tk.StringVar(value=app.nom_variable_suivant())
        self.v_n = tk.StringVar(value="30")
        self.v_dec, self.v_graine = tk.StringVar(value="3"), tk.StringVar()
        self.v_rempl = tk.BooleanVar(value=False)
        self.v_params = [tk.StringVar() for _ in range(3)]

        ttk.Label(cadre, text="Nom de la variable").grid(row=0, column=0, sticky="e", padx=4, pady=4)
        ttk.Entry(cadre, textvariable=self.v_nom, width=22).grid(row=0, column=1, columnspan=2, sticky="w", pady=4)
        ttk.Label(cadre, text="Nombre de valeurs").grid(row=1, column=0, sticky="e", padx=4, pady=4)
        ttk.Entry(cadre, textvariable=self.v_n, width=10).grid(row=1, column=1, sticky="w", pady=4)

        ttk.Label(cadre, text="Loi").grid(row=2, column=0, sticky="e", padx=4, pady=(10, 4))
        cb = ttk.Combobox(cadre, textvariable=self.v_loi, state="readonly", width=24, values=[l[0] for l in LOIS_GENERATION])
        cb.grid(row=2, column=1, columnspan=2, sticky="w", pady=(10, 4))
        cb.bind("<<ComboboxSelected>>", lambda e: self._etat())
        self.cadre_params = ttk.Frame(cadre)
        self.cadre_params.grid(row=3, column=0, columnspan=5, sticky="w", padx=4)
        self.lbl_params, self.ent_params = [], []
        for k in range(3):
            lb = ttk.Label(self.cadre_params, text="")
            en = ttk.Entry(self.cadre_params, textvariable=self.v_params[k], width=10)
            lb.grid(row=0, column=2 * k, sticky="e", padx=(12 if k else 0, 4), pady=4)
            en.grid(row=0, column=2 * k + 1, sticky="w", pady=4)
            self.lbl_params.append(lb)
            self.ent_params.append(en)
        self.lbl_desc = ttk.Label(cadre, text="", wraplength=520, justify="left", foreground="#555555")
        self.lbl_desc.grid(row=4, column=0, columnspan=5, sticky="w", padx=4, pady=(2, 4))

        ttk.Label(cadre, text="Décimales").grid(row=5, column=0, sticky="e", padx=4, pady=(10, 4))
        ttk.Entry(cadre, textvariable=self.v_dec, width=6).grid(row=5, column=1, sticky="w", pady=(10, 4))
        ttk.Label(cadre, text="Valeur d'initialisation (facultatif)").grid(row=5, column=2, columnspan=2, sticky="e", padx=(8, 4), pady=(10, 4))
        ttk.Entry(cadre, textvariable=self.v_graine, width=10).grid(row=5, column=4, sticky="w", pady=(10, 4))
        ttk.Label(cadre, text="Une même valeur d'initialisation redonne exactement le même tirage.", foreground="#666666").grid(row=6, column=0, columnspan=5, sticky="w", padx=4)
        ttk.Checkbutton(cadre, text="Remplacer les données actuelles (sinon la variable est ajoutée)", variable=self.v_rempl).grid(row=7, column=0, columnspan=5, sticky="w", padx=4, pady=(8, 4))

        barre = ttk.Frame(cadre)
        barre.grid(row=8, column=0, columnspan=5, sticky="e", pady=(8, 0))
        ttk.Button(barre, text="Générer", command=self.generer).pack(side="left", padx=4)
        ttk.Button(barre, text="Fermer", command=self.win.destroy).pack(side="left", padx=4)
        self._etat()
        self.win.bind("<Escape>", lambda e: self.win.destroy())
        self.win.bind("<Return>", lambda e: self.generer())

    def _loi(self):
        return next(l for l in LOIS_GENERATION if l[0] == self.v_loi.get())

    def _etat(self):
        """Champs de paramètres de la loi choisie (valeurs proposées) et description."""
        _, _, params, desc = self._loi()
        for k in range(3):
            if k < len(params):
                self.lbl_params[k].configure(text=params[k][1])
                self.v_params[k].set(params[k][2])
                self.lbl_params[k].grid()
                self.ent_params[k].grid()
            else:
                self.v_params[k].set("")
                self.lbl_params[k].grid_remove()
                self.ent_params[k].grid_remove()
        self.lbl_desc.configure(text=desc)
        code = self._loi()[1]
        if code == "aleatoire":
            self.v_dec.set("0")
        elif self.v_dec.get() == "0":
            self.v_dec.set("3")
        self.win.update_idletasks()

    @staticmethod
    def _nombre(texte, nom):
        v = lire_nombre(texte, nom)
        if v is None:
            raise ErreurStat(f"Saisir {nom}.")
        return v

    def generer(self):
        from tkinter import messagebox
        try:
            _, code, params, _ = self._loi()
            val = {cle: self._nombre(self.v_params[k].get(), f"« {lib} »") for k, (cle, lib, _) in enumerate(params)}
            if code == "normale":
                valeurs = generer_normale(self.v_n.get(), val["moyenne"], val["ecart_type"], self.v_dec.get(), self.v_graine.get())
            elif code in ("uniforme", "aleatoire"):
                valeurs = generer_uniforme(self.v_n.get(), val["min"], val["max"], self.v_dec.get(), self.v_graine.get())
            else:
                self.win.configure(cursor="watch")
                self.win.update_idletasks()
                try:
                    valeurs = self.app.etude.generer_loi(code, val, self.v_n.get(), self.v_dec.get(), self.v_graine.get())
                finally:
                    self.win.configure(cursor="")
            nom = self.app.etude.generer(self.v_nom.get(), valeurs, remplacer=self.v_rempl.get())
        except ErreurStat as e:
            messagebox.showerror(TITRE, str(e), parent=self.win)
            return
        self.app.donnees_generees(nom, len(valeurs))
        self.v_nom.set(self.app.nom_variable_suivant())
        self.v_rempl.set(False)


class EntreeJournal:
    """Un résultat conservé dans le journal : onglet d'origine, heure, résultat, inclusion dans le rapport."""

    def __init__(self, onglet, res, heure=None, inclus=True):
        self.onglet = onglet
        self.res = res
        self.inclus = inclus
        self.heure = heure or datetime.datetime.now().isoformat(timespec="seconds")


class ZoneResultat:
    """Zone de résultats : texte à chasse fixe à gauche, graphiques à droite (séparateur déplaçable)."""

    def __init__(self, parent, app, nom=""):
        import tkinter as tk
        from tkinter import ttk, font as tkfont
        self.app = app
        self.nom = nom                       # nom de l'onglet, pour le journal des résultats
        self.resultat = None
        self.frame = ttk.Frame(parent)
        barre = ttk.Frame(self.frame)
        barre.pack(fill="x", pady=(6, 2))
        ttk.Button(barre, text="Enregistrer le résultat…", command=self.enregistrer).pack(side="left")
        ttk.Button(barre, text="Graphiques en plein écran…", command=self.agrandir).pack(side="left", padx=8)
        if ILLUSTRATION_ONGLET.get(nom):
            ttk.Button(barre, text="Illustration…", command=lambda: app.ouvrir_illustration(ILLUSTRATION_ONGLET[nom])).pack(side="left")
        self.pan = ttk.PanedWindow(self.frame, orient="horizontal")
        self.pan.pack(fill="both", expand=True)
        gauche = ttk.Frame(self.pan)
        self.texte = tk.Text(gauche, wrap="none", height=10, width=40, state="disabled", padx=8, pady=6,
                             font=tkfont.nametofont("TkFixedFont"), background="white", relief="solid", borderwidth=1)
        sy = ttk.Scrollbar(gauche, orient="vertical", command=self.texte.yview)
        sx = ttk.Scrollbar(gauche, orient="horizontal", command=self.texte.xview)
        self.texte.configure(yscrollcommand=sy.set, xscrollcommand=sx.set)
        sx.pack(side="bottom", fill="x")
        sy.pack(side="right", fill="y")
        self.texte.pack(side="left", fill="both", expand=True)
        self.pan.add(gauche, weight=2)
        self.graphes = PanneauGraphiques(self.pan, plein_ecran=self.agrandir)
        self._graphes_visibles = False
        gras = tkfont.nametofont("TkFixedFont").copy()
        gras.configure(weight="bold")
        self.texte.tag_configure("titre", font=gras, foreground="#1f4e8c")
        self.texte.tag_configure("remarque", foreground="#8a5a00", wrap="word")
        self.texte.tag_configure("para", wrap="word")
        self.texte.tag_configure("erreur", foreground="#b00020")
        self.texte.tag_configure("section", font=gras)

    def _montrer_graphes(self, oui):
        if oui and not self._graphes_visibles:
            self.pan.add(self.graphes.frame, weight=3)
            self._graphes_visibles = True
            self.frame.after(80, self._placer_separateur)
            self.frame.after(500, lambda: self._placer_separateur(1))
        elif not oui and self._graphes_visibles:
            self.pan.forget(self.graphes.frame)
            self._graphes_visibles = False

    def _placer_separateur(self, essai=0):
        """Texte : 42 % de la largeur, graphiques : le reste. Réessaie tant que la zone n'est pas affichée ;
        ne touche pas à un séparateur déplacé à la main (sauf valeur aberrante)."""
        try:
            larg = self.pan.winfo_width()
            if larg < 200:
                raise ValueError("zone non affichée")
            pos = self.pan.sashpos(0)
            if essai == 0 or pos < 0.15 * larg or pos > 0.85 * larg:
                self.pan.sashpos(0, int(larg * 0.42))
        except Exception:
            if essai < 8:
                self.frame.after(150, lambda: self._placer_separateur(essai + 1))

    def agrandir(self):
        from tkinter import messagebox
        if self.resultat is None or not self.resultat.figures:
            messagebox.showinfo(TITRE, "Aucun graphique à agrandir.")
            return
        Visionneuse(self.app.root, self.resultat.figures)

    def _ecrire(self, contenu, tag=None):
        self.texte.insert("end", contenu, tag or ())

    def vider(self):
        self.texte.configure(state="normal")
        self.texte.delete("1.0", "end")
        self.texte.configure(state="disabled")

    def reinitialiser(self):
        """Vide la zone (texte et graphiques) sans supprimer le résultat, qui reste dans le journal."""
        self.resultat = None
        self._montrer_graphes(False)
        self.graphes.definir([])
        self.vider()

    def afficher_erreur(self, message):
        self.resultat = None
        self._montrer_graphes(False)
        self.graphes.definir([])
        self.vider()
        self.texte.configure(state="normal")
        self._ecrire("Erreur : " + message + "\n", "erreur")
        self.texte.configure(state="disabled")

    def afficher(self, res):
        self.resultat = res                  # les fichiers du résultat précédent restent disponibles pour le journal
        self.vider()
        t = self.texte
        t.configure(state="normal")
        self._ecrire(res.titre + "\n", "titre")
        self._ecrire("=" * len(res.titre) + "\n\n")
        if res.lignes:
            self._ecrire("\n".join(res.lignes) + "\n\n", "para")
        for nom, (cols, rows) in res.tables.items():
            self._ecrire(libelle(nom) + "\n", "section")
            self._ecrire(table_texte(cols, rows, libre=nom in res.libres) + "\n\n")
        for a in res.avertissements:
            self._ecrire("Remarque : " + a + "\n", "remarque")
        t.configure(state="disabled")
        t.yview_moveto(0)
        self._montrer_graphes(bool(res.figures))
        self.graphes.definir(res.figures)

    def enregistrer(self):
        from tkinter import filedialog, messagebox
        if self.resultat is None:
            messagebox.showinfo(TITRE, "Aucun résultat à enregistrer.")
            return
        chemin = choisir_fichier_ecriture(self.app.root, title="Enregistrer le résultat", defaultextension=".txt",
                                              filetypes=[("Texte", "*.txt")], initialdir=str(Path.home()))
        if not chemin:
            return
        p = Path(chemin)
        p.write_text(self.resultat.texte() + "\n", encoding="utf-8")
        for f in self.resultat.figures:
            shutil.copy(f, p.with_name(f"{p.stem}_{f.name}"))
        self.app.statut(f"Résultat enregistré : {p.name}" + (" (figures PNG à côté)" if self.resultat.figures else ""))


def _placer_fenetre(win, parent, largeur_min=460):
    """Dimensionne une fenêtre de dialogue d'après son contenu (boutons toujours visibles) et la centre sur la fenêtre principale."""
    win.update_idletasks()
    w, h = max(win.winfo_reqwidth(), largeur_min), win.winfo_reqheight() + 8
    win.minsize(w, h)
    try:
        x = parent.winfo_rootx() + (parent.winfo_width() - w) // 2
        y = parent.winfo_rooty() + (parent.winfo_height() - h) // 3
    except Exception:
        x = y = 100
    win.geometry(f"{w}x{h}+{max(x, 0)}+{max(y, 0)}")


def boite_dialogue(parent, titre, message, boutons=("OK",), defaut=0, details=None):
    """Petite boîte de dialogue à boutons personnalisés, au texte à la ligne ; retourne l'indice du bouton choisi
    (le dernier si la fenêtre est fermée). 'details' : liste de couples (libellé, valeur) affichés « libellé : valeur » sur une ligne."""
    import tkinter as tk
    from tkinter import ttk
    win = tk.Toplevel(parent)
    win.title(titre)
    win.transient(parent)
    choix = {"i": len(boutons) - 1}

    def fin(i):
        choix["i"] = i
        win.destroy()
    cadre = ttk.Frame(win, padding=(16, 14, 16, 12))
    cadre.pack(fill="both", expand=True)
    barre = ttk.Frame(cadre)
    barre.pack(side="bottom", anchor="e", pady=(14, 0))           # d'abord, pour rester visible même si la fenêtre est basse
    boutons_tk = []
    for i, texte in enumerate(boutons):
        b = tk.Button(barre, text=texte, command=lambda i=i: fin(i), padx=14, pady=3, default="active" if i == defaut else "normal")   # bouton classique : toujours dessiné
        b.pack(side="left", padx=4)
        boutons_tk.append(b)
    if message:
        ttk.Label(cadre, text=message, wraplength=520, justify="left", font=("TkDefaultFont", 10, "bold")).pack(anchor="w", fill="x", pady=(0, 6))
    if details:
        grille = ttk.Frame(cadre)
        grille.pack(anchor="w", fill="x")
        grille.columnconfigure(1, weight=1)
        for r, (lib, val) in enumerate(details):
            ttk.Label(grille, text=f"{lib} :").grid(row=r, column=0, sticky="nw", padx=(0, 6), pady=2)
            ttk.Label(grille, text=val, wraplength=440, justify="left").grid(row=r, column=1, sticky="nw", pady=2)
    win.bind("<Escape>", lambda e: fin(len(boutons) - 1))
    win.bind("<Return>", lambda e: fin(defaut))
    win.protocol("WM_DELETE_WINDOW", lambda: fin(len(boutons) - 1))
    _placer_fenetre(win, parent)
    win.update()                                  # dessine tout le contenu avant d'attendre l'utilisateur
    try:
        win.grab_set()
    except Exception:
        pass
    try:
        boutons_tk[defaut].focus_set()
    except Exception:
        pass
    parent.wait_window(win)
    return choix["i"]


def choisir_fichier_ecriture(parent, **options):
    """Choix d'un fichier à écrire. Le fichier existant est confirmé par une fenêtre propre (celle de Tk est tassée) ;
    retourne le chemin, ou '' si l'utilisateur annule."""
    from tkinter import filedialog
    while True:
        try:
            chemin = filedialog.asksaveasfilename(parent=parent, confirmoverwrite=False, **options)
        except Exception:
            chemin = filedialog.asksaveasfilename(parent=parent, **options)
        if not chemin:
            return ""
        p = Path(chemin)
        if not p.exists():
            return chemin
        rep = boite_dialogue(parent, TITRE, "Ce fichier existe déjà. Voulez-vous le remplacer ?",
                             ("Remplacer", "Choisir un autre nom", "Annuler"), defaut=1,
                             details=[("Dossier", str(p.parent)), ("Fichier", p.name)])
        if rep == 0:
            return chemin
        if rep == 2:
            return ""
        options["initialdir"], options["initialfile"] = str(p.parent), p.name


ALT_F = ["Variances différentes", "Variance de X supérieure à celle de Y", "Variance de X inférieure à celle de Y"]
ALT_F_R = dict(zip(ALT_F, ["two.sided", "greater", "less"]))
TESTS_NORMALITE = {"sw": "Shapiro-Wilk", "ad": "Anderson-Darling", "lf": "Kolmogorov-Smirnov (Lilliefors)",
                   "cvm": "Cramér-von Mises", "jb": "Jarque-Bera", "rj": "Ryan-Joiner"}
TYPES_KHI2 = {"Indépendance de deux variables": "independance", "Homogénéité de plusieurs populations": "homogeneite",
              "Conformité à une répartition ou à une loi": "conformite"}
TYPES_CARTE = ["Automatique (selon la taille des sous-groupes)", "X-barre / R (moyennes et étendues)",
               "X-barre / S (moyennes et écarts-types)", "Valeurs individuelles / étendues mobiles (I-MR)"]
TYPES_CARTE_R = dict(zip(TYPES_CARTE, ["auto", "xbar_r", "xbar_s", "imr"]))
LOIS_KHI2 = ["Répartition attendue (modalités)", "Loi normale"]

PERMIS_UI = {"sep": ["Automatique", "Tabulation", "Point-virgule", "Virgule", "Espace"],
             "dec": ["Automatique", "Point", "Virgule"], "ent": ["Automatique", "Oui", "Non"],
             "talt": ["Différent de", "Supérieur à", "Inférieur à"], "amode": ["large", "long"], "kmode": ["deux", "ref", "long"],
             "falt": ALT_F, "xtype": list(TYPES_KHI2), "xformat": ["brut", "table"], "xloi": LOIS_KHI2,
             "ctype": TYPES_CARTE, "cfmt": ["colonnes", "long"], "rrmode": ["croisee", "type1"]}
COLONNES_UI = ("cordre", "tx", "ty", "aval", "agrp", "cs", "ka", "kb", "kref", "kresp", "kev", "kech", "kess", "kreflong",
               "fx", "fy", "xa", "xb", "xvar", "xlab", "cval", "cgrp", "r1col", "rcp", "rco", "rcm")   # réglages qui désignent une colonne de données


class Application:
    def __init__(self, root):
        import tkinter as tk
        from tkinter import ttk
        self.tk, self.ttk = tk, ttk
        self.root = root
        self.etude = Etude()
        self.file = queue.Queue()
        self.occupe = False
        self.journal = []                    # tous les résultats calculés (EntreeJournal), repris dans le projet et le rapport
        self.projet_chemin = None
        self.zones_par_nom = {}
        self._dlg_gen = None
        self._titre_fenetre()
        sw, sh = root.winfo_screenwidth(), root.winfo_screenheight()
        root.geometry(f"{int(sw * 0.96)}x{int(sh * 0.92)}+{int(sw * 0.02)}+0")   # grande fenêtre, adaptée à l'écran
        try:
            root.attributes("-zoomed", True)                                      # agrandie au maximum (X11)
        except Exception:
            pass
        root.minsize(860, 600)
        self.statut_var = tk.StringVar(value="Prêt.")
        self.v_alpha = tk.StringVar(value="0,05")
        self.v_gcol, self.v_gper = tk.StringVar(value=AUCUN_GROUPE), tk.StringVar(value="Jour")
        self.v_gresume = tk.StringVar(value="aucun : toutes les données sont analysées ensemble.")
        self.groupe_vals = []
        self._construire()
        self._maj_choix()
        root.protocol("WM_DELETE_WINDOW", self.quitter)
        root.after(120, self._surveiller)
        threading.Thread(target=self._verifier_r, daemon=True).start()

    # -- utilitaires ------------------------------------------------------------------
    def statut(self, texte):
        self.statut_var.set(texte)

    def _verifier_r(self):
        ok, msg = verifier_r()
        if not ok:
            self.file.put(("statut", "⚠ " + msg))

    def _surveiller(self):
        try:
            while True:
                item = self.file.get_nowait()
                if item[0] == "statut":
                    self.statut(item[1])
                    continue
                if item[0] == "erreur":
                    from tkinter import messagebox
                    self.statut("Erreur.")
                    messagebox.showerror(TITRE, item[1])
                    continue
                if item[0] == "pdf":
                    self._pdf_termine(item[1])
                    continue
                zone, res, err = item
                self.occupe = False
                if err:
                    zone.afficher_erreur(err)
                    self.statut("Erreur.")
                else:
                    zone.afficher(res)
                    self._journaliser(zone.nom, res)
                    self.statut("Calcul terminé.")
        except queue.Empty:
            pass
        self.root.after(120, self._surveiller)

    def lancer(self, zone, fonction, sans_donnees=False):
        if self.etude.donnees is None and not sans_donnees:
            zone.afficher_erreur("Aucune donnée chargée : utiliser l'onglet « Données ».")
            return
        if self.occupe:
            self.statut("Un calcul est déjà en cours…")
            return
        try:
            self.etude.alpha = lire_alpha(self.v_alpha.get())
        except ErreurStat as e:
            zone.afficher_erreur(str(e))
            return
        self.occupe = True
        self.etude.echelle = echelle_ecran(self.root)                    # graphiques R nets à la taille de l'écran
        self.statut("Calcul en cours…")

        def tache():
            try:
                self.file.put((zone, fonction(), None))
            except ErreurStat as e:
                self.file.put((zone, None, str(e)))
            except Exception as e:
                self.file.put((zone, None, f"{type(e).__name__} : {e}"))
        threading.Thread(target=tache, daemon=True).start()

    def quitter(self):
        vus = set()
        for res in [e.res for e in self.journal] + [z.resultat for z in self.zones]:
            if res is not None and id(res) not in vus:
                vus.add(id(res))
                res.liberer()
        self.root.destroy()

    # -- construction -----------------------------------------------------------------
    def _construire(self):
        ttk = self.ttk
        self.zones = []
        self.carnets = {}
        self.vars_decl, self.listes_decl, self.champs_decl, self.infos_decl = {}, {}, [], []
        barre = ttk.Frame(self.root)
        barre.pack(fill="x", padx=8, pady=(8, 0))
        ttk.Label(barre, text="Risque α des tests").pack(side="left")
        ttk.Entry(barre, textvariable=self.v_alpha, width=7, justify="center").pack(side="left", padx=6)
        ttk.Label(barre, foreground="#555555",
                  text="(0,05 = 5 %) — H0 est rejetée si la p-value est inférieure à α ; elle est conservée si la p-value est supérieure ou égale à α.").pack(side="left")
        barre2 = ttk.Frame(self.root)
        barre2.pack(fill="x", padx=8, pady=(4, 0))
        ttk.Label(barre2, text="Analyser par groupe").pack(side="left")
        self.cb_gcol = ttk.Combobox(barre2, textvariable=self.v_gcol, state="readonly", width=22, values=[AUCUN_GROUPE])
        self.cb_gcol.pack(side="left", padx=6)
        self.cb_gcol.bind("<<ComboboxSelected>>", lambda e: self._changer_groupe())
        self.cb_gper = ttk.Combobox(barre2, textvariable=self.v_gper, state="disabled", width=7, values=list(PERIODES_DATE))
        self.cb_gper.pack(side="left")
        self.cb_gper.bind("<<ComboboxSelected>>", lambda e: self._changer_groupe())
        self.btn_groupes = ttk.Button(barre2, text="Choisir les groupes…", command=self._choisir_groupes, state="disabled")
        self.btn_groupes.pack(side="left", padx=6)
        ttk.Label(barre2, textvariable=self.v_gresume, foreground="#555555").pack(side="left", padx=4)
        nb = ttk.Notebook(self.root)
        nb.pack(fill="both", expand=True, padx=8, pady=(6, 0))
        ttk.Label(self.root, textvariable=self.statut_var, anchor="w", relief="sunken", padding=(6, 2)).pack(fill="x", padx=8, pady=6)
        self.nb = nb
        self._onglet_donnees(nb)
        g = self._groupe(nb, "Description")
        self._onglet_descriptif(g)
        self._onglet_normalite(g)
        self._onglet_identification(g)
        g = self._groupe(nb, "Tests")
        self._onglet_t(g)
        self._onglet_z(g)
        self._onglet_poisson(g)
        self._onglet_estimation(g)
        self._onglet_f(g)
        self._onglet_anova(g)
        self._onglet_anova2(g)
        self._onglet_khi2(g)
        g = self._groupe(nb, "Modélisation")
        self._onglet_correlation(g)
        self._onglet_regression(g)
        self._onglet_serie(g)
        g = self._groupe(nb, "Qualité")
        self._onglet_capabilite(g)
        self._onglet_cartes(g)
        self._onglet_cartes_attributs(g)
        self._onglet_rr(g)
        self._onglet_kappa(g)
        self._onglet_graphiques(nb)
        self._onglet_illustrations(nb)
        self._onglet_rapport(nb)
        self._onglet_vocabulaire(nb)
        self._onglet_aide(nb)

    def _groupe(self, nb, titre):
        """Onglet de premier niveau contenant lui-même des onglets ; renvoie le carnet intérieur."""
        ttk = self.ttk
        f = ttk.Frame(nb)
        nb.add(f, text=titre)
        interieur = ttk.Notebook(f)
        interieur.pack(fill="both", expand=True, padx=4, pady=4)
        self.carnets[titre] = interieur
        return interieur

    def _montrer_page(self, page):
        """Affiche une page (onglet), quel que soit son niveau d'imbrication."""
        carnet = self.nb
        chemin = []
        w = page
        while w is not None and str(w) != str(self.nb):
            if w.winfo_class() != "TNotebook":
                chemin.append(w)
            w = w.master
        for w in reversed(chemin):
            try:
                carnet.select(w)
            except Exception:
                break
            for sous in self.carnets.values():
                if str(sous.master) == str(w):
                    carnet = sous
                    break

    def _chemin_onglet(self):
        """Position de l'onglet affiché : [haut] ou [haut, sous-onglet]."""
        haut = self.nb.index(self.nb.select())
        chemin = [haut]
        page = self.nb.nametowidget(self.nb.select())
        for sous in self.carnets.values():
            if str(sous.master) == str(page):
                chemin.append(sous.index(sous.select()))
        return chemin

    def _aller_chemin(self, chemin):
        if isinstance(chemin, int) and not isinstance(chemin, bool):
            chemin = [chemin]
        if not (isinstance(chemin, list) and chemin and all(isinstance(i, int) and not isinstance(i, bool) for i in chemin)):
            return
        onglets = self.nb.tabs()
        if not 0 <= chemin[0] < len(onglets):
            return
        self.nb.select(onglets[chemin[0]])
        page = self.nb.nametowidget(onglets[chemin[0]])
        for sous in self.carnets.values():
            if str(sous.master) == str(page) and len(chemin) > 1 and 0 <= chemin[1] < len(sous.tabs()):
                sous.select(sous.tabs()[chemin[1]])

    def _nouvelle_zone(self, parent, nom):
        z = ZoneResultat(parent, self, nom)
        z.frame.pack(fill="both", expand=True, padx=8, pady=(0, 8))
        self.zones.append(z)
        self.zones_par_nom[nom] = z
        return z

    def _onglet(self, nb, titre):
        f = self.ttk.Frame(nb)
        nb.add(f, text=titre)
        return f

    # --- Données
    def _onglet_donnees(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Données")
        barre = ttk.Frame(f)
        barre.pack(fill="x", padx=8, pady=8)
        ttk.Button(barre, text="Ouvrir un fichier…", command=self.ouvrir_fichier).pack(side="left", padx=(0, 6))
        ttk.Button(barre, text="Charger la saisie", command=self.charger_saisie).pack(side="left", padx=6)
        ttk.Button(barre, text="Effacer la saisie", command=lambda: self.saisie.delete("1.0", "end")).pack(side="left", padx=6)
        ttk.Button(barre, text="Exporter en CSV…", command=self.exporter_csv).pack(side="left", padx=6)
        ttk.Button(barre, text="Générer des données…", command=self.generer_donnees).pack(side="left", padx=6)
        self._bouton_illustration(barre, "Données").pack(side="left", padx=6)

        opt = ttk.LabelFrame(f, text="Lecture")
        opt.pack(fill="x", padx=8)
        self.v_sep = tk.StringVar(value="Automatique")
        self.v_dec = tk.StringVar(value="Automatique")
        self.v_ent = tk.StringVar(value="Automatique")
        for i, (lib, var, vals) in enumerate([
                ("Séparateur", self.v_sep, ["Automatique", "Tabulation", "Point-virgule", "Virgule", "Espace"]),
                ("Décimale", self.v_dec, ["Automatique", "Point", "Virgule"]),
                ("Ligne de noms de colonnes", self.v_ent, ["Automatique", "Oui", "Non"])]):
            ttk.Label(opt, text=lib).grid(row=0, column=2 * i, padx=(8, 4), pady=6, sticky="e")
            ttk.Combobox(opt, textvariable=var, values=vals, state="readonly", width=15).grid(row=0, column=2 * i + 1, padx=(0, 8), pady=6)

        pan = ttk.PanedWindow(f, orient="vertical")
        pan.pack(fill="both", expand=True, padx=8, pady=8)
        cadre_s = ttk.LabelFrame(pan, text="Saisie ou collage Ctrl+V (une série par colonne, noms de colonnes en première ligne)")
        from tkinter import font as tkfont
        self.saisie = tk.Text(cadre_s, height=8, undo=True, wrap="none", font=tkfont.nametofont("TkFixedFont"))
        sy = ttk.Scrollbar(cadre_s, orient="vertical", command=self.saisie.yview)
        self.saisie.configure(yscrollcommand=sy.set)
        sy.pack(side="right", fill="y")
        self.saisie.pack(fill="both", expand=True, padx=4, pady=4)
        self._menu_saisie()
        pan.add(cadre_s, weight=1)

        cadre_a = ttk.LabelFrame(pan, text="Données chargées")
        self.resume_var = tk.StringVar(value="Aucune donnée chargée.")
        ttk.Label(cadre_a, textvariable=self.resume_var).pack(anchor="w", padx=6, pady=4)
        corps = ttk.Frame(cadre_a)
        corps.pack(fill="both", expand=True, padx=4, pady=(0, 4))
        self.arbre = ttk.Treeview(corps, show="headings", height=8)
        ay = ttk.Scrollbar(corps, orient="vertical", command=self.arbre.yview)
        ax = ttk.Scrollbar(corps, orient="horizontal", command=self.arbre.xview)
        self.arbre.configure(yscrollcommand=ay.set, xscrollcommand=ax.set)
        ax.pack(side="bottom", fill="x")
        ay.pack(side="right", fill="y")
        self.arbre.pack(side="left", fill="both", expand=True)
        pan.add(cadre_a, weight=2)

    def _menu_saisie(self):
        """Menu du clic droit sur la zone de saisie (Couper, Copier, Coller, Tout sélectionner, Effacer)."""
        tk = self.tk
        t = self.saisie
        menu = tk.Menu(t, tearoff=0)
        menu.add_command(label="Couper", command=lambda: t.event_generate("<<Cut>>"))
        menu.add_command(label="Copier", command=lambda: t.event_generate("<<Copy>>"))
        menu.add_command(label="Coller", command=lambda: (t.focus_set(), t.event_generate("<<Paste>>")))
        menu.add_separator()
        menu.add_command(label="Tout sélectionner", command=lambda: (t.tag_add("sel", "1.0", "end-1c"), t.mark_set("insert", "end-1c")))
        menu.add_command(label="Tout effacer", command=lambda: t.delete("1.0", "end"))

        def ouvrir(ev):
            t.focus_set()
            a_selection = bool(t.tag_ranges("sel"))
            menu.entryconfigure("Couper", state="normal" if a_selection else "disabled")
            menu.entryconfigure("Copier", state="normal" if a_selection else "disabled")
            try:
                menu.tk_popup(ev.x_root, ev.y_root)
            finally:
                menu.grab_release()
            return "break"

        for seq in ("<Button-3>", "<Button-2>", "<Control-Button-1>"):
            t.bind(seq, ouvrir)
        self.menu_saisie = menu

    # --- Analyse par groupe
    def _periode_groupe(self, col):
        d = self.etude.donnees
        return PERIODES_DATE.get(self.v_gper.get(), "jour") if (d and col in d.types and d.types[col] == "date") else None

    def _changer_groupe(self, garder=False):
        """Colonne de groupes choisie : calcule les groupes (tous retenus, ou ceux déjà retenus si garder) et règle l'étude."""
        d, col = self.etude.donnees, self.v_gcol.get()
        ancien = list(self.groupe_vals)
        if d is None or col not in d.colonnes:
            self.v_gcol.set(AUCUN_GROUPE)
            self.cb_gper.configure(state="disabled")
            self.btn_groupes.configure(state="disabled")
            self.groupe_vals = []
            self.etude.groupe = None
            self.v_gresume.set("aucun : toutes les données sont analysées ensemble.")
            return
        est_date = d.types[col] == "date"
        self.cb_gper.configure(state="readonly" if est_date else "disabled")
        tous = d.groupe_valeurs(col, self._periode_groupe(col))
        if len(tous) > MAX_GROUPES:
            from tkinter import messagebox
            messagebox.showinfo(TITRE, f"La colonne « {col} » compte {len(tous)} groupes : le maximum est {MAX_GROUPES}."
                                       + (" Choisir une période plus large (mois ou année)." if est_date else " Choisir une autre colonne."))
            self.v_gcol.set(AUCUN_GROUPE)
            self._changer_groupe()
            return
        retenus = [g for g in tous if g in ancien] if garder else []
        self.groupe_vals = retenus or tous
        self.btn_groupes.configure(state="normal")
        self._maj_groupe()

    def _maj_groupe(self):
        d, col = self.etude.donnees, self.v_gcol.get()
        if d is None or col not in d.colonnes or not self.groupe_vals:
            self.etude.groupe = None
            return
        per = self._periode_groupe(col)
        tous = d.groupe_valeurs(col, per)
        self.etude.groupe = {"colonne": col, "periode": per, "valeurs": list(self.groupe_vals)}
        liste = ", ".join(self.groupe_vals)
        if len(liste) > 70:
            liste = liste[:67] + "…"
        self.v_gresume.set(f"{len(self.groupe_vals)} groupe(s) sur {len(tous)} : {liste} — chaque analyse est lancée groupe par groupe.")

    def _choisir_groupes(self):
        tk, ttk = self.tk, self.ttk
        d, col = self.etude.donnees, self.v_gcol.get()
        if d is None or col not in d.colonnes:
            return
        tous = d.groupe_valeurs(col, self._periode_groupe(col))
        win = tk.Toplevel(self.root)
        win.title(f"{TITRE} — groupes de « {col} »")
        win.transient(self.root)
        cadre = ttk.Frame(win, padding=12)
        cadre.pack(fill="both", expand=True)
        barre = ttk.Frame(cadre)
        barre.pack(side="bottom", fill="x", pady=(10, 0))
        ttk.Label(cadre, text="Groupes à analyser (un calcul et des graphiques par groupe)").pack(anchor="w", pady=(0, 6))
        zone = ttk.Frame(cadre)
        zone.pack(fill="both", expand=True)
        lb = tk.Listbox(zone, selectmode="multiple", exportselection=False, height=min(max(len(tous), 4), 14), width=36)
        sb = ttk.Scrollbar(zone, orient="vertical", command=lb.yview)
        lb.configure(yscrollcommand=sb.set)
        lb.pack(side="left", fill="both", expand=True)
        sb.pack(side="left", fill="y")
        for i, g in enumerate(tous):
            lb.insert("end", g)
            if g in self.groupe_vals:
                lb.select_set(i)

        def valider():
            sel = [lb.get(i) for i in lb.curselection()]
            if not sel:
                from tkinter import messagebox
                messagebox.showinfo(TITRE, "Choisir au moins un groupe.", parent=win)
                return
            self.groupe_vals = sel
            self._maj_groupe()
            win.destroy()
        ttk.Button(barre, text="Tout", command=lambda: lb.select_set(0, "end")).pack(side="left")
        ttk.Button(barre, text="Aucun", command=lambda: lb.selection_clear(0, "end")).pack(side="left", padx=4)
        ttk.Button(barre, text="Annuler", command=win.destroy).pack(side="right")
        ttk.Button(barre, text="OK", command=valider).pack(side="right", padx=4)
        _placer_fenetre(win, self.root, 380)
        win.grab_set()

    def _options_lecture(self):
        sep = {"Automatique": "auto", "Tabulation": "\t", "Point-virgule": ";", "Virgule": ",", "Espace": " "}[self.v_sep.get()]
        dec = {"Automatique": "auto", "Point": ".", "Virgule": ","}[self.v_dec.get()]
        ent = {"Automatique": "auto", "Oui": True, "Non": False}[self.v_ent.get()]
        return dict(sep=sep, dec=dec, entete=ent)

    def charger_saisie(self):
        from tkinter import messagebox
        try:
            self.etude.charger_texte(self.saisie.get("1.0", "end"), **self._options_lecture())
        except ErreurStat as e:
            messagebox.showerror(TITRE, str(e))
            return
        self._donnees_chargees("saisie")

    def ouvrir_fichier(self):
        from tkinter import filedialog, messagebox
        chemin = filedialog.askopenfilename(title="Ouvrir un fichier de données", initialdir=str(Path.home()),
                                            filetypes=[("Fichiers de données", "*.csv *.txt *.tsv *.dat *.xlsx"), ("Tous les fichiers", "*.*")])
        if not chemin:
            return
        try:
            self.etude.charger_fichier(chemin, **self._options_lecture())
            contenu = lire_texte_fichier(chemin)
        except ErreurStat as e:
            messagebox.showerror(TITRE, str(e))
            return
        except Exception as e:
            messagebox.showerror(TITRE, f"Lecture impossible : {e}")
            return
        if contenu is not None:
            self.saisie.delete("1.0", "end")
            self.saisie.insert("1.0", contenu)
        self._donnees_chargees(Path(chemin).name)

    def _donnees_chargees(self, origine):
        d = self.etude.donnees
        self.resume_var.set(f"{origine} — {d.resume()}")
        arbre = self.arbre
        arbre.delete(*arbre.get_children())
        arbre["columns"] = d.noms
        for n in d.noms:
            arbre.heading(n, text=n + {"num": "", "date": " (date)"}.get(d.types[n], " (texte)"))
            arbre.column(n, width=130 if d.types[n] == "date" else 110, anchor="e" if d.types[n] == "num" else "w", stretch=False)
        dates = [d.types[n] == "date" for n in d.noms]
        for ligne in d.lignes(500):
            arbre.insert("", "end", values=[("" if v is None else (date_affichee(v) if dt else fmt_nombre(v, 10))) for v, dt in zip(ligne, dates)])
        self._maj_choix()
        msg = d.resume()
        if d.avertissements:
            msg += " Attention : " + " ".join(d.avertissements)
        self.statut(msg)

    def exporter_csv(self):
        from tkinter import filedialog, messagebox
        if self.etude.donnees is None:
            messagebox.showinfo(TITRE, "Aucune donnée chargée.")
            return
        chemin = choisir_fichier_ecriture(self.root, title="Exporter les données", defaultextension=".csv",
                                              filetypes=[("CSV", "*.csv")], initialdir=str(Path.home()))
        if chemin:
            Path(chemin).write_text(self.etude.donnees.vers_csv_texte(), encoding="utf-8-sig")
            self.statut(f"Données exportées : {Path(chemin).name} (séparateur point-virgule, décimale virgule)")

    # --- Génération de données
    def nom_variable_suivant(self):
        d = self.etude.donnees
        pris = set(d.noms) if d else set()
        k = 1
        while f"Serie_{k}" in pris:
            k += 1
        return f"Serie_{k}"

    def generer_donnees(self):
        if self._dlg_gen is not None:
            try:
                if self._dlg_gen.win.winfo_exists():
                    self._dlg_gen.win.lift()
                    return
            except Exception:
                pass
        self._dlg_gen = DialogueGeneration(self)

    def donnees_generees(self, nom, n):
        self._remplir_saisie()
        self._donnees_chargees("données générées")
        self.statut(f"Variable « {nom} » : {n} valeur(s) générée(s). {self.etude.donnees.resume()}")

    def _remplir_saisie(self):
        """Affiche les données courantes dans la zone de saisie (si elles tiennent raisonnablement dedans)."""
        d = self.etude.donnees
        self.saisie.delete("1.0", "end")
        if d is not None and d.nb_lignes <= 5000:
            self.saisie.insert("1.0", d.vers_csv_texte())

    # --- Journal des résultats, projet et rapport
    def _titre_fenetre(self):
        nom = f" — {Path(self.projet_chemin).name}" if self.projet_chemin else ""
        self.root.title(f"{TITRE} — JFBConseils{nom}")

    def _journaliser(self, onglet, res):
        self.journal.append(EntreeJournal(onglet, res))
        self._maj_journal()

    def _maj_journal(self):
        arbre = self.arbre_j
        arbre.delete(*arbre.get_children())
        for i, e in enumerate(self.journal):
            arbre.insert("", "end", iid=str(i), values=("☑" if e.inclus else "☐", e.heure[11:16], e.onglet, e.res.titre))
        if self.journal:
            arbre.see(str(len(self.journal) - 1))

    def _clic_journal(self, event):
        arbre = self.arbre_j
        if arbre.identify_region(event.x, event.y) != "cell" or arbre.identify_column(event.x) != "#1":
            return None
        iid = arbre.identify_row(event.y)
        if not iid:
            return None
        e = self.journal[int(iid)]
        e.inclus = not e.inclus
        arbre.set(iid, "inc", "☑" if e.inclus else "☐")
        return "break"

    def _inclure_tout(self, oui):
        for e in self.journal:
            e.inclus = oui
        self._maj_journal()

    def afficher_du_journal(self):
        from tkinter import messagebox
        sel = self.arbre_j.selection()
        if not sel:
            messagebox.showinfo(TITRE, "Sélectionner d'abord une analyse dans la liste.")
            return
        e = self.journal[int(sel[0])]
        zone = self.zones_par_nom.get(e.onglet)
        if zone is None:
            return
        zone.afficher(e.res)
        self._montrer_page(zone.frame.master)
        self.statut(f"Analyse du {e.heure[11:16]} réaffichée dans l'onglet « {e.onglet} » : {e.res.titre}")

    def retirer_du_journal(self):
        sel = {int(i) for i in self.arbre_j.selection()}
        if not sel:
            return
        self.journal = [e for k, e in enumerate(self.journal) if k not in sel]
        self._maj_journal()

    def _onglet_rapport(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Rapport")
        projet = ttk.LabelFrame(f, text="Projet (pour analyse ultérieure)")
        projet.pack(fill="x", padx=8, pady=8)
        ttk.Button(projet, text="Enregistrer le projet…", command=self.enregistrer_projet).pack(side="left", padx=6, pady=6)
        ttk.Button(projet, text="Ouvrir un projet…", command=self.ouvrir_projet).pack(side="left", padx=6, pady=6)
        ttk.Label(projet, text="Les données, les résultats (texte, tableaux, graphiques) et les réglages des onglets sont conservés.").pack(side="left", padx=10)
        self._bouton_illustration(projet, "Rapport").pack(side="right", padx=6, pady=6)

        rap = ttk.LabelFrame(f, text="Rapport de synthèse PDF")
        rap.pack(fill="both", expand=True, padx=8, pady=(0, 8))
        haut = ttk.Frame(rap)
        haut.pack(fill="x", padx=6, pady=(6, 2))
        self.v_rap_titre = tk.StringVar(value=TITRE_RAPPORT)
        self.v_rap_auteur = tk.StringVar()
        ttk.Label(haut, text="Titre").grid(row=0, column=0, sticky="e", padx=4, pady=2)
        ttk.Entry(haut, textvariable=self.v_rap_titre, width=60).grid(row=0, column=1, sticky="w", pady=2)
        ttk.Label(haut, text="Auteur").grid(row=1, column=0, sticky="e", padx=4, pady=2)
        ttk.Entry(haut, textvariable=self.v_rap_auteur, width=40).grid(row=1, column=1, sticky="w", pady=2)

        ttk.Label(rap, text="Conclusions et commentaires (pour le rapport)").pack(anchor="w", padx=10, pady=(6, 0))
        cadre_t = ttk.Frame(rap)
        cadre_t.pack(fill="x", padx=8, pady=2)
        self.txt_rap = tk.Text(cadre_t, height=5, wrap="word", undo=True)
        sy = ttk.Scrollbar(cadre_t, orient="vertical", command=self.txt_rap.yview)
        self.txt_rap.configure(yscrollcommand=sy.set)
        sy.pack(side="right", fill="y")
        self.txt_rap.pack(side="left", fill="x", expand=True)

        ttk.Label(rap, text="Analyses effectuées (case : inclure ou non dans le rapport ; double-clic : revoir le résultat dans son onglet)").pack(anchor="w", padx=10, pady=(8, 0))
        cadre_j = ttk.Frame(rap)
        cadre_j.pack(fill="both", expand=True, padx=8, pady=2)
        self.arbre_j = ttk.Treeview(cadre_j, columns=("inc", "heure", "onglet", "titre"), show="headings", selectmode="extended", height=8)
        for cid, lib, larg, anc in [("inc", "Inclure", 60, "center"), ("heure", "Heure", 60, "center"),
                                    ("onglet", "Onglet", 110, "w"), ("titre", "Analyse", 600, "w")]:
            self.arbre_j.heading(cid, text=lib)
            self.arbre_j.column(cid, width=larg, anchor=anc, stretch=(cid == "titre"))
        sj = ttk.Scrollbar(cadre_j, orient="vertical", command=self.arbre_j.yview)
        self.arbre_j.configure(yscrollcommand=sj.set)
        sj.pack(side="right", fill="y")
        self.arbre_j.pack(side="left", fill="both", expand=True)
        self.arbre_j.bind("<Button-1>", self._clic_journal)
        self.arbre_j.bind("<Double-Button-1>", lambda e: self.afficher_du_journal() if self.arbre_j.identify_column(e.x) != "#1" else None)

        boutons = ttk.Frame(rap)
        boutons.pack(fill="x", padx=8, pady=(4, 8))
        ttk.Button(boutons, text="Tout inclure", command=lambda: self._inclure_tout(True)).pack(side="left", padx=(0, 6))
        ttk.Button(boutons, text="Ne rien inclure", command=lambda: self._inclure_tout(False)).pack(side="left", padx=6)
        ttk.Button(boutons, text="Revoir l'analyse sélectionnée", command=self.afficher_du_journal).pack(side="left", padx=6)
        ttk.Button(boutons, text="Retirer du journal", command=self.retirer_du_journal).pack(side="left", padx=6)
        ttk.Button(boutons, text="Générer le rapport PDF…", command=self.generer_pdf).pack(side="right")

    def _meta_rapport(self):
        return {"titre": self.v_rap_titre.get().strip() or TITRE_RAPPORT, "auteur": self.v_rap_auteur.get().strip(),
                "commentaires": self.txt_rap.get("1.0", "end").strip()}

    def _vars_ui(self):
        return {"sep": self.v_sep, "dec": self.v_dec, "ent": self.v_ent, "alpha": self.v_alpha, "tx": self.v_tx, "ty": self.v_ty,
                "mu": self.v_mu, "talt": self.v_talt, "tapp": self.v_tapp, "tvar": self.v_tvar, "amode": self.v_amode,
                "aval": self.v_aval, "agrp": self.v_agrp, "cs": self.v_cs, "lsl": self.v_lsl, "cib": self.v_cib,
                "usl": self.v_usl, "sig": self.v_sig, "kmode": self.v_kmode, "ka": self.v_ka, "kb": self.v_kb, "kref": self.v_kref,
                "kresp": self.v_kresp, "kev": self.v_kev, "kech": self.v_kech, "kess": self.v_kess, "kreflong": self.v_kreflong,
                "fx": self.v_fx, "fy": self.v_fy, "falt": self.v_falt, "xtype": self.v_xtype, "xformat": self.v_xformat,
                "xa": self.v_xa, "xb": self.v_xb, "xvar": self.v_xvar, "xlab": self.v_xlab, "xloi": self.v_xloi,
                "xprop": self.v_xprop, "xmu": self.v_xmu, "xsd": self.v_xsd, "xk": self.v_xk,
                "cordre": self.v_cordre, "cphase": self.v_cphase, "crupt": self.v_crupt, "ctype": self.v_ctype, "cfmt": self.v_cfmt, "cval": self.v_cval, "cgrp": self.v_cgrp,
                **{f"ct{i + 1}": v for i, v in enumerate(self.v_ctests)},
                **{f"nt_{k}": v for k, v in self.v_ntests.items()},
                **self.vars_decl,
                "rrmode": self.v_rrmode, "r1col": self.v_r1col, "r1ref": self.v_r1ref, "r1tol": self.v_r1tol, "r1k": self.v_r1k, "r1l": self.v_r1l,
                "rcp": self.v_rcp, "rco": self.v_rco, "rcm": self.v_rcm, "rctol": self.v_rctol, "rcl": self.v_rcl, "rca": self.v_rca}

    def _listes_ui(self):
        return {"desc": self.lb_desc, "norm": self.lb_norm, "anova": self.lb_anova, "kev": self.lb_kev, "xcols": self.lb_xcols, "ccols": self.lb_ccols, **self.listes_decl}

    def _etat_ui(self):
        """Réglages courants des onglets (pour analyse ultérieure)."""
        etat = {"v": {k: v.get() for k, v in self._vars_ui().items()},
                "listes": {k: [lb.get(i) for i in lb.curselection()] for k, lb in self._listes_ui().items()}}
        if self.etude.groupe:
            etat["groupe"] = {"colonne": self.etude.groupe["colonne"], "periode": self.v_gper.get(), "valeurs": list(self.groupe_vals)}
        try:
            etat["onglet"] = self._chemin_onglet()
        except Exception:
            pass
        return etat

    def _appliquer_ui(self, etat):
        if not isinstance(etat, dict):
            return
        vues = etat.get("v") if isinstance(etat.get("v"), dict) else {}
        d = self.etude.donnees
        noms = set(d.noms) if d else set()
        for cle, var in self._vars_ui().items():
            if cle not in vues:
                continue
            val = vues[cle]
            if isinstance(var, self.tk.BooleanVar):
                if isinstance(val, bool):
                    var.set(val)
            elif isinstance(val, str):
                if cle in PERMIS_UI and val not in PERMIS_UI[cle]:
                    continue
                if cle in COLONNES_UI and val not in noms and not (cle in ("ty", "kess", "xlab", "cordre") and val.startswith("—")):
                    continue
                var.set(val)
        listes = etat.get("listes") if isinstance(etat.get("listes"), dict) else {}
        self.lb_xcols.configure(state="normal")
        for cle, lb in self._listes_ui().items():
            choisis = listes.get(cle)
            if not isinstance(choisis, list):
                continue
            lb.selection_clear(0, "end")
            for i, n in enumerate(lb.get(0, "end")):
                if n in choisis:
                    lb.selection_set(i)
        self._maj_khi2()
        g = etat.get("groupe")
        if isinstance(g, dict) and isinstance(g.get("colonne"), str) and g["colonne"] in noms:
            if g.get("periode") in PERIODES_DATE:
                self.v_gper.set(g["periode"])
            self.v_gcol.set(g["colonne"])
            self.groupe_vals = [v for v in g.get("valeurs", []) if isinstance(v, str)]
            self._changer_groupe(garder=True)
        self._aller_chemin(etat.get("onglet"))

    def enregistrer_projet(self):
        from tkinter import filedialog, messagebox
        if self.etude.donnees is None and not self.journal:
            messagebox.showinfo(TITRE, "Rien à enregistrer : aucune donnée ni aucun résultat.")
            return
        init = Path(self.projet_chemin) if self.projet_chemin else None
        chemin = choisir_fichier_ecriture(
            self.root, title="Enregistrer le projet", defaultextension=EXT_PROJET,
            filetypes=[("Projet Statistiques", f"*{EXT_PROJET}")],
            initialdir=str(init.parent if init else Path.home()),
            initialfile=init.name if init else f"projet_{datetime.date.today():%Y%m%d}{EXT_PROJET}")
        if not chemin:
            return
        try:
            enregistrer_projet(chemin, self.etude.donnees, [(e.onglet, e.heure, e.res, e.inclus) for e in self.journal],
                               self._meta_rapport(), self._etat_ui())
        except Exception as e:
            messagebox.showerror(TITRE, f"Enregistrement impossible : {e}")
            return
        self.projet_chemin = chemin
        self._titre_fenetre()
        self.statut(f"Projet enregistré : {Path(chemin).name}")

    def ouvrir_projet(self):
        from tkinter import filedialog, messagebox
        if (self.etude.donnees is not None or self.journal) and not messagebox.askokcancel(
                TITRE, "Ouvrir un projet remplace les données et les résultats actuels. Continuer ?"):
            return
        chemin = filedialog.askopenfilename(
            title="Ouvrir un projet", initialdir=str(Path(self.projet_chemin).parent if self.projet_chemin else Path.home()),
            filetypes=[("Projet Statistiques", f"*{EXT_PROJET}"), ("Tous les fichiers", "*.*")])
        if not chemin:
            return
        try:
            meta, donnees, entrees, ui = lire_projet(chemin)
        except ErreurStat as e:
            messagebox.showerror(TITRE, str(e))
            return
        except Exception as e:
            messagebox.showerror(TITRE, f"Lecture impossible : {e}")
            return
        self.etude.donnees = donnees
        self.journal = [EntreeJournal(o, r, h or None, i) for (o, h, r, i) in entrees]
        for z in self.zones:
            z.reinitialiser()
        if donnees is not None:
            self._remplir_saisie()
            self._donnees_chargees(f"projet {Path(chemin).name}")
        else:
            self.saisie.delete("1.0", "end")
            self.arbre.delete(*self.arbre.get_children())
            self.resume_var.set("Aucune donnée chargée.")
            self._maj_choix()
        self.v_rap_titre.set(str(meta.get("titre") or TITRE_RAPPORT))
        self.v_rap_auteur.set(str(meta.get("auteur") or ""))
        self.txt_rap.delete("1.0", "end")
        self.txt_rap.insert("1.0", str(meta.get("commentaires") or ""))
        self._maj_journal()
        self._appliquer_ui(ui)
        derniers = {}
        for e in self.journal:
            derniers[e.onglet] = e
        for nom, e in derniers.items():
            zone = self.zones_par_nom.get(nom)
            if zone is not None:
                zone.afficher(e.res)
        self.projet_chemin = chemin
        self._titre_fenetre()
        self.statut(f"Projet ouvert : {Path(chemin).name} — {len(self.journal)} résultat(s) dans le journal.")

    def generer_pdf(self):
        from tkinter import filedialog, messagebox
        inclus = [e for e in self.journal if e.inclus]
        if self.etude.donnees is None and not inclus:
            messagebox.showinfo(TITRE, "Rien à mettre dans le rapport : charger des données et lancer des analyses.")
            return
        if str(DOSSIER) not in sys.path:
            sys.path.insert(0, str(DOSSIER))
        try:
            import rapport_statistique as rs
        except ImportError:
            messagebox.showerror(TITRE, "Le fichier rapport_statistique.py est introuvable dans le dossier du programme.")
            return
        ok, msg = rs.verifier()
        if not ok:
            messagebox.showerror(TITRE, msg)
            return
        if not inclus and not messagebox.askyesno(TITRE, "Aucune analyse n'est cochée : le rapport ne contiendra que la description des données. Continuer ?"):
            return
        chemin = choisir_fichier_ecriture(
            self.root, title="Enregistrer le rapport PDF", defaultextension=".pdf", filetypes=[("PDF", "*.pdf")],
            initialdir=str(Path(self.projet_chemin).parent if self.projet_chemin else Path.home()),
            initialfile=f"Rapport_{datetime.date.today():%Y%m%d}.pdf")
        if not chemin:
            return
        meta = self._meta_rapport()
        meta["date"] = datetime.datetime.now().strftime("%d/%m/%Y %H:%M")
        meta["pied"] = f"{TITRE} — JFBConseils"
        resume = self.etude.donnees.resume_rapport() if self.etude.donnees is not None else None
        analyses = []
        for k, e in enumerate(inclus, 1):
            a = e.res.pour_rapport()
            a.update(numero=k, onglet=e.onglet, heure=e.heure[11:16])
            analyses.append(a)
        self.statut("Création du rapport PDF…")

        def tache():
            try:
                rs.generer_pdf(chemin, meta, resume, analyses, logo=str(ICONE) if ICONE.is_file() else None)
                self.file.put(("pdf", chemin))
            except Exception as e:
                self.file.put(("erreur", f"Rapport impossible : {type(e).__name__} : {e}"))
        threading.Thread(target=tache, daemon=True).start()

    def _pdf_termine(self, chemin):
        from tkinter import messagebox
        self.statut(f"Rapport enregistré : {Path(chemin).name}")
        if self._demander_ouverture(chemin):
            try:
                subprocess.Popen(["xdg-open", chemin], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            except Exception:
                messagebox.showinfo(TITRE, "Ouverture automatique impossible : ouvrir le PDF depuis le gestionnaire de fichiers.")

    def _demander_ouverture(self, chemin):
        """Fenêtre « Rapport enregistré » : dossier et fichier à la ligne, boutons Ouvrir / Fermer toujours visibles."""
        p = Path(chemin)
        return boite_dialogue(self.root, TITRE, "Rapport enregistré", ("Ouvrir", "Fermer"), defaut=0,
                              details=[("Dossier", str(p.parent)), ("Fichier", p.name)]) == 0

    # --- Onglets d'analyse
    def _liste_multi(self, parent, hauteur=4):
        tk, ttk = self.tk, self.ttk
        cadre = ttk.Frame(parent)
        lb = tk.Listbox(cadre, selectmode="multiple", exportselection=False, height=hauteur, width=28)
        sb = ttk.Scrollbar(cadre, orient="vertical", command=lb.yview)
        lb.configure(yscrollcommand=sb.set)
        lb.pack(side="left", fill="both")
        sb.pack(side="left", fill="y")
        return cadre, lb

    def _combo(self, parent, var, largeur=26):
        return self.ttk.Combobox(parent, textvariable=var, state="readonly", width=largeur, values=[])

    def _onglet_descriptif(self, nb):
        ttk = self.ttk
        f = self._onglet(nb, "Descriptif")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        ttk.Label(c, text="Séries à analyser").grid(row=0, column=0, sticky="nw", padx=(0, 8))
        cadre, self.lb_desc = self._liste_multi(c)
        cadre.grid(row=0, column=1, sticky="w")
        b = ttk.Frame(c)
        b.grid(row=0, column=2, padx=12, sticky="n")
        ttk.Button(b, text="Tout sélectionner", command=lambda: self.lb_desc.select_set(0, "end")).pack(fill="x", pady=2)
        ttk.Button(b, text="Lancer", command=self.run_descriptif).pack(fill="x", pady=2)
        self.z_desc = self._nouvelle_zone(f, "Descriptif")

    def run_descriptif(self):
        cols = [self.lb_desc.get(i) for i in self.lb_desc.curselection()] or None
        self.lancer(self.z_desc, lambda: self.etude.descriptif(cols))

    def _onglet_normalite(self, nb):
        ttk = self.ttk
        f = self._onglet(nb, "Normalité")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        ttk.Label(c, text="Séries à tester").grid(row=0, column=0, sticky="nw", padx=(0, 8))
        cadre, self.lb_norm = self._liste_multi(c)
        cadre.grid(row=0, column=1, sticky="w")
        b = ttk.Frame(c)
        b.grid(row=0, column=2, padx=12, sticky="n")
        ttk.Button(b, text="Tout sélectionner", command=lambda: self.lb_norm.select_set(0, "end")).pack(fill="x", pady=2)
        ttk.Button(b, text="Lancer", command=self.run_normalite).pack(fill="x", pady=2)
        ft = ttk.LabelFrame(c, text="Tests de normalité")
        ft.grid(row=0, column=3, sticky="nw", padx=12)
        self.v_ntests = {k: self.tk.BooleanVar(value=(k in ("sw", "ad"))) for k in TESTS_NORMALITE}
        for k, lib in TESTS_NORMALITE.items():
            ttk.Checkbutton(ft, text=lib, variable=self.v_ntests[k]).pack(anchor="w", padx=6)
        ttk.Label(c, text="Un graphique (histogramme et droite de Henry) est tracé pour chaque série.\nLes tests n'ont pas tous la même puissance : Shapiro-Wilk et Anderson-Darling sont les plus sensibles ;\nJarque-Bera suppose beaucoup de données.",
                  foreground="#555555", justify="left").grid(row=0, column=4, sticky="nw", padx=12)
        self.z_norm = self._nouvelle_zone(f, "Normalité")

    def run_normalite(self):
        cols = [self.lb_norm.get(i) for i in self.lb_norm.curselection()]      # lecture dans le thread principal
        tests = [k for k, v in self.v_ntests.items() if v.get()]
        self.lancer(self.z_norm, lambda: self.etude.normalite(cols, tests))

    def _onglet_t(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Test t (Student)")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_tx, self.v_ty = tk.StringVar(), tk.StringVar()
        self.v_mu = tk.StringVar(value="0")
        self.v_talt = tk.StringVar(value="Différent de")
        self.v_tapp, self.v_tvar = tk.BooleanVar(value=False), tk.BooleanVar(value=False)
        ttk.Label(c, text="Série X").grid(row=0, column=0, sticky="e", padx=4, pady=3)
        self.cb_tx = self._combo(c, self.v_tx)
        self.cb_tx.grid(row=0, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Série Y").grid(row=1, column=0, sticky="e", padx=4, pady=3)
        self.cb_ty = self._combo(c, self.v_ty)
        self.cb_ty.grid(row=1, column=1, sticky="w", pady=3)
        ttk.Label(c, text="(« — un échantillon — » : comparaison de X à une valeur de référence)").grid(row=1, column=2, sticky="w", padx=8)
        ttk.Label(c, text="Valeur de référence").grid(row=2, column=0, sticky="e", padx=4, pady=3)
        ttk.Entry(c, textvariable=self.v_mu, width=14).grid(row=2, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Hypothèse alternative").grid(row=3, column=0, sticky="e", padx=4, pady=3)
        ttk.Combobox(c, textvariable=self.v_talt, state="readonly", width=24,
                     values=["Différent de", "Supérieur à", "Inférieur à"]).grid(row=3, column=1, sticky="w", pady=3)
        ttk.Checkbutton(c, text="Mesures appariées", variable=self.v_tapp).grid(row=4, column=1, sticky="w", pady=2)
        ttk.Checkbutton(c, text="Variances supposées égales (échantillons indépendants)", variable=self.v_tvar).grid(row=5, column=1, sticky="w", pady=2)
        ttk.Button(c, text="Lancer", command=self.run_t).grid(row=6, column=1, sticky="w", pady=8)
        self.z_t = self._nouvelle_zone(f, "Test t")

    def run_t(self):
        try:
            mu = lire_nombre(self.v_mu.get(), "la valeur de référence") or 0.0
        except ErreurStat as e:
            self.z_t.afficher_erreur(str(e))
            return
        y = self.v_ty.get()
        y = "" if y.startswith("—") else y
        alt = {"Différent de": "two.sided", "Supérieur à": "greater", "Inférieur à": "less"}[self.v_talt.get()]
        x = self.v_tx.get()
        apparie, var_egales = self.v_tapp.get(), self.v_tvar.get()
        self.lancer(self.z_t, lambda: self.etude.test_t(x, y or None, mu, apparie, var_egales, alt))

    # --- Onglets décrits par une liste de champs (construction commune)
    def _onglet_decl(self, nb, titre, prefixe, champs, commande, libre=None, aide=None, fixes=None):
        """Onglet formé de champs décrits par des dictionnaires :
        id, lib, type (col | cols | liste | choix | num | case), cols (num | all | txt), opts [(libellé, code)],
        defaut, option (champ facultatif), si (fonction des valeurs courantes : le champ n'est montré que si elle renvoie vrai), aide."""
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, titre)
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        ligne = {}
        vals = {}
        info = {"champs": champs, "prefixe": prefixe, "ligne": ligne, "vals": vals, "fixes": fixes or {}}
        self.infos_decl.append(info)

        def codes():
            out = {}
            for ch in champs:
                v = vals.get(ch["id"])
                if ch["type"] == "choix":
                    out[ch["id"]] = dict(ch["opts"]).get(v.get(), "") if v is not None else ""
                elif v is not None:
                    out[ch["id"]] = v.get()
            return out

        def maj_visibilite(*_):
            cv = codes()
            for ch in champs:
                if ch.get("si") is None:
                    continue
                try:
                    vis = bool(ch["si"](cv))
                except Exception:
                    vis = True
                for w in ligne.get(ch["id"], []):
                    (w.grid if vis else w.grid_remove)()
        info["maj_visibilite"] = maj_visibilite
        for r, ch in enumerate(champs):
            cle = f"{prefixe}_{ch['id']}"
            ws = []
            lab = ttk.Label(c, text=ch["lib"])
            lab.grid(row=r, column=0, sticky="ne" if ch["type"] in ("cols", "liste") else "e", padx=4, pady=3)
            ws.append(lab)
            if ch["type"] in ("cols", "liste"):
                cadre, lb = self._liste_multi(c, hauteur=ch.get("hauteur", 5))
                cadre.grid(row=r, column=1, sticky="w", pady=3)
                ws.append(cadre)
                self.listes_decl[cle] = lb
                if ch["type"] == "liste":
                    for lib, _ in ch["opts"]:
                        lb.insert("end", lib)
                    lb.select_set(0, "end")
                ch["_lb"] = lb
            else:
                if ch["type"] == "case":
                    v = tk.BooleanVar(value=bool(ch.get("defaut", False)))
                    w = ttk.Checkbutton(c, variable=v)
                elif ch["type"] == "choix":
                    v = tk.StringVar(value=ch["opts"][0][0])
                    if ch.get("defaut"):
                        v.set(ch["defaut"])
                    w = ttk.Combobox(c, textvariable=v, state="readonly", width=34, values=[o[0] for o in ch["opts"]])
                    w.bind("<<ComboboxSelected>>", maj_visibilite)
                elif ch["type"] == "num":
                    v = tk.StringVar(value=str(ch.get("defaut", "")))
                    w = ttk.Entry(c, textvariable=v, width=14)
                else:                                                   # col
                    v = tk.StringVar()
                    w = ttk.Combobox(c, textvariable=v, state="readonly", width=26, values=[])
                    self.champs_decl.append((w, v, ch))
                w.grid(row=r, column=1, sticky="w", pady=3)
                ws.append(w)
                vals[ch["id"]] = v
                self.vars_decl[cle] = v
                v.trace_add("write", maj_visibilite)
            if ch.get("aide"):
                h = ttk.Label(c, text=ch["aide"], foreground="#666666")
                h.grid(row=r, column=2, sticky="w", padx=8)
                ws.append(h)
            ligne[ch["id"]] = ws
        r = len(champs)
        b = ttk.Frame(c)
        b.grid(row=r, column=1, sticky="w", pady=8)
        ttk.Button(b, text="Lancer", command=lambda: self._lancer_decl(zone, info, commande, libre)).pack(side="left")
        if aide:
            ttk.Label(c, text=aide, foreground="#555555", wraplength=760, justify="left").grid(row=r + 1, column=0, columnspan=3, sticky="w", padx=4)
        zone = self._nouvelle_zone(f, titre)
        maj_visibilite()
        return zone, info

    def _params_decl(self, info):
        """Paramètres de la commande R à partir des champs visibles ; ErreurStat si un champ obligatoire manque."""
        champs, vals = info["champs"], info["vals"]
        cv = {}
        for ch in champs:
            v = vals.get(ch["id"])
            if ch["type"] == "choix":
                cv[ch["id"]] = dict(ch["opts"]).get(v.get(), "")
            elif v is not None:
                cv[ch["id"]] = v.get()
        params = {}
        for ch in champs:
            if ch.get("si") is not None and not ch["si"](cv):
                continue
            i, t = ch["id"], ch["type"]
            if t == "choix":
                params[i] = cv[i]
            elif t == "case":
                params[i] = bool(cv[i])
            elif t == "col":
                val = cv[i]
                if not val or val.startswith("—"):
                    if not ch.get("option"):
                        raise ErreurStat(f"Choisir {ch['lib'].lower()}.")
                    val = None
                params[i] = val
            elif t == "num":
                n = lire_nombre(cv[i], f"« {ch['lib']} »")
                if n is None and not ch.get("option") and ch.get("defaut", "") == "":
                    raise ErreurStat(f"Saisir « {ch['lib']} ».")
                params[i] = n
            else:
                lb = ch["_lb"]
                sel = [lb.get(k) for k in lb.curselection()]
                if t == "liste":
                    sel = [dict(ch["opts"])[x] for x in sel]
                if ch.get("exclure"):
                    sel = [x for x in sel if x != cv.get(ch["exclure"])]
                if not sel:
                    raise ErreurStat(f"Sélectionner au moins un élément dans « {ch['lib']} ».")
                params[i] = sel
        params.update(info.get("fixes", {}))
        return params

    def _lancer_decl(self, zone, info, commande, libre):
        try:
            params = self._params_decl(info)
        except ErreurStat as e:
            zone.afficher_erreur(str(e))
            return
        sans = bool(libre and libre(params))
        self.lancer(zone, lambda: (self.etude.commande_libre if sans else self.etude.commande)(commande, params), sans_donnees=sans)

    def _maj_champs_decl(self):
        d = self.etude.donnees
        noms = {"num": d.noms_num if d else [], "all": d.noms if d else [], "txt": d.noms_txt if d else [],
                "ordre": (d.noms_date + d.noms_num) if d else []}
        for w, v, ch in self.champs_decl:
            vals = list(noms.get(ch.get("cols", "num"), []))
            if ch.get("option"):
                vals = [AUCUN_GROUPE] + vals
            w["values"] = vals
            nouveau = ch.get("_vals") != vals
            ch["_vals"] = vals
            if v.get() not in vals:
                k = ch.get("rang", 0)
                v.set(vals[min(k, len(vals) - 1)] if vals else "")
            if ch.get("cols") == "ordre" and nouveau:
                v.set(d.noms_date[0] if d and d.noms_date else AUCUN_GROUPE)
        for info_ch in self._champs_listes_cols():
            lb = info_ch["_lb"]
            lb.delete(0, "end")
            for n in noms.get(info_ch.get("cols", "num"), []):
                lb.insert("end", n)
            if info_ch.get("defaut_sel", True):
                lb.select_set(0, "end")

    def _champs_listes_cols(self):
        return [ch for infos in self.infos_decl for ch in infos["champs"] if ch["type"] == "cols"]

    def _onglet_correlation(self, nb):
        champs = [
            {"id": "colonnes", "lib": "Séries à corréler", "type": "cols", "aide": "au moins deux séries ; toutes les paires sont calculées"},
            {"id": "methode", "lib": "Coefficient", "type": "choix", "opts": [("Pearson (relation linéaire)", "pearson"), ("Spearman (rangs)", "spearman"), ("Kendall (rangs)", "kendall")]}]
        self.z_corr, _ = self._onglet_decl(nb, "Corrélation", "cor", champs, "correlation")

    def _onglet_regression(self, nb):
        champs = [
            {"id": "y", "lib": "Réponse (Y)", "type": "col"},
            {"id": "x", "lib": "Variables explicatives (X)", "type": "cols", "defaut_sel": False, "exclure": "y", "aide": "une ou plusieurs séries (régression multiple)"},
            {"id": "degre", "lib": "Modèle", "type": "choix", "opts": [("Linéaire (degré 1)", "1"), ("Polynôme de degré 2", "2"), ("Polynôme de degré 3", "3")],
             "aide": "degré 2 ou 3 : une seule variable X"}]
        self.z_reg, _ = self._onglet_decl(nb, "Régression", "reg", champs, "regression")

    def _onglet_serie(self, nb):
        champs = [
            {"id": "y", "lib": "Série", "type": "col"},
            {"id": "ordre", "lib": "Ordre des données", "type": "col", "cols": "ordre", "option": True, "aide": "colonne de dates ou de numéros d'ordre (sinon : ordre du tableau)"},
            {"id": "modeles", "lib": "Modèles de tendance", "type": "liste", "hauteur": 4, "opts": [("Linéaire", "lineaire"), ("Quadratique", "quadratique"), ("Exponentielle", "exponentielle"), ("Logistique (courbe en S)", "logistique")]},
            {"id": "horizon", "lib": "Périodes à prévoir", "type": "num", "defaut": "0"}]
        self.z_serie, _ = self._onglet_decl(nb, "Série chronologique", "ser", champs, "serie",
                                            aide="Avec « Analyser par groupe », la tendance est ajustée groupe par groupe (un graphique par groupe).")

    def _onglet_cartes_attributs(self, nb):
        champs = [
            {"id": "type", "lib": "Type de carte", "type": "choix", "opts": [("Carte p — proportion d'unités défectueuses", "p"), ("Carte np — nombre d'unités défectueuses", "np"),
                                                                              ("Carte c — nombre de défauts par unité de contrôle", "c"), ("Carte u — défauts par unité", "u")]},
            {"id": "defauts", "lib": "Colonne des comptages", "type": "col", "aide": "unités défectueuses (cartes p et np) ou nombre de défauts (cartes c et u) de chaque échantillon"},
            {"id": "effectif", "lib": "Colonne des effectifs", "type": "col", "option": True, "si": lambda v: v.get("type") != "c",
             "aide": "taille de chaque échantillon (ou nombre d'unités contrôlées) ; sinon, effectif constant ci-dessous. Carte c : aucun effectif (unités de taille constante)"},
            {"id": "n", "lib": "Effectif constant", "type": "num", "option": True, "si": lambda v: v.get("type") != "c", "aide": "utilisé si aucune colonne d'effectifs n'est choisie"},
            {"id": "ordre", "lib": "Ordre des données", "type": "col", "cols": "ordre", "option": True, "aide": "colonne de dates ou de numéros d'ordre (sinon : ordre du tableau)"},
            {"id": "phase", "lib": "Phases (avant / après)", "type": "col", "cols": "all", "option": True, "aide": "colonne qui désigne la phase de chaque ligne : limites calculées séparément"},
            {"id": "rupture", "lib": "ou « Après » à partir du point n°", "type": "num", "option": True, "aide": "sans colonne de phases"}]
        self.z_attr, _ = self._onglet_decl(nb, "Cartes d'attributs", "att", champs, "carte_attributs", fixes={"tests": [1, 2, 3, 4]})

    TYPES_GRAPH = [("Histogramme", "histogramme"), ("Secteurs (camembert)", "secteurs"), ("Barres", "barres"), ("Bulles", "bulles"),
                   ("Nuage de points", "nuage"), ("Valeurs individuelles", "individuelles"), ("Intervalles", "intervalles"),
                   ("Boîte à moustaches", "boite"), ("Diagramme de probabilité", "probabilite")]

    def _onglet_graphiques(self, nb):
        T = lambda *ts: (lambda v: v["type_graphique"] in ts)
        series = ("histogramme", "individuelles", "intervalles", "boite", "probabilite")
        champs = [
            {"id": "type_graphique", "lib": "Graphique", "type": "choix", "opts": self.TYPES_GRAPH},
            {"id": "colonnes", "lib": "Séries", "type": "cols", "si": T(*series), "aide": "une série par colonne (un graphique ou une boîte par série)"},
            {"id": "valeur", "lib": "Valeurs (colonne)", "type": "col", "option": True, "si": T(*series, "secteurs", "barres"),
             "aide": "séries : avec une colonne de groupes, remplace les séries ; secteurs et barres : valeurs à totaliser"},
            {"id": "groupe", "lib": "Groupes (colonne)", "type": "col", "cols": "all", "option": True, "si": T(*series, "barres"), "aide": "séries : une boîte ou un intervalle par groupe ; barres : barres côte à côte"},
            {"id": "categorie", "lib": "Catégories (colonne)", "type": "col", "cols": "all", "si": T("secteurs", "barres")},
            {"id": "fonction", "lib": "Mesure par catégorie", "type": "choix", "opts": [("Effectif (nombre de lignes)", "effectif"), ("Somme des valeurs", "somme"), ("Moyenne des valeurs", "moyenne")], "si": T("secteurs", "barres")},
            {"id": "x", "lib": "Axe X", "type": "col", "si": T("bulles", "nuage")},
            {"id": "y", "lib": "Axe Y", "type": "col", "rang": 1, "si": T("bulles", "nuage")},
            {"id": "taille", "lib": "Taille des bulles", "type": "col", "rang": 2, "si": T("bulles")},
            {"id": "couleur", "lib": "Couleur selon", "type": "col", "cols": "all", "option": True, "si": T("bulles", "nuage")},
            {"id": "ajust", "lib": "Droite d'ajustement", "type": "case", "defaut": True, "si": T("nuage")},
            {"id": "classes", "lib": "Nombre de classes", "type": "num", "option": True, "si": T("histogramme"), "aide": "facultatif (automatique par défaut)"},
            {"id": "normale", "lib": "Courbe normale", "type": "case", "defaut": False, "si": T("histogramme")},
            {"id": "loi", "lib": "Loi de référence", "type": "choix", "opts": self.LOIS_ID, "si": T("probabilite")}]
        self.z_graph, _ = self._onglet_decl(nb, "Graphiques", "gr", champs, "graphique",
                                            aide="Avec « Analyser par groupe » en haut de la fenêtre, un graphique est produit pour chaque groupe.")

    ALT3 = [("Différent de", "two.sided"), ("Supérieur à", "greater"), ("Inférieur à", "less")]

    def _onglet_z(self, nb):
        champs = [
            {"id": "mode", "lib": "Test", "type": "choix", "opts": [("1 échantillon", "1"), ("2 échantillons", "2")]},
            {"id": "x", "lib": "Série X", "type": "col"},
            {"id": "sigma", "lib": "Écart-type connu de X (σ)", "type": "num", "aide": "σ de la population, connu par ailleurs (pas celui de l'échantillon)"},
            {"id": "y", "lib": "Série Y", "type": "col", "rang": 1, "si": lambda v: v["mode"] == "2"},
            {"id": "sigma2", "lib": "Écart-type connu de Y (σ₂)", "type": "num", "si": lambda v: v["mode"] == "2"},
            {"id": "mu", "lib": "Valeur de référence", "type": "num", "defaut": "0",
             "aide": "moyenne de référence (1 échantillon) ou différence supposée des moyennes (2 échantillons)"},
            {"id": "alternative", "lib": "Hypothèse alternative", "type": "choix", "opts": self.ALT3}]
        self.z_z, _ = self._onglet_decl(nb, "Test Z", "z", champs, "z")

    def _onglet_poisson(self, nb):
        SRC = [("Colonne de comptages", "col"), ("Totaux saisis", "resume")]
        m2 = lambda v: v["mode"] == "2"
        champs = [
            {"id": "mode", "lib": "Test", "type": "choix", "opts": [("1 échantillon", "1"), ("2 échantillons", "2")]},
            {"id": "source", "lib": "Données (échantillon 1)", "type": "choix", "opts": SRC},
            {"id": "x", "lib": "Colonne de comptages", "type": "col", "si": lambda v: v["source"] == "col",
             "aide": "nombre d'événements par observation (entiers ≥ 0)"},
            {"id": "duree", "lib": "Taille de chaque observation", "type": "num", "defaut": "1", "si": lambda v: v["source"] == "col",
             "aide": "durée, surface, lot… identique pour chaque observation"},
            {"id": "total", "lib": "Nombre total d'événements", "type": "num", "si": lambda v: v["source"] == "resume"},
            {"id": "duree_tot", "lib": "Taille totale observée", "type": "num", "si": lambda v: v["source"] == "resume"},
            {"id": "source2", "lib": "Données (échantillon 2)", "type": "choix", "opts": SRC, "si": m2},
            {"id": "x2", "lib": "Colonne de comptages 2", "type": "col", "rang": 1, "si": lambda v: m2(v) and v["source2"] == "col"},
            {"id": "duree2", "lib": "Taille de chaque observation 2", "type": "num", "defaut": "1", "si": lambda v: m2(v) and v["source2"] == "col"},
            {"id": "total2", "lib": "Nombre total d'événements 2", "type": "num", "si": lambda v: m2(v) and v["source2"] == "resume"},
            {"id": "duree_tot2", "lib": "Taille totale observée 2", "type": "num", "si": lambda v: m2(v) and v["source2"] == "resume"},
            {"id": "lambda0", "lib": "Taux de référence (λ₀)", "type": "num", "si": lambda v: v["mode"] == "1", "aide": "nombre moyen d'événements par unité de taille"},
            {"id": "ratio0", "lib": "Rapport de référence λ₁ / λ₂", "type": "num", "defaut": "1", "si": m2, "aide": "1 = taux égaux"},
            {"id": "alternative", "lib": "Hypothèse alternative", "type": "choix", "opts": self.ALT3}]
        def libre(p):
            return p.get("source") == "resume" and (p.get("mode") != "2" or p.get("source2") == "resume")
        self.z_poisson, _ = self._onglet_decl(nb, "Test de Poisson", "poi", champs, "poisson", libre=libre)

    def _onglet_estimation(self, nb):
        champs = [
            {"id": "source", "lib": "Données", "type": "choix", "opts": [("Colonne de données", "col"), ("Résumé saisi (n, moyenne, écart-type)", "resume")]},
            {"id": "x", "lib": "Série", "type": "col", "si": lambda v: v["source"] == "col"},
            {"id": "n", "lib": "Taille de l'échantillon (n)", "type": "num", "si": lambda v: v["source"] == "resume"},
            {"id": "moyenne", "lib": "Moyenne de l'échantillon", "type": "num", "si": lambda v: v["source"] == "resume"},
            {"id": "ecart_type", "lib": "Écart-type de l'échantillon (s)", "type": "num", "option": True, "si": lambda v: v["source"] == "resume"},
            {"id": "sigma", "lib": "Écart-type connu (σ)", "type": "num", "option": True, "aide": "facultatif : loi normale si connu, loi t de Student sinon"},
            {"id": "niveau", "lib": "Niveau de confiance (%)", "type": "num", "defaut": "95"},
            {"id": "cote", "lib": "Intervalle", "type": "choix", "opts": [("Bilatéral", "two.sided"), ("Unilatéral — borne inférieure", "lower"), ("Unilatéral — borne supérieure", "upper")]},
            {"id": "marge", "lib": "Marge d'erreur souhaitée", "type": "num", "option": True, "aide": "facultatif : donne la taille d'échantillon nécessaire"}]
        self.z_estim, _ = self._onglet_decl(nb, "Estimation", "est", champs, "estimation", libre=lambda p: p.get("source") == "resume")

    LOIS_ID = [("Normale", "normale"), ("Log-normale", "lognormale"), ("Exponentielle", "exponentielle"), ("Gamma", "gamma"),
               ("Weibull", "weibull"), ("Logistique", "logistique"), ("Valeur extrême (Gumbel)", "gumbel")]

    def _onglet_identification(self, nb):
        champs = [
            {"id": "colonnes", "lib": "Séries à analyser", "type": "cols"},
            {"id": "lois", "lib": "Lois à comparer", "type": "liste", "opts": self.LOIS_ID, "hauteur": 7},
            {"id": "simulations", "lib": "Tirages de simulation", "type": "num", "defaut": "200",
             "aide": "pour la p-value (plus de tirages = plus précis, plus lent)"}]
        self.z_ident, _ = self._onglet_decl(nb, "Identification de loi", "ident", champs, "identification",
                                            aide="Chaque loi est ajustée aux données ; H0 : les données suivent cette loi. Elle est conservée si p-value ≥ α. "
                                                 "Avec « Analyser par groupe », l'identification est faite groupe par groupe.")

    def _onglet_anova(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "ANOVA")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_amode = tk.StringVar(value="large")
        ttk.Radiobutton(c, text="Chaque colonne numérique est un groupe", variable=self.v_amode, value="large").grid(row=0, column=0, columnspan=3, sticky="w")
        cadre, self.lb_anova = self._liste_multi(c)
        cadre.grid(row=1, column=0, columnspan=3, sticky="w", padx=20, pady=4)
        ttk.Radiobutton(c, text="Une colonne de mesures et une colonne de groupes", variable=self.v_amode, value="long").grid(row=2, column=0, columnspan=3, sticky="w", pady=(8, 0))
        self.v_aval, self.v_agrp = tk.StringVar(), tk.StringVar()
        ttk.Label(c, text="Mesures").grid(row=3, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_aval = self._combo(c, self.v_aval)
        self.cb_aval.grid(row=3, column=1, sticky="w")
        ttk.Label(c, text="Groupes").grid(row=4, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_agrp = self._combo(c, self.v_agrp)
        self.cb_agrp.grid(row=4, column=1, sticky="w")
        ttk.Button(c, text="Lancer", command=self.run_anova).grid(row=5, column=1, sticky="w", pady=8)
        self.z_anova = self._nouvelle_zone(f, "ANOVA")

    def run_anova(self):
        if self.v_amode.get() == "large":
            cols = [self.lb_anova.get(i) for i in self.lb_anova.curselection()]
            self.lancer(self.z_anova, lambda: self.etude.anova("large", colonnes=cols))
        else:
            v, g = self.v_aval.get(), self.v_agrp.get()
            self.lancer(self.z_anova, lambda: self.etude.anova("long", valeur=v, groupe=g))

    def _onglet_anova2(self, nb):
        champs = [
            {"id": "valeur", "lib": "Réponse (mesures)", "type": "col"},
            {"id": "facteur_a", "lib": "Facteur A", "type": "col", "cols": "all", "aide": "colonne de catégories (texte ou numéros de niveaux)"},
            {"id": "facteur_b", "lib": "Facteur B", "type": "col", "cols": "all", "aide": "colonne de catégories, différente du facteur A"},
            {"id": "interaction", "lib": "Interaction A × B", "type": "case", "defaut": True, "aide": "à retirer s'il n'y a qu'une mesure par combinaison"}]
        self.z_anova2, _ = self._onglet_decl(nb, "ANOVA 2 facteurs", "an2", champs, "anova2",
                                             aide="Une ligne par mesure, avec une colonne pour chaque facteur. Sommes des carrés ajustées (type III) ; comparaisons de Tukey pour les facteurs significatifs si le plan est équilibré.")

    # --- Test F
    def _onglet_f(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Test F")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_fx, self.v_fy = tk.StringVar(), tk.StringVar()
        self.v_falt = tk.StringVar(value=ALT_F[0])
        ttk.Label(c, text="Série X").grid(row=0, column=0, sticky="e", padx=4, pady=3)
        self.cb_fx = self._combo(c, self.v_fx)
        self.cb_fx.grid(row=0, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Série Y").grid(row=1, column=0, sticky="e", padx=4, pady=3)
        self.cb_fy = self._combo(c, self.v_fy)
        self.cb_fy.grid(row=1, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Hypothèse alternative").grid(row=2, column=0, sticky="e", padx=4, pady=3)
        ttk.Combobox(c, textvariable=self.v_falt, state="readonly", width=34, values=ALT_F).grid(row=2, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Compare les variances de deux séries ; le résultat indique aussi s'il faut préférer le test t de Student ou celui de Welch.").grid(row=0, column=2, rowspan=2, sticky="w", padx=12)
        ttk.Button(c, text="Lancer", command=self.run_f).grid(row=3, column=1, sticky="w", pady=8)
        self.z_f = self._nouvelle_zone(f, "Test F")

    def run_f(self):
        x, y, alt = self.v_fx.get(), self.v_fy.get(), ALT_F_R[self.v_falt.get()]
        self.lancer(self.z_f, lambda: self.etude.test_f(x, y, alt))

    # --- Khi²
    def _onglet_khi2(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Khi²")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_xtype = tk.StringVar(value=list(TYPES_KHI2)[0])
        self.v_xformat = tk.StringVar(value="brut")
        self.v_xa, self.v_xb, self.v_xvar, self.v_xlab = (tk.StringVar() for _ in range(4))
        self.v_xloi = tk.StringVar(value=LOIS_KHI2[0])
        self.v_xprop, self.v_xmu, self.v_xsd, self.v_xk = (tk.StringVar() for _ in range(4))
        ttk.Label(c, text="Test").grid(row=0, column=0, sticky="e", padx=4, pady=3)
        cb = ttk.Combobox(c, textvariable=self.v_xtype, state="readonly", width=44, values=list(TYPES_KHI2))
        cb.grid(row=0, column=1, columnspan=3, sticky="w", pady=3)
        cb.bind("<<ComboboxSelected>>", lambda e: self._maj_khi2())
        ttk.Label(c, text="Données").grid(row=1, column=0, sticky="e", padx=4, pady=3)
        self.rb_xbrut = ttk.Radiobutton(c, text="Données brutes (une ligne par individu)", variable=self.v_xformat, value="brut", command=self._maj_khi2)
        self.rb_xbrut.grid(row=1, column=1, columnspan=3, sticky="w")
        self.rb_xtab = ttk.Radiobutton(c, text="Tableau d'effectifs (cases déjà comptées)", variable=self.v_xformat, value="table", command=self._maj_khi2)
        self.rb_xtab.grid(row=2, column=1, columnspan=3, sticky="w")
        self.lb_xa_txt = ttk.Label(c, text="Variable A (lignes)")
        self.lb_xa_txt.grid(row=3, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_xa = self._combo(c, self.v_xa)
        self.cb_xa.grid(row=3, column=1, sticky="w")
        self.lb_xb_txt = ttk.Label(c, text="Variable B (colonnes)")
        self.lb_xb_txt.grid(row=4, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_xb = self._combo(c, self.v_xb)
        self.cb_xb.grid(row=4, column=1, sticky="w")
        self.lb_xcols_txt = ttk.Label(c, text="Colonnes d'effectifs")
        self.lb_xcols_txt.grid(row=5, column=0, sticky="ne", padx=(20, 4), pady=3)
        cadre, self.lb_xcols = self._liste_multi(c)
        cadre.grid(row=5, column=1, sticky="w", pady=3)
        self.lb_xlab_txt = ttk.Label(c, text="Libellés des lignes")
        self.lb_xlab_txt.grid(row=6, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_xlab = self._combo(c, self.v_xlab)
        self.cb_xlab.grid(row=6, column=1, sticky="w")
        ttk.Separator(c, orient="vertical").grid(row=1, column=2, rowspan=6, sticky="ns", padx=14)
        self.lb_xloi_txt = ttk.Label(c, text="Loi testée")
        self.lb_xloi_txt.grid(row=1, column=3, sticky="e", padx=(0, 4), pady=3)
        self.cb_xloi = ttk.Combobox(c, textvariable=self.v_xloi, state="readonly", width=34, values=LOIS_KHI2)
        self.cb_xloi.grid(row=1, column=4, sticky="w", pady=3)
        self.cb_xloi.bind("<<ComboboxSelected>>", lambda e: self._maj_khi2())
        self.lb_xvar_txt = ttk.Label(c, text="Variable testée")
        self.lb_xvar_txt.grid(row=2, column=3, sticky="e", padx=(0, 4), pady=3)
        self.cb_xvar = self._combo(c, self.v_xvar)
        self.cb_xvar.grid(row=2, column=4, sticky="w")
        self.lb_xprop_txt = ttk.Label(c, text="Proportions attendues")
        self.lb_xprop_txt.grid(row=3, column=3, sticky="e", padx=(0, 4), pady=3)
        self.e_xprop = ttk.Entry(c, textvariable=self.v_xprop, width=36)
        self.e_xprop.grid(row=3, column=4, sticky="w")
        self.lb_xnorm_txt = ttk.Label(c, text="Moyenne, écart-type")
        self.lb_xnorm_txt.grid(row=4, column=3, sticky="e", padx=(0, 4), pady=3)
        fn = ttk.Frame(c)
        fn.grid(row=4, column=4, sticky="w")
        self.e_xmu = ttk.Entry(fn, textvariable=self.v_xmu, width=10)
        self.e_xmu.pack(side="left")
        self.e_xsd = ttk.Entry(fn, textvariable=self.v_xsd, width=10)
        self.e_xsd.pack(side="left", padx=6)
        self.lb_xk_txt = ttk.Label(c, text="Classes")
        self.lb_xk_txt.grid(row=5, column=3, sticky="e", padx=(0, 4), pady=3)
        self.e_xk = ttk.Entry(c, textvariable=self.v_xk, width=10)
        self.e_xk.grid(row=5, column=4, sticky="w")
        self.lb_xaide = ttk.Label(c, justify="left", foreground="#555555")
        self.lb_xaide.grid(row=7, column=0, columnspan=5, sticky="w", padx=20, pady=(2, 0))
        ttk.Button(c, text="Lancer", command=self.run_khi2).grid(row=8, column=1, sticky="w", pady=8)
        self.z_x = self._nouvelle_zone(f, "Khi²")
        self._maj_khi2()

    def _maj_khi2(self):
        """Active seulement les champs utiles au test et au format de données choisis."""
        tl = TYPES_KHI2.get(self.v_xtype.get(), "independance")
        brut = self.v_xformat.get() == "brut"
        conf = tl == "conformite"
        normale = conf and self.v_xloi.get() == LOIS_KHI2[1]
        if normale:
            brut = True

        def etat(widgets, actif):
            for w in widgets:
                try:
                    w.configure(state=("readonly" if isinstance(w, self.ttk.Combobox) else "normal") if actif else "disabled")
                except Exception:
                    pass
        etat([self.cb_xa, self.cb_xb], not conf and brut)
        etat([self.lb_xcols, self.cb_xlab], not normale and not brut)
        etat([self.cb_xloi], conf)
        etat([self.cb_xvar], conf and brut)
        etat([self.e_xprop], conf and not normale)
        etat([self.e_xmu, self.e_xsd, self.e_xk], normale)
        for rb in (self.rb_xbrut, self.rb_xtab):
            rb.configure(state="disabled" if normale else "normal")
        if tl == "homogeneite":
            self.lb_xa_txt.configure(text="Populations (lignes)")
            self.lb_xb_txt.configure(text="Variable étudiée (colonnes)")
            self.lb_xcols_txt.configure(text="Colonnes d'effectifs (modalités)")
            self.lb_xlab_txt.configure(text="Nom des populations")
        else:
            self.lb_xa_txt.configure(text="Variable A (lignes)")
            self.lb_xb_txt.configure(text="Variable B (colonnes)")
            self.lb_xcols_txt.configure(text="Colonnes d'effectifs" if not conf else "Colonne des effectifs observés")
            self.lb_xlab_txt.configure(text="Libellés des lignes" if not conf else "Libellés des modalités")
        if conf:
            self.lb_xaide.configure(text="Conformité : les effectifs observés sont comparés à une répartition attendue (une proportion par modalité, séparées par des espaces ; vide = égales)\n"
                                         "ou à une loi normale sur des données numériques (moyenne et écart-type facultatifs, estimés si vides ; classes vide = automatique).")
        else:
            self.lb_xaide.configure(text="Tableau d'effectifs : une ligne par ligne du tableau, une colonne par modalité, et si on le souhaite une colonne de libellés des lignes.\n"
                                         "Données brutes : deux colonnes de catégories, une ligne par individu.")

    def run_khi2(self):
        tl = TYPES_KHI2[self.v_xtype.get()]
        fmt = self.v_xformat.get()
        lab = self.v_xlab.get()
        lab = "" if lab.startswith("—") else lab
        cols = [self.lb_xcols.get(i) for i in self.lb_xcols.curselection()]
        try:
            if tl == "conformite":
                normale = self.v_xloi.get() == LOIS_KHI2[1]
                params = {"test": "conformite", "loi": "normale" if normale else "repartition", "format": "brut" if normale else fmt,
                          "variable": self.v_xvar.get(), "colonnes": cols, "libelles": lab, "proportions": self.v_xprop.get(),
                          "moyenne": lire_nombre(self.v_xmu.get(), "la moyenne") if normale else None,
                          "ecart_type": lire_nombre(self.v_xsd.get(), "l'écart-type") if normale else None,
                          "classes": lire_nombre(self.v_xk.get(), "le nombre de classes") if normale else None}
            else:
                params = {"test": tl, "format": fmt, "a": self.v_xa.get(), "b": self.v_xb.get(), "colonnes": cols, "libelles": lab}
        except ErreurStat as e:
            self.z_x.afficher_erreur(str(e))
            return
        self.lancer(self.z_x, lambda: self.etude.commande("khi2", params))

    # --- Cartes de contrôle
    def _onglet_cartes(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Cartes de contrôle")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_ctype = tk.StringVar(value=TYPES_CARTE[0])
        self.v_cfmt = tk.StringVar(value="colonnes")
        self.v_cval, self.v_cgrp = tk.StringVar(), tk.StringVar()
        self.v_cordre = tk.StringVar(value=AUCUN_ORDRE)
        self.v_ctests = [tk.BooleanVar(value=(i < 4)) for i in range(8)]
        ttk.Label(c, text="Type de carte").grid(row=0, column=0, sticky="e", padx=4, pady=3)
        ttk.Combobox(c, textvariable=self.v_ctype, state="readonly", width=48, values=TYPES_CARTE).grid(row=0, column=1, columnspan=2, sticky="w", pady=3)
        ttk.Radiobutton(c, text="Chaque ligne est un sous-groupe ; les colonnes choisies en sont les valeurs", variable=self.v_cfmt, value="colonnes").grid(row=1, column=0, columnspan=3, sticky="w", pady=(8, 0))
        cadre, self.lb_ccols = self._liste_multi(c)
        cadre.grid(row=2, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Colonnes").grid(row=2, column=0, sticky="ne", padx=(20, 4), pady=3)
        ttk.Label(c, text="Les cellules vides donnent des sous-groupes de tailles différentes.\nValeurs individuelles : une seule colonne.", foreground="#555555", justify="left").grid(row=2, column=2, sticky="nw", padx=12)
        ttk.Radiobutton(c, text="Une colonne de mesures et une colonne qui désigne le sous-groupe de chaque mesure", variable=self.v_cfmt, value="long").grid(row=3, column=0, columnspan=3, sticky="w", pady=(8, 0))
        ttk.Label(c, text="Mesures").grid(row=4, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_cval = self._combo(c, self.v_cval)
        self.cb_cval.grid(row=4, column=1, sticky="w")
        ttk.Label(c, text="Sous-groupes").grid(row=5, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_cgrp = self._combo(c, self.v_cgrp)
        self.cb_cgrp.grid(row=5, column=1, sticky="w")
        ttk.Label(c, text="Des sous-groupes de tailles différentes donnent des limites de contrôle variables.", foreground="#555555").grid(row=5, column=2, sticky="w", padx=12)
        ttk.Label(c, text="Tests de causes spéciales").grid(row=6, column=0, sticky="e", padx=4, pady=(10, 3))
        ft = ttk.Frame(c)
        ft.grid(row=6, column=1, columnspan=2, sticky="w", pady=(10, 3))
        for i, v in enumerate(self.v_ctests):
            ttk.Checkbutton(ft, text=str(i + 1), variable=v).pack(side="left", padx=4)
        ttk.Label(c, text="1 : point hors limites — 2 : 9 points du même côté — 3 : 6 points en tendance — 4 : 14 points alternés — 5 à 8 : règles des zones (Minitab / Nelson)",
                  foreground="#555555").grid(row=7, column=0, columnspan=3, sticky="w", padx=20)
        ttk.Label(c, text="Ordre des données").grid(row=8, column=0, sticky="e", padx=4, pady=(10, 3))
        self.cb_cordre = ttk.Combobox(c, textvariable=self.v_cordre, state="readonly", width=26, values=[AUCUN_ORDRE])
        self.cb_cordre.grid(row=8, column=1, sticky="w", pady=(10, 3))
        ttk.Label(c, text="Une colonne de dates (ou de numéros d'ordre) trie les points chronologiquement et sert d'étiquette à l'axe.", foreground="#555555").grid(row=8, column=2, sticky="w", padx=12, pady=(10, 3))
        self.v_cphase, self.v_crupt = tk.StringVar(value=AUCUN_GROUPE), tk.StringVar()
        ttk.Label(c, text="Phases (avant / après)").grid(row=9, column=0, sticky="e", padx=4, pady=3)
        self.cb_cphase = ttk.Combobox(c, textvariable=self.v_cphase, state="readonly", width=26, values=[AUCUN_GROUPE])
        self.cb_cphase.grid(row=9, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Une colonne de phases : limites calculées séparément dans chaque phase (valeurs dans l'ordre d'apparition).", foreground="#555555").grid(row=9, column=2, sticky="w", padx=12)
        ttk.Label(c, text="ou « Après » à partir du point n°").grid(row=10, column=0, sticky="e", padx=4, pady=3)
        ttk.Entry(c, textvariable=self.v_crupt, width=8).grid(row=10, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Sans colonne de phases : les points avant ce numéro forment la phase « Avant », les suivants la phase « Après ».", foreground="#555555").grid(row=10, column=2, sticky="w", padx=12)
        ttk.Button(c, text="Lancer", command=self.run_carte).grid(row=11, column=1, sticky="w", pady=8)
        self.z_carte = self._nouvelle_zone(f, "Cartes de contrôle")

    def run_carte(self):
        try:
            rupt = lire_nombre(self.v_crupt.get(), "le numéro de point de la phase « Après »")
        except ErreurStat as e:
            self.z_carte.afficher_erreur(str(e))
            return
        type_ = TYPES_CARTE_R[self.v_ctype.get()]
        tests = [i + 1 for i, v in enumerate(self.v_ctests) if v.get()]
        params = {"type": type_, "format": self.v_cfmt.get(), "colonnes": [self.lb_ccols.get(i) for i in self.lb_ccols.curselection()],
                  "valeur": self.v_cval.get(), "groupe": self.v_cgrp.get(), "tests": tests or [0],
                  "ordre": "" if self.v_cordre.get() == AUCUN_ORDRE else self.v_cordre.get(),
                  "phase": "" if self.v_cphase.get() == AUCUN_GROUPE else self.v_cphase.get(), "rupture": rupt}
        self.lancer(self.z_carte, lambda: self.etude.commande("carte", params))

    # --- R&R
    def _onglet_rr(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "R&R")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_rrmode = tk.StringVar(value="croisee")
        self.v_r1col, self.v_r1ref, self.v_r1tol = tk.StringVar(), tk.StringVar(), tk.StringVar()
        self.v_r1k, self.v_r1l = tk.StringVar(value="20"), tk.StringVar(value="6")
        self.v_rcp, self.v_rco, self.v_rcm = tk.StringVar(), tk.StringVar(), tk.StringVar()
        self.v_rctol, self.v_rcl, self.v_rca = tk.StringVar(), tk.StringVar(value="6"), tk.StringVar(value="0,05")
        ttk.Radiobutton(c, text="R&R croisée : chaque opérateur mesure toutes les pièces", variable=self.v_rrmode, value="croisee").grid(row=0, column=0, columnspan=2, sticky="w")
        ttk.Radiobutton(c, text="R&R emboîtée : chaque pièce n'est mesurée que par un opérateur (essai destructif)", variable=self.v_rrmode, value="emboitee").grid(row=0, column=2, columnspan=2, sticky="w", padx=(16, 0))
        for i, (lib, var, attr) in enumerate([("Pièces", self.v_rcp, "cb_rcp"), ("Opérateurs", self.v_rco, "cb_rco"), ("Mesures", self.v_rcm, "cb_rcm")]):
            ttk.Label(c, text=lib).grid(row=1 + i, column=0, sticky="e", padx=(20, 4), pady=3)
            cb = self._combo(c, var)
            cb.grid(row=1 + i, column=1, sticky="w")
            setattr(self, attr, cb)
        ttk.Label(c, text="Tolérance (facultative)").grid(row=1, column=2, sticky="e", padx=(16, 4))
        ttk.Entry(c, textvariable=self.v_rctol, width=12).grid(row=1, column=3, sticky="w")
        ttk.Label(c, text="L (écarts-types de la variation d'étude)").grid(row=2, column=2, sticky="e", padx=(16, 4))
        ttk.Entry(c, textvariable=self.v_rcl, width=12).grid(row=2, column=3, sticky="w")
        ttk.Label(c, text="Seuil de l'interaction (croisée)").grid(row=3, column=2, sticky="e", padx=(16, 4))
        ttk.Entry(c, textvariable=self.v_rca, width=12).grid(row=3, column=3, sticky="w")
        ttk.Radiobutton(c, text="R&R de type 1 : une seule pièce étalon mesurée de nombreuses fois", variable=self.v_rrmode, value="type1").grid(row=4, column=0, columnspan=4, sticky="w", pady=(10, 0))
        ttk.Label(c, text="Mesures").grid(row=5, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_r1col = self._combo(c, self.v_r1col)
        self.cb_r1col.grid(row=5, column=1, sticky="w")
        ttk.Label(c, text="Valeur de référence").grid(row=6, column=0, sticky="e", padx=(20, 4), pady=3)
        ttk.Entry(c, textvariable=self.v_r1ref, width=14).grid(row=6, column=1, sticky="w")
        ttk.Label(c, text="Tolérance").grid(row=5, column=2, sticky="e", padx=(16, 4))
        ttk.Entry(c, textvariable=self.v_r1tol, width=12).grid(row=5, column=3, sticky="w")
        ttk.Label(c, text="K (% de la tolérance)").grid(row=6, column=2, sticky="e", padx=(16, 4))
        ttk.Entry(c, textvariable=self.v_r1k, width=12).grid(row=6, column=3, sticky="w")
        ttk.Label(c, text="L (écarts-types)").grid(row=7, column=2, sticky="e", padx=(16, 4))
        ttk.Entry(c, textvariable=self.v_r1l, width=12).grid(row=7, column=3, sticky="w")
        ttk.Label(c, text="Valeurs par défaut : conventions de Minitab (méthode ANOVA, L = 6, interaction retirée au seuil 0,05 ; type 1 : K = 20 %, L = 6, Cg et Cgk ≥ 1,33).\n"
                          "La norme AIAG retient 0,25 pour le seuil de l'interaction. La tolérance est l'écart entre les limites de spécification.\n"
                          "R&R croisée et emboîtée : une ligne par mesure, avec les colonnes Pièces, Opérateurs et Mesures ; plan équilibré. En R&R emboîtée, une même pièce ne figure que chez un seul opérateur.",
                  foreground="#555555", justify="left").grid(row=8, column=0, columnspan=4, sticky="w", padx=20, pady=(8, 0))
        ttk.Button(c, text="Calculer", command=self.run_rr).grid(row=9, column=1, sticky="w", pady=8)
        self.z_rr = self._nouvelle_zone(f, "R&R")

    def run_rr(self):
        try:
            if self.v_rrmode.get() == "type1":
                params = {"colonne": self.v_r1col.get(), "reference": lire_nombre(self.v_r1ref.get(), "la valeur de référence"),
                          "tolerance": lire_nombre(self.v_r1tol.get(), "la tolérance"), "k": lire_nombre(self.v_r1k.get(), "K"),
                          "l": lire_nombre(self.v_r1l.get(), "L")}
                cmd = "rr1"
            else:
                params = {"piece": self.v_rcp.get(), "operateur": self.v_rco.get(), "mesure": self.v_rcm.get(),
                          "tolerance": lire_nombre(self.v_rctol.get(), "la tolérance"), "l": lire_nombre(self.v_rcl.get(), "L")}
                cmd = "rr_emboitee" if self.v_rrmode.get() == "emboitee" else "rr_croisee"
                if cmd == "rr_croisee":
                    params["alpha"] = lire_nombre(self.v_rca.get(), "le seuil de l'interaction")
        except ErreurStat as e:
            self.z_rr.afficher_erreur(str(e))
            return
        self.lancer(self.z_rr, lambda: self.etude.commande(cmd, params))

    def _onglet_capabilite(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Capabilité")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_cs = tk.StringVar()
        self.v_lsl, self.v_cib, self.v_usl, self.v_sig = (tk.StringVar() for _ in range(4))
        ttk.Label(c, text="Série de mesures").grid(row=0, column=0, sticky="e", padx=4, pady=3)
        self.cb_cap = self._combo(c, self.v_cs)
        self.cb_cap.grid(row=0, column=1, sticky="w")
        for i, (lib, var) in enumerate([("LSL — limite inférieure", self.v_lsl), ("Cible (facultatif)", self.v_cib),
                                        ("USL — limite supérieure", self.v_usl),
                                        ("Écart-type intra-sous-groupe (facultatif)", self.v_sig)], start=1):
            ttk.Label(c, text=lib).grid(row=i, column=0, sticky="e", padx=4, pady=3)
            ttk.Entry(c, textvariable=var, width=16).grid(row=i, column=1, sticky="w")
        ttk.Label(c, text="Laisser vide une limite absente. Sans écart-type intra-sous-groupe, seuls Pp et Ppk sont calculés.").grid(row=5, column=0, columnspan=3, sticky="w", padx=4, pady=3)
        ttk.Button(c, text="Calculer", command=self.run_cap).grid(row=6, column=1, sticky="w", pady=8)
        self.z_cap = self._nouvelle_zone(f, "Capabilité")

    def run_cap(self):
        try:
            lsl = lire_nombre(self.v_lsl.get(), "LSL")
            usl = lire_nombre(self.v_usl.get(), "USL")
            cib = lire_nombre(self.v_cib.get(), "la cible")
            sig = lire_nombre(self.v_sig.get(), "l'écart-type intra-sous-groupe")
        except ErreurStat as e:
            self.z_cap.afficher_erreur(str(e))
            return
        col = self.v_cs.get()
        self.lancer(self.z_cap, lambda: self.etude.capabilite(col, lsl, usl, cib, sig))

    def _onglet_kappa(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Kappa")
        c = ttk.Frame(f)
        c.pack(fill="x", padx=8, pady=8)
        self.v_kmode = tk.StringVar(value="deux")
        self.v_ka, self.v_kb, self.v_kref = tk.StringVar(), tk.StringVar(), tk.StringVar()
        self.v_kresp, self.v_kev, self.v_kech = tk.StringVar(), tk.StringVar(), tk.StringVar()
        self.v_kess, self.v_kreflong = tk.StringVar(), tk.StringVar()
        ttk.Radiobutton(c, text="Comparer deux évaluations", variable=self.v_kmode, value="deux").grid(row=0, column=0, columnspan=3, sticky="w")
        ttk.Label(c, text="Première évaluation").grid(row=1, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_ka = self._combo(c, self.v_ka)
        self.cb_ka.grid(row=1, column=1, sticky="w")
        ttk.Label(c, text="Deuxième évaluation").grid(row=2, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_kb = self._combo(c, self.v_kb)
        self.cb_kb.grid(row=2, column=1, sticky="w")
        ttk.Radiobutton(c, text="Comparer des évaluateurs à une référence (norme) — une colonne par évaluateur", variable=self.v_kmode, value="ref").grid(row=3, column=0, columnspan=3, sticky="w", pady=(8, 0))
        ttk.Label(c, text="Référence (norme)").grid(row=4, column=0, sticky="e", padx=(20, 4), pady=3)
        self.cb_kref = self._combo(c, self.v_kref)
        self.cb_kref.grid(row=4, column=1, sticky="w")
        ttk.Label(c, text="Évaluateurs").grid(row=5, column=0, sticky="ne", padx=(20, 4), pady=3)
        cadre, self.lb_kev = self._liste_multi(c)
        cadre.grid(row=5, column=1, sticky="w", pady=3)
        ttk.Label(c, text="Une ligne par échantillon : une colonne pour la référence (valeur reconnue) et une colonne par évaluateur (ou par essai).\n"
                          "Kappa > 0,75 : satisfaisant ; de 0,40 à 0,75 : à améliorer ; < 0,40 : insuffisant (repères AIAG).").grid(row=5, column=2, sticky="w", padx=12)
        ttk.Radiobutton(c, text="Comparer des évaluateurs à une référence (norme) — données en format long (une ligne par évaluation)", variable=self.v_kmode, value="long").grid(row=6, column=0, columnspan=3, sticky="w", pady=(8, 0))
        lignes = [("Réponse", self.v_kresp, "cb_kresp"), ("Évaluateur", self.v_kev, "cb_kevl"), ("Échantillon (pièce)", self.v_kech, "cb_kech"),
                  ("Essai (facultatif)", self.v_kess, "cb_kess"), ("Référence (norme)", self.v_kreflong, "cb_kreflong")]
        for k, (lib, var, attr) in enumerate(lignes):
            ttk.Label(c, text=lib).grid(row=7 + k, column=0, sticky="e", padx=(20, 4), pady=3)
            cb = self._combo(c, var)
            cb.grid(row=7 + k, column=1, sticky="w")
            setattr(self, attr, cb)
        ttk.Label(c, text="Une ligne par évaluation : la réponse donnée, l'évaluateur, l'échantillon (et l'essai s'il y en a plusieurs)\n"
                          "et la valeur de référence de l'échantillon, répétée sur chaque ligne.").grid(row=7, column=2, rowspan=2, sticky="w", padx=12)
        ttk.Button(c, text="Calculer", command=self.run_kappa).grid(row=12, column=1, sticky="w", pady=8)
        self.z_kappa = self._nouvelle_zone(f, "Kappa")

    def run_kappa(self):
        mode = self.v_kmode.get()
        if mode == "ref":
            ref = self.v_kref.get()
            evs = [self.lb_kev.get(i) for i in self.lb_kev.curselection()]
            self.lancer(self.z_kappa, lambda: self.etude.kappa_reference(ref, evs))
        elif mode == "long":
            resp, ev, ech = self.v_kresp.get(), self.v_kev.get(), self.v_kech.get()
            ess, ref = self.v_kess.get(), self.v_kreflong.get()
            self.lancer(self.z_kappa, lambda: self.etude.kappa_long(resp, ev, ech, ess if not ess.startswith("—") else None, ref))
        else:
            a, b = self.v_ka.get(), self.v_kb.get()
            self.lancer(self.z_kappa, lambda: self.etude.kappa(a, b))

    # --- Illustrations
    def _onglet_illustrations(self, nb):
        tk, ttk = self.tk, self.ttk
        f = self._onglet(nb, "Illustrations")
        ttk.Label(f, foreground="#555555", wraplength=900, justify="left",
                  text=f"Images de référence (PNG) placées dans le dossier « {DOSSIER_ILLUSTRATIONS.name} » à côté du programme. "
                       "Plusieurs images pour un même sujet : <sujet>_1.png, <sujet>_2.png… ; « Ajouter une image… » les copie avec le bon nom.").pack(anchor="w", padx=10, pady=(8, 2))
        corps = ttk.Frame(f)
        corps.pack(fill="both", expand=True, padx=8, pady=4)
        gauche = ttk.Frame(corps)
        gauche.pack(side="left", fill="y", padx=(0, 8))
        self.lb_illus = tk.Listbox(gauche, exportselection=False, width=38, height=18, activestyle="none")
        self.lb_illus.pack(side="top", fill="y", expand=True)
        self.lb_illus.bind("<<ListboxSelect>>", lambda e: self._afficher_illustration())
        ttk.Button(gauche, text="Ajouter une image…", command=self._ajouter_illustration).pack(fill="x", pady=(6, 0))
        self.panneau_illus = PanneauGraphiques(corps, objet="Image")
        self.panneau_illus.frame.pack(side="left", fill="both", expand=True)
        self._maj_liste_illustrations()

    def _maj_liste_illustrations(self, garder=None):
        sel = garder if garder is not None else (self.lb_illus.curselection() or (0,))[0]
        self.lb_illus.delete(0, "end")
        for cle, titre in ILLUSTRATIONS:
            n = len(images_illustration(cle))
            self.lb_illus.insert("end", f"{titre}  ({n} image{'s' if n > 1 else ''})" if n else f"{titre}  (aucune image)")
        self.lb_illus.selection_set(sel)
        self._afficher_illustration()

    def _afficher_illustration(self):
        sel = self.lb_illus.curselection()
        if not sel:
            return
        cle = ILLUSTRATIONS[sel[0]][0]
        self.panneau_illus.definir(images_illustration(cle))
        if not self.panneau_illus.figures:
            self.panneau_illus.zone.delete("all")
            self.panneau_illus.zone.create_text(14, 14, anchor="nw", justify="left", fill="#666666",
                                                text=f"Aucune image pour ce sujet.\nPlacer « {cle}.png » (ou {cle}_1.png, {cle}_2.png…) dans le dossier\n« {DOSSIER_ILLUSTRATIONS.name} », ou utiliser « Ajouter une image… ».")

    def _ajouter_illustration(self):
        from tkinter import filedialog, messagebox
        sel = self.lb_illus.curselection()
        if not sel:
            messagebox.showinfo(TITRE, "Choisir d'abord un sujet dans la liste.")
            return
        cle = ILLUSTRATIONS[sel[0]][0]
        chemins = filedialog.askopenfilenames(parent=self.root, title="Choisir une ou plusieurs images PNG", filetypes=[("Image PNG", "*.png")])
        if not chemins:
            return
        try:
            DOSSIER_ILLUSTRATIONS.mkdir(exist_ok=True)
            existantes = images_illustration(cle)
            if existantes and not messagebox.askyesno(TITRE, f"{len(existantes)} image(s) existe(nt) déjà sur ce thème. Ajouter ?\n(Non = remplacer)", parent=self.root):
                for e in existantes:
                    e.unlink()
                existantes = []
            k = len(existantes)
            for ch in chemins:
                k += 1
                shutil.copy(ch, DOSSIER_ILLUSTRATIONS / (f"{cle}.png" if k == 1 and len(chemins) == 1 and not existantes else f"{cle}_{k}.png"))
        except OSError as e:
            messagebox.showerror(TITRE, f"Copie impossible : {e}", parent=self.root)
            return
        self._maj_liste_illustrations(garder=sel[0])

    def _bouton_illustration(self, parent, nom):
        """Bouton « Illustration… » d'un onglet qui n'a pas de zone de résultats (Données, Rapport) ; à placer avec pack."""
        return self.ttk.Button(parent, text="Illustration…", command=lambda: self.ouvrir_illustration(ILLUSTRATION_ONGLET[nom]))

    def ouvrir_illustration(self, cle):
        """Affiche le sujet donné dans l'onglet Illustrations."""
        for i, (c, _) in enumerate(ILLUSTRATIONS):
            if c == cle:
                self._montrer_page(self.nb.nametowidget(next(t for t in self.nb.tabs() if self.nb.tab(t, "text") == "Illustrations")))
                self.lb_illus.selection_clear(0, "end")
                self.lb_illus.selection_set(i)
                self.lb_illus.see(i)
                self._maj_liste_illustrations(garder=i)
                return

    # --- Vocabulaire
    def _onglet_vocabulaire(self, nb):
        tk, ttk = self.tk, self.ttk
        from tkinter import font as tkfont
        f = self._onglet(nb, "Vocabulaire")
        self._vocab, self._vocab_vus, self._vocab_mtime = [], [], None
        haut = ttk.Frame(f)
        haut.pack(fill="x", padx=8, pady=(8, 2))
        self.v_vocab_q = tk.StringVar()
        self.v_vocab_def = tk.BooleanVar(value=True)
        self.v_vocab_nb = tk.StringVar()
        ttk.Label(haut, text="Rechercher").pack(side="left")
        e = ttk.Entry(haut, textvariable=self.v_vocab_q, width=32)
        e.pack(side="left", padx=6)
        ttk.Checkbutton(haut, text="aussi dans les définitions", variable=self.v_vocab_def, command=self._filtrer_vocabulaire).pack(side="left", padx=6)
        ttk.Label(haut, textvariable=self.v_vocab_nb, foreground="#555555").pack(side="left", padx=10)
        self.v_vocab_q.trace_add("write", lambda *_: self._filtrer_vocabulaire())
        corps = ttk.PanedWindow(f, orient="horizontal")
        corps.pack(fill="both", expand=True, padx=8, pady=4)
        gauche = ttk.Frame(corps)
        self.lb_vocab = tk.Listbox(gauche, exportselection=False, width=40, activestyle="none")
        sy = ttk.Scrollbar(gauche, orient="vertical", command=self.lb_vocab.yview)
        self.lb_vocab.configure(yscrollcommand=sy.set)
        sy.pack(side="right", fill="y")
        self.lb_vocab.pack(side="left", fill="both", expand=True)
        self.lb_vocab.bind("<<ListboxSelect>>", lambda e: self._afficher_terme())
        self.lb_vocab.bind("<Double-Button-1>", lambda e: self._editer_terme(self._terme_choisi()))
        droite = ttk.Frame(corps)
        base = tkfont.nametofont("TkDefaultFont")
        self.tx_vocab = tk.Text(droite, wrap="word", padx=12, pady=10, background="white", relief="solid", borderwidth=1, font=base, state="disabled", width=60)
        sy2 = ttk.Scrollbar(droite, orient="vertical", command=self.tx_vocab.yview)
        self.tx_vocab.configure(yscrollcommand=sy2.set)
        sy2.pack(side="right", fill="y")
        self.tx_vocab.pack(side="left", fill="both", expand=True)
        gras = base.copy()
        gras.configure(weight="bold", size=base.cget("size") + 3)
        self.tx_vocab.tag_configure("terme", font=gras, spacing3=8)
        corps.add(gauche, weight=1)
        corps.add(droite, weight=3)
        bas = ttk.Frame(f)
        bas.pack(fill="x", padx=8, pady=(2, 8))
        for lib, cmd in (("Ajouter…", lambda: self._editer_terme(None)), ("Modifier…", lambda: self._editer_terme(self._terme_choisi())),
                         ("Supprimer", self._supprimer_terme), ("Recharger", lambda: self._charger_vocabulaire(True)),
                         ("Ouvrir le fichier…", self._ouvrir_fichier_vocabulaire)):
            ttk.Button(bas, text=lib, command=cmd).pack(side="left", padx=(0, 6))
        ttk.Label(bas, foreground="#555555", text=f"Fichier : {FICHIER_VOCABULAIRE.name} (dossier du programme) — « Terme : définition », une entrée par ligne.").pack(side="left", padx=8)
        self.nb.bind("<<NotebookTabChanged>>", lambda e: self._vocabulaire_modifie(), add="+")
        self._charger_vocabulaire()

    def _mtime_vocabulaire(self):
        try:
            return FICHIER_VOCABULAIRE.stat().st_mtime
        except OSError:
            return None

    def _vocabulaire_modifie(self):
        """Relit le fichier si on l'a modifié à la main depuis la dernière lecture (appelé à l'affichage de l'onglet)."""
        try:
            if self.nb.tab(self.nb.select(), "text") == "Vocabulaire" and self._mtime_vocabulaire() != self._vocab_mtime:
                self._charger_vocabulaire()
        except Exception:
            pass

    def _charger_vocabulaire(self, message=False):
        self._vocab = lire_vocabulaire()
        self._vocab_mtime = self._mtime_vocabulaire()
        self._filtrer_vocabulaire()
        if message:
            self.statut(f"Vocabulaire rechargé : {len(self._vocab)} terme(s).")

    def _filtrer_vocabulaire(self, garder=None):
        q = self.v_vocab_q.get()
        self._vocab_vus = chercher_vocabulaire(self._vocab, q, self.v_vocab_def.get())
        self.lb_vocab.delete(0, "end")
        for terme, _ in self._vocab_vus:
            self.lb_vocab.insert("end", terme)
        self.v_vocab_nb.set(f"{len(self._vocab_vus)} terme(s)" + (f" sur {len(self._vocab)}" if q.strip() else ""))
        i = 0
        if garder:
            i = next((k for k, (t, _) in enumerate(self._vocab_vus) if t == garder), 0)
        if self._vocab_vus:
            self.lb_vocab.selection_set(i)
            self.lb_vocab.see(i)
        self._afficher_terme()

    def _terme_choisi(self):
        sel = self.lb_vocab.curselection()
        return self._vocab_vus[sel[0]] if sel else None

    def _afficher_terme(self):
        e = self._terme_choisi()
        self.tx_vocab.configure(state="normal")
        self.tx_vocab.delete("1.0", "end")
        if e:
            self.tx_vocab.insert("end", e[0] + "\n", "terme")
            self.tx_vocab.insert("end", e[1])
        elif not self._vocab:
            self.tx_vocab.insert("end", f"Aucun terme : le fichier « {FICHIER_VOCABULAIRE.name} » est absent ou vide.\nUtiliser « Ajouter… » pour le créer.")
        else:
            self.tx_vocab.insert("end", "Aucun terme ne correspond à la recherche.")
        self.tx_vocab.configure(state="disabled")

    def _editer_terme(self, entree):
        """Fenêtre d'ajout (entree = None) ou de modification d'un terme ; l'enregistrement réécrit le fichier."""
        tk, ttk = self.tk, self.ttk
        from tkinter import messagebox
        win = tk.Toplevel(self.root)
        win.title(f"{TITRE} — " + ("modifier un terme" if entree else "ajouter un terme"))
        win.transient(self.root)
        cadre = ttk.Frame(win, padding=12)
        cadre.pack(fill="both", expand=True)
        v_terme = tk.StringVar(value=entree[0] if entree else "")
        ttk.Label(cadre, text="Terme").grid(row=0, column=0, sticky="ne", padx=4, pady=4)
        ttk.Entry(cadre, textvariable=v_terme, width=60).grid(row=0, column=1, sticky="we", pady=4)
        ttk.Label(cadre, text="Définition").grid(row=1, column=0, sticky="ne", padx=4, pady=4)
        tx = tk.Text(cadre, wrap="word", width=70, height=12, padx=6, pady=4, relief="solid", borderwidth=1)
        tx.grid(row=1, column=1, sticky="nsew", pady=4)
        if entree:
            tx.insert("1.0", entree[1])
        cadre.columnconfigure(1, weight=1)
        cadre.rowconfigure(1, weight=1)
        ttk.Label(cadre, foreground="#555555", text="Un retour à la ligne dans la définition crée un nouveau paragraphe.").grid(row=2, column=1, sticky="w")

        def enregistrer():
            terme = " ".join(v_terme.get().split())
            definition = tx.get("1.0", "end").strip()
            if not terme or not definition:
                messagebox.showwarning(TITRE, "Saisir le terme et sa définition.", parent=win)
                return
            if ":" in terme:
                messagebox.showwarning(TITRE, "Le terme ne doit pas contenir de deux-points (« : » sépare le terme de sa définition).", parent=win)
                return
            autres = [e for e in self._vocab if not entree or e[0] != entree[0]]
            if any(cle_tri_texte(e[0]) == cle_tri_texte(terme) for e in autres):
                messagebox.showwarning(TITRE, f"Le terme « {terme} » existe déjà : le modifier plutôt que l'ajouter.", parent=win)
                return
            try:
                ecrire_vocabulaire(autres + [(terme, definition)])
            except OSError as e:
                messagebox.showerror(TITRE, f"Enregistrement impossible : {e}", parent=win)
                return
            win.destroy()
            self.v_vocab_q.set("")
            self._charger_vocabulaire()
            self._filtrer_vocabulaire(garder=terme)
            self.statut(f"Terme « {terme} » enregistré dans {FICHIER_VOCABULAIRE.name}.")

        barre = ttk.Frame(cadre)
        barre.grid(row=3, column=0, columnspan=2, sticky="e", pady=(10, 0))
        for i, (lib, cmd) in enumerate((("Enregistrer", enregistrer), ("Annuler", win.destroy))):
            tk.Button(barre, text=lib, command=cmd, padx=14, pady=3, default="active" if i == 0 else "normal").pack(side="left", padx=4)
        _placer_fenetre(win, self.root, 640)
        win.update()
        try:
            win.grab_set()
        except Exception:
            pass

    def _supprimer_terme(self):
        from tkinter import messagebox
        e = self._terme_choisi()
        if not e:
            return
        if not messagebox.askyesno(TITRE, f"Supprimer le terme « {e[0]} » du vocabulaire ?", parent=self.root):
            return
        try:
            ecrire_vocabulaire([x for x in self._vocab if x[0] != e[0]])
        except OSError as err:
            messagebox.showerror(TITRE, f"Suppression impossible : {err}", parent=self.root)
            return
        self._charger_vocabulaire()

    def _ouvrir_fichier_vocabulaire(self):
        from tkinter import messagebox
        if not FICHIER_VOCABULAIRE.exists():
            try:
                ecrire_vocabulaire([])
            except OSError as err:
                messagebox.showerror(TITRE, f"Fichier impossible à créer : {err}", parent=self.root)
                return
        try:
            subprocess.Popen(["xdg-open", str(FICHIER_VOCABULAIRE)])
            self.statut("Fichier ouvert dans l'éditeur : après modification, « Recharger » (ou revenir sur cet onglet).")
        except OSError:
            messagebox.showinfo(TITRE, f"Ouvrir le fichier à la main :\n{FICHIER_VOCABULAIRE}", parent=self.root)

    def _onglet_aide(self, nb):
        tk = self.tk
        from tkinter import font as tkfont
        f = self._onglet(nb, "Aide")
        t = tk.Text(f, wrap="word", padx=12, pady=10, background="white", relief="flat", font=tkfont.nametofont("TkDefaultFont"))
        t.insert("1.0", TEXTE_AIDE)
        t.configure(state="disabled")
        t.pack(fill="both", expand=True, padx=8, pady=8)

    # -- mise à jour des listes de colonnes -----------------------------------------------
    def _maj_choix(self):
        self._maj_champs_decl()
        d = self.etude.donnees
        num = d.noms_num if d else []
        tout = d.noms if d else []
        txt = d.noms_txt if d else []
        self.cb_gcol["values"] = [AUCUN_GROUPE] + tout
        self._changer_groupe(garder=True)
        self.cb_cordre["values"] = [AUCUN_ORDRE] + (d.noms_date if d else []) + num
        self.cb_cphase["values"] = [AUCUN_GROUPE] + tout
        if self.v_cphase.get() not in self.cb_cphase["values"]:
            self.v_cphase.set(AUCUN_GROUPE)
        self.v_cordre.set(d.noms_date[0] if d and d.noms_date else AUCUN_ORDRE)
        self.lb_desc.delete(0, "end")
        self.lb_norm.delete(0, "end")
        self.lb_anova.delete(0, "end")
        for n in num:
            self.lb_desc.insert("end", n)
            self.lb_norm.insert("end", n)
            self.lb_anova.insert("end", n)
        self.lb_desc.select_set(0, "end")
        self.lb_norm.select_set(0, "end")
        self.lb_anova.select_set(0, "end")
        for cb, vals, var in [(self.cb_tx, num, self.v_tx), (self.cb_cap, num, self.v_cs),
                              (self.cb_aval, num, self.v_aval), (self.cb_agrp, tout, self.v_agrp),
                              (self.cb_ka, tout, self.v_ka), (self.cb_kb, tout, self.v_kb), (self.cb_kref, tout, self.v_kref)]:
            cb["values"] = vals
            var.set(vals[0] if vals else "")
        self.cb_ty["values"] = ["— un échantillon —"] + num
        self.v_ty.set("— un échantillon —")
        if len(num) >= 2:
            self.v_ty.set(num[1])
        cand = txt if len(txt) >= 2 else tout          # Kappa : colonnes texte en priorité
        if len(cand) >= 2:
            self.v_ka.set(cand[0])
            self.v_kb.set(cand[1])
        for cb, vals, var in [(self.cb_fx, num, self.v_fx), (self.cb_fy, num, self.v_fy), (self.cb_xa, tout, self.v_xa),
                              (self.cb_xb, tout, self.v_xb), (self.cb_xvar, tout, self.v_xvar)]:
            cb["values"] = vals
            var.set(vals[0] if vals else "")
        if len(num) >= 2:
            self.v_fy.set(num[1])
        if len(tout) >= 2:
            self.v_xb.set(tout[1])
        self.cb_xlab["values"] = ["— aucune —"] + tout
        self.v_xlab.set(txt[0] if txt else "— aucune —")
        self.lb_xcols.configure(state="normal")          # une liste désactivée ignore les modifications
        self.lb_xcols.delete(0, "end")
        for n in num:
            self.lb_xcols.insert("end", n)
        self.lb_xcols.select_set(0, "end")
        self._maj_khi2()
        self.lb_ccols.delete(0, "end")
        for n in num:
            self.lb_ccols.insert("end", n)
        self.lb_ccols.select_set(0, "end")
        self.cb_cval["values"] = num
        self.cb_cgrp["values"] = tout
        self.v_cval.set(num[0] if num else "")
        self.v_cgrp.set(txt[0] if txt else (tout[1] if len(tout) > 1 else ""))
        self.v_cfmt.set("long" if (txt and num) else "colonnes")
        def trouver_rr(mots, parmi, exclus=()):
            return next((n for n in parmi if n not in exclus and any(m in n.lower() for m in mots)), "")
        rc_p = trouver_rr(MOTS_ECHANTILLON, tout)
        rc_o = trouver_rr(MOTS_EVALUATEUR, tout, (rc_p,))
        rc_m = trouver_rr(("mesure", "valeur", "measure", "reponse", "réponse", "resultat", "résultat"), num, (rc_p, rc_o)) \
            or next((n for n in reversed(num) if n not in (rc_p, rc_o) and not any(m in n.lower() for m in MOTS_ESSAI)), "")
        for cb in (self.cb_rcp, self.cb_rco):
            cb["values"] = tout
        self.cb_rcm["values"] = num
        self.cb_r1col["values"] = num
        self.v_rcp.set(rc_p or (tout[0] if tout else ""))
        self.v_rco.set(rc_o or (tout[1] if len(tout) > 1 else ""))
        self.v_rcm.set(rc_m or (num[-1] if num else ""))
        self.v_r1col.set(num[0] if num else "")
        self.v_rrmode.set("croisee" if (rc_p and rc_o) else "type1")
        self.lb_kev.delete(0, "end")
        for n in tout:
            self.lb_kev.insert("end", n)
        def trouver(mots, exclus=()):
            return next((n for n in tout if n not in exclus and any(m in n.lower() for m in mots)), "")
        refs = [n for n in tout if any(m in n.lower() for m in MOTS_REFERENCE)]
        long_ref = refs[0] if refs else ""
        long_ev = trouver(MOTS_EVALUATEUR, (long_ref,))
        long_rep = trouver(MOTS_REPONSE, (long_ref, long_ev))
        long_ech = trouver(MOTS_ECHANTILLON, (long_ref, long_ev, long_rep))
        long_ess = trouver(MOTS_ESSAI, (long_ref, long_ev, long_rep, long_ech))
        for cb in (self.cb_kresp, self.cb_kevl, self.cb_kech, self.cb_kreflong):
            cb["values"] = tout
        self.cb_kess["values"] = ["— aucun —"] + tout
        self.v_kresp.set(long_rep or (tout[0] if tout else ""))
        self.v_kev.set(long_ev or (tout[1] if len(tout) > 1 else ""))
        self.v_kech.set(long_ech or (tout[2] if len(tout) > 2 else ""))
        self.v_kess.set(long_ess or "— aucun —")
        self.v_kreflong.set(long_ref or (tout[-1] if tout else ""))
        format_long = bool(long_ref and long_ev and long_rep and long_ech)
        if format_long:
            ref, evs = long_ref, []
            self.v_kmode.set("long")
        elif refs:                                       # colonne de référence (norme) reconnue par son nom : mode référence
            ref, evs = refs[0], [n for n in cand if n != refs[0]]
            self.v_kmode.set("ref")
        else:
            ref, evs = (cand[0] if cand else ""), cand[1:]
            self.v_kmode.set("deux")
        self.v_kref.set(ref)
        for i, n in enumerate(tout):
            if n in evs:
                self.lb_kev.select_set(i)
        if txt and num:                                # colonne de groupes textuelle : format long par défaut
            self.v_amode.set("long")
            self.v_agrp.set(txt[0])
        else:
            self.v_amode.set("large")
            if len(num) >= 2:
                self.v_agrp.set(num[1])


class EcranAccueil:
    """Fenêtre d'accueil sans bordure avec l'icône du projet."""

    def __init__(self, root, duree_ms, suite):
        import tkinter as tk
        self.win = tk.Toplevel(root)
        self.win.overrideredirect(True)
        try:
            self.win.attributes("-topmost", True)
        except Exception:
            pass
        cadre = tk.Frame(self.win, bg="white", highlightbackground="#1f4e8c", highlightthickness=2)
        cadre.pack(fill="both", expand=True)
        self.photo = None
        if ICONE.is_file():
            try:
                self.photo = charger_image(ICONE, 340)
                tk.Label(cadre, image=self.photo, bg="white").pack(padx=30, pady=(24, 8))
            except Exception:
                self.photo = None
        tk.Label(cadre, text=TITRE, bg="white", fg="#1f4e8c", font=("TkDefaultFont", 20, "bold")).pack(padx=30, pady=(8 if self.photo else 30, 2))
        tk.Label(cadre, text="JFBConseils — Statistiques", bg="white", fg="#555555").pack()
        tk.Label(cadre, text="Chargement…", bg="white", fg="#888888").pack(pady=(10, 22))
        self.win.update_idletasks()
        w, h = self.win.winfo_reqwidth(), self.win.winfo_reqheight()
        x = (self.win.winfo_screenwidth() - w) // 2
        y = (self.win.winfo_screenheight() - h) // 2
        self.win.geometry(f"{w}x{h}+{x}+{y}")
        self.win.update()
        root.after(duree_ms, lambda: (self.win.destroy(), suite()))


def main():
    try:
        import tkinter as tk
    except ImportError:
        sys.exit("Tkinter est absent : sudo apt install -y python3-tk")
    import tkinter.font  # noqa: F401  (rend tk.font disponible)
    root = tk.Tk()
    root.withdraw()
    if ICONE.is_file():
        try:
            root.iconphoto(True, charger_image(ICONE, 128))
        except Exception:
            pass

    def ouvrir():
        Application(root)
        root.deiconify()
    EcranAccueil(root, 1800, ouvrir)
    root.mainloop()


if __name__ == "__main__":
    main()
