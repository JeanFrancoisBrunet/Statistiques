#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# =============================================================================
#  rapport_statistique.py — rapport de synthèse PDF de Statistiques
#  Utilisé par statistiques.py (onglet « Rapport »).
#  Contenu du rapport : titre, auteur, date ; conclusions saisies ; synthèse des analyses ;
#  description des données ; détail de chaque analyse (résultats, tableaux, graphiques, remarques).
#
#  Dépendance : reportlab  (sudo apt install -y python3-reportlab)
#  Polices    : DejaVu (paquet fonts-dejavu-core, présent par défaut sur Raspberry Pi OS) ;
#               à défaut, polices standard du PDF (quelques symboles sont alors remplacés).
#  Ce module ne dépend pas de statistiques.py : il reçoit des données déjà mises en forme.
#  Auteur : Jean-François BRUNET – JFBConseils – Novembre 2026
# =============================================================================

import re
from pathlib import Path
from xml.sax.saxutils import escape

_DOSSIERS_POLICES = ("/usr/share/fonts/truetype/dejavu", "/usr/share/fonts/dejavu",
                     "/usr/share/fonts/TTF", "/usr/local/share/fonts")
_REMPLACEMENTS = {"σ": "sigma", "−": "-", "∞": "inf", "≤": "<=", "≥": ">=", "→": "->", "…": "...", "☑": "[x]", "☐": "[ ]",
                  " ": " ", " ": " "}
_COULEURS_VERDICT = {"Satisfaisant": "#1b7a43", "À améliorer": "#a86400", "Insuffisant": "#b03030"}
_police_cache = {}


def verifier():
    """(ok, message) : reportlab est-il disponible ?"""
    try:
        import reportlab  # noqa: F401
    except ImportError:
        return False, "Le paquet Python « reportlab » est absent. Installer : sudo apt install -y python3-reportlab"
    return True, "reportlab disponible."


def _chercher(nom):
    for d in _DOSSIERS_POLICES:
        p = Path(d) / nom
        if p.is_file():
            return str(p)
    return None


def _polices():
    """(normale, grasse, italique, unicode) : noms de polices PDF. DejaVu si présente, sinon polices standard."""
    if "p" in _police_cache:
        return _police_cache["p"]
    from reportlab.pdfbase import pdfmetrics
    from reportlab.pdfbase.ttfonts import TTFont
    normal, gras = _chercher("DejaVuSans.ttf"), _chercher("DejaVuSans-Bold.ttf")
    if normal:
        ital = _chercher("DejaVuSans-Oblique.ttf") or normal
        pdfmetrics.registerFont(TTFont("RapSans", normal))
        pdfmetrics.registerFont(TTFont("RapSansB", gras or normal))
        pdfmetrics.registerFont(TTFont("RapSansI", ital))
        pdfmetrics.registerFontFamily("RapSans", normal="RapSans", bold="RapSansB", italic="RapSansI", boldItalic="RapSansB")
        res = ("RapSans", "RapSansB", "RapSansI", True)
    else:
        res = ("Helvetica", "Helvetica-Bold", "Helvetica-Oblique", False)
    _police_cache["p"] = res
    return res


def _prep(texte):
    """Texte prêt à être écrit dans le PDF (remplace les symboles absents des polices standard)."""
    t = "" if texte is None else str(texte)
    if _polices()[3]:
        return t
    for a, b in _REMPLACEMENTS.items():
        t = t.replace(a, b)
    return t.encode("cp1252", "replace").decode("cp1252")


def _x(texte):
    """Texte pour un Paragraph : préparé puis échappé (le balisage XML de reportlab n'est pas utilisé)."""
    return escape(_prep(texte))


def _styles():
    from reportlab.lib import colors
    from reportlab.lib.styles import ParagraphStyle
    n, g, i, _u = _polices()
    bleu = colors.HexColor("#1f4e8c")
    return {
        "titre": ParagraphStyle("titre", fontName=g, fontSize=20, leading=24, textColor=bleu, spaceAfter=2),
        "meta": ParagraphStyle("meta", fontName=n, fontSize=9, leading=12, textColor=colors.HexColor("#555555")),
        "h1": ParagraphStyle("h1", fontName=g, fontSize=13, leading=16, textColor=bleu, spaceBefore=14, spaceAfter=6, keepWithNext=1),
        "h2": ParagraphStyle("h2", fontName=g, fontSize=11, leading=14, textColor=bleu, spaceBefore=12, spaceAfter=2, keepWithNext=1),
        "corps": ParagraphStyle("corps", fontName=n, fontSize=9.5, leading=13.5, spaceAfter=3),
        "ligne": ParagraphStyle("ligne", fontName=n, fontSize=9, leading=12),
        "conclusion": ParagraphStyle("conclusion", fontName=n, fontSize=9, leading=12, textColor=colors.HexColor("#222222"),
                                     backColor=colors.HexColor("#eef3fa"), borderPadding=(4, 5, 4, 5), spaceBefore=4, spaceAfter=6),
        "remarque": ParagraphStyle("remarque", fontName=i, fontSize=8.5, leading=11, textColor=colors.HexColor("#8a5a00")),
        "nom_table": ParagraphStyle("nom_table", fontName=g, fontSize=9, leading=12, spaceBefore=6, spaceAfter=2),
        "petit": ParagraphStyle("petit", fontName=n, fontSize=8, leading=10, textColor=colors.HexColor("#666666")),
    }


_RE_NOMBRE = re.compile(r"^[<>]?\s*[-+−]?\d+([.,]\d+)?(e[-+]?\d+)?\s*%?$", re.IGNORECASE)
_NEUTRES = {"", "—", "non calculable"}


def _colonne_numerique(cellules, j):
    vals = [r[j].strip() for r in cellules[1:] if r[j].strip() not in _NEUTRES]
    return bool(vals) and all(_RE_NOMBRE.match(v) for v in vals)


def _tableau(entetes, lignes, largeur):
    """Tableau à la largeur disponible : taille de police réduite si besoin, puis retour à la ligne des cellules.
    Les colonnes numériques sont alignées à droite, les colonnes de texte à gauche."""
    from reportlab.lib import colors
    from reportlab.pdfbase.pdfmetrics import stringWidth
    from reportlab.platypus import Paragraph, Table, TableStyle
    from reportlab.lib.styles import ParagraphStyle
    n, g, _i, _u = _polices()
    nc = len(entetes)
    cellules = [[_prep(e) for e in entetes]] + [[_prep(c) for c in (list(l) + [""] * nc)[:nc]] for l in lignes]
    taille, larg = None, None
    for t in (8.5, 8, 7.5, 7, 6.5, 6, 5.5):
        w = [max(stringWidth(r[j], g if k == 0 else n, t) for k, r in enumerate(cellules)) + 10 for j in range(nc)]
        if sum(w) <= largeur:
            taille, larg = t, w
            break
    if taille is None:                              # trop large : cellules qui passent à la ligne
        taille = 6.5
        poids = [min(max(stringWidth(r[j], g if k == 0 else n, taille) for k, r in enumerate(cellules)) + 10, 160) for j in range(nc)]
        larg = [p * largeur / sum(poids) for p in poids]
        ps = ParagraphStyle("c", fontName=n, fontSize=taille, leading=taille + 2)
        psg = ParagraphStyle("cg", fontName=g, fontSize=taille, leading=taille + 2)
        donnees = [[Paragraph(escape(c), psg if k == 0 else ps) for c in r] for k, r in enumerate(cellules)]
    else:
        donnees = cellules
    style = [("FONTNAME", (0, 0), (-1, 0), g), ("FONTNAME", (0, 1), (-1, -1), n), ("FONTSIZE", (0, 0), (-1, -1), taille),
             ("BACKGROUND", (0, 0), (-1, 0), colors.HexColor("#dce6f4")), ("GRID", (0, 0), (-1, -1), 0.25, colors.HexColor("#b8c4d6")),
             ("ROWBACKGROUNDS", (0, 1), (-1, -1), [colors.white, colors.HexColor("#f5f8fc")]),
             ("VALIGN", (0, 0), (-1, -1), "MIDDLE"), ("ALIGN", (0, 0), (-1, -1), "RIGHT"),
             ("TOPPADDING", (0, 0), (-1, -1), 2), ("BOTTOMPADDING", (0, 0), (-1, -1), 2),
             ("LEFTPADDING", (0, 0), (-1, -1), 4), ("RIGHTPADDING", (0, 0), (-1, -1), 4)]
    for j in range(nc):
        if j == 0 or not _colonne_numerique(cellules, j):
            style.append(("ALIGN", (j, 0), (j, -1), "LEFT"))
    for i, r in enumerate(cellules[1:], 1):         # verdicts colorés (Kappa avec référence)
        for j, c in enumerate(r):
            if c in _COULEURS_VERDICT:
                style.append(("TEXTCOLOR", (j, i), (j, i), colors.HexColor(_COULEURS_VERDICT[c])))
    t = Table(donnees, colWidths=larg, repeatRows=1, hAlign="LEFT")
    t.setStyle(TableStyle(style))
    return t


def _image(chemin, largeur_max, hauteur_max):
    from reportlab.lib.utils import ImageReader
    from reportlab.platypus import Image
    l, h = ImageReader(str(chemin)).getSize()
    k = min(largeur_max / l, hauteur_max / h)
    return Image(str(chemin), width=l * k, height=h * k)


def _paragraphes(texte, style):
    """Un Paragraph par ligne non vide d'un texte libre."""
    from reportlab.platypus import Paragraph
    return [Paragraph(_x(l), style) for l in str(texte).splitlines() if l.strip()]


def generer_pdf(chemin, meta, donnees, analyses, logo=None):
    """Écrit le rapport PDF.

    meta     : dict titre, auteur, date, commentaires, pied
    donnees  : dict source, nb_lignes, colonnes [(nom, type, valides, manquantes)], avertissements — ou None
    analyses : liste de dict numero, onglet, heure, titre, conclusion, lignes, tables [(nom, entetes, lignes)],
               figures [chemins PNG], remarques
    """
    ok, msg = verifier()
    if not ok:
        raise RuntimeError(msg)
    from reportlab.lib import colors
    from reportlab.lib.pagesizes import A4
    from reportlab.lib.units import cm
    from reportlab.platypus import (CondPageBreak, HRFlowable, KeepTogether, Paragraph, SimpleDocTemplate, Spacer, Table,
                                    TableStyle)

    n, g, _i, _u = _polices()
    st = _styles()
    marge = 2 * cm
    largeur = A4[0] - 2 * marge
    titre = (meta.get("titre") or "Rapport d'analyse statistique").strip()
    pied = meta.get("pied") or ""

    def decor(canvas, doc):
        canvas.saveState()
        canvas.setFont(n, 8)
        canvas.setFillColor(colors.HexColor("#777777"))
        canvas.drawString(marge, 1.05 * cm, _prep(pied))
        canvas.drawRightString(A4[0] - marge, 1.05 * cm, _prep(f"Page {doc.page}"))
        canvas.setStrokeColor(colors.HexColor("#c8d2e0"))
        canvas.line(marge, 1.5 * cm, A4[0] - marge, 1.5 * cm)
        canvas.restoreState()

    doc = SimpleDocTemplate(str(chemin), pagesize=A4, leftMargin=marge, rightMargin=marge, topMargin=1.8 * cm, bottomMargin=2 * cm,
                            title=_prep(titre), author=_prep(meta.get("auteur") or ""), subject="Rapport de synthèse")
    el = []

    # -- en-tête ---------------------------------------------------------------------------
    bloc = [Paragraph(_x(titre), st["titre"])]
    ligne_meta = " — ".join(x for x in (meta.get("auteur") or "", meta.get("date") or "") if x)
    if ligne_meta:
        bloc.append(Paragraph(_x(ligne_meta), st["meta"]))
    if logo and Path(logo).is_file():
        try:
            entete = Table([[bloc, _image(logo, 2.3 * cm, 2.3 * cm)]], colWidths=[largeur - 2.7 * cm, 2.7 * cm])
            entete.setStyle(TableStyle([("VALIGN", (0, 0), (-1, -1), "TOP"), ("ALIGN", (1, 0), (1, 0), "RIGHT"),
                                        ("LEFTPADDING", (0, 0), (-1, -1), 0), ("RIGHTPADDING", (0, 0), (-1, -1), 0)]))
            el.append(entete)
        except Exception:
            el.extend(bloc)
    else:
        el.extend(bloc)
    el.append(HRFlowable(width="100%", thickness=1.2, color=colors.HexColor("#1f4e8c"), spaceBefore=6, spaceAfter=4))

    # -- conclusions saisies -----------------------------------------------------------------
    commentaires = (meta.get("commentaires") or "").strip()
    if commentaires:
        el.append(Paragraph("Conclusions et commentaires", st["h1"]))
        el.extend(_paragraphes(commentaires, st["corps"]))

    # -- synthèse ---------------------------------------------------------------------------
    if analyses:
        el.append(Paragraph("Synthèse des analyses", st["h1"]))
        cell = st["ligne"]
        rows = [[Paragraph("<b>N°</b>", cell), Paragraph("<b>Analyse</b>", cell), Paragraph("<b>Conclusion</b>", cell)]]
        for a in analyses:
            rows.append([Paragraph(str(a["numero"]), cell), Paragraph(_x(a["titre"]), cell),
                         Paragraph(_x(a.get("conclusion") or "—"), cell)])
        t = Table(rows, colWidths=[1.0 * cm, 5.8 * cm, largeur - 6.8 * cm], repeatRows=1, hAlign="LEFT")
        t.setStyle(TableStyle([("BACKGROUND", (0, 0), (-1, 0), colors.HexColor("#dce6f4")),
                               ("GRID", (0, 0), (-1, -1), 0.25, colors.HexColor("#b8c4d6")), ("VALIGN", (0, 0), (-1, -1), "TOP"),
                               ("ROWBACKGROUNDS", (0, 1), (-1, -1), [colors.white, colors.HexColor("#f5f8fc")]),
                               ("TOPPADDING", (0, 0), (-1, -1), 3), ("BOTTOMPADDING", (0, 0), (-1, -1), 3)]))
        el.append(t)

    # -- données ----------------------------------------------------------------------------
    if donnees:
        el.append(Paragraph("Données analysées", st["h1"]))
        el.append(Paragraph(_x(f"Source : {donnees.get('source', '')} — {donnees.get('nb_lignes', 0)} ligne(s), "
                               f"{len(donnees.get('colonnes', []))} colonne(s)."), st["corps"]))
        cols = donnees.get("colonnes") or []
        if cols:
            el.append(Spacer(1, 4))
            el.append(_tableau(["Variable", "Type", "Valeurs valides", "Valeurs manquantes"],
                               [[c[0], c[1], str(c[2]), str(c[3])] for c in cols], largeur))
        for a in donnees.get("avertissements") or []:
            el.append(Paragraph(_x("Remarque : " + a), st["remarque"]))

    # -- détail des analyses -----------------------------------------------------------------
    for k, a in enumerate(analyses):
        el.append(CondPageBreak(8 * cm))                 # pas de titre isolé en bas de page
        if k == 0:
            el.append(Paragraph("Détail des analyses", st["h1"]))
        el.append(Paragraph(_x(f"{a['numero']}. {a['titre']}"), st["h2"]))
        el.append(Paragraph(_x(f"Onglet « {a['onglet']} » — calculé à {a['heure']}"), st["petit"]))
        if a.get("conclusion"):
            el.append(Paragraph("<b>Conclusion :</b> " + _x(a["conclusion"]), st["conclusion"]))
        for l in a.get("lignes") or []:
            el.append(Paragraph(_x(l) if l.strip() else "&nbsp;", st["ligne"]))
        for nom, entetes, lignes in a.get("tables") or []:
            el.append(Paragraph(_x(nom), st["nom_table"]))
            el.append(_tableau(entetes, lignes, largeur))
        for f in a.get("figures") or []:
            try:
                el.append(Spacer(1, 6))
                el.append(KeepTogether([_image(f, largeur, 12 * cm)]))
            except Exception:
                el.append(Paragraph(_x("(graphique non disponible)"), st["remarque"]))
        if a.get("remarques"):
            el.append(Spacer(1, 4))
            for r in a["remarques"]:
                el.append(Paragraph(_x("Remarque : " + r), st["remarque"]))

    if not (commentaires or analyses or donnees):
        el.append(Paragraph("Rapport vide.", st["corps"]))
    doc.build(el, onFirstPage=decor, onLaterPages=decor)
    return str(chemin)
