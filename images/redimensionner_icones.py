#!/usr/bin/env python3
"""
Redimensionne des images en icônes carrées PNG à fond opaque (sans transparence).

Étapes pour chaque image :
  1. les zones transparentes éventuelles sont remplies avec la couleur de fond ;
  2. (option) l'image est rognée au plus près du motif ;
  3. le motif est ajusté dans un carré, centré, sans déformation, avec une marge réglable ;
     les espaces restants sont remplis avec la couleur de fond.

Les originaux ne sont jamais modifiés.

Couleur de fond :
  - par défaut ("auto") : couleur du coin haut-gauche de l'image, ou blanc si l'image
    contient de la transparence ;
  - sinon, toute couleur reconnue par Pillow : nom (white, black...) ou code #RRGGBB.

Utilisation :
    python3 redimensionner_icones.py image1.png image2.png
    python3 redimensionner_icones.py dossier_d_images
    python3 redimensionner_icones.py dossier_d_images -t 640 -m 4 -o dossier_de_sortie
    python3 redimensionner_icones.py image.png --fond "#ffffff" --rogner

Dépendance : Pillow  (sudo apt install python3-pil  ou  pip install pillow)
"""

import argparse
import sys
from pathlib import Path

try:
    from PIL import Image, ImageChops, ImageColor
except ImportError:
    sys.exit("Pillow est requis : sudo apt install python3-pil  (ou pip install pillow)")

EXTENSIONS = {".png", ".jpg", ".jpeg", ".bmp", ".gif", ".webp"}


def collecter(entrees):
    """Retourne la liste des fichiers image à traiter."""
    fichiers = []
    for entree in entrees:
        chemin = Path(entree).expanduser()
        if chemin.is_dir():
            fichiers += sorted(
                f for f in chemin.iterdir()
                if f.is_file() and f.suffix.lower() in EXTENSIONS
            )
        elif chemin.is_file():
            fichiers.append(chemin)
        else:
            print(f"Ignoré (introuvable) : {chemin}")
    return fichiers


def a_du_transparent(img):
    """Indique si l'image (RGBA) contient des pixels transparents."""
    return img.getchannel("A").getextrema()[0] < 250


def choisir_fond(img, choix, transparent):
    """Détermine la couleur de fond (R, G, B)."""
    if choix.lower() != "auto":
        return ImageColor.getrgb(choix)[:3]
    if transparent:
        return (255, 255, 255)
    return img.getpixel((0, 0))[:3]


def aplatir(img, fond):
    """Remplit les zones transparentes avec la couleur de fond et renvoie une image RGB."""
    base = Image.new("RGBA", img.size, fond + (255,))
    base.alpha_composite(img)
    return base.convert("RGB")


def rogner(img, fond):
    """Rogne l'image au plus près des pixels différents de la couleur de fond."""
    uni = Image.new("RGB", img.size, fond)
    ecart = ImageChops.difference(img, uni).convert("L")
    boite = ecart.point(lambda v: 255 if v > 10 else 0).getbbox()
    return img.crop(boite) if boite else img


def creer_icone(source, destination, taille, marge_pct, choix_fond, avec_rognage):
    """Crée une icône carrée opaque à partir d'une image."""
    with Image.open(source) as original:
        img = original.convert("RGBA")

    fond = choisir_fond(img, choix_fond, a_du_transparent(img))
    img = aplatir(img, fond)

    if avec_rognage:
        img = rogner(img, fond)

    marge = round(taille * marge_pct / 100)
    zone = max(1, taille - 2 * marge)

    # Ajustement proportionnel dans la zone utile
    ratio = min(zone / img.width, zone / img.height)
    nouvelle_taille = (
        max(1, round(img.width * ratio)),
        max(1, round(img.height * ratio)),
    )
    img = img.resize(nouvelle_taille, Image.LANCZOS)

    # Canevas opaque de la couleur de fond, image centrée
    canevas = Image.new("RGB", (taille, taille), fond)
    x = (taille - img.width) // 2
    y = (taille - img.height) // 2
    canevas.paste(img, (x, y))

    destination.parent.mkdir(parents=True, exist_ok=True)
    canevas.save(destination, "PNG")


def main():
    parser = argparse.ArgumentParser(
        description="Redimensionne des images en icônes carrées PNG à fond opaque."
    )
    parser.add_argument("entrees", nargs="+",
                        help="fichiers image et/ou dossiers à traiter")
    parser.add_argument("-t", "--taille", type=int, default=640,
                        help="côté de l'icône en pixels (défaut : 640)")
    parser.add_argument("-m", "--marge", type=float, default=0,
                        help="marge en %% de chaque côté (défaut : 0)")
    parser.add_argument("-o", "--sortie", default="icones",
                        help="dossier de sortie (défaut : ./icones)")
    parser.add_argument("--fond", default="auto",
                        help="couleur de fond : auto, nom de couleur ou #RRGGBB (défaut : auto)")
    parser.add_argument("--rogner", action="store_true",
                        help="rogner autour du motif avant l'ajustement")
    args = parser.parse_args()

    if args.taille < 16:
        sys.exit("La taille doit être d'au moins 16 pixels.")
    if not 0 <= args.marge < 40:
        sys.exit("La marge doit être comprise entre 0 et 40 %.")
    if args.fond.lower() != "auto":
        try:
            ImageColor.getrgb(args.fond)
        except ValueError:
            sys.exit(f"Couleur de fond non reconnue : {args.fond}")

    fichiers = collecter(args.entrees)
    if not fichiers:
        sys.exit("Aucune image à traiter.")

    dossier_sortie = Path(args.sortie).expanduser()
    ok = 0
    for source in fichiers:
        destination = dossier_sortie / f"{source.stem}_{args.taille}.png"
        try:
            creer_icone(source, destination, args.taille, args.marge,
                        args.fond, args.rogner)
            print(f"OK  {source.name} -> {destination}")
            ok += 1
        except Exception as erreur:
            print(f"ERREUR  {source.name} : {erreur}")

    print(f"\n{ok}/{len(fichiers)} icône(s) créée(s) dans {dossier_sortie}")


if __name__ == "__main__":
    main()
