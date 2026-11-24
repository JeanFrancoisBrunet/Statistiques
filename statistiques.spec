# -*- mode: python ; coding: utf-8 -*-
# Construction :  pyinstaller --noconfirm statistiques.spec
# Résultat    :  dist\Statistiques\Statistiques.exe  (dossier à copier tel quel)
# Après construction, copier à côté de l'exe (voir LISEZMOI_WINDOWS.md) :
#   R-portable\, fonts\, icons\, illustrations\, *.R, vocabulaire.txt
a = Analysis(
    ["statistiques.py"],
    pathex=["."],
    binaries=[],
    datas=[],
    hiddenimports=["rapport_statistique", "reportlab", "reportlab.pdfbase.ttfonts", "reportlab.lib.styles",
                   "PIL.ImageTk", "PIL._tkinter_finder", "openpyxl"],
    excludes=["requests"],
)
pyz = PYZ(a.pure)
exe = EXE(pyz, a.scripts, [], exclude_binaries=True, name="Statistiques", console=False, icon=None)
coll = COLLECT(exe, a.binaries, a.datas, name="Statistiques")
