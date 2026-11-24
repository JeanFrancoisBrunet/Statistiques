@echo off
rem Construit dist\Statistiques (programme sans installation) et copie les fichiers de donnees a cote de l'exe.
rem A lancer depuis le dossier du projet. PY = version de Python utilisee pour la construction (3.12 ou 3.13 conseille).
set PY=py -3.13
%PY% -m pip install pillow reportlab openpyxl pyinstaller || goto :fin
%PY% -m PyInstaller --noconfirm statistiques.spec || goto :fin
set D=dist\Statistiques
copy /Y *.R %D%\ >nul
if exist vocabulaire.txt copy /Y vocabulaire.txt %D%\ >nul
xcopy /E /I /Y icons %D%\icons >nul
if exist illustrations xcopy /E /I /Y illustrations %D%\illustrations >nul
if exist fonts xcopy /E /I /Y fonts %D%\fonts >nul
if exist R-portable xcopy /E /I /Y R-portable %D%\R-portable >nul
echo.
echo Termine : %D%\Statistiques.exe
:fin
pause
