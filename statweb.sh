#!/bin/bash
# Démarre Shiny s'il n'est pas déjà lancé, attend qu'il écoute, puis ouvre le navigateur.
cd /home/jfbrunet/Projects/Statistiques || exit 1
if ! pgrep -f "shiny::runApp" >/dev/null; then
    setsid nohup R -q -e "shiny::runApp('.', host='0.0.0.0', port=3838)" > shiny.log 2>&1 &
    for i in $(seq 1 40); do
        ss -ltn | grep -q ':3838 ' && break
        sleep 1
    done
fi
chromium http://localhost:3838 >/dev/null 2>&1 &
