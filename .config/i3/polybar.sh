#!/usr/bin/env bash
set -euo pipefail
set -x
# Thanks https://www.reddit.com/r/i3wm/comments/6lo0z0/how_to_use_polybar/djvp7ok?utm_source=share&utm_medium=web2x&context=3

# Terminate already running bar instances
#killall -q polybar

# Wait until the processes have been shut down
#while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch polybar
if [[ $(polybar --list-monitors | wc -l) == 1 ]]; then
    MAINMONITOR=eDP-1
else
    MAINMONITOR=DVI-I-3-2
fi
for m in $(polybar --list-monitors | cut -d":" -f1); do
    if [[ $m == "${MAINMONITOR}" ]]; then
        POMO=polypomo TRAY_POS=right MONITOR=$m polybar --reload example &
    else
        MONITOR=$m polybar --reload example &
    fi
done
