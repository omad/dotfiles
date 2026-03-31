#!/usr/bin/env bash
set -euo pipefail

INPUT="${1:-/dev/stdin}"

# temp file to avoid recomputing cat input repeatedly
TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT
cat "$INPUT" > "$TMP"

# shellcheck disable=SC2016
cmd_preview='
kind={1};
name={2};
ns={3};

# echo $kind $name $ns

if [ "$ns" = "null" ] || [ -z "$ns" ] || [ "$ns" == "NA" ]; then
  filt=".kind == \"$kind\" and .metadata.name == \"$name\""
else
  filt=".kind == \"$kind\" and .metadata.name == \"$name\" and .metadata.namespace == \"$ns\""
fi
# echo $filt
# echo  yq e --colors "select($filt)" '"$TMP"'

yq e --colors "select($filt)" '"$TMP"'
'

# Preprocess the YAML file we've captured to handle listed items results from the K8s API
# like you get from `kubectl get pods -A` that have an `items:` key with a list of API Objects.
if head "${TMP}" | grep -q '^items:$'; then
  # Split in place the list of objects under items into separate YAML documents
  yq -i '.items.[] | split_doc' "${TMP}"
fi


#  | (bat -l yaml --color=always 2>/dev/null || cat)
 # | rich --csv - \
       # --delimiter ' | ' \
       # --with-nth=1,2,3 \

# tv, tidy-viewer is prettier, but, totally unnecessary
 # | tv --no-dimensions --no-row-numbering --force-all-rows --color-always | sed '1,2d;$d' \
SELECTED=$(yq   '[{"kind": .kind, "name": .metadata.name, "Namespace": .metadata.namespace}]'  "${TMP}" | yq -o csv \
  | column -ts, \
 | fzf --ansi \
       --reverse \
       --info inline \
       --header-lines 1 \
       --with-shell="bash -c" \
       --preview "$cmd_preview")
       # --preview-window up:border-down \
       # --preview-window down,90% \

[ -z "$SELECTED" ] && exit 0

# Reference
# fzf --tmux 80%,100%,border-native --ansi \
# --info inline --reverse --header-lines 4 \
# --preview 'GH_FORCE_TTY=$FZF_PREVIEW_COLUMNS gh issue view --comments {1}' \
# --preview-window up:border-down \
# --with-shell 'bash -c' \
# --bind 'start:preview(echo Loading issues requests ...)+reload:GH_FORCE_TTY=95% gh issue list --limit=1000' \
# --bind 'load:transform:(( FZF_TOTAL_COUNT )) || echo become:echo No pull issues' \
# --bind 'ctrl-o:execute-silent:gh issue view --web {1}' \
# --bind 'ctrl-v:execute:gh issue view {1} | sed "s/\r//g" | view - +"setf markdown"' \
# --bind 'enter:become:echo gh issue checkout {1}' \
# --footer 'Press Enter to checkout / CTRL-O to open in browser / CTRL-V to open in editor'

# When selected, print the manifest (same code as in preview)
kind=$(echo "$SELECTED" | cut -d',' -f1)
name=$(echo "$SELECTED" | cut -d',' -f2)
ns=$(echo "$SELECTED" | cut -d',' -f3)

if [ "$ns" = "null" ] || [ -z "$ns" ]; then
    FILTER=".kind == \"$kind\" and .metadata.name == \"$name\""
else
    FILTER=".kind == \"$kind\" and .metadata.name == \"$name\" and .metadata.namespace == \"$ns\""
fi

yq e "select($FILTER)" "$TMP"
