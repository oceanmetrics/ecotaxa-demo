#!/usr/bin/env bash
# launch_project.sh: from the LAPTOP, get a login token on the node host and start render_project.sh
# on the mini in tmux window om:prj<id>. The token goes msens -> laptop pipe -> a mode-600 file on the
# mini that the render deletes on start; it is never printed or committed.
#
#   scripts/launch_project.sh <project_id> [first_notebook]
#   then: ssh macmini 'tail -f ~/Github/oceanmetrics/ecotaxa-demo/data/run_project-<id>.log'
set -euo pipefail
PID=${1:?project id}; FROM=${2:-01}
HERE=$(cd "$(dirname "$0")" && pwd)
ssh macmini "cd ~/Github/oceanmetrics/ecotaxa-demo && git pull -q --ff-only && git log --oneline -1"
ssh msens 'python3 -I - token' < "$HERE/node/ecotaxa_admin.py" |
  ssh macmini "umask 077; mkdir -p ~/.cache; cat > ~/.cache/ecotaxa_token_prj$PID;
    tmux has-session -t om 2>/dev/null || tmux new-session -d -s om;
    tmux kill-window -t om:prj$PID 2>/dev/null || true;
    tmux new-window -t om -n prj$PID ~/Github/oceanmetrics/ecotaxa-demo/scripts/render_project.sh $PID $FROM"
echo "started om:prj$PID from notebook $FROM"
