#!/bin/zsh -l
# render_project.sh: render notebooks 01-04 for one EcoTaxa project into _site_project-<id> (never
# published by `quarto publish`, which uses _site). Runs on the MINI, inside tmux; started by
# scripts/launch_project.sh, which leaves a login token in ~/.cache/ecotaxa_token_prj<id> (mode 600).
# The token is read into the environment and the file is deleted before anything renders.
#
#   render_project.sh <project_id> [first_notebook]     e.g. render_project.sh 4 04
#
# writes data/run_project-<id>.log and data/run_project-<id>.done ("ok" or "failed") so another
# session can tell finished from stuck without attaching.
PID=${1:?project id}; FROM=${2:-01}
cd ~/Github/oceanmetrics/ecotaxa-demo || exit 1
TF=~/.cache/ecotaxa_token_prj$PID
if [[ -f $TF ]]; then export ECOTAXA_TOKEN="$(cat $TF)"; rm -f $TF; fi
export ECOTAXA_PROJECT_ID=$PID
mkdir -p data; touch data/.metadata_never_index   # keep Spotlight off a million images
LOG=data/run_project-$PID.log; DONE=data/run_project-$PID.done; rm -f $DONE
for f in 01_export_ecotaxa 02_taxa_worms 03_publish_to-obis 04_archive_to-ncei; do
  [[ ${f%%_*} < $FROM ]] && continue
  echo "== $(date -u +%FT%TZ) start $f" >> $LOG
  if ! quarto render $f.qmd --output-dir _site_project-$PID >> $LOG 2>&1; then
    echo "== $(date -u +%FT%TZ) FAILED $f" >> $LOG; echo failed > $DONE; exit 1
  fi
  echo "== $(date -u +%FT%TZ) done $f" >> $LOG
done
echo ok > $DONE
