# scripts

Everything run by hand to migrate a project and run the notebooks on it, so the next one is reproducible.
The order and the reasons are in [`05_migrate_from-ifremer.qmd`](../05_migrate_from-ifremer.qmd).

| Script | Runs on | Purpose |
|---|---|---|
| `fetch_backup.sh <drive_id> <dir>` | laptop | stream a backup zip from Drive to the node host (the Drive token stays on the laptop), unzip under `file_srv` |
| `node/ecotaxa_admin.py <command>` | node host, from stdin: `ssh msens 'python3 -I - <command>' < scripts/node/ecotaxa_admin.py` | `token`, `create-project`, `import`, `job`, `delete-job`, `users`, `set-access`, `verify`; the admin login is read from `/share/ecotaxa/.env` and never printed |
| `node/schema_diff.py` | inside `ecotaxa-back`: `ssh msens 'docker exec -i -w /app ecotaxa-back python3 -' < scripts/node/schema_diff.py` | read-only: live DB versus the back end's models |
| `node/2026-11-10_schema_to_head.sql` | `ecotaxa-pgdb` | record of the one-off schema fix of 2026-11-10, with its runbook; do not re-run |
| `launch_project.sh <id> [from]` | laptop | token from the node host to the mini (mode-600 file, deleted on start), then `render_project.sh` in tmux `om:prj<id>` |
| `render_project.sh <id> [from]` | mini | render 01–04 (or from `from`) into `_site_project-<id>`, with a log and a `.done` marker under `data/` |

People's names and emails for `ecotaxa_admin.py users` go in `scripts/node/users_<project>.csv`, which is
git-ignored.
