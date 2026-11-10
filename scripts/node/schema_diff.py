"""schema_diff.py: compare an EcoTaxa node's live database with the back end's full model metadata.

Read-only. Run INSIDE the back-end container, which has the code, the config and the DB connection:

    ssh msens 'docker exec -i -w /app ecotaxa-back python3 -' < scripts/node/schema_diff.py

Every module under DB/ is imported first, because `manage.py build` creates only the tables of
the modules that happen to be imported (how taxo_recast and projects_variables went missing on
msens1, 2026-11-10). An empty report, or only the three `modify_nullable` lines on the
*_old_2_new primary keys (a model artifact), means the schema matches the code.
"""
import sys, importlib, pkgutil, pprint
sys.path.insert(0, "/app")
import DB
for m in pkgutil.iter_modules(DB.__path__):
    try: importlib.import_module("DB." + m.name)
    except Exception as e: print("skip module", m.name, type(e).__name__)
from DB.Project import Project
from alembic.migration import MigrationContext
from alembic.autogenerate import compare_metadata
eng = None
try:
    from API_operations.helpers.Service import Service
    from helpers.AppConfig import Config
    conn = Service.build_connection(Config()); eng = conn.engine
except Exception as e:
    print("connection via Service failed:", e); raise
with eng.connect() as c:
    diff = compare_metadata(MigrationContext.configure(c), Project.metadata)
kinds = {}
for d in diff:
    d0 = d[0] if isinstance(d, tuple) else d[0][0]
    kinds.setdefault(d0, []).append(d)
for k, v in kinds.items():
    print("==", k, len(v))
    for x in v[:15]:
        s = repr(x); print("  ", s[:260])
