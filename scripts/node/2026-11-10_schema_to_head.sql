-- 2026-11-10_schema_to_head.sql: bring the msens1 EcoTaxa DB to the back end's alembic head
--
-- Applied once, 2026-11-10 10:3x UTC, on msens1 (ecotaxa/ecotaxa_back:v3.4.0), with Ben's authorization.
-- Kept as the record of what was run; do not re-run on a DB that already has these tables.
--
-- Why: the DB was built with `manage.py build` (SQLAlchemy create_all), which creates only the tables of
-- the model modules that happen to be imported, and stamps no alembic version. taxo_recast and
-- projects_variables (plus the empty *_old_2_new id maps) were missing, so every general export failed
-- with `relation "taxo_recast" does not exist`. scripts/node/schema_diff.py showed nothing else differed.
--
-- Order, old to new, from each migration's own SQL (`alembic upgrade --sql <prev>:<rev>`, inside ecotaxa-back):
--   1. backup:   docker exec ecotaxa-pgdb pg_dump -U postgres -Fc ecotaxa > /share/ecotaxa/backups/ecotaxa_pre-schema-fix_<UTC>.dump
--   2. this file: docker exec -i ecotaxa-pgdb psql -U postgres -d ecotaxa -v ON_ERROR_STOP=1 --single-transaction < this.sql
--   3. stamp:    docker exec -w /app/cmds -e PYTHONPATH=.. ecotaxa-back alembic stamp head
--   4. upgrade:  docker exec -w /app/cmds -e PYTHONPATH=.. ecotaxa-back alembic upgrade head   (a no-op: nothing pending)
--   5. check:    scripts/node/schema_diff.py -> only 3 modify_nullable on *_old_2_new primary keys (model artifact)
-- The upstream README's grant to readerole did not apply: there is no such role on this node.

-- 521c25353fa0 new_report_tables (projects_variables part; obj_field_acquisid_objfid_idx already present)
CREATE TABLE projects_variables (
    project_id INTEGER NOT NULL,
    subsample_coef VARCHAR,
    total_water_volume VARCHAR,
    individual_volume VARCHAR,
    PRIMARY KEY (project_id),
    FOREIGN KEY(project_id) REFERENCES projects (projid) ON DELETE CASCADE
);
-- 34d91185174c taxo_recast aka remapping storage
CREATE TABLE taxo_recast (
    recast_id INTEGER GENERATED ALWAYS AS IDENTITY,
    collection_id INTEGER,
    project_id INTEGER,
    operation VARCHAR(16) NOT NULL,
    transforms JSONB NOT NULL,
    documentation JSONB NOT NULL,
    PRIMARY KEY (recast_id),
    FOREIGN KEY(collection_id) REFERENCES collection (id) ON DELETE CASCADE,
    FOREIGN KEY(project_id) REFERENCES projects (projid) ON DELETE CASCADE
);
-- ca50a945b178: its two FKs are the ones declared just above; the model's column is VARCHAR(32)
ALTER TABLE taxo_recast ALTER COLUMN operation TYPE VARCHAR(32);
-- 5d49f4994e0c partition: only the (empty) id-mapping tables; the rest is already in the schema, and the
-- migration itself refuses to run on a DB with objects
CREATE TABLE acqid_old_2_new (old_id INTEGER NOT NULL, new_id BIGINT NOT NULL, PRIMARY KEY (old_id));
CREATE INDEX acquisid_mapping_idx  ON acqid_old_2_new (new_id) INCLUDE (old_id);
CREATE INDEX acquisid_mapping_idx2 ON acqid_old_2_new (old_id) INCLUDE (new_id);
CREATE TABLE objid_old_2_new (old_id BIGINT NOT NULL, new_id BIGINT NOT NULL, PRIMARY KEY (old_id));
CREATE UNIQUE INDEX objid_mapping_new_to_old ON objid_old_2_new (new_id) INCLUDE (old_id);
CREATE UNIQUE INDEX objid_mapping_old_to_new ON objid_old_2_new (old_id) INCLUDE (new_id);
CREATE TABLE samid_old_2_new (old_id INTEGER NOT NULL, new_id BIGINT NOT NULL, PRIMARY KEY (old_id));
