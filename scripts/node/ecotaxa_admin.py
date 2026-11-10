"""ecotaxa_admin.py: admin calls against an EcoTaxa node's REST API, run ON the node's host.

Credentials never leave the host: the admin login is read from the stack's .env
(ECOTAXA_ADMIN_EMAIL, ECOTAXA_ADMIN_PASSWORD), and nothing here prints a password.
Standard library only, so it runs from stdin with no install:

    ssh msens 'python3 -I - <command> [options]' < scripts/node/ecotaxa_admin.py

Commands (see 05_migrate_from-ifremer.qmd for the order they were used in):
    token                          print a login JWT on stdout (to hand to a render, never to a file in git)
    create-project  --title T --instrument I [--access 0|1|2] [--manager-id N ...] [--contact-id N]
                                   create the project if no project has that title, then set managers/contact
    import          --project P --source PATH  start a file import from PATH (relative to the node's /file_srv)
    job             --id J [--wait] [--max-hours H]   show a job; --wait polls until it ends or asks a question
    delete-job      --id J         DELETE /jobs/J (a running job is marked Killed; a finished one is removed)
    users           --csv FILE     create INACTIVE placeholder accounts (status 0, random unsaved password)
                                   from a CSV with columns name,email (keep that CSV out of git: it holds emails)
    set-access      --project P --access 0|1|2   0 private, 1 public (login needed), 2 open (anonymous read)
    verify          --project P    project record, taxo stats, one object query, anonymous access, one image
"""
import argparse, csv, json, secrets, sys, time
import urllib.error as E
import urllib.parse as Q
import urllib.request as U

ENV_PATH = "/share/ecotaxa/.env"
BASE = "https://ecotaxa.oceanmetrics.io"


def env(path=ENV_PATH):
    out = {}
    for line in open(path):
        line = line.strip()
        if "=" in line and not line.startswith("#"):
            k, v = line.split("=", 1)
            out[k] = v
    return out


def call(method, path, body=None, token=None, timeout=120):
    r = U.Request(BASE + "/api" + path, method=method,
                  data=None if body is None else json.dumps(body).encode(),
                  headers={"Content-Type": "application/json",
                           **({"Authorization": "Bearer " + token} if token else {})})
    try:
        with U.urlopen(r, timeout=timeout) as f:
            return f.status, json.loads(f.read() or "null")
    except E.HTTPError as e:
        return e.code, e.read().decode()[:800]


def login():
    e = env()
    s, tok = call("POST", "/login", {"username": e["ECOTAXA_ADMIN_EMAIL"], "password": e["ECOTAXA_ADMIN_PASSWORD"]})
    if s != 200 or not isinstance(tok, str):
        sys.exit("login failed: HTTP %s" % s)
    return tok


def show_job(job):
    return {k: job.get(k) for k in ["id", "type", "state", "progress_pct", "progress_msg", "question", "errors"]}


def cmd_token(a):
    sys.stdout.write(login() + "\n")


def cmd_create_project(a):
    tok = login()
    s, found = call("GET", "/projects/search?also_others=true&title_filter=" + Q.quote(a.title), token=tok)
    same = [p for p in (found if s == 200 else []) if p["title"] == a.title]
    if same:
        pid = same[0]["projid"]; print("exists", pid)
    else:
        s, pid = call("POST", "/projects/create", {"title": a.title, "instrument": a.instrument,
                                                   "visible": True, "access": a.access}, token=tok)
        if s != 200: sys.exit("create failed: %s %s" % (s, pid))
        print("created", pid)
    s, p = call("GET", "/projects/%d" % pid, token=tok)
    for uid in a.manager_id or []:
        if uid not in [m["id"] for m in p["managers"]]:
            s, u = call("GET", "/users/%d" % uid, token=tok); p["managers"].append(u)
    if a.contact_id:
        s, p["contact"] = call("GET", "/users/%d" % a.contact_id, token=tok)
    s, r = call("PUT", "/projects/%d" % pid, p, token=tok); print("update", s, r)
    s, p = call("GET", "/projects/%d" % pid, token=tok)
    print({k: p.get(k) for k in ["projid", "title", "instrument", "access"]},
          "managers", [m["email"] for m in p["managers"]], "contact", (p.get("contact") or {}).get("email"))


def cmd_import(a):
    tok = login()
    s, r = call("POST", "/file_import/%d" % a.project, {"source_path": a.source}, token=tok)
    print("import", s, r)


def cmd_job(a):
    t0, last = time.time(), None
    while True:
        tok = login()  # a fresh token each poll: an import can outlive a token
        s, job = call("GET", "/jobs/%d/" % a.id, token=tok)
        if s != 200: sys.exit("job %d: HTTP %s %s" % (a.id, s, job))
        line = json.dumps(show_job(job))
        if line != last: print(time.strftime("%H:%M:%S"), line, flush=True); last = line
        if not a.wait or job.get("state") in ("A", "E", "F"): break
        if time.time() - t0 > a.max_hours * 3600: sys.exit("still running after %s h" % a.max_hours)
        time.sleep(60)


def cmd_delete_job(a):
    tok = login()
    print("delete", a.id, call("DELETE", "/jobs/%d" % a.id, token=tok))
    s, job = call("GET", "/jobs/%d/" % a.id, token=tok)
    print("now", s, show_job(job) if s == 200 else job)


def cmd_users(a):
    tok = login()
    for row in csv.DictReader(open(a.csv)):
        s, r = call("POST", "/users/create", {
            "id": -1, "email": row["email"], "name": row["name"],
            "password": secrets.token_urlsafe(32) + "aA1!",  # never stored, never printed
            "organisation": row.get("organisation") or "not recorded",
            "country": row.get("country") or "not recorded",
            "usercreationreason": row.get("reason") or "annotator carried over from a project backup; inactive placeholder",
            "status": 0, "mail_status": False}, token=tok)
        print(row["name"], s, "" if s == 200 else r, flush=True)


def cmd_set_access(a):
    tok = login()
    s, p = call("GET", "/projects/%d" % a.project, token=tok)
    p["access"] = a.access
    print("update", call("PUT", "/projects/%d" % a.project, p, token=tok))
    s, p = call("GET", "/projects/%d" % a.project, token=tok)
    print({k: p.get(k) for k in ["projid", "access", "license"]})


def cmd_verify(a):
    tok = login(); P = a.project
    s, p = call("GET", "/projects/%d" % P, token=tok)
    print({k: p.get(k) for k in ["projid", "title", "access", "objcount", "pctvalidated", "pctclassified"]},
          [m["email"] for m in p["managers"]], (p.get("contact") or {}).get("email"))
    s, st = call("GET", "/project_set/taxo_stats?ids=%d" % P, token=tok)
    print("taxo_stats", s, {k: (len(v) if k == "used_taxa" else v) for k, v in st[0].items()} if s == 200 else st)
    s, q = call("POST", "/object_set/%d/query?fields=img.file_name&window_size=1&window_start=0" % P, {}, token=tok)
    print("object query", s)
    s, anon = call("GET", "/projects/%d" % P); print("anonymous read", s)
    if s == 200 and isinstance(q, dict) and q.get("details"):
        u = BASE + "/vault/" + q["details"][0][0]
        try:
            with U.urlopen(u, timeout=30) as r: print("image", r.status, r.headers.get("content-type"))
        except Exception as e: print("image", e)


def main(argv):
    ap = argparse.ArgumentParser(prog="ecotaxa_admin.py")
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("token")
    c = sub.add_parser("create-project"); c.add_argument("--title", required=True); c.add_argument("--instrument", required=True)
    c.add_argument("--access", default="1", choices=["0", "1", "2"]); c.add_argument("--manager-id", type=int, action="append")
    c.add_argument("--contact-id", type=int)
    c = sub.add_parser("import"); c.add_argument("--project", type=int, required=True); c.add_argument("--source", required=True)
    c = sub.add_parser("job"); c.add_argument("--id", type=int, required=True); c.add_argument("--wait", action="store_true")
    c.add_argument("--max-hours", type=float, default=8)
    c = sub.add_parser("delete-job"); c.add_argument("--id", type=int, required=True)
    c = sub.add_parser("users"); c.add_argument("--csv", required=True)
    c = sub.add_parser("set-access"); c.add_argument("--project", type=int, required=True); c.add_argument("--access", required=True, choices=["0", "1", "2"])
    c = sub.add_parser("verify"); c.add_argument("--project", type=int, required=True)
    a = ap.parse_args(argv)
    {"token": cmd_token, "create-project": cmd_create_project, "import": cmd_import, "job": cmd_job,
     "delete-job": cmd_delete_job, "users": cmd_users, "set-access": cmd_set_access, "verify": cmd_verify}[a.cmd](a)


if __name__ == "__main__":
    main(sys.argv[1:])
