# Submission notes

Update to avoid breaking test with the forthcoming release of Matrix 1.4-1.

# Checks

## Test environments

* local macOS 12.3, R 4.1.3
* Ubuntu 20.04 LTS, R 4.1.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.

## Dependency conflicts

None: This release causes no breaks in other packages, as checked via `revdepcheck::revdepcheck()`.
