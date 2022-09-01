# Submission notes

Update to avoid breaking tests with the forthcoming release of Matrix 1.4-2/1.5.

# Checks

## Test environments

* local macOS 12.5.1, R 4.2.1
* Ubuntu 20.04 LTS, R 4.2.1
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.

## Dependency conflicts

None: This release causes no breaks in other packages, as checked via `revdepcheck::revdepcheck()`.
