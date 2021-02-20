# Submission notes

Updates to remove minor warnings and to ensure compatibility with forthcoming quanteda v3.

This new submission comes so soon after submitting 0.92 this morning, because I mistakenly submitted the wrong branch.

# Checks

## Test environments

* local macOS 10.15.7, R 4.0.3
* Ubuntu 18.04 LTS and 18.10, R 4.0.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.


## Dependency conflicts

None: This release causes no breaks in other packages, as checked via `revdepcheck::revdepcheck()`.
