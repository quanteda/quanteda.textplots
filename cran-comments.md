# Submission notes

Fixes problems from 0.94.4 that led to the package being archived, because the authors were either in the process of moving countries and taking on a new job, on summer holidays, or frantically trying to fix UBSAN problems in the main **quanteda** package.

# Checks

## Test environments

* local macOS 14.4.1, R 4.4.1, and via devtools::check_mac_release()
* Ubuntu 22.04 LTS, R 4.4.1
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.

## Dependency conflicts

None: This release causes no breaks in other packages, as checked via `revdepcheck::revdepcheck()`.
