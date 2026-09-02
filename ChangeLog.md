# Changelog for reggression

## 2.2.0

- Split-DB support: `db*` methods accept optional `fitDb` parameter; command strings embed `egraph_path:fit_path` paths
- `withBackendSplit` for dual e-graph/fit-DB connections
- `db-insert` command for content-addressed expression insertion with frontier marking
- `db-set-fit` command for recording fitness into per-dataset fit DB
- `db-eq-sat-frontier` command for re-saturating only recently-changed classes
- Python API: `dbFitFile=""` parameter on `EGP.__init__`, `eggp_run`, `eggp_run_data`
- SWIG binding updated for split-DB parameters
- Embedded RTS tuning: `hs_init_with_rtsopts` via `GHCRTS`/`+RTS` for runtime GC control

## 2.1.0

- Added `--ci` flag to `top`, `pareto`, `optimize`, `report`, `db-top`,
  `db-pareto` commands for profile-likelihood confidence intervals
- Python API: `ci=False` parameter on `top()`, `pareto()`, `optimize()`,
  `report()`, `dbTop()`, `dbPareto()`
- `importEqs` signature changed to `Maybe String` dataset (structural-only
  import when no dataset given)

## 2.0.0

- Initial release with DB-backed equality saturation and eggp integration
