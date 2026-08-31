# Changelog for reggression

## 2.1.0

- Added `--ci` flag to `top`, `pareto`, `optimize`, `report`, `db-top`,
  `db-pareto` commands for profile-likelihood confidence intervals
- Python API: `ci=False` parameter on `top()`, `pareto()`, `optimize()`,
  `report()`, `dbTop()`, `dbPareto()`
- `importEqs` signature changed to `Maybe String` dataset (structural-only
  import when no dataset given)

## 2.0.0

- Initial release with DB-backed equality saturation and eggp integration
