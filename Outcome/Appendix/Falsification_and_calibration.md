# Placebo Interruption and Calibration

This attachment evaluates whether the best-model forecasting pipeline produces spurious operational alerts when the RP/BP workflow is applied to pre-pandemic placebo interruption dates, and whether the predictive distributions remain calibrated under the same placebo windows.
Across the three placebo windows, the deterministic median rule produced 15 to 17 false alerts (rate 62.5% to 70.8%), whereas the exploratory tempered rule produced 10 to 13 false alerts.
The lowest tempered false-alert burden occurred for Pseudo interruption 2018, with 10 false alerts, mean 95% empirical coverage of 79.2%, and mean WIS of 1822.88.

**Portfolio-level placebo summary**
| Pseudo interruption | Diseases assessed | False alerts | False-alert rate | Tempered false alerts | Tempered false-alert rate | Mean 80% coverage | Mean 95% coverage | Mean 95% interval score | Mean WIS |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Pseudo interruption 2019 | 24 | 17 | 0.708 | 13 | 0.542 | 0.611 | 0.726 | 25681.83 | 2967.054 |
| Pseudo interruption 2018 | 24 | 16 | 0.667 | 10 | 0.417 | 0.665 | 0.792 | 14527.48 | 1822.884 |
| Pseudo interruption 2017 | 24 | 15 | 0.625 | 12 | 0.500 | 0.568 | 0.764 | 22530.86 | 2681.696 |

![Falsification and calibration figure](./Supplementary%20Appendix%201_8/falsification_and_calibration.png)

Source files:
- `./Tables/Falsification_and_calibration.xlsx`
- `./Tables/Placebo_interruption_portfolio_summary.csv`
- `./Tables/Placebo_interruption_disease_summary.csv`
- `./Tables/Calibration_horizon_summary.csv`
- `./Tables/Placebo_interruption_group_summary.csv`
