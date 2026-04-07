# Placebo Interruption and Calibration

This attachment evaluates whether the best-model forecasting pipeline produces spurious operational alerts when the same RP/BP workflow is applied to pre-pandemic placebo interruption dates, and whether predictive interval coverage remains calibrated in those placebo windows.
Across the three placebo windows, the lowest false-alert count occurred for Pseudo interruption 2017, with 15 false alerts and mean 95% interval coverage of 96.2%.

**Portfolio-level placebo summary**
| Pseudo interruption | Diseases assessed | False alerts | False suppressed | Mean 80% coverage | Mean 95% coverage | Mean 95% interval width |
| --- | --- | --- | --- | --- | --- | --- |
| Pseudo interruption 2019 | 24 | 17 | 13 | 0.875 | 0.951 |  6781.3 |
| Pseudo interruption 2018 | 24 | 17 |  3 | 0.911 | 0.988 |  8454.6 |
| Pseudo interruption 2017 | 24 | 15 |  7 | 0.875 | 0.962 | 11794.4 |

![Falsification and calibration figure](./Supplementary%20Appendix%201_8/falsification_and_calibration.png)

Source files:
- `./Tables/Falsification_and_calibration.xlsx`
- `./Tables/Placebo_interruption_portfolio_summary.csv`
- `./Tables/Placebo_interruption_disease_summary.csv`
- `./Tables/Placebo_interruption_split_level.csv`
