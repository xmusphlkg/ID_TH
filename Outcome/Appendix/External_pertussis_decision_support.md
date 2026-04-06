# External Pertussis Decision-Support Case Study

This attachment applies the same disease-specific model-selection logic used in the main manuscript to six external pertussis surveillance series with mixed reporting cadence.
Selected best models were: Australia = Neural Network; China = ARIMA + Fourier; Japan = ARIMA + Fourier; New Zealand = ARIMA + Fourier; Sweden = ARIMA + Fourier; United States = SARIMA.
Across all six countries, sustained incidence-only normalization still preceded cumulative balance, with decision-discordance windows ranging from 6.5 months to 25.0 months and a median of 14.5 months.
4 countries (Australia, China, Japan, New Zealand) reached cumulative balance within follow-up, whereas the remaining countries stayed cumulatively unresolved through the end of 2025.

**Country-level median and 95% PI summary**
| Country | Cadence | Best model | Follow-up median forecast | Follow-up median 95% PI | End date | End forecast median | End 95% PI | Normalization date | Balance date |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Australia | Monthly surveillance | Neural Network |  978.9 | 694.9 to 1831.6 | 2025-12-01 |  981.7 | 694.8 to 1858.9 | 2024-03-01 | 2024-12-01 |
| China | Monthly surveillance | ARIMA + Fourier | 1916.4 | 191.2 to 5413.1 | 2025-12-01 | 1346.3 | 0 to 6502.6 | 2021-12-01 | 2024-01-01 |
| Japan | Weekly surveillance | ARIMA + Fourier |  217.2 | 23.2 to 551.3 | 2025-12-22 |  165.4 | 0 to 636.8 | 2025-01-20 | 2025-08-04 |
| New Zealand | Monthly surveillance | ARIMA + Fourier |   35.5 | 0 to 700.3 | 2025-12-01 |   56.9 | 0 to 1259.7 | 2024-05-01 | 2024-12-01 |
| Sweden | Monthly surveillance | ARIMA + Fourier |   58.3 | 29.6 to 109.5 | 2025-12-01 |   74.7 | 39.9 to 131.5 | 2024-03-01 | Not reached |
| United States | Weekly surveillance | SARIMA |  122.3 | 24.4 to 300 | 2025-12-28 |  121.7 | 4.7 to 376.3 | 2024-04-28 | Not reached |

![External pertussis decision-support figure](./Supplementary%20Appendix%201_6/external_pertussis_decision_support.png)

Source files:
- `/home/mpi/ID_TH/Outcome/Appendix/Tables/External_pertussis_decision_support.xlsx`
- `/home/mpi/ID_TH/Outcome/Appendix/Tables/External_pertussis_decision_support_summary.csv`
- `/home/mpi/ID_TH/Outcome/Appendix/Tables/External_pertussis_model_cv_metrics.csv`
- `/home/mpi/ID_TH/Outcome/Appendix/Tables/External_pertussis_best_model_forecasts.csv`
- `/home/mpi/ID_TH/Outcome/Appendix/Tables/External_pertussis_country_median_pi_summary.csv`
