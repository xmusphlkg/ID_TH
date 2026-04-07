# Temporal Utility Validation

This attachment evaluates whether framework-based review queues created at fixed decision freeze points better captured later disease-level review needs than an incidence-only comparator.
Two freeze points were evaluated: 2023-12-01 and 2024-06-01.
The largest capture gain occurred at the 2023-12-01 freeze point, where the framework captured 17 later review-needing diseases versus 6 under incidence-only review.

**Freeze-point summary**
| Freeze point | Validation end | Later review diseases | Framework captured | Incidence-only captured | Averted under-triage | Framework accuracy | Incidence-only accuracy |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 2023-12-01 | 2024-12-01 | 17 | 17 | 6 | 11 | 0.958 | 0.542 |
| 2024-06-01 | 2025-12-01 | 15 | 13 | 4 |  9 | 0.750 | 0.542 |

![Temporal utility validation figure](./Supplementary%20Appendix%201_7/temporal_utility_validation.png)

Source files:
- `./Tables/Temporal_utility_validation.xlsx`
- `./Tables/Temporal_utility_freeze_summary.csv`
- `./Tables/Temporal_utility_disease_level.csv`
