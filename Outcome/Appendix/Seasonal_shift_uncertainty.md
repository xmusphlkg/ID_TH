# Seasonal Shift Uncertainty

This attachment adds bootstrap uncertainty to the center-of-mass seasonal shift metric and compares the main spline-based weekly-to-monthly reconstruction with a simpler day-allocation baseline for the late follow-up period.

**Bootstrap summary for center-of-mass shifts**
| Shortname | Point shift vs pre | 95% CI vs pre | Pr(|shift|>=2) vs pre | Point shift vs pred | 95% CI vs pred | Pr(|shift|>=2) vs pred | Borderline | COM/max agree |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CA (HPV) |  4 | 0.00 to 6.00 | 0.904 |  4 | 2.00 to 5.00 | 1.000 | Yes | No |
| Genital herpes |  2 | 0.00 to 3.00 | 0.762 |  3 | 2.00 to 3.00 | 1.000 | Yes | Yes |
| HAV | -5 | -6.00 to 5.00 | 0.990 | -5 | -6.00 to 5.00 | 1.000 | Yes | Yes |
| HBV |  2 | 2.00 to 3.00 | 1.000 |  2 | 2.00 to 2.00 | 1.000 | Yes | Yes |
| Melioidosis | -2 | -3.00 to 0.00 | 0.747 | -2 | -2.00 to 0.00 | 0.734 | Yes | Yes |
| Mumps |  6 | -5.00 to 6.00 | 0.992 |  4 | 2.00 to 6.00 | 1.000 | Yes | Yes |
| Rubella |  3 | -4.00 to 5.00 | 0.844 |  2 | -6.00 to 3.00 | 1.000 | Yes | Yes |
| Syphilis | -2 | -5.00 to 1.00 | 0.536 | -3 | -5.00 to -1.00 | 0.915 | Yes | Yes |
| Amebiasis |  0 | -2.00 to 1.00 | 0.139 |  0 | -1.00 to 1.00 | 0.000 | Yes | No |
| Chancroid |  0 | -1.00 to 2.00 | 0.046 |  0 | 0.00 to 1.00 | 0.000 | Yes | No |
| Chickenpox | -1 | -5.00 to 6.00 | 0.366 | -1 | -5.00 to 6.00 | 0.366 | Yes | Yes |
| Gonorrhoea |  0 | -1.00 to 2.00 | 0.069 |  1 | 0.00 to 2.00 | 0.170 | Yes | Yes |
| Influenza |  1 | -2.00 to 2.00 | 0.158 |  0 | -2.00 to 1.00 | 0.153 | Yes | Yes |
| Scarlet fever | -1 | -3.00 to 5.00 | 0.470 | -1 | -5.00 to 4.00 | 0.294 | Yes | Yes |
| Shigellosis |  1 | 0.00 to 2.00 | 0.142 |  1 | 0.00 to 2.00 | 0.145 | Yes | No |
| Typhoid |  0 | -2.00 to 2.00 | 0.144 | -1 | -3.00 to 0.00 | 0.382 | Yes | Yes |
| HCV |  4 | 3.00 to 5.00 | 1.000 |  4 | 3.00 to 5.00 | 1.000 | No | Yes |
| Dengue fever |  0 | -1.00 to 1.00 | 0.007 |  0 | -1.00 to 1.00 | 0.000 | No | Yes |
| HFMD |  1 | 0.00 to 1.00 | 0.000 |  0 | 0.00 to 0.00 | 0.000 | No | Yes |
| Leptospirosis |  1 | 0.00 to 1.00 | 0.000 |  1 | 0.00 to 1.00 | 0.000 | No | Yes |
| Malaria |  0 | 0.00 to 0.00 | 0.000 |  0 | 0.00 to 0.00 | 0.000 | No | Yes |
| Pneumonia |  1 | 0.00 to 1.00 | 0.019 |  1 | 0.00 to 1.00 | 0.000 | No | Yes |
| S. suis |  0 | -1.00 to 1.00 | 0.009 |  0 | -1.00 to 1.00 | 0.000 | No | Yes |
| Scrub Typhus |  0 | 0.00 to 0.00 | 0.000 |  0 | 0.00 to 0.00 | 0.000 | No | Yes |

**Queue changes under alternative weekly-to-monthly reconstruction**
| Result | Value |
| --- | --- |
| Queue changes detected | None |

Source files:
- `./Tables/Seasonal_shift_uncertainty.xlsx`
- `./Tables/Seasonal_shift_bootstrap_summary.csv`
- `./Tables/Seasonal_shift_reconstruction_sensitivity.csv`
