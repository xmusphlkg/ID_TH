# Transform and Rate Sensitivity

This attachment evaluates whether the RP/BP recovery classifications are sensitive to the positive-support transform or to replacing monthly counts with all-age incidence rates.

**Summary of sensitivity scenarios**
| Config | DiseasesAssessed | PhenotypeChanged | StatusChanged | MaterialTimingShift | MedianAbsRPShift | MedianAbsBPShift | MaxAbsRPShift | MaxAbsBPShift | MeanDeltaPrRP | MeanDeltaPrBP |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Count log | 24 | 3 | 3 | 4 | 0 | 1 | 16 | 12 | -0.127 | -0.107 |
| Rate sqrt | 24 | 4 | 4 | 8 | 0 | 2 | 12 | 15 | -0.036 | -0.111 |

**Diseases with phenotype changes or >=6-month timing shifts**
| Shortname | Config | Baseline phenotype | Sensitivity phenotype | RP month delta | BP month delta | Delta Pr(RP) | Delta Pr(BP) | Delta primary-status probability |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Amebiasis | Rate sqrt | Balanced | Recovered but not balanced |   6 | NA |  0.043 | -0.333 | -0.173 |
| Chancroid | Count log | Recovered but not balanced | Suppressed | NA | NA | -0.331 | -0.219 | -0.437 |
| Shigellosis | Count log | Recovered but not balanced | Suppressed | NA | NA | -0.170 | -0.021 |  0.066 |
| Syphilis | Count log | Balanced | Suppressed | NA | NA | -0.054 | -0.090 |  0.165 |
| Influenza | Rate sqrt | Recovered but not balanced | Balanced |  -1 | NA | NA | NA | NA |
| Shigellosis | Rate sqrt | Recovered but not balanced | Suppressed | NA | NA | -0.175 | -0.146 |  0.096 |
| Syphilis | Rate sqrt | Balanced | Recovered but not balanced |   2 | NA | -0.099 | -0.140 | -0.130 |
| CA (HPV) | Count log | Balanced | Balanced |   8 | 12 | -0.261 | -0.308 | -0.308 |
| Dengue fever | Count log | Recovered but not balanced | Recovered but not balanced |   7 | NA | -0.013 |  0.006 | -0.018 |
| Leptospirosis | Count log | Balanced | Balanced |  16 |  0 |  0.042 |  0.057 |  0.057 |
| Mumps | Count log | Recovered but not balanced | Recovered but not balanced | -10 | NA | NA | NA | NA |
| Chickenpox | Rate sqrt | Recovered but not balanced | Recovered but not balanced | -12 | NA | NA | NA | NA |
| Dengue fever | Rate sqrt | Recovered but not balanced | Recovered but not balanced |   7 | NA |  0.072 | -0.009 |  0.082 |
| HCV | Rate sqrt | Balanced | Balanced |   3 |  6 | NA | NA | NA |
| HFMD | Rate sqrt | Balanced | Balanced |   0 | 15 | NA | NA | NA |
| Melioidosis | Rate sqrt | Balanced | Balanced |   0 |  8 | NA | NA | NA |
| S. suis | Rate sqrt | Balanced | Balanced |   0 | 13 | NA | NA | NA |
| Scrub Typhus | Rate sqrt | Balanced | Balanced |   1 | 11 | NA | NA | NA |

Source files:
- `./Tables/Transform_rate_sensitivity.xlsx`
- `./Tables/Transform_rate_sensitivity_summary.csv`
- `./Tables/Transform_rate_sensitivity_comparison.csv`
