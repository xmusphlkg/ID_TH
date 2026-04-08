# Transform and Rate Sensitivity

This attachment evaluates whether the RP/BP recovery classifications are sensitive to the positive-support transform or to replacing monthly counts with all-age incidence rates.

**Summary of sensitivity scenarios**
| Config | DiseasesAssessed | PhenotypeChanged | StatusChanged | MaterialTimingShift | MedianAbsRPShift | MedianAbsBPShift | MaxAbsRPShift | MaxAbsBPShift | MeanDeltaPrRP | MeanDeltaPrBP |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Count log | 24 | 2 | 2 | 2 | 0 | 1 | 16 |  5 | -0.119 | -0.113 |
| Rate sqrt | 24 | 3 | 3 | 7 | 0 | 3 | 12 | 16 | -0.041 | -0.091 |

**Diseases with phenotype changes or >=6-month timing shifts**
| Shortname | Config | Baseline phenotype | Sensitivity phenotype | RP month delta | BP month delta | Delta Pr(RP) | Delta Pr(BP) | Delta primary-status probability |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Amebiasis | Rate sqrt | Balanced | Recovered but not balanced |   6 | NA | -0.002 | -0.338 | -0.213 |
| Chancroid | Count log | Recovered but not balanced | Suppressed | NA | NA | -0.296 | -0.209 | -0.472 |
| Syphilis | Count log | Balanced | Suppressed | NA | NA | -0.139 | -0.135 |  0.195 |
| Influenza | Rate sqrt | Recovered but not balanced | Balanced |  -1 | NA | NA | NA | NA |
| Shigellosis | Rate sqrt | Recovered but not balanced | Suppressed | NA | NA | -0.265 | -0.156 |  0.186 |
| Leptospirosis | Count log | Balanced | Balanced |  16 |  0 |  0.042 |  0.072 |  0.072 |
| Mumps | Count log | Recovered but not balanced | Recovered but not balanced | -10 | NA | NA | NA | NA |
| Chickenpox | Rate sqrt | Recovered but not balanced | Recovered but not balanced | -12 | NA | NA | NA | NA |
| HCV | Rate sqrt | Balanced | Balanced |   3 |  6 | NA | NA | NA |
| HFMD | Rate sqrt | Balanced | Balanced |   0 | 16 | NA | NA | NA |
| Melioidosis | Rate sqrt | Balanced | Balanced |   0 |  8 | NA | NA | NA |
| S. suis | Rate sqrt | Balanced | Balanced |   0 | 13 | NA | NA | NA |
| Scrub Typhus | Rate sqrt | Balanced | Balanced |   1 | 11 | NA | NA | NA |

Source files:
- `./Tables/Transform_rate_sensitivity.xlsx`
- `./Tables/Transform_rate_sensitivity_summary.csv`
- `./Tables/Transform_rate_sensitivity_comparison.csv`
