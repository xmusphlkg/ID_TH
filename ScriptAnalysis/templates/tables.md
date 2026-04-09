## Supplementary Tables

<!-- BEGIN TABLE_S1 -->
### Table S1. Disease flow from 72 monitored series to the 43-disease descriptive analysis and 24-disease counterfactual analysis.

{{TABLE_S1_BODY}}

<!-- END TABLE_S1 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S2 -->
### Table S2. Excluded disease series and exclusion category.

{{TABLE_S2_BODY}}

These 29 excluded series were concentrated in overlapping surveillance categories (11), diseases outside the transmissible infectious-disease framework (6), ill-defined or residual categories (5), zero-incidence series (3), incompletely reported recent series (3), and one series with a structural surveillance-definition change.

<!-- END TABLE_S2 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S3 -->
### Table S3. Diseases retained in the 43-disease descriptive analysis but not modelled counterfactually, with direct reason for descriptive-only retention.

{{TABLE_S3_BODY}}

Across these 19 diseases, the main reasons for descriptive-only retention were insufficient pre-pandemic counts or sparse long-horizon signal (12 diseases), non-seasonal pre-pandemic structure (3 diseases), insufficient time coverage (2 diseases), and ill-defined residual categories (2 diseases). This pattern indicates that the 24-disease forecasting subset was selected primarily on time-series suitability rather than on a single transmission category, although vector-borne and respiratory pathogens remained differentially represented after this second-stage restriction. In this table and subsequent appendix tables, IDs in group labels denote infectious diseases.

<!-- END TABLE_S3 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S4 -->
### Table S4. Predictor definitions used in time-to-recovery analyses

{{TABLE_S4_BODY}}

This lookup table covers all 24 modelled diseases and supplied the disease-level predictors used in the recovery-timing analyses. Vaccine status was classified as unavailable for 12 diseases, optional for 7 diseases, and part of the national Expanded Program on Immunization (EPI) schedule for 5 diseases. In this first disease-metadata table, HAV denotes hepatitis A virus, HFMD denotes hand, foot, and mouth disease, CA (HPV) denotes condyloma acuminatum associated with human papillomavirus, HBV denotes hepatitis B virus, HCV denotes hepatitis C virus, *S. suis* denotes *Streptococcus suis*, and NA denotes not applicable.

<!-- END TABLE_S4 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S5 -->
### Table S5. Sensitivity of RP/BP classifications to alternative RP thresholds and persistence requirements.

{{TABLE_S5_BODY}}

These sensitivity checks were computed from the exported disease-specific outcome tables underlying Fig. 3. No disease changed RP/BP classification when the RP threshold was varied across 90%, 95%, and 100% with persistence requirements of 2, 3, or 4 months, indicating that the principal recovery typology was stable to plausible operational definition changes.

<!-- END TABLE_S5 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S6 -->
### Table S6. Summary metrics for overlap-period validation of weekly-to-monthly reconstruction.

{{TABLE_S6_BODY}}

These overlap-period validation summaries were recalculated directly from the disease-month comparison cache used to validate the weekly-to-monthly reconstruction. The refreshed cache retains 2064 disease-month pairs across 2020-2023.

<!-- END TABLE_S6 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S7 -->
### Table S7. Illustrative disease-specific overlap-period reconstruction error metrics.

{{TABLE_S7_BODY}}

High-burden diseases that materially contribute to the main analyses showed low relative reconstruction error across these 6 illustrative examples, whereas some low-count series had larger percentage error because small absolute monthly differences inflate relative measures. Together with the disease-specific visual comparisons in Part 1, these summaries support the robustness of the reconstructed monthly series for the principal RP/BP and seasonal analyses.

<!-- END TABLE_S7 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S8 -->
### Table S8. Legacy fixed-family and best-standard robustness comparators retained for reference.

{{TABLE_S8_BODY}}

These legacy comparator reruns are retained for reference only. The refreshed primary workflow instead emphasizes uncertainty propagation (**Table S9**), interruption-date sensitivity (**Table S10**), and alternative model-selection aggregation rules (**Table S11**).

<!-- END TABLE_S8 -->

<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S9 -->
### Table S9. Uncertainty-aware RP/BP classification from 5,000 simulated counterfactual trajectories.

{{TABLE_S9_BODY}}

Here, primary phenotype stability denotes the probability that the deterministic median-based phenotype was retained across the 5,000 simulated trajectories; values below 0.80 were treated as uncertainty-sensitive in the revised main-text review layer. Under that pragmatic flag, 9 diseases were uncertainty-sensitive: Scarlet fever, Dengue fever, Malaria, Leptospirosis, Amebiasis, Shigellosis, Syphilis, CA (HPV), Chancroid. For the `No deficit` phenotype, the reported RP/BP probabilities and timing summaries are descriptive outputs of the simulation workflow rather than decision-defining milestones.

<!-- END TABLE_S9 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S10 -->
### Table S10. Sensitivity of deterministic RP/BP classification to alternative interruption dates.

{{TABLE_S10_BODY}}

No disease changed classification when the analytical start date was moved from January 2020 to March 2020 or April 2020 (March changes: 0; April changes: 0), supporting the use of January 2020 as a pragmatic portfolio-level interruption anchor.

<!-- END TABLE_S10 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S11 -->
### Table S11. Alternative model-selection rules compared with the primary equal-weight composite rule.

{{TABLE_S11_BODY}}

Across the 24 diseases, the primary selected family was also recovered for 16 diseases under rank aggregation, 15 diseases under sMAPE-only selection, and 21 diseases under the horizon-weighted composite. Most disagreements were concentrated in a small subset of diseases rather than a single model family, suggesting that the principal conclusions were not driven by one aggregation formula. In this table, ETS denotes exponential-smoothing state-space, SARIMA denotes seasonal autoregressive integrated moving-average, TBATS denotes trigonometric seasonality, Box-Cox transformation, autoregressive moving-average errors, trend, and seasonal components, and ARIMA + Fourier denotes autoregressive integrated moving-average with Fourier terms.

<!-- END TABLE_S11 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S12 -->
### Table S12. Joint operational synthesis of recovery phenotype and seasonal displacement.

{{TABLE_S12_BODY}}

This joint table clarifies the retrospective decision utility of the framework. A monthly-incidence-only interpretation would have marked 22 diseases as monthly-normalized, but the integrated RP/BP-seasonality synthesis separated them into 8 low-priority routine-review cases, 6 cumulative-review cases, 6 recovered-but-recalibrate-seasonality cases, and 2 recalibrate-and-monitor cases, while 1 disease remained in high-priority manual review and 1 disease remained in no-deficit monitoring.

<!-- END TABLE_S12 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S13 -->
### Table S13. Alternative endpoint sensitivity analyses for the 24 modelled diseases. Month values are counted from January 2020, so month 0 corresponds to January 2020.

{{TABLE_S13_BODY}}

Among the 23 diseases that entered a sustained cumulative deficit, 18 re-entered the disease-specific 95% predictive interval for at least 3 months, 22 met the sustained observed-to-expected ratio endpoint of at least 1.0, and 18 halved their cumulative deficit by end follow-up. The ratio endpoint preserved the same achieved-versus-not-achieved distinction as the primary RP definition for all 23 deficit-entering diseases, whereas the half-deficit milestone was reached by 4 of the 8 recovered-but-not-balanced diseases.

<!-- END TABLE_S13 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S14 -->
### Table S14. External contextual triangulation period summary.

{{TABLE_S14_BODY}}

The portfolio-level observed-to-expected ratio rose across the restriction-intensive, transition, and post-PHSM periods. Policy indicators were available through December 2022, whereas WHO COVID-19 burden was available through June 2024.

<!-- END TABLE_S14 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S15 -->
### Table S15. External contextual triangulation: correlations with the portfolio observed-to-expected ratio.

{{TABLE_S15_BODY}}

Indicators are ranked by absolute Spearman rho. All rho values are negative, indicating that higher restriction or burden was associated with lower portfolio ratios. These correlations were treated as descriptive context and were not used as predictive covariates or interpreted causally.

<!-- END TABLE_S15 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S16 -->
### Table S16. External contextual triangulation: selected anchor months.

{{TABLE_S16_BODY}}

These milestone months were used to anchor the timing of portfolio suppression and normalization. They were not used as predictive covariates and do not support causal attribution.

<!-- END TABLE_S16 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S17 -->
### Table S17. Task-based heuristic assessment of the final dashboard build.

{{TABLE_S17_BODY}}

All six prespecified surveillance-review tasks were directly supported in the final build, with a mean heuristic score of 3.72 and median minimum interaction count of 1. This assessment documents functional interface coverage but should not be interpreted as a substitute for prospective end-user usability testing.

<!-- END TABLE_S17 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S18 -->
### Table S18. Country-level counterfactual median and 95% predictive-interval summary for the external pertussis case study.

{{TABLE_S18_BODY}}

This supplementary case study is included as a cross-setting transportability demonstration rather than external validation of the Thailand thresholds or a substitute for end-user testing in Thailand.

<!-- END TABLE_S18 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S19 -->
### Table S19. Freeze-point temporal utility validation summary.

{{TABLE_S19_BODY}}

This supplementary analysis compares framework-based and incidence-only review queues generated at fixed decision freeze points against realized disease trajectories in the subsequent follow-up window.

<!-- END TABLE_S19 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S20 -->
### Table S20. Portfolio-level placebo interruption and predictive-distribution calibration summary.

{{TABLE_S20_BODY}}

This supplementary analysis applies the RP/BP workflow to pre-pandemic placebo interruption dates, summarizes predictive-distribution calibration by empirical coverage and weighted interval score, and compares the primary deterministic rule with an exploratory tempered rule that requires corroboration from the lower 80% predictive interval before retaining a placebo alert.

<!-- END TABLE_S20 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S21 -->
### Table S21. Portfolio-level transform and denominator sensitivity summary.

{{TABLE_S21_BODY}}

This supplementary analysis holds the selected model family fixed for each disease and re-runs the recovery workflow under the primary square-root count specification, a log-transformed count specification, and a square-root incidence-rate specification using the linked annual population denominators.

<!-- END TABLE_S21 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S22 -->
### Table S22. Diseases with phenotype changes or material timing shifts in transform and denominator sensitivity analyses.

{{TABLE_S22_BODY}}

The disease-level outputs for all 24 modelled diseases are provided in `Tables/Transform_rate_sensitivity.xlsx`.

<!-- END TABLE_S22 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S23 -->
### Table S23. Bootstrap uncertainty for center-of-mass seasonal shift estimates.

{{TABLE_S23_BODY}}

This supplementary analysis adds bootstrap uncertainty intervals to the center-of-mass seasonal timing metric and tests whether a simpler day-allocation weekly-to-monthly reconstruction changes seasonal flags or operational queue assignment.

<!-- END TABLE_S23 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S24 -->
### Table S24. Queue changes under alternative weekly-to-monthly reconstruction.

{{TABLE_S24_BODY}}

Across 24 modelled diseases, the alternative reconstruction changed the seasonal shift flag for 0 disease(s) and the operational queue for 0 disease(s).

<!-- END TABLE_S24 -->
<div style="page-break-after: always;"></div>

<!-- BEGIN TABLE_S25 -->
### Table S25. Agreement between the primary BP rule and the segmented cumulative-deviation comparator.

{{TABLE_S25_BODY}}

This supplementary analysis fits an exploratory segmented linear comparator to the cumulative observed-minus-expected deviation trajectory for each disease, using the empirical trough as the knot and the post-trough fitted slope to estimate a complementary BP date. Across 24 modelled diseases, the segmented comparator agreed with the primary BP call within 6 months for 12 disease(s), both approaches left 10 disease(s) unresolved, and 2 disease(s) showed materially different BP timing.

<!-- END TABLE_S25 -->
<div style="page-break-after: always;"></div>
